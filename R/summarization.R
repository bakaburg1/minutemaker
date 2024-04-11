
#' Generate Recording Details
#'
#' This function generates a formatted string containing details about a
#' recording. The details include the type of talk/meeting, the session in which
#' it took place, the title of the talk/meeting, the main speakers, and the
#' moderators. All parameters are optional.
#'
#' @param agenda_element A list containing the details of the recording. If this
#'   is provided, the other parameters are ignored.
#' @param type The type of the talk/meeting, e.g., "conference talk",
#'   "workshop", "panel discussion", "conference welcome message", etc.
#' @param session The session in which the talk/meeting took place.
#' @param title The title of the talk/meeting.
#' @param description A description of the talk/meeting topic.
#' @param speakers The main speakers of the talk/meeting as a vector.
#' @param moderators The moderators of the talk/meeting as a vector.
#'
#' @return A formatted string containing the details of the recording, or NULL
#'   if all parameters are NULL.
#'
#' @export
generate_recording_details <- function(
    agenda_element = NULL,
    type = NULL,
    session = NULL,
    title = NULL,
    description = NULL,
    speakers = NULL,
    moderators = NULL
) {

  # If agenda_element is not NULL, extract the information from it
  if (!is.null(agenda_element)) {
    type <- agenda_element$type
    session <- agenda_element$session
    title <- agenda_element$title
    description <- agenda_element$description
    speakers <- agenda_element$speakers
    moderators <- agenda_element$moderators
  }

  # Return NULL if all the arguments are NULL
  if (is.null(type) && is.null(session) && is.null(description) &&
      is.null(title) && is.null(speakers) && is.null(moderators)) {
    return(NULL)
  }

  # Helper function to add a section to the details string if it is not NULL
  add_section <- function(section, section_name) {
    if (!is.null(section)) {
      paste0(
        "- ", stringr::str_flatten_comma(section_name),
        ": ", stringr::str_flatten_comma(section), ";\n") # Moving \n in paste doesn't work for some reason
    }
  }

  # Build the details prompt based on which arguments are not NULL
  paste0(
    add_section(type, "Type (the type of this specific talk/meeting)"),
    add_section(session, "Session (the conference/larger meeting session in which this talk/meeting took place)"),
    add_section(title, "Title (the title of the talk/meeting)"),
    add_section(description, "Description (a description of the talk/meeting topic)"),
    add_section(speakers, "Speakers (the main speakers of this talk/meeting)"),
    add_section(moderators, "Moderators (the moderators of this talk/meeting)")
  ) |> trimws()
}

#' Summarise a recording transcript
#'
#' This function takes a transcript and various optional parameters, and uses an
#' LLM to generate a summary. For `diarization_instructions` and
#' `output_instructions`, the default prompts are used if they are not provided
#' (check them using `get_prompts()`). All the other parameters apart from
#' `transcript` are optional, and if not provided they will be ignored in the
#' final prompt.
#'
#' @param transcript_data A character vector or a data frame representing the
#'   transcript to be summarised. A data frame transcript with start and end
#'   segment times is necessary if the "rolling window" method is used.
#' @param method The method to use to summarise the transcript. Can be either
#'   "simple" or "rolling", with the first being faster and the second
#'   (theoritically) more accurate. If "rolling", the transcript is split into
#'   segments of `window_size` minutes and each segment is summarised separately
#'   and then aggregated in one summary. If "simple", the whole transcript is
#'   summarised in one go. Default is "rolling".
#' @param window_size The size of the window in minutes to use when summarising
#'   the transcript using the rolling window method. Default is 15.
#' @param output_length An indication to the LLM regarding the length of the
#'   output. Mind that the LLM will not respect this indication precisely, but
#'   this parameter will impact the length of the output. Default is 3 (pages).
#' @param event_description A description of the event.
#' @param recording_details Details on the specific recording. We suggest using
#'   `generate_recording_details` to generate this string.
#' @param audience A description the intended audience and the focus of the
#'   summarisation.
#' @param vocabulary A character vector of specific vocabulary words, names,
#'   definitions, to help the LLM recognise misspellings and abbreviations.
#' @param consider_diarization A logical indicating whether the summarisation
#'   should take into account the diarization (e.g. the speakers) of the
#'   transcript. Default is TRUE
#' @param summary_structure The expected structure of the summary. Run
#'   get_prompts("summary_structure") to see the default structure.
#' @param extra_diarization_instructions Diarization instructions, e.g., "Do not
#'   report the name of the speaker", "Use only the second name of the speaker",
#'   etc... These instructions will be added to the defaults, which can be
#'   visualized using get_prompts("diarization_template").
#' @param extra_output_instructions Additional instruction to tune the output of
#'   the summarisation. These instructions will be added to the
#'   get_prompts("output_summarisation") or
#'   get_prompts("output_rolling_aggregation") prompts depending on the task.
#' @param prompt_only If TRUE, only the prompt is returned, the LLM is not
#'   interrogated. Default is FALSE.
#' @param ... Additional arguments passed to the `interrogate_llm` function,
#'   such as the LLM provider.
#'
#' @return A summary of the transcript.
#'
#' @export
#'
summarise_transcript <- function(
    transcript_data,
    method = c("simple", "rolling"),
    window_size = 15,
    output_length = 3,
    event_description = NULL,
    recording_details = NULL,
    audience = "An audience with understanding of the topic",
    vocabulary = NULL,
    consider_diarization = TRUE,

    summary_structure = get_prompts("summary_structure"),
    extra_diarization_instructions = NULL,
    extra_output_instructions = NULL,

    prompt_only = FALSE,
    ...
) {

  method <- match.arg(method)

  args <- as.list(environment())

  # Set the default prompts if not already set
  set_prompts()

  # Revert to simple summarisation if the transcript argument is not of the
  # correct type
  if (method == "rolling" &&
      (is.data.frame(transcript_data) && !"start" %in% names(transcript_data)) ||
      (!is.data.frame(transcript_data) && length(transcript_data) == 1)
  ) {
    stop(
      "To apply the \"rolling window\" method, the `transcript_data` argument ",
      "must be a transcript data frame with segments' start times or ",
      "a list of transcript data frames, or a vector of transcript text."
    )

  }

  if (method == "simple" && !is.data.frame(transcript_data) &&
      length(transcript_data) > 1) {
    stop(
      "To apply the \"simple\" summarisation method, ",
      "the `transcript_data` argument ",
      "must be a transcript data frame or a single transcript text."
    )
  }

  if (method == "rolling" && is.data.frame(transcript_data)) {

    # window_size is in minutes, convert to seconds
    window_size <- window_size * 60

    transcript_duration <- diff(range(transcript_data$start))

    # Only apply the rolling window method if the transcript is longer than
    # 1.5 * window_size
    if (transcript_duration > 1.5 * window_size) {
      # Generate the breakpoints
      breakpoints <- seq(
        transcript_data$start[1], max(transcript_data$start), by = window_size)

      last_segment <- max(transcript_data$start) - tail(breakpoints, n=1)

      # Adjust if the last segment is less than window_size / 2 seconds
      if (last_segment < (window_size / 2)) {
        breakpoints <- utils::head(breakpoints, -1)
      }

      # Add the stopping point
      breakpoints <- c(breakpoints, max(transcript_data$start))

      # Split the array using the breakpoints
      transcript_data <- seq_along(breakpoints) |> utils::head(-1) |>
        purrr::map(\(bp) {
          transcript_data |>
            dplyr::filter(
              .data$start >= breakpoints[bp],
              .data$start < breakpoints[bp+1])
        })

    } else {
      warning(
        "The transcript is too short (< 1.5 * window_size) ",
        "to apply the \"rolling window method\". ",
        "\nReverting to the \"simple\" summarisation method.",
        call. = FALSE, immediate. = TRUE
      )

      method <- "simple"
    }
  }

  # Enclose single transcript data frames in a list
  if (is.data.frame(transcript_data)) {
    transcript_data <- list(transcript_data)
  }

  # Generate the summarisation prompts
  prompts <- purrr::imap_chr(transcript_data, \(transcript, i) {

    # Convert the transcript to text if it is a data frame
    if (is.data.frame(transcript)) {
      transcript <- extract_text_from_transcript(
        transcript,
        import_diarization = consider_diarization)
    }

    args <- args[
      c("event_description", "recording_details", "audience", "vocabulary",
        "consider_diarization", "summary_structure",
        "extra_diarization_instructions", "extra_output_instructions")
    ]

    # Generate the prompt
    generate_summarisation_prompt(
      transcript,
      args = args
    )
  })

  if (prompt_only) {
    return(prompts)
  }

  # Generate the summaries
  summaries <- purrr::imap(prompts, \(prompt, i) {
    if (method == "rolling") {
      message("Processing transcript segment ", i,
              " of ", length(transcript_data))
    }

    # Interrogate the LLM
    interrogate_llm(
      c(
        system = get_prompts("persona"),
        user = prompt),
      ...
    )

  })

  # Return the summaries if there is only one
  if (length(summaries) == 1) {
    return(summaries[[1]])
  }

  # If the method is "rolling", aggregate the summaries
  message("\nAggregating summaries")

  args <- args[
    c("event_description", "recording_details", "audience",
      "summary_structure", "extra_output_instructions")
  ]

  # Aggregate the summaries
  aggregation_prompt <- generate_rolling_aggregation_prompt(
    summaries,
    args = args
  )

  interrogate_llm(
    c(
      system = get_prompts("persona"),
      user = aggregation_prompt),
    ...
  )

}

#' Summarise a full meeting
#'
#' This function summarises a full event comprising multiple meetings/talks
#' described in an `agenda`. It generates a summary for each talk in the meeting
#' and saves the results in an output file. It's built on top of
#' `summarise_transcript`.
#'
#' @param transcript_data The transcript data of the meeting as a data frame.
#' @param agenda The agenda of the meeting, that is, a list of agenda elements
#'   each with a session name, a title, speaker and moderator lists, type of
#'   talk and start and end times. Alternatively, the path to an R file
#'   containing such a list.
#' @param method The method to use to summarise the transcript. Can be either
#'   "simple" or "rolling", with the first being faster and the second
#'   (theoritically) more accurate. See `summarise_transcript` for more details.
#' @param output_file The file to save the results to as a list. Default is
#'   "event_summary.R".
#' @param window_size The size of the window in minutes to use when summarising
#'   the transcript using the "rolling window" method. See
#'   `summarise_transcript` for more details.
#' @param output_length An indication to the LLM regarding the length of the
#'   output. See `summarise_transcript` for more details.
#' @param event_start_time The start time of the event in the HH:MM(:SS)( AM/PM)
#'   format. Necessary to convert the agenda times to seconds if the first event
#'   time is not the start time of the event.
#' @param event_description The description of the event See
#'   `summarise_transcript` for more details.
#' @param audience The audience of the event See `summarise_transcript`
#'   for more details.
#' @param vocabulary The vocabulary used in the meeting. See
#'   `summarise_transcript` for more details.
#' @param consider_diarization Whether to take into account the diarization of
#'   the transcript. Default is TRUE See `summarise_transcript` for more
#'   details.
#' @param summary_structure,extra_diarization_instructions,extra_output_instructions
#'   Specific instructions necessary to build the summarisation prompt. See
#'   `summarise_transcript` for more details and run `get_prompts()` to see the
#'   defaults. See `summarise_transcript` for more details.
#' @param overwrite Whether to overwrite existing summaries. Default is FALSE.
#' @param ... Additional arguments passed to `interrogate_llm` function, such as
#'   the LLM provider.
#'
#' @return The result tree of the meeting summary. Also saves the results in the
#'   output file as side effect.
#'
#' @export
summarise_full_meeting <- function(
    transcript_data,
    agenda,
    method = c("simple", "rolling"),
    output_file = "event_summary.R",

    window_size = 15,
    output_length = 3,

    event_start_time = getOption("minutemaker_event_start_time"),
    event_description = NULL,
    audience = "An audience with understanding of the topic",
    vocabulary = NULL,
    consider_diarization = TRUE,

    summary_structure = get_prompts("summary_structure"),
    extra_diarization_instructions = NULL,
    extra_output_instructions = NULL,

    overwrite = FALSE,
    ...
) {

  # Import agenda from file
  if (is.character(agenda)) {
    agenda <- dget(agenda)
  }

  # Convert the agenda times to seconds if they are in "HH:MM(:SS)( AM/PM)"
  # format
  agenda <- convert_agenda_times(
    agenda, convert_to = "seconds",
    event_start_time = event_start_time)

  # Generate the output container if it doesn't exist
  if (!file.exists(output_file)) {
    result_tree <- list()
  } else {
    result_tree <- dget(output_file)
  }

  # Build the id of each talk using the session and title
  talks_ids <- build_ids_from_agenda(agenda)

  for (id in talks_ids) {

    # Skip if the talk has already been summarised
    if (!is.null(result_tree[[id]]) && !overwrite) {
      next
    }

    message("\n# Talk: ", id)

    # Get the index of the current talk in the agenda
    i <- which(talks_ids == id)

    # Get the agenda element of the current talk
    agenda_element <- agenda[[i]]

    # Validate the agenda element to make sure it has a start and end time
    is_valid_agenda <- validate_agenda_element(
      agenda_element,
      from = TRUE, to = TRUE
    )

    if (!is_valid_agenda) {
      warning("The agenda element ", id, " is not valid. Skipping.",
              call. = FALSE, immediate. = TRUE)
      next
    }

    # Extract the talk from the transcript
    transcript_subset <- transcript_data |>
      dplyr::filter(
        .data$start >= agenda_element$from,
        .data$start <= agenda_element$to)

    if (nrow(transcript_subset) == 0) {
      warning("The transcript subset for the talk ", id, " is empty. ",
              if (!is.null(event_start_time)) {
                "Did you provide the correct event start time?"
                } else {
                  "Did you provide the correct agenda times?"
                }, ". Skipping.",
              call. = FALSE, immediate. = TRUE)
      next
    }

    # Extract the details of the talk
    recording_details <- generate_recording_details(agenda_element)

    # Extract the vocabulary of the talk
    talk_summary <- summarise_transcript(
      transcript_data = transcript_subset,

      method = method,
      window_size = window_size,
      output_length = output_length,

      event_description = event_description,
      recording_details = recording_details,
      vocabulary = vocabulary,
      audience = audience,
      consider_diarization = consider_diarization,

      summary_structure = summary_structure,
      extra_diarization_instructions = extra_diarization_instructions,
      extra_output_instructions = extra_output_instructions,
      ...
    )

    # Create the output object for the current talk
    output <- list(
      session = agenda_element$session,
      title = agenda_element$title,
      summary = talk_summary
    )

    # Add the output to the results tree
    result_tree[[id]] <- output

    # Update the results file
    dput(result_tree, file = output_file)
    styler::style_file(output_file)
  }

  if (length(result_tree) == 0) {
    stop("The final result tree has lenght zero. No talks were summarised.")
  }

  # Return the result tree invisibly
  invisible(result_tree)
}


#' Infer the agenda from a transcript
#'
#' This function takes a transcript and various optional parameters, and uses an
#' LLM to generate an agenda.
#'
#' @param transcript The transcript to be summarised. Can be a file path or a
#'   data frame.
#' @param event_description A description of the event. Provide context about
#'   the event.
#' @param vocabulary A character vector of specific vocabulary words, names,
#'   definitions, to help the LLM recognise misspellings and abbreviations.
#' @param diarization_instructions Instructions for the diarization of the
#'   transcript. Default is NULL. If provided, it will help the LLM in
#'   recognizing the speakers in the transcript.
#' @param start_time The start time of the event in the HH:MM(:SS)( AM/PM)
#'   format. Necessary to convert the agenda times from seconds to an easier to
#'   read format.
#' @param expected_agenda The expected agenda of the event. A text description
#'   of the expected agenda. If provided, the LLM will be asked to generate an
#'   agenda that matches this description.
#' @param window_size The time window that will be taken into account when
#'   inferring the agenda. Default is 2 hours. A larger window will increase the
#'   accuracy of the agenda since it will provide context and will prevent to
#'   have talks crossing the window boundaries; also decrease the chance of
#'   having the LLM being over sensitive to small changes in topics, generating
#'   too many small talks. However, a larger window will also require a larger
#'   LLM context.
#' @param output_file An optional file to save the results to. Default is NULL,
#'   i.e., the results are not saved to a file.
#' @param ... Additional arguments passed to the `interrogate_llm` function.
#'   Keep in consideration that this function needs LLMs that manages long
#'   context and that produce valid JSON outputs. The `force_json` argument is
#'   used with OpenAI based LLM but it's not accepted by other LLMs; therefore
#'   the user may need to edit the system prompts to ensure that the output is a
#'   valid JSON.
#'
#' @return An agenda in the usual list format.
#'
#' @export
#'
infer_agenda_from_transcript <- function(
    transcript,
    event_description = NULL,
    vocabulary = NULL,
    diarization_instructions = NULL,
    start_time = NULL,
    expected_agenda = NULL,
    window_size = 3600,
    output_file = NULL,
    ...
) {

  # Set the default prompts if not already set
  set_prompts()

  # import the transcript if it's a file path
  if (is.character(transcript)) {
    # Is the transcript a CSV file?
    if (stringr::str_detect(transcript, "\\.csv$")) {
      transcript_data <- readr::read_csv(transcript, show_col_types = FALSE)
    }
    # Is the transcript a subtitle file?
    else {
      transcript_data <- import_transcript_from_file(transcript)
    }
  } else if (is.data.frame(transcript)) {
    transcript_data <- transcript
  } else {
    stop("The transcript must be a file path or a data frame.")
  }

  transcript_data <- transcript_data |>
    select("start", "end", "text", any_of("speaker")) |>
    mutate(
      across(all_of(c("start", "end")), ~ round(.x)),
    ) |>
    filter(!is_silent(.data$text))

  breakpoints <- seq(
    transcript_data$start[1], max(transcript_data$start), by = window_size)

  pause_duration <- 1200

  pauses <- transcript_data |>
    filter(
      .data$start - lag(.data$end, default = 0) > pause_duration
    ) |> pull("start")

  breakpoints <- c(breakpoints, pauses) |> sort()

  for (i in which(breakpoints %in% pauses)) {
    if (breakpoints[i] - breakpoints[i - 1] < pause_duration) {
      breakpoints <- breakpoints[-(i - 1)]
    }

    if (breakpoints[i + 1] - breakpoints[i] < pause_duration) {
      breakpoints <- breakpoints[-(i + 1)]
    }
  }

  last_segment <- max(transcript_data$start) - tail(breakpoints, n=1)

  # Adjust if the last segment is less than window_size / 2 seconds
  if (last_segment < (window_size / 2)) {
    breakpoints <- utils::head(breakpoints, -1)
  }

  stop <- FALSE
  cur_bp <- 1
  json_error <- FALSE

  # Check if there was an already started session that got interrupted
  arg_hash <- rlang::hash(
    list(
      transcript_data = transcript_data,
      event_description = event_description,
      vocabulary = vocabulary,
      diarization_instructions = diarization_instructions,
      start_time = start_time,
      expected_agenda = expected_agenda,
      window_size = window_size)
  )

  # Reset the temporary agenda if the arguments have changed
  if (is.null(getOption("minutemaker_temp_agenda"))) {
    options(
      "minutemaker_temp_agenda" = list(),
      "minutemaker_temp_agenda_last_bp" = NULL
      )
  } else if (getOption("minutemaker_temp_agenda_hash", "") != arg_hash) {
    options(
      "minutemaker_temp_agenda" = list(),
      "minutemaker_temp_agenda_last_bp" = NULL
    )
  } else {
    message("A temporary agenda was found. Resuming the inference.")
  }

  options("minutemaker_temp_agenda_hash" = arg_hash)

  update_agenda <- function(agenda_elements) {
    cur_agenda <- c(
      getOption("minutemaker_temp_agenda", list()),
      agenda_elements |> sort()
    )

    options("minutemaker_temp_agenda" = cur_agenda)
  }

  message("- Inferring the agenda from the transcript")

  while (isFALSE(stop)) {

    bp_left <- breakpoints[cur_bp]
    bp_right <- breakpoints[cur_bp + 1]

    # Stop if reached the end
    if (is.na(bp_right)) {
      bp_right <- max(transcript_data$start) + 1
    }

    # Check if the current segment was already processed
    if (cur_bp <= getOption("minutemaker_temp_agenda_last_bp", 0)) {
      if (cur_bp == length(breakpoints)) stop <- TRUE

      cur_bp <- cur_bp + 1

      next
    }

    transcript_segment <- transcript_data |>
      dplyr::filter(
        .data$start >= bp_left,
        .data$start < bp_right
      )

    # Skip empty segments
    if (nrow(transcript_segment) == 0) {
      if (cur_bp == length(breakpoints)) stop <- TRUE

      cur_bp <- cur_bp + 1

      next
    }

    transcript_segment <- transcript_segment |> readr::format_csv()

    prompt <- generate_agenda_inference_prompt(
      transcript_segment,
      args = mget(
        c("event_description", "vocabulary",
          "diarization_instructions", "expected_agenda"),
        ifnotfound = list(NULL))
    )

    # Build the prompt set
    prompt_set <- c(
      system = get_prompts("persona"),
      user = prompt
    )

    # If this is a retry for failed json parsing, add the previous result to the
    # prompt set and add instructions to fix the output
    if (json_error) {
      prompt_set <- c(
        prompt_set,
        assistant = result_json,
        user = "Your output was not a valid JSON.
          Please correct it to provide a valid output.")
    }

    # Attempt to interrogate the LLM
    result_json <- try(interrogate_llm(
      prompt_set,
      ...,
      force_json = TRUE
    ), silent = TRUE)

    # If the interrogation fails due to too long output, retry with a smaller
    # window
    if (inherits(result_json, "try-error") &&
        grepl("Answer exhausted the context window", result_json)) {

      warning(
        "Answer exhausted the context window. retrying...",
        immediate. = T, call. = F)

      # Add a new breakpoint in the middle of the current segment
      new_bp <- (bp_left + bp_right) / 2
      breakpoints <- sort(c(breakpoints, new_bp))

      # Prevent stopping, in case the error happened on the last segment
      stop <- FALSE

      next
    } else if (inherits(result_json, "try-error")) {

      stop(result_json)

    }

    cat(result_json)

    # Attempt to parse the result json
    parsed_result <- try(
      jsonlite::fromJSON(result_json, simplifyDataFrame = F)$start_times,
      silent = TRUE)

    # If the parsing fails...
    if (inherits(parsed_result, "try-error")) {

      # If this is the first parsing error, retry with instructions to fix the
      # output
      if (!json_error) {
        warning(
          "Output not a valid JSON. retrying...",
          immediate. = T, call. = F)

        json_error <- TRUE
      }
      # If this is the second parsing error, shorten the window
      else {

        warning(
          "Output not a valid JSON. Shortening the window...",
          immediate. = T, call. = F)

        json_error <- FALSE

        # Add a new breakpoint in the middle of the current segment
        new_bp <- (bp_left + bp_right) / 2
        breakpoints <- sort(c(breakpoints, new_bp))

      }

      # Prevent stopping, in case the error happened on the last segment
      stop <- FALSE

      next
    }

    # If the parsing is successful, update the agenda
    update_agenda(parsed_result)

    json_error <- FALSE

    options("minutemaker_temp_agenda_last_bp" = cur_bp)

    if (cur_bp == length(breakpoints)) stop <- TRUE

    cur_bp <- cur_bp + 1

  }

  agenda_times <- getOption("minutemaker_temp_agenda", list())

  if (length(agenda_times) == 0) {
    warning("No agenda was inferred from the transcript.",
            immediate. = T, call. = F)
    return(NULL)
  }

  # Remove segments that are too short or that precede the previous one.
  agenda_times <- agenda_times |> purrr::imap(\(x, i) {
    if (i == 1) return(agenda_times[[i]])

    this_time <- agenda_times[[i]]
    prev_time <- agenda_times[[i - 1]]

    # segments should last at least 5 minutes and not be negative
    if (this_time - prev_time < 150) return(NULL)

    return(this_time)
  }) |> unlist()

  message("- Extracting agenda items details")

  # Extract the talks' details from the transcript
  agenda <- purrr::imap(agenda_times, \(start, i) {
    # if (i == 1) start <- 1

    # Stop at the end of the transcript if there is no next agenda element
    end <- min(
      c(agenda_times[i + 1], max(transcript_data$end)),
      na.rm = TRUE)

    # Stop at the pause if there is one in the talk segment
    pauses <- pauses[between(pauses, start, end)]
    end <- min(c(end, pauses), na.rm = TRUE)

    element <- list(
      # Sometimes, int are produced, which creates problems when converting to
      # clocktime
      from = as.numeric(start),
      to = as.numeric(end)
    )

    transcript_segment <- transcript_data |>
      filter(
        .data$start >= element$from,
        .data$end <= element$to,
      ) |> readr::format_csv()

    prompt <- generate_agenda_element_prompt(
      transcript_segment,
      # I cannot use mget here because the prompt function is not in the
      # environment of the calling function. Probably there's a way to use mget
      # also here
      args = list(
        event_description = event_description,
        vocabulary = vocabulary,
        diarization_instructions = diarization_instructions)
    )

    # Build the prompt set
    prompt_set <- c(
      system = get_prompts("persona"),
      user = prompt
    )

    result_json <- interrogate_llm(
      prompt_set,
      ..., force_json = TRUE
    )

    jsonlite::fromJSON(result_json, simplifyDataFrame = F) |>
      c(element)
  })

  if (!is.null(start_time)) {
    agenda <- agenda |>
      convert_agenda_times(
        convert_to = "clocktime",
        event_start_time = start_time)
  }

  if (!is.null(output_file)) {
    dput(agenda, file = output_file)
    styler::style_file(output_file)
  }

  options(
    minutemaker_temp_agenda_last_bp = NULL,
    minutemaker_temp_agenda = NULL,
    minutemaker_temp_agenda_hash = NULL
  )

  agenda
}

#' Extract entities from a text
#'
#' This function takes a text and extracts entities from it. The entities can be
#' people, acronyms, organizations, and concepts. The function returns a vector
#' with the entities found in the text. Can be useful to build vocabularies for
#' the LLMs starting from an event description or a transcript.
#'
#' @param text The text from which to extract the entities.
#' @param entities A character vector with the entities to extract. Can be
#'   "people", "acronyms", "organizations", and "concepts". Default is all of
#'   them.
#' @param prompt_only If TRUE, only the prompt is returned, the LLM is not
#'   interrogated. Default is FALSE.
#' @param ... Additional arguments passed to the `interrogate_llm` function.
#'
#' @return A vector with the entities found in the text.
#'
#' @export
#'
entity_extractor <- function(
    text,
    entities = c("people", "acronyms", "organizations", "concepts"),
    prompt_only = FALSE,
    ...
    ) {

  text <- paste(text, collapse = "--------\n\n\n")

  acro_or_concepts <- entities[entities %in% c("acronyms", "concepts")]

  task <- paste0(
    "You will be passed one or more text documents. For each document, you ",
    "should extract the following entities from the text:\n\n",
    sprintf("-`%s`;", entities) |> paste(collapse = "\n"),
    "\n\nYou should return a JSON object of the entities found in the text, with each ",
    "entity type as a key and a list of the entities of that type as the ",
    "value. For example, if you find two people and one organization in the ",
    "text, you should return a list with two keys, 'people' and 'organizations', ",
    "and the corresponding lists of entities as values.\n\n",
    if (length(acro_or_concepts) > 0) {
      paste0("If you find", paste(acro_or_concepts, collapse = " or "),
      "they should be returned list of strings, with each element ",
      "formatted as 'entity: definition'",
      "trying to infer the definition from the context. ",
      "If you are not 100% sure, or it's self explanatory, just list the concepts",
      "as strings.\n\n")
    },
    "Here is an example of the expected output:\n\n",
    '```json
 {
   "people": ["John Doe", "Jane Smith"],
   "organizations": ["Acme Corp"],
   "acronyms": [
     "LLM: Large Language Model",
     "NLP: Natural Language Processing"
   ],
   "concepts": [
     "Arxiv: Open access repository of scientific articles",
     "Escherichia coli"
   ]
 }
 ```\n\n',
    "Here is the text from which you should extract the entities:\n\n####\n\n",
    text, "\n\n####\n\nProvide your JSON output below.")

  if (prompt_only) {
    return(task)
  }

  interrogate_llm(
    c("system" = get_prompts("persona"), "user" = task),
    force_json = TRUE, ...) |>
    jsonlite::fromJSON() |>
    unlist() |> unname()
}
