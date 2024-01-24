
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
      message("Processing transcript segment", i,
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
#' @param event_description The description of the event See
#'   `summarise_transcript` for more details.
#' @param event_audience The audience of the event See `summarise_transcript`
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

    event_description = NULL,
    event_audience = "An audience with understanding of the topic",
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
  agenda <- convert_agenda_times(agenda)

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

    message("Talk: ", id)

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
      audience = event_audience,
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
  }

  # Return the result tree invisibly
  invisible(result_tree)
}
