#' Parse the tts JSON output
#'
#' Takes the JSON output of `perform_speech_to_text` and parses it into a data
#' frame with the text and time of each segment.
#'
#' @param transcript_json The JSON output of `perform_speech_to_text` in
#'   multiple formats: the raw JSON, a list, a path to a JSON file, or a path to
#'   a folder containing JSON files. If a folder is provided, the function will
#'   parse all JSON files in the folder.
#' @param pretty_times A boolean indicating whether the start and end times
#'   should be also formatted as hours-seconds-minutes instead of cumulative
#'   seconds.
#'
#' @return A data frame with the text and start and end time of each segment.
parse_transcript_json <- function(
    transcript_json,
    pretty_times = TRUE
) {

  json_files <- NA

  # Is the input a string?
  transcript_list <- if (is.character(transcript_json)) {
    # Is the string a path to a folder?
    if (dir.exists(transcript_json)) {
      # Get all JSON files in the folder
      json_files <- list.files(transcript_json, pattern = "\\.json$")

      # Reorder the files by the number in the file name, otherwise they will be
      # processed in alphabetical order putting e.g. segment 10 before segment 2
      file_order <- stringr::str_order(json_files, numeric = TRUE)

      json_files <- file.path(transcript_json, json_files[file_order])

      # Loop through each JSON file
      tryCatch(
        purrr::map(json_files, jsonlite::read_json),
        error = function(e) {
          stop("Error parsing JSON file: ", e$message)
        })
    } else if (file.exists(transcript_json)) { # Is the string a path to a file?

      json_files <- transcript_json

      # Read the JSON file
      tryCatch(
        list(jsonlite::read_json(transcript_json)),
        error = function(e) {
          stop("Error parsing JSON file: ", e$message)
        })
    } else { # Is the string a JSON string?
      tryCatch(
        list(jsonlite::fromJSON(transcript_json)),
        error = function(e) {
          stop("Error parsing JSON: ", e$message)
        })
    }
  } else if (vctrs::vec_is_list(transcript_json)) {
    list(transcript_json)
  } else {
    stop("Unsupported transcript format.")
  }

  full_transcript <- data.frame()
  last_time <- 0
  to_import <- c("start", "end", "text", "speaker",
                 "avg_logprob", "no_speech_prob")

  # Extract the data from each JSON file
  for (i in seq_along(transcript_list)) {

    # Raise an error if the JSON file does not contain any data
    if (!"segments" %in% names(transcript_list[[i]])) {
      stop("File ", basename(json_files[i]), " does not contain any data. ",
      "Please remove it and try transcription again.")
    }

    transcript_data <- transcript_list[[i]]$segments |>
      bind_rows() |>
      # Select only the columns to import
      select(any_of(to_import)) |>
      # Unlist nested lists
      mutate(across(where(is.list), unlist)) |>
      # Remove duplicated rows arising from the original JSON structure being an
      # array of data frames
      distinct() |>
      # Add the file indicators
      mutate(
        doc = i,
        file = basename(json_files[i]),
        .before = 1
      ) |>
      # Add the previous file time to the start and end times
      mutate(
        start = .data$start + last_time,
        end = .data$end + last_time
      )

    full_transcript <- rbind(full_transcript, transcript_data)

    # Get the last segment info
    last_time <- transcript_data$end |> tail(1)
  }

  full_transcript <- clean_transcript(full_transcript)

  if (pretty_times) {
    full_transcript <- full_transcript |>
      mutate(
        across(
          any_of(c("start", "end")),
          ~ lubridate::seconds_to_period(.x) |> round(),
          .names = "{.col}_formatted")
      )
  }

  full_transcript
}

#' Clean transcript
#'
#' Cleans a transcript by removing isolated text, consecutive segments with the
#' same text, and, optionally, silence segments "[...]".
#'
#' @param transcript_data A data frame containing the transcript data.
#' @param remove_silence A boolean indicating whether silence segments should be
#'   removed.
#'
#' @return A data frame containing the cleaned transcript data.
#'
#' @export
#'
clean_transcript <- function(
    transcript_data,
    remove_silence = FALSE
) {

  # Remove double spaces and spaces at the beginning and end of the text
  transcript_data$text <- stringr::str_squish(transcript_data$text)

  # Normalize missing text
  transcript_data$text[transcript_data$text %in% c("", NA)] <- silent()

  # Set consecutive segments with the same text to "silent" symbol
  transcript_data$text <- mutate(
    transcript_data,
    text = if_else(
      .data$text == lag(.data$text, default = first(.data$text)),
      silent(), .data$text)
  )

  # Remove pieces of text that are sorrounded by > 4 "[...]" as they are
  # likely hallucinations from the speech-to-text model or non-relevant text
  # (e.g. talking outside sessions)
  for (i in 1:nrow(transcript_data)) {
    indexes <- pmax(i + c(-4:-1, 1:4), 0) |> unique()
    if (transcript_data$text[i] != silent()) {
      if (all(is_silent(transcript_data$text[indexes]))) {
        transcript_data$text[i] <- silent()
      }
    }
  }

  # Segments with multiple repetitions of the same word are likely
  # hallucinations
  repetitions <- purrr::map_int(
    transcript_data$text,
    ~ str_split_1(.x, ", *") |> table() |> max())

  transcript_data$text[repetitions > 10] <- silent()

  # Use the Whisper API results statistics to remove segments with low
  # confidence
  if (all(c("avg_logprob", "no_speech_prob") %in% names(transcript_data))) {
    condition <- with(transcript_data, avg_logprob < -0.5 & no_speech_prob > 0.9)
    transcript_data$text[condition] <- silent()

    # Remove "avg_logprob", "no_speech_prob" columns
    transcript_data <- transcript_data |>
      select(-any_of(c("avg_logprob", "no_speech_prob")))
  }

  # Remove silence segments
  if (remove_silence) {
    transcript_data <- transcript_data |>
      filter(!is_silent(.data$text))
  }

  transcript_data
  }

#' Extracts a specific talk text from a given transcript data
#'
#' The specific talk text can be identify using the relative agenda element or
#' manually inputting the start and end times.
#'
#' @param transcript_data A data frame containing the transcript data. It must
#'   have the following columns: 'start', 'end' and 'text'
#' @param agenda_element A list with 'from' and 'to' fields indicating the start
#'   and end times of the agenda element.
#' @param start_time The start time for the extraction.
#' @param end_time The end time for the extraction.
#' @param import_diarization A boolean indicating whether the speaker should be
#'  mentioned, if present. Will be ignored if the transcript does not contain
#'  speaker information or there is only one speaker.
#'
#' @return A string containing the extracted text.
#'
#' @export
#'
extract_text_from_transcript <- function(
    transcript_data,
    agenda_element = NULL,
    start_time = NULL, end_time = NULL,
    import_diarization = TRUE
) {

  # If no parameters are provided, the whole transcript is used and a warning is issued
  if (is.null(agenda_element) && is.null(start_time) && is.null(end_time)) {
    warning("The whole transcript will be used (may be very long)."
            , call. = FALSE, immediate. = TRUE)
  }

  # If an agenda element is provided, its start and end times are used
  if (!is.null(agenda_element)) {
    start_time <- agenda_element$from
    end_time <- agenda_element$to
  }

  # If no start time is provided, start from the beginning of the transcript
  if (is.null(start_time)) {
    start_time <- min(transcript_data$start)
  }

  # If no end time is provided, end at the end of the transcript
  if (is.null(end_time)) {
    end_time <- max(transcript_data$end)
  }

  # Ignore the `import_diarization` parameter if the transcript does not contain
  # speaker information
  if (all(is.na(transcript_data$speaker)) ||
      n_distinct(transcript_data$speaker, na.rm = T) == 1) {
    import_diarization <- FALSE

    transcript_data$speaker <- "None"
  }

  transcript <- transcript_data %>%

    # filtered based on the start and end times
    filter(.data$start >= start_time, .data$end <= end_time) |>

    # Remove silence segments
    filter(!is_silent(.data$text)) |>

    # Add an ID to identify consecutive segments from the same speaker
    mutate(
      speakerID = consecutive_id(.data$speaker),
    ) |>

    # Concatenate consecutive segments from the same speaker
    summarize(
      text = sprintf(
        "Speaker: %s\n%s",
        .data$speaker[1], paste(.data$text, collapse = "\n")),
      .by = .data$speakerID
    ) |>

    # Extract the text
    pull(.data$text) |> paste(collapse = "\n\n")

  # Remove the speaker id import_diarization is FALSE
  if (!import_diarization) {
    transcript <- stringr::str_remove_all(transcript, "Speaker:.*?\n")
  }

  transcript
}

#' Builds IDs from each agenda item session and titles
#'
#' @param agenda A list containing the agenda items.
#'
#' @return A character vector containing the IDs for each agenda item.
#'
build_ids_from_agenda <- function(agenda) {
  # Build the id of each talk using the session and title
  talks_ids <- purrr::imap_chr(agenda, ~ {
    # If the title is NULL, use an index as title
    if (is.null(.x$title)) .x$title <- .y

    paste0(
      # If the session is not NULL, prepend it to the title
      if (!is.null(.x$session)) paste0(.x$session, "_"),
      .x$title
    )
  })
}

#' Formats a summary tree into a human-readable text
#'
#' The summary functions returns a machine-readable summary tree. This function
#' takes the summary tree and formats it into a human-readable text.
#'
#' @param summary_tree A list containing the summary tree, or a string with the
#'   path to a file containing the summary tree.
#' @param agenda A list containing the agenda items.
#' @param output_file A string with the path to the output file. If NULL, the
#'   output is returned invisibly and not written to a file.
#'
#' @return A string containing the formatted summary tree, invisibly. This is printed to
#'   the output file if one is provided.

#' @export
format_summary_tree <- function(
    summary_tree,
    agenda,
    output_file = NULL
) {

  # If summary_tree is a file path, load the data from the file
  if (is.character(summary_tree) && file.exists(summary_tree)) {
    summary_tree <- dget(summary_tree)
  }

  # Import agenda from file
  if (is.character(agenda)) {
    agenda <- dget(agenda)
  }

  # Initialize the output string
  output <- ""

  # Build the id of each talk using the session and title
  talk_ids <- build_ids_from_agenda(agenda)

  # Loop over each talk id
  for (id in talk_ids) {
    # Extract the summary for the current talk
    talk_summary <- summary_tree[[id]]$summary
    # Extract the agenda element for the current talk
    agenda_element <- agenda[[which(talk_ids == id)]]

    # If no summary is found for the current talk, issue a warning and skip to the next talk
    if (is.null(talk_summary)) {
      warning("No summary found for ", id, ". Skipping.", call. = FALSE)
      next
    }

    # Generate a text version of the summary, with session, title, speakers, moderators and summary
    output_piece <- with(agenda_element, {
      speakers <- stringr::str_flatten_comma(speakers)
      moderators <- stringr::str_flatten_comma(moderators)

      stringr::str_glue("Session: {session};
    Title: {title};
    Speakers: {speakers};
    Moderators: {moderators};
    Summary:

    {talk_summary}")
    }) |>
      stringr::str_remove_all("\\n?\\w+:\\s*;") # Remove empty fields

    # Append the output piece to the output string
    output <- paste0(output, output_piece, "\n\n####################\n\n")
  }

  # If an output file is specified, write the output to the file
  if (!is.null(output_file)) {
    readr::write_lines(output, output_file)
  }

  # Return the output string invisibly
  invisible(output)
}


#' Validates an agenda element
#'
#' @param agenda_element A list containing the agenda element.
#' @param session A boolean indicating whether the `session` item should be present.
#' @param title A boolean indicating whether the `title` item should be present.
#' @param speakers A boolean indicating whether the `speakers` item should be present.
#' @param moderators A boolean indicating whether the `moderators` item should be present.
#' @param type A boolean indicating whether the `type` item should be present.
#' @param from A boolean indicating whether the `from` item should be present.
#' @param to A boolean indicating whether the `to` item should be present.
#'
#' @return A boolean indicating whether the agenda element is valid.
#'
validate_agenda_element <- function(
    agenda_element,
    session = FALSE,
    title = FALSE,
    speakers = FALSE,
    moderators = FALSE,
    type = FALSE,
    from = FALSE,
    to = FALSE
) {

  # Get the arguments as a list
  args <- as.list(environment())

  # Remove the 'agenda_element' argument from the list
  args$agenda_element <- NULL

  # Check if the required items are present in the agenda element
  is_valid <- purrr::imap_lgl(args, ~ {
    !is.null(agenda_element[[.y]]) || isFALSE(.x)
  }) |> all()

  # Return the validation result
  is_valid
}

#' Import transcript from subtitle file
#'
#' Import a transcript from typical subtitle file formats like SRT and VTT.
#'
#' @param transcript_file A string with the path to the transcript file.
#'   Supported formats are SRT and VTT.
#' @param import_diarization A boolean indicating whether the speaker should be
#'   recorded, if present.
#'
#' @return A data frame containing the transcript data with a start and end time
#'   in seconds for each text segment.
#'
#' @export
#'
import_transcript_from_file <- function(
    transcript_file,
    import_diarization = TRUE
) {

  # Read the file content
  lines <- readLines(transcript_file, warn = FALSE)

  # Check the file extension to determine the format
  file_extension <- tools::file_ext(transcript_file)

  if (!file_extension %in% c("srt", "vtt")) {
    stop("Unsupported file format. Supported formats are SRT and VTT.")
  }

  times_pos <- which(stringr::str_detect(lines, "-->"))

  # Iterate over each time position
  purrr::map(times_pos, ~ {
    # Extract the start and end times
    times <- stringr::str_split_1(lines[.x], "-->") |>
      stringr::str_squish() |>
      lubridate::hms() |>
      as.numeric()

    # Extract the text
    text <- lines[.x + 1]

    # Return the data
    extract <- data.frame(
      start = times[1],
      end = times[2],
      text = text
    )

    # If the transcript is diarized, extract the speaker
    if (import_diarization && lines[.x - 1] != "") {
      # Extract the speaker
      cur_speaker <- lines[.x - 1] |>
        # the name is between double quotes
        stringr::str_extract_all('(?<=").*(?=")') |>
        unlist()

      # In the unlikely case that there are multiple speakers, join them
      if (length(cur_speaker) > 1) {
        cur_speaker <- stringr::str_flatten_comma(speaker)
      }

      # Add the speaker to the data
      extract <- extract |> mutate(
        speaker = cur_speaker,
        .before = text
      )
    }

    extract
  }) |> dplyr::bind_rows()
}

#' Merge two transcripts
#'
#' Merges two transcripts into one, overwriting segments that are empty in the
#' first transcript with the corresponding segments from the second transcript.
#' Also imports the diarization information if present.
#'
#' @param transcript_x A data frame containing the transcript to which the second
#'   transcript will be merged.
#' @param transcript_y A data frame containing the transcript to be merged in.
#' @param import_diarization A boolean indicating whether the diarization
#'   information should be kept from the second transcript.
#'
#' @return A data frame containing the merged transcript.
#'
#' @export
#'
merge_transcripts <- function(
    transcript_x,
    transcript_y,
    import_diarization = TRUE
) {

  empty_segments <- which(transcript_x$text %in% c("", "[...]"))

  if (length(empty_segments) == 0 && !import_diarization) {
    message("No empty segments found in the transcript.")
    return(transcript_x)
  }

  transcript_x$seen <- FALSE

  col_names <- intersect(names(transcript_x), names(transcript_y))

  if (import_diarization) {
    # This message is useful only if also diarization is imported
    message("Merging...")

    # The actual transcript_y will be modified, so a copy is stored for the
    # diarization import
    stored_y <- transcript_y
  }

  added <- 0
  removed <- 0

  while (any(!transcript_x$seen)) {

    i <- which(!transcript_x$seen)[1]

    transcript_x$seen[i] <- TRUE

    # Find the closest segment in the second transcript
    time <- transcript_x$start[i]
    candidate_pos <- which.min(abs(transcript_y$start - time))
    candidate <- transcript_y[candidate_pos, ]

    # This line allows to speed up the process if there are non-consecutive
    # empty segments in the first transcript
    transcript_y <- transcript_y[-candidate_pos, ]

    # Find a segment in the first transcript close to the candidate
    to_change <- which.min(abs(candidate$start - transcript_x$start))

    # Overwrite the segment in the first transcript with the candidate if it is
    # empty
    if (transcript_x$text[to_change] %in% c("", "[...]")) {
      transcript_x$start[to_change] <- candidate$start
      transcript_x$end[to_change] <- candidate$end
      transcript_x$text[to_change] <- candidate$text
      transcript_x$seen[to_change] <- TRUE

      added <- added + 1
    }

    # Remove segments in the first transcript that are empty and
    # are included in the newly added segment
    redundant <- which(transcript_x$start >= candidate$start &
                         transcript_x$end <= candidate$end &
                         transcript_x$text %in% c("", "[...]")
    )

    if (any(redundant)) {
      transcript_x <- transcript_x[-redundant, ]

      removed <- removed + length(redundant)
    }

    # All segments in the second transcript have been used. Stop the loop.
    if (nrow(transcript_y) == 0) {
      break
    }

  }

  transcript_x$seen <- NULL

  cat("Added", added, "segments.\n")
  cat("Removed", removed, "segments.\n")

  if (import_diarization) {

    # Train a GloVe model on both transcripts
    glove_model <- c(transcript_x$text, transcript_y$text) |>
      purrr::discard(is_silent) |>
      generate_model()

    message("Importing diarization...")

    transcript_y <- stored_y
    transcript_x$speaker <- NA

    for (i in 1:nrow(transcript_x)) {

      x_text <- transcript_x$text[i]

      # Skip silent segments
      if (is_silent(x_text) || !is.na(transcript_x$speaker[i])) {
        next
      }

      # Search the closest segments in the second transcript in a 30 seconds
      # range
      search_time_range <- transcript_x$start[i] + c(-30, 30)

      y_range <- seq(
        which.min(abs(transcript_y$start - search_time_range[1])),
        which.min(abs(transcript_y$start - search_time_range[2])),
      )

      # No need to use speaker imputation if there is only one speaker in the
      # range
      speakers <- transcript_y$speaker[y_range] |> stats::na.omit() |> unique()

      if (length(speakers) == 1) {
        transcript_x$speaker[i] <- speakers
        next
      }

      # Built a data frame joining all the segments in the second transcript
      # that are in the search range coming from an non-interrupted series of
      # segments from the same speakers
      y_probes <- transcript_y[y_range,] |>
        group_by(gID = consecutive_id(.data$speaker)) |>
        summarize(
          text = paste(.data$text, collapse = " "),
          speaker = .data$speaker[1]
        )

      # Calculate the semantic similarity between the segment in the first
      # transcript and the candidate segments in the second transcript
      y_probes$similarity <- compute_text_sim(
        x_text, y_probes$text, glove_model
      )

      # Select the most likely speaker
      candidate_pos <- which.max(y_probes$similarity)
      transcript_x$speaker[i] <- y_probes$speaker[candidate_pos]

    }
  }

  clean_transcript(transcript_x)
}

#' Add chat messages to a transcript
#'
#' This function adds chat messages written during an online/hybrid meeting to
#' the transcript.
#'
#' @param transcript A data frame containing the transcript.
#' @param chat_data A string with the path to the chat file or a data frame
#'   containing the chat data.
#' @param start_time The start time of the meeting. A string in the format
#'   "HH:MM AM/PM" or "HH:MM:SS".
#' @param chat_format A string indicating the online meeting platform used to
#'   generate the chat file. Only "webex" is supported at the moment.
#'
#' @return A data frame containing the transcript with the chat messages added.
#'
#' @export
#'
add_chat_transcript <- function(
    transcript,
    chat_data,
    start_time,
    chat_format = "webex"
) {

  chat_format <- match.arg(chat_format)

  if (is.character(chat_data)) {

    if (!file.exists(chat_data)) {
      stop("Chat file not found.")
    }

    # Guess time format
    time_format <- if_else(
      stringr::str_detect(start_time, "AM|PM"),
      "%I:%M %p", "%H:%M:%S"
    )

    # Parse the start time
    start_time <- lubridate::parse_date_time(start_time, time_format) |>
      as.numeric()

    tryCatch(
      chat_data <- readLines(chat_data, skipNul = T) |>
        # Remove encoding characters
        stringr::str_remove_all("\xfe|\xff|\xff|\xfe|\xef|\xbb|\xbf") |>
        purrr::discard(is_silent) |>
        purrr::map(~ stringr::str_split_1(.x, "\\t\\s*") |> t()) |>
        do.call(what = "rbind") |>
        as.data.frame() |>
        setNames(c("date", "start", "speaker", "text")) |>
        mutate(
          date = NULL,
          start = lubridate::parse_date_time(.data$start, time_format) |>
            as.numeric(),
          start = .data$start - start_time,
          speaker = stringr::str_remove(.data$speaker, "from ") |>
            # Do not remove the white space in the regex, is important!
            stringr::str_remove("( to everyone)?:") |>
            stringr::str_replace_all("(?<=\\s)[a-z]", toupper),
          speaker = paste(.data$speaker, "(from chat)")
        ),
      error = \(e) {
        stop("Error parsing chat file: ", e)
      })
  }

  # Add the chat messages to the transcript
  # left_join should position the chat messages in the right place
  left_join(transcript, chat_data)
}

#' Full Speech-To-Summary Workflow Execution
#'
#' This function executes the full speech-to-summary workflow, from the
#' transcription of the audio recording to the generation of the summary tree,
#' to the formatting of the summary tree into a human-readable text.
#'
#' @param audio_path A string with the path to an audio file or a folder containing
#'   audio files. If a folder is provided, the function will process all audio
#'   files in the folder. See `perform_speech_to_text` for more details.
#' @param stt_intermediate_dir A string with the path to the folder where the
#'   intermediate json files generated by the speech-to-text model will be
#'   stored. See `perform_speech_to_text` for more details.
#' @param stt_overwrite_intermediate_files A boolean indicating whether the
#'   intermediate json files generated by the speech-to-text model should be
#'   overwritten if they already exist. See `perform_speech_to_text` for more
#'   details.
#' @param stt_model A string indicating the speech-to-text model to use. See
#'   `perform_speech_to_text` for more details.
#' @param stt_initial_prompt A string with the path to a file containing the
#'   initial prompt to be used by the speech-to-text model. See
#'   `perform_speech_to_text` for more details.
#' @param stt_language A string indicating the language of the audio file. See
#'   `perform_speech_to_text` for more details.
#' @param extra_stt_args A list containing extra arguments to be passed to the
#'   stt model function. See `perform_speech_to_text` for more details.
#' @param transcript_to_merge A string with the path to the transcript file to
#'   be merged with the transcript generated by the speech-to-text model. See
#'   `merge_transcripts` for more details.
#' @param import_diarization_on_merge A boolean indicating whether the speaker
#'   should be imported from the transcript to be merged, if present. See
#'   `merge_transcripts` for more details.
#' @param chat_file A string with the path to a file containing the chat data.
#'   See `add_chat_transcript` for more details.
#' @param chat_start_time The start time of the meeting. A string in the format
#'   "HH:MM AM/PM" or "HH:MM:SS". See `add_chat_transcript` for more details.
#' @param chat_format A string indicating the online meeting platform used to
#'   generate the chat file. See `add_chat_transcript` for more details.
#' @param transcript_output_file A string with the path to the output file where
#'   the transcript will be written.
#' @param overwrite_transcript A boolean indicating whether the transcript
#'   output file should be overwritten if it already exists.
#' @param agenda A list containing the agenda items or a path to an R file
#'   containing such list. See `summarise_full_meeting` for more details.
#' @param meeting_description A string containing a description of the meeting.
#'   See `summarise_transcript` for more details.
#' @param meeting_audience A string containing a description of the audience of
#'   the meeting and what to focus on in the summary. See `summarise_transcript`
#'   for more details.
#' @param vocabulary A character vector of specific vocabulary words, names,
#'   definitions, to help the LLM recognise misspellings and abbreviations. See
#'   `summarise_transcript` for more details.
#' @param consider_diarization A logical indicating whether the summarisation
#'   should take into account the diarization of the transcript. See
#'   `summarise_transcript` for more details.
#' @param extra_diarization_instructions Additional diarization instructions.
#'   See `summarise_transcript` for more details.
#' @param llm_provider A string indicating the LLM provider to use for the
#'   summarization. See `summarise_transcript` for more details.
#' @param extra_summarise_args Additional arguments passed to the
#'   `interrogate_llm` function. See `summarise_transcript` for more details.
#' @param summarization_output_file A string with the path to the output file
#'   where the summary tree will be written. Should be a .R file. See
#'   `summarise_full_meeting` for more details.
#' @param overwrite_summary_tree A boolean indicating whether the summary tree
#'   output file should be overwritten if it already exists.
#' @param formatted_output_file A string with the path to the output file where the
#'   formatted summary will be written.
#' @param overwrite_formatted_output A boolean indicating whether the formatted
#'   summary output file should be overwritten if it already exists.
#'
#' @return A list containing the transcript data, the summary tree and the
#'   formatted summary.
#'
#' @export
#'
#'
speech_to_summary_workflow <- function(
    # Arguments for `perform_speech_to_text`
  audio_path,

  stt_intermediate_dir = file.path(dirname(audio_path), "transcripts"),
  stt_overwrite_intermediate_files = FALSE,
  stt_model,
  stt_initial_prompt = NULL,
  stt_language = "en",
  extra_stt_args = NULL,

  # Arguments for `merge_transcripts`
  transcript_to_merge = NULL,
  import_diarization_on_merge = TRUE,

  # Arguments for `add_chat_transcript`
  chat_file = NULL,
  chat_start_time = NULL,
  chat_format = "webex",

  transcript_output_file = file.path(dirname(audio_path), "transcript.csv"),
  overwrite_transcript = FALSE,

  # Arguments for `summarise_full_meeting`
  agenda = file.path(dirname(audio_path), "agenda.R"),

  meeting_description = NULL,
  meeting_audience = NULL,
  vocabulary = NULL,
  consider_diarization = TRUE,
  extra_diarization_instructions = NULL,
  llm_provider = NULL,
  extra_summarise_args = NULL,

  summarization_output_file = file.path(dirname(audio_path), "event_summary.R"),
  overwrite_summary_tree = FALSE,

  # Arguments for `format_summary_tree`
  formatted_output_file = file.path(dirname(audio_path), "event_summary.txt"),
  overwrite_formatted_output = FALSE
) {

  # Perform speech to text
  if (overwrite_transcript ||
      (is.character(transcript_output_file) &&
       !file.exists(transcript_output_file))) {

    message("\n### Performing speech to text...\n")

    stt_args <- c(list(
      audio_path = audio_path,
      output_dir = stt_intermediate_dir,
      model = stt_model,
      initial_prompt = stt_initial_prompt,
      overwrite = stt_overwrite_intermediate_files,
      language = stt_language
    ), extra_stt_args)

    # Use do.call to pass extra_stt_args to the ... argument
    transcript_data <- do.call(perform_speech_to_text, stt_args)

    # Merge transcripts
    if (!is.null(transcript_to_merge)) {
      message("\n### Merging transcripts...\n ")

      # If the transcript to merge is a file path, load the data from the file
      transcript_to_merge <- import_transcript_from_file(
        transcript_file = transcript_to_merge,
        import_diarization = import_diarization_on_merge)

      # Merge the transcripts
      transcript_data <- merge_transcripts(
        transcript_x = transcript_data,
        transcript_y = transcript_to_merge,
        import_diarization = import_diarization_on_merge)
    }

    # Add chat transcript
    if (!is.null(chat_file)) {
      message("\n### Adding chat transcript...\n")
      transcript_data <- add_chat_transcript(
        transcript = transcript_data,
        chat_data = chat_file,
        start_time = chat_start_time,
        chat_format = chat_format
      )
    }

    # Write transcript to file
    readr::write_csv(
      transcript_data, file = transcript_output_file)

  } else {
    message("\n### Loading existing transcript...\n")
    transcript_data <- readr::read_csv(transcript_output_file,
                                       show_col_types = FALSE)
  }

  # Perform summarization
  if (overwrite_summary_tree ||
      (is.character(summarization_output_file) &&
       !file.exists(summarization_output_file))) {

    # Agenda is not provided, ask whether to generate a default agenda
    if (is.null(agenda) || (is.character(agenda) && !file.exists(agenda))) {
      choice <- utils::menu(
        choices = c(
          "Generate default agenda (i.e., process the transcript as one talk)",
          "Exit (write your own agenda)"),
        title = "No valid agenda found. What do you want to do?",
      )

      if (choice != 1) stop("Aborted by user.")

      # Generate a default agenda with 1 talk/meeting if none is provided
      agenda <- list(
        list(
          from = min(transcript_data$start),
          to = max(transcript_data$end)
        )
      )
    }

    message("\n### Summarizing transcript...\n")

    summarization_args <- c(list(
      transcript_data = transcript_data,
      agenda = agenda,
      output_file = summarization_output_file,
      meeting_description = meeting_description,
      meeting_audience = meeting_audience,
      vocabulary = vocabulary,
      consider_diarization = consider_diarization,
      extra_diarization_instructions = extra_diarization_instructions,
      provider = llm_provider,
      overwrite = overwrite_summary_tree
    ), extra_summarise_args)


    summary_tree <- do.call(summarise_full_meeting, summarization_args)

  } else {
    message("\n### Loading existing summary tree...\n")
    summary_tree <- dget(summarization_output_file)
  }

  # Format summary tree

  if (overwrite_formatted_output ||
      (is.character(formatted_output_file) &&
       !file.exists(formatted_output_file))) {

    message("\n### Formatting summary tree...\n")

    formatted_summary <- format_summary_tree(
      summary_tree = summary_tree,
      agenda = agenda,
      output_file = formatted_output_file)

  } else {
    message("\n### Loading existing formatted summary...\n")
    formatted_summary <- readr::read_file(formatted_output_file)
  }

  mget(c("transcript_data", "summary_tree", "formatted_summary"))
}
