#' Parse the speech-to-text JSON output into a transcript data frame
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
#' @param event_start_time The start time of the event in the HH:MM(:SS)( AM/PM)
#'   format. Will be used to add the actual clock time to the transcript
#'   segments. If NULL, the clock time will not be added.
#'
#' @return A data frame with the text and start and end time of each segment.
#'
#' @export
parse_transcript_json <- function(
    transcript_json,
    pretty_times = TRUE,
    event_start_time = getOption("minutemaker_event_start_time")
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
  } else if (vctrs::obj_is_list(transcript_json)) {
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

    if (length(transcript_list[[i]]$segments) == 0) {
      # skip this file, there was nothing to transcribe
      next
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

  if (!is.null(event_start_time)) {
    event_start_time <- parse_event_time(event_start_time)

    if (is.na(event_start_time)) {
      stop("Start time not interpretable. Use the HH:MM(:SS)( AM/PM) format.")
    }

    full_transcript <- full_transcript |>
      mutate(
        across(
          any_of(c("start", "end")),
          ~ (event_start_time + lubridate::seconds(.x)) |> format("%T"),
          .names = "{.col}_clock")
      )
  }

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
  transcript_data <- mutate(
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
    ~ stringr::str_split_1(.x, ", *") |> table() |> max())

  transcript_data$text[repetitions > 10] <- silent()

  # Use the Whisper API results statistics to remove segments with low
  # confidence
  if (all(c("avg_logprob", "no_speech_prob") %in% names(transcript_data))) {
    condition <- with(
      transcript_data, avg_logprob < -0.5 & no_speech_prob > 0.9)
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

  # If no parameters are provided, the whole transcript is used and a warning is
  # issued
  if (
    is.null(agenda_element) && is.null(start_time) && is.null(end_time)
  ) {
    duration <- max(transcript_data$end) - min(transcript_data$start)

    if (duration > 3600) {
      warning(
        "Summarising a transcript covering more than 1 hour may ",
        "result in a loss of details. ",
        "Consider treating it as a collection of taks/meetings to ",
        "summarise separately. Cfr. `summarise_full_meeting()`"
        , call. = FALSE, immediate. = TRUE)
    }
  }

  # If an agenda element is provided, its start and end times are used
  if (!is.null(agenda_element)) {
    if (!is.numeric(agenda_element$from) || !is.numeric(agenda_element$to)) {
      stop("The agenda element must contain 'from' and 'to' ",
           "fields in numeric format. Use convert_agenda_times() to convert ",
           "the agenda element times to numeric format.")
    }

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
  if (!"speaker" %in% names(transcript_data) ||
      all(is.na(transcript_data$speaker)) ||
      n_distinct(transcript_data$speaker, na.rm = T) == 1) {
    import_diarization <- FALSE

    transcript_data$speaker <- "Unknown"
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

#' Convert agenda clock time to seconds from start and vice versa
#'
#' Users can provide events' times in the agenda following the HH:MM(:SS)(
#' AM/PM) format or as number of seconds from the start of the event. This
#' function converts the times to the other format.
#'
#' @param agenda The agenda of the meeting, that is, a list of agenda elements
#'   each with a session name, a title, speaker and moderator lists, type of
#'   talk and start and end times. Alternatively, an agenda element directly.
#' @param convert_to A string indicating whether to convert the times to seconds
#'   from the start or to clock time. Conversion to seconds will happen only if
#'   the times are in clock time format, and vice versa.
#' @param event_start_time The start time of the event in the HH:MM(:SS)( AM/PM)
#'   format or a POSIXct object. Will be used to convert event times to seconds
#'   from the start. If `NULL`, defaults to the start time of the first element
#'   in the agenda, with a warning.
#' @param conversion_format The format to use when converting to clock time.
#'   E.g., %T for HH:MM:SS, or %R for HH:MM, etc... See `?strptime` for a list
#'   of supported formats.
#'
#' @return The agenda (or agenda element) with all the times in seconds from the
#'   start.
#'
convert_agenda_times <- function(
    agenda,
    convert_to = c("seconds", "clocktime"),
    event_start_time = getOption("minutemaker_event_start_time"),
    conversion_format = "%T"
) {

  convert_to <- match.arg(convert_to)

  agenda_orig <- agenda

  # Check if agenda is actually an agenda item only
  is_agenda_element <- any(c("from", "to") %in% names(agenda))

  # Convert to agenda format in case it's just an item
  if (is_agenda_element) {
    agenda <- list(agenda)
  }

  # Check all agenda elements before converting
  for (i in seq_along(agenda)) {
    validate_agenda_element(agenda[[i]], from = TRUE, to = TRUE)
  }

  # Check if agenda times are all of the same class
  if (!all(purrr::map_lgl(agenda, ~ is.numeric(.x$from))) &&
      !all(purrr::map_lgl(agenda, ~ {
        inherits(.x$from, c("POSIXct", "character"))
      }))
  ) {
    stop("Agenda times must be all of the same class across the agenda.")
  }

  if (!is.null(event_start_time)) {

    if (inherits(event_start_time, c("POSIXct", "character"))) {

      # Parse the start time if not null
      temp <- parse_event_time(event_start_time)

      if (is.na(temp)) {
        stop("Agenda time conversion failed: ", event_start_time)
      }

      event_start_time <- temp
    } else {
      # Only character and POSIXct event start times are supported
      stop("Invalid event start time format.")
    }

  } else if (
    agenda[[1]]$from |> inherits(c("POSIXct", "character")) &&
    convert_to == "seconds"
  ) {
    # Use the first agenda time otherwise
    event_start_time <- agenda[[1]]$from |>
      parse_event_time()

    warning("No start time provided.\n",
            "Using the start time of the first agenda element.",
            call. = FALSE, immediate. = TRUE)
  } else {
    event_start_time <- lubridate::origin

    warning("No start time provided.\n",
            "Using \"00:00:00\" as start time.",
            call. = FALSE, immediate. = TRUE)
  }

  for (i in seq_along(agenda)) {

    # Convert the times
    for (time in c("from", "to")) {
      if (convert_to == "seconds" && !is.numeric(agenda[[i]][[time]])) {
        # Convert to seconds
        agenda[[i]][[time]] <- agenda[[i]][[time]] |>
          time_to_numeric(origin = event_start_time)
      } else if (convert_to == "clocktime"){

        if (is.numeric(agenda[[i]][[time]])) {
          # Convert to clock time
          cur_time <- agenda[[i]][[time]]

          agenda[[i]][[time]] <- (
            event_start_time +
              lubridate::seconds_to_period(cur_time))
        } else {
          # Allow users to change the format
          agenda[[i]][[time]] <- parse_event_time(agenda[[i]][[time]])
        }


        agenda[[i]][[time]] <- format(agenda[[i]][[time]], conversion_format)
      } else {
        # Do nothing, the time is already in the correct format
      }
    }

    # Revalidate times after transformations
    validate_agenda_element(agenda[[i]], from = TRUE, to = TRUE)

  }

  if (is_agenda_element) {
    agenda <- agenda[[1]]
  }

  agenda
}

#' Formats a summary tree into a human-readable text
#'
#' The summary functions returns a machine-readable summary tree. This function
#' takes the summary tree and formats it into a human-readable text.
#'
#' @param summary_tree A list containing the summary tree, or a string with the
#'   path to a file containing the summary tree.
#' @param agenda A list containing the agenda items. It is used to extract a
#'   number of information about each talk, such as the session, title,
#'   speakers, moderators, start and ending times. Agenda sessions/titles must
#'   be consistent with those in the `summary_tree` object.
#' @param event_start_time If agenda timings are in seconds, the starting time
#'   is needed to convert them to actual clock time. If `NULL` it will use the
#'   timing as reported in the agenda.
#' @param output_file A string with the path to the output file. If NULL, the
#'   output is returned invisibly and not written to a file.
#'
#' @return A string containing the formatted summary tree, invisibly. This is
#'   printed to the output file if one is provided.

#' @export
format_summary_tree <- function(
    summary_tree,
    agenda,
    event_start_time = getOption("minutemaker_event_start_time"),
    output_file = NULL
) {

  # Check the consistency of the summary tree and the agenda
  check_agenda_summary_tree_consistency(agenda, summary_tree)

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

    # If no summary is found for the current talk, issue a warning and skip to
    # the next talk
    if (is.null(talk_summary)) {
      warning("No summary found for ", id, ". Skipping.", call. = FALSE)
      next
    }

    # Covert times from second to clock time if possible
    agenda_element <- convert_agenda_times(
      agenda_element, convert_to = "clock",
      event_start_time = event_start_time, conversion_format = "%R"
    )

    # Generate a text version of the summary, with session, title, speakers,
    # moderators and summary
    # TODO: streamline this repetitive code
    agenda_element$speakers <- stringr::str_flatten_comma(
      agenda_element$speakers)
    agenda_element$moderators <- stringr::str_flatten_comma(
      agenda_element$moderators)
    agenda_element$session <- ifelse(
      is.null(agenda_element$session), "", agenda_element$session)
    agenda_element$title <- ifelse(is.null(
      agenda_element$title), "", agenda_element$title)

    output_piece <- stringr::str_glue_data(agenda_element,
                                           "Session: {session};
    Title: {title};
    Speakers: {speakers};
    Moderators: {moderators};
    Time: {from} - {to};

    {talk_summary}") |>
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
#' @param transcript_x A data frame containing the transcript to which the
#'   second transcript will be merged.
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
      generate_glove_model()

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

      # Some segments are too short to compute embeddings, so they are removed
      y_probes <- y_probes |> filter(!is.na(.data$similarity))

      if (nrow(y_probes) == 0) {
        # No segments in the second transcript are similar to the segment in the
        # first transcript. Skip the segment.
        transcript_x$speaker[i] <- NA
      } else {
        # Select the most likely speaker
        candidate_pos <- which.max(y_probes$similarity)
        transcript_x$speaker[i] <- y_probes$speaker[candidate_pos]
      }

    }
  }

  # Change set empty speakers (e.g. empty, non-NA, strings) to NA
  transcript_x$speaker[is_silent(transcript_x$speaker)] <- NA

  clean_transcript(transcript_x)
}

#' Add chat messages to a transcript
#'
#' This function adds chat messages written during an online/hybrid meeting to
#' the transcript.
#'
#' @param transcript_data A data frame containing the transcript.
#' @param chat_transcript A string with the path to the chat file or a data frame
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
    transcript_data,
    chat_transcript,
    start_time,
    chat_format = "webex"
) {

  chat_format <- match.arg(chat_format)

  if (is.character(chat_transcript)) {

    if (!file.exists(chat_transcript)) {
      stop("Chat file not found.")
    }

    # Parse the start time
    start_time <- parse_event_time(start_time) |>
      as.numeric()

    tryCatch({
      chat_transcript <- readLines(chat_transcript, skipNul = T) |>
        # Remove encoding characters
        stringr::str_remove_all("\xfe|\xff|\xff|\xfe|\xef|\xbb|\xbf") |>
        purrr::discard(is_silent) |>
        purrr::map(~ stringr::str_split_1(.x, "\\t\\s*") |> t())

      # Loop over the chat messages and concatenate the ones that are split
      # across multiple lines
      for (i in rev(seq_along(chat_transcript))) {
        curr <- chat_transcript[[i]]
        if (ncol(curr) == 1) {
          prev <- chat_transcript[[i - 1]]
          chat_transcript[[i - 1]][1, ncol(prev)] <- paste(
            prev[1, ncol(prev)], curr[1, ncol(curr)])
          chat_transcript[[i]] <- NULL
        }
      }

      chat_transcript <- do.call("rbind", chat_transcript) |>
        as.data.frame() |>
        setNames(c("date", "start", "speaker", "text")) |>
        mutate(
          date = NULL,
          start = parse_event_time(.data$start) |>
            as.numeric(),
          start = .data$start - start_time,
          speaker = stringr::str_remove(.data$speaker, "from ") |>
            # Do not remove the white space in the regex, is important!
            stringr::str_remove("( to everyone)?:") |>
            stringr::str_replace_all("(?<=\\s)[a-z]", toupper),
          speaker = paste(.data$speaker, "(from chat)")
        )},
      error = \(e) {
        stop("Error parsing chat file: ", basename(chat_transcript), "\n\n", e)
      })
  } else if (is.data.frame(chat_transcript)) {
    # Check if the chat data has the required columns
    if (!all(c("start", "speaker", "text") %in% names(chat_transcript))) {
      stop("Chat data must contain 'start', 'speaker' and 'text' columns.")
    }
  } else {
    stop("Unsupported chat data format.")
  }

  # Add the chat messages to the transcript
  # left_join should position the chat messages in the right place
  left_join(transcript_data, chat_transcript)
}

#' Full Speech-To-Summary Workflow Execution
#'
#' This function executes the full speech-to-summary workflow, from the
#' transcription of the audio recording to the generation of the summary tree,
#' to the formatting of the summary tree into a human-readable text.
#'
#' @param target_dir A path to the folder where the summarisation process will
#'   take place. All the intermediate and output files will be stored and looked
#'   for here. It allows encapsulating a single summarisation task from start to
#'   end.
#' @param source_audio A path to an audio file in wav or mp3/4 format. If
#'   `split_audio` is TRUE, it will be split into multiple files which will be
#'   place into the `stt_audio_dir` folder. See `split_audio` for more details.
#' @param split_audio A boolean indicating whether the audio file should be
#'   split into multiple files. Some models, like "Whisper" based ones, can
#'   process files only up to 25 MB. See `split_audio` for more details.
#' @param split_audio_duration The duration of each splitted audio file in
#'   minutes. 20 minutes equate to more or less 7-8 MB. See `split_audio` for
#'   more details.
#' @param stt_audio_dir A string with the path to the folder where the audio
#'   files to transcribe will be stored. See `split_audio` and
#'   `perform_speech_to_text` for more details. The files in this folder will be
#'   used as input for the speech-to-text model, so you can manually add audio
#'   files to transcribe directly here.
#' @param overwrite_stt_audio A boolean indicating whether the audio files to
#'   transcribe should be overwritten if they already exist.
#' @param stt_output_dir A string with the path to the folder where the json
#'   files generated by the speech-to-text model will be stored. See
#'   `perform_speech_to_text` for more details.
#' @param stt_model A string indicating the speech-to-text model to use. See
#'   `perform_speech_to_text` for more details.
#' @param stt_initial_prompt A string with the path to a file containing the
#'   initial prompt to be used by the speech-to-text model. See
#'   `perform_speech_to_text` for more details.
#' @param stt_language A string indicating the language of the audio file. See
#'   `perform_speech_to_text` for more details.
#' @param extra_stt_args A list containing extra arguments to be passed to the
#'   stt model function. See `perform_speech_to_text` for more details.
#' @param stt_overwrite_output_files A boolean indicating whether the json files
#'   generated by the speech-to-text model should be overwritten if they already
#'   exist. See `perform_speech_to_text` for more details.
#' @param transcript_file A path to the output file where the transcript will be
#'   written.
#' @param event_start_time The start time of the event in the HH:MM(:SS)( AM/PM)
#'   format. Will be used to add the actual clock time to the transcript
#'   segments. If NULL, the clock time will not be added. See
#'   `parse_transcript_json()` for more details.
#' @param overwrite_transcript A boolean indicating whether the transcript
#'   output file should be overwritten if it already exists.
#' @param transcript_to_merge A string with the path to the transcript file to
#'   be merged with the transcript generated by the speech-to-text model. It
#'   will be picked up automatically if a .vtt or .srt file is available in the
#'   target directory. Pass NULL to disable the automatic importation of
#'   transcript files. See `merge_transcripts` for more details.
#' @param import_diarization_on_merge A boolean indicating whether the speaker
#'   should be imported from the transcript to be merged, if present. See
#'   `merge_transcripts` for more details.
#' @param chat_file A string with the path to a file containing the chat data.
#'   It will be picked up automatically if a file with "Chat" in its name is
#'   available in the target directory. Pass NULL to disable the automatic chat
#'   file importation. See `add_chat_transcript` for more details. Note that
#'   `event_start_time` must be set if `chat_file` is not NULL.
#' @param chat_format A string indicating the online meeting platform used to
#'   generate the chat file. See `add_chat_transcript` for more details.
#' @param agenda The agenda of the meeting, that is, a list of agenda elements
#'   each with a session name, a title, speaker and moderator lists, type of
#'   talk, talk description and start and end times. Alternatively, the path to
#'   an R file containing such a list. See `summarise_full_meeting` for more
#'   details. If NULL, the user will be asked if the system should try to
#'   generate the agenda automatically, using the `infer_agenda_from_transcript`
#'   function.
#' @param expected_agenda A character string. Only used if the `agenda` argument
#'   is `NULL` and the user requests the automatic agenda generation. this
#'   string will be used to drive the LLM while generating the agenda. See
#'   `infer_agenda_from_transcript` for more details.
#' @param agenda_generation_window_size The size of the window in seconds to
#'   analyze at once when generating the agenda. See
#'   `infer_agenda_from_transcript` for more details.
#' @param agenda_generation_output_file A string with the path to the output
#'   file where the automatically generated agenda will be written. Should be a
#'   .R file. See `infer_agenda_from_transcript` for more details.
#' @param extra_agenda_generation_args Additional arguments passed to the
#'  `infer_agenda_from_transcript` function. See `infer_agenda_from_transcript`
#'  for more details. Note that the `diarization_instructions` argument for this
#'  function will be taken from the `extra_agenda_generation_args` argument.
#' @param summarization_method A string indicating the summarization method to
#'   use. See `summarise_full_meeting` for more details.
#' @param event_description A string containing a description of the meeting.
#'   See `summarise_transcript` for more details.
#' @param event_audience A string containing a description of the audience of
#'   the meeting and what to focus on in the summary. See `summarise_transcript`
#'   for more details.
#' @param vocabulary A character vector of specific vocabulary words, names,
#'   definitions, to help the LLM recognise misspellings and abbreviations. See
#'   `summarise_transcript` for more details.
#' @param consider_diarization A logical indicating whether the summarisation
#'   should take into account the diarization of the transcript. See
#'   `summarise_transcript` for more details.
#' @param summary_structure,extra_diarization_instructions,extra_output_instructions
#' Specific instructions necessary to build the summarisation prompt. See
#' `summarise_transcript` for more details and run `get_prompts()` to see the
#' defaults. See `summarise_transcript` for more details.
#' @param llm_provider A string indicating the LLM provider to use for the
#'   summarization. See `summarise_transcript` for more details.
#' @param extra_summarise_args Additional arguments passed to the
#'   `interrogate_llm` function. See `summarise_transcript` for more details.
#' @param summarization_window_size The size of the summarization window in
#'   minutes if the "rolling"  method is used. See `summarise_transcript` for
#'   more details.
#' @param summarization_output_length An indication to the LLM regarding the
#'   length of the output. See `summarise_transcript` for more details.
#' @param summarization_output_file A string with the path to the output file
#'   where the summary tree will be written. Should be a .R file. See
#'   `summarise_full_meeting` for more details.
#' @param overwrite_summary_tree A boolean indicating whether the summary tree
#'   output file should be overwritten if it already exists.
#' @param formatted_output_file A string with the path to the output file where
#'   the formatted summary will be written.
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

  target_dir = getwd(),

  # Arguments for `split_audio`
  source_audio = list.files(target_dir, pattern = ".*\\.(wav|mp\\d)$"
                            , full.names = T)[1],
  split_audio = TRUE,
  split_audio_duration = 40,
  stt_audio_dir = file.path(target_dir, "audio_to_transcribe"),
  overwrite_stt_audio = FALSE,

  # Arguments for `perform_speech_to_text`
  stt_output_dir = file.path(target_dir, "transcription_output_data"),
  stt_model = getOption("minutemaker_stt_model"),
  stt_initial_prompt = NULL,
  stt_language = "en",
  extra_stt_args = NULL,
  stt_overwrite_output_files = FALSE,

  # Arguments for `parse_transcript_json`
  transcript_file = file.path(target_dir, "transcript.csv"),
  event_start_time = getOption("minutemaker_event_start_time"),
  overwrite_transcript = FALSE,

  # Arguments for `merge_transcripts`
  transcript_to_merge = list.files(target_dir, pattern = "\\.(vtt|srt)"
                                   , full.names = T)[1],
  import_diarization_on_merge = TRUE,

  # Arguments for `add_chat_transcript`
  chat_file = list.files(target_dir, pattern = "Chat"
                         , full.names = T)[1],
  chat_format = "webex",

  # Arguments for `summarise_full_meeting` and `infer_agenda_from_transcript`
  agenda = file.path(target_dir, "agenda.R"),
  expected_agenda = NULL,
  agenda_generation_window_size = 3600,
  agenda_generation_output_file = file.path(target_dir, "agenda.R"),
  extra_agenda_generation_args = NULL,

  event_description = NULL,
  event_audience = "An audience with understanding of the topic",
  vocabulary = NULL,
  consider_diarization = TRUE,
  summary_structure = get_prompts("summary_structure"),
  extra_diarization_instructions = NULL,
  extra_output_instructions = NULL,
  llm_provider = NULL,
  extra_summarise_args = NULL,
  summarization_window_size = 15,
  summarization_output_length = 3,
  summarization_method = c("simple", "rolling"),

  summarization_output_file = file.path(target_dir, "event_summary.R"),
  overwrite_summary_tree = FALSE,

  # Arguments for `format_summary_tree`
  formatted_output_file = file.path(target_dir, "event_summary.txt"),
  overwrite_formatted_output = FALSE
) {

  summarization_method <- match.arg(summarization_method)

  ## Perform audio splitting ##

  # Check if the stt audio dir is empty or overwrite is TRUE
  if (
    overwrite_stt_audio || purrr::is_empty(list.files(stt_audio_dir))
  ) {

    if (purrr::is_empty(source_audio)) {
      stop("\nNo valid source audio file provided.\n")
    }


    if (isFALSE(split_audio)) {
      # If `split_audio` is FALSE, the audio file will be just copied to the
      # `stt_audio_dir` folder

      if (!dir.exists(stt_audio_dir)) {
        dir.create(stt_audio_dir)
      }

      message("\n### Moving source audio file to ", stt_audio_dir,
              ", without modification...\n")

      file.copy(source_audio, stt_audio_dir)
    } else {

      message("\n### Splitting audio file...\n")

      # Split the audio file. The splitted audio will be saved in the
      # "recording_parts" folder in the same directory as the original audio file
      split_audio(
        audio_file = source_audio,
        output_folder = stt_audio_dir,
        segment_duration = split_audio_duration
      )
    }
  } else {
    message("\n### Loading existing splitted audio files from ",
            basename(stt_audio_dir), "...\n")
  }

  ## Perform speech to text ##

  # Check if the stt output folder is empty or overwrite is TRUE
  if (
    stt_overwrite_output_files ||
    length(list.files(stt_output_dir)) < length(list.files(stt_audio_dir))
  ) {

    message("\n### Performing speech to text...\n")

    # A speech-to-text model is required
    if (is.null(stt_model)) {
      stop("No speech-to-text model provided.")
    }

    # Check if the splitted audio files exist
    if (purrr::is_empty(list.files(stt_audio_dir))) {
      stop("No audio files found in ", stt_audio_dir)
    }

    stt_args <- c(list(
      audio_path = stt_audio_dir,
      output_dir = stt_output_dir,
      model = stt_model,
      initial_prompt = stt_initial_prompt,
      overwrite = stt_overwrite_output_files,
      language = stt_language
    ), extra_stt_args)

    # Use do.call to pass extra_stt_args to the ... argument
    do.call(perform_speech_to_text, stt_args)
  } else {
    message("\n### Loading existing transcript data files from ",
            basename(target_dir), "...\n")
  }

  ## Create the transcript file ##

  # Check if the transcript file doesn't exists or overwrite is TRUE
  if (overwrite_transcript || !file.exists(transcript_file)) {

    # Generate the trascript from the json output data
    transcript_data <- parse_transcript_json(
      stt_output_dir,
      event_start_time = event_start_time)

    # Merge transcripts
    if (!purrr::is_empty(transcript_to_merge) && !is.na(transcript_to_merge)) {

      message("\n### Merging transcripts...\n ")

      if (!file.exists(transcript_to_merge)) {
        stop("Transcript file to merge not valid.")
      }

      # If the transcript to merge is a file path, load the data from the file
      if (is.character(transcript_to_merge)) {
        transcript_to_merge <- import_transcript_from_file(
          transcript_file = transcript_to_merge,
          import_diarization = import_diarization_on_merge)
      }

      # Merge the transcripts
      transcript_data <- merge_transcripts(
        transcript_x = transcript_data,
        transcript_y = transcript_to_merge,
        import_diarization = import_diarization_on_merge)
    }

    # Add chat transcript
    if (!purrr::is_empty(chat_file) && !is.na(chat_file)) {

      message("\n### Adding chat transcript...\n")

      if (is.null(event_start_time)) {
        stop("Chat file found but no start time provided.")
      }

      transcript_data <- add_chat_transcript(
        transcript_data = transcript_data,
        chat_transcript = chat_file,
        start_time = event_start_time,
        chat_format = chat_format
      )
    }

    # Write transcript to file
    readr::write_csv(
      transcript_data, file = transcript_file)

  } else {
    message("\n### Loading existing transcript from ",
            basename(transcript_file), "...\n")

    transcript_data <- readr::read_csv(transcript_file,
                                       show_col_types = FALSE)
  }

  ## Perform summarization ##

  # Agenda is not provided, ask whether to generate a default agenda
  if (purrr::is_empty(agenda) ||
      (is.character(agenda) && !file.exists(agenda))) {

    cat("No agenda was provided or found in the target directory.\n")

    # If interactive() ask the user whether to generate a dummy agenda otherwise
    # stop the process.
    if (!interactive()) {

      stop("Create an agenda using the information in the transcript.\n")

    } else {
      choice <- utils::menu(
        choices = c(
          "Generate the agenda automatically (You will need to review it before proceeding)",
          "Exit (write your own agenda)"
        ),
        title = "How do you want to proceed?"
      )
    }

    if (choice != 1) {
      message("Aborted by user. Returning transcript data only (invisibly).")
      return(invisible(transcript_data))
    }

    # Generate a default agenda with 1 talk/meeting if none is provided
    agenda_infer_args <- c(list(
      transcript = transcript_data,
      event_description = event_description,
      vocabulary = vocabulary,
      diarization_instructions = extra_diarization_instructions,
      start_time = event_start_time,
      expected_agenda = expected_agenda,
      window_size = agenda_generation_window_size,
      output_file = file.path(target_dir, "agenda.R"),
      provider = llm_provider
    ), extra_agenda_generation_args)

    agenda <- do.call(infer_agenda_from_transcript, agenda_infer_args)

    message("Agenda generated. Please review it before proceeding.")
    return(invisible(transcript_data))
  }

  message("\n### Summarizing transcript...\n")

  if (is.null(llm_provider)) {
    stop("No LLM provider defined.")
  }

  summarization_args <- c(list(
    transcript_data = transcript_data,
    agenda = agenda,
    method = summarization_method,

    window_size = summarization_window_size,
    output_length = summarization_output_length,

    output_file = summarization_output_file,

    event_start_time = event_start_time,
    event_description = event_description,
    event_audience = event_audience,
    vocabulary = vocabulary,
    consider_diarization = consider_diarization,

    summary_structure = summary_structure,
    extra_diarization_instructions = extra_diarization_instructions,
    extra_output_instructions = extra_output_instructions,

    provider = llm_provider,
    overwrite = overwrite_summary_tree
  ), extra_summarise_args)


  summary_tree <- do.call(summarise_full_meeting, summarization_args)

  ## Format summary tree ##

  if (overwrite_formatted_output || !file.exists(formatted_output_file)) {

    message("\n### Formatting summary tree...\n")

    formatted_summary <- format_summary_tree(
      summary_tree = summary_tree,
      agenda = agenda,
      event_start_time = event_start_time,
      output_file = formatted_output_file)

  } else {
    message("\n### Loading existing formatted summary...\n")
    formatted_summary <- readr::read_file(formatted_output_file)
  }

  mget(c("transcript_data", "summary_tree", "formatted_summary"))
}
