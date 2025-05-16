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
  file_input <- FALSE

  # Is the input a string?
  transcript_list <- if (is.character(transcript_json)) {
    # Is the string a path to a folder?
    if (dir.exists(transcript_json)) {
      # Mark the input as a path
      file_input <- TRUE

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
          cli::cli_abort(
            c(
              "Error parsing JSON file(s) from directory
                {.path {transcript_json}}:",
              "x" = "{e$message}",
              "i" = "Ensure all JSON files in the directory are valid."
            ),
            parent = e
          )
        }
      )
    } else if (file.exists(transcript_json)) {
      # Is the string a path to a file?

      # Mark the input as a path
      file_input <- TRUE

      json_files <- transcript_json

      # Read the JSON file
      tryCatch(
        list(jsonlite::read_json(transcript_json)),
        error = function(e) {
          cli::cli_abort(
            c(
              "Error parsing JSON file {.path {transcript_json}}:",
              "x" = "{e$message}"
            ),
            parent = e
          )
        }
      )
    } else {
      # Is the string a JSON string?

      # Read the JSON string
      tryCatch(
        list(jsonlite::fromJSON(transcript_json)),
        error = function(e) {
          cli::cli_abort(
            c(
              "Error parsing JSON string input:",
              "x" = "{e$message}"
            ),
            parent = e
          )
        }
      )
    }
  } else if (rlang::is_list(transcript_json)) {
    # Is the input a list?
    list(transcript_json)
  } else {
    cli::cli_abort(
      "Unsupported transcript format provided to
      {.fn parse_transcript_json}.
      Expecting a JSON string, file path, directory path, or list."
    )
  }

  # Initialize full_transcript with the expected schema.
  # This ensures that even if no segments are processed, the function returns
  # a data frame with the correct structure.
  full_transcript <- dplyr::tibble(
    doc = integer(),
    file = character(),
    start = numeric(),
    end = numeric(),
    text = character(),
    speaker = character()
  )
  last_time <- 0
  to_import <- c(
    "start",
    "end",
    "text",
    "speaker",
    "avg_logprob",
    "no_speech_prob"
  )

  # Extract the data from each JSON file
  for (i in seq_along(transcript_list)) {
    # Raise an error if the JSON file does not contain any data
    if (!"segments" %in% names(transcript_list[[i]])) {
      cli::cli_abort(
        c(
          "JSON file is missing expected 'segments' data.",
          "x" = "File {.file {basename(json_files[i])}}
            does not contain transcription segments.",
          "i" = "Please remove this file or ensure it has the correct format
            and try again."
        ),
        wrap = TRUE
      )
    }

    if (length(transcript_list[[i]]$segments) == 0) {
      # skip this file, there was nothing to transcribe
      cli::cli_inform(
        c(
          "!" = "Skipping empty file (no segments found):
        {.file {basename(json_files[i])}}"
        )
      )

      next
    }

    # Check if the file path is valid, unless the input is a string
    if (
      file_input &&
        (!rlang::is_scalar_character(json_files[i]) ||
          !file.exists(json_files[i]))
    ) {
      cli::cli_abort(
        c(
          "Invalid file path format.",
          "x" = "File path at index {i} is not valid.",
          "i" = "Please ensure all file paths are valid strings."
        )
      )
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
        file = if (!is.na(json_files[i])) {
          basename(json_files[i])
        } else NA_character_,
        .before = 1
      ) |>
      # Add the previous file time to the start and end times
      mutate(
        start = .data$start + last_time,
        end = .data$end + last_time
      )

    full_transcript <- dplyr::bind_rows(full_transcript, transcript_data)

    # Get the last segment info
    last_time <- transcript_data$end |> tail(1)
  }

  full_transcript <- clean_transcript(full_transcript)

  if (!is.null(event_start_time)) {
    event_start_time <- parse_event_time(event_start_time)

    if (is.na(event_start_time)) {
      cli::cli_abort(
        c(
          "Event start time
          {.val {getOption(\"minutemaker_event_start_time\", \"NULL\")}}
          is not interpretable.",
          "x" = "Failed to parse the start time.",
          "i" = "Please use the HH:MM(:SS) or HH:MM(:SS) AM/PM format
            (e.g., \"09:30\" or \"2:00 PM\")."
        )
      )
    }

    full_transcript <- full_transcript |>
      mutate(
        across(
          any_of(c("start", "end")),
          ~ (event_start_time + lubridate::seconds(.x)) |> format("%T"),
          .names = "{.col}_clock"
        )
      )
  }

  if (pretty_times) {
    full_transcript <- full_transcript |>
      mutate(
        across(
          any_of(c("start", "end")),
          ~ lubridate::seconds_to_period(.x) |> round(),
          .names = "{.col}_formatted"
        )
      )
  }

  full_transcript
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
    cli::cli_abort(
      "Unsupported file format. Supported formats are SRT and VTT."
    )
  }

  times_pos <- which(stringr::str_detect(lines, "-->"))

  # Iterate over each time position
  purrr::map(
    times_pos,
    ~ {
      # Extract the start and end times
      times <- stringr::str_split_1(lines[.x], "-->") |>
        stringr::str_squish() |>
        lubridate::hms() |>
        as.numeric()

      # Safely get the line content for text (line after timestamp)
      text_line_idx <- .x + 1
      raw_text_content <- if (text_line_idx <= length(lines)) {
        lines[text_line_idx]
      } else {
        NA_character_
      }

      # Clean the text content: remove MS Teams speaker tag (if any) from the
      # text field, and squish whitespace.
      # Note: Speaker extraction from the tag (if import_diarization is TRUE)
      # happens using raw_text_content later.
      # stringr::str_remove_all handles NA input by returning NA.
      # stringr::str_squish handles NA input by returning NA.
      processed_text <- raw_text_content |>
        stringr::str_remove_all("</?v.*?>") |>
        stringr::str_squish()

      # Ensure final text is NA_character_ if it's effectively empty or was
      # initially NA
      final_text <- if (is.na(processed_text) || processed_text == "") {
        NA_character_
      } else {
        processed_text
      }

      extract <- dplyr::tibble(
        start = times[1],
        end = times[2],
        text = final_text
      )

      # If the transcript is diarized, extract the speaker
      if (import_diarization) {
        speaker_to_assign <- NA_character_ # Initialize speaker

        # Get content of line above timestamp, if it exists
        line_above_idx <- .x - 1
        line_above_content <- if (line_above_idx >= 1)
          lines[line_above_idx] else NA_character_

        # Check line above for standard VTT cue ID with quoted speaker
        # Guard against NA input for str_detect. Regex checks for digits then a
        # space.
        if (
          !is.na(line_above_content) &&
            stringr::str_detect(line_above_content, "^\\d+\\s")
        ) {
          extracted_name_std_vtt <- stringr::str_extract(
            line_above_content,
            '(?<=")([^"]+)(?=")'
          )
          if (!is.na(extracted_name_std_vtt)) {
            speaker_to_assign <- extracted_name_std_vtt
          }
          # If cue ID line matches pattern but no speaker in quotes,
          # speaker_to_assign remains NA.
        } else if (
          !is.na(raw_text_content) &&
            stringr::str_detect(raw_text_content, "^<v\\s")
        ) {
          # Else, check current text line (raw_text_content) for MS Teams VTT
          # style (<v Speaker>Text)
          # This is only checked if the line above was not a VTT Cue ID with a
          # speaker.
          # Regex checks for "<v" then a space.
          extracted_name_ms_teams <- stringr::str_extract(
            raw_text_content,
            '(?<=<v\\s)([^>]+)(?=>)'
          )
          if (!is.na(extracted_name_ms_teams)) {
            speaker_to_assign <- extracted_name_ms_teams
          }
        }
        # If neither pattern matches, speaker_to_assign remains NA_character_.

        extract <- extract |>
          dplyr::mutate(
            speaker = speaker_to_assign,
            .before = text
          )
      }

      extract
    }
  ) |>
    dplyr::bind_rows()
}

#' Add chat messages to a transcript
#'
#' This function adds chat messages written during an online/hybrid meeting to
#' the transcript.
#'
#' @param transcript_data A data frame containing the transcript.
#' @param chat_transcript A string with the path to the chat file or a data
#'   frame containing the chat data.
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

  if (rlang::is_scalar_character(chat_transcript)) {
    if (!file.exists(chat_transcript)) {
      cli::cli_abort("Chat file not found.")
    }

    file_name <- chat_transcript
    # Silence the linter since this var is used in the error message only
    file_name

    # Parse the start time
    start_time <- parse_event_time(start_time) |>
      as.numeric()

    tryCatch(
      {
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
              prev[1, ncol(prev)],
              curr[1, ncol(curr)]
            )
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
          )
      },
      error = \(e) {
        cli::cli_abort(
          c(
            "Error parsing chat file: {.file {file_name}}",
            "x" = "{e$message}",
            "i" = "Ensure the chat file is in a supported format."
          ),
          parent = e
        )
      }
    )
  } else if (is.data.frame(chat_transcript)) {
    # Check if the chat data has the required columns
    if (!all(c("start", "speaker", "text") %in% names(chat_transcript))) {
      cli::cli_abort(
        "Chat data must contain {.var start}, {.var speaker}
        and {.var text} columns."
      )
    }
  } else {
    cli::cli_abort(
      "Unsupported chat data format.",
      "x" = "Expected a file path or a data frame,
      got {.var {chat_transcript}}."
    )
  }

  # Add the chat messages to the transcript
  # left_join should position the chat messages in the right place
  dplyr::left_join(transcript_data, chat_transcript)
}
