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
    if (fs::dir_exists(transcript_json)) {
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
    } else if (fs::file_exists(transcript_json)) {
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
          !fs::file_exists(json_files[i]))
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
        } else {
          NA_character_
        },
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
  # Check the file extension to determine the format
  file_extension <- tolower(tools::file_ext(transcript_file))

  if (file_extension == "docx") {
    rlang::check_installed("officer")

    doc <- tryCatch(
      officer::read_docx(transcript_file),
      error = function(e) {
        cli::cli_abort(
          c(
            "Unsupported transcript format for {.file {basename(transcript_file)}}.",
            "x" = "{e$message}",
            "i" = "Ensure the DOCX is a valid Word document exported from MS Teams."
          ),
          parent = e
        )
      }
    )
    content <- officer::docx_summary(doc)

    # MS Teams transcripts are typically tables or specific paragraph
    # structures.
    if (any(content$content_type == "table cell")) {
      # Extract table data
      table_data <- content |>
        dplyr::filter(.data$content_type == "table cell") |>
        dplyr::select("text", "row_id", "cell_id") |>
        tidyr::pivot_wider(
          names_from = "cell_id",
          values_from = "text",
          names_prefix = "col_"
        )

      # Teams tables usually have 3 columns: Time, Speaker, Message
      # Plus the row_id column from pivot_wider
      if (ncol(table_data) >= 4) {
        # Helper function to parse a time string in "MM:SS" or "HH:MM:SS" format
        # Returns the total number of seconds as a numeric value.
        # - t: a character string, e.g., "01:23" for 1 minute 23 seconds, or
        #   "01:23:45" for 1 hour 23 minutes 45 seconds.
        # - Returns NA_real_ if the input cannot be parsed.
        parse_relative_time <- function(t) {
          parts <- as.numeric(stringr::str_split_1(t, ":"))
          if (length(parts) == 2) {
            # MM:SS format
            return(parts[1] * 60 + parts[2])
          } else if (length(parts) == 3) {
            # HH:MM:SS format
            return(parts[1] * 3600 + parts[2] * 60 + parts[3])
          }
          # If not matching expected time formats, return NA
          NA_real_
        }

        transcript_data <- table_data |>
          dplyr::transmute(
            start = purrr::map_dbl(.data$col_1, parse_relative_time),
            end = .data$start,
            speaker = if (import_diarization) .data$col_2 else NA_character_,
            text = .data$col_3
          ) |>
          dplyr::filter(!is.na(.data$start))
      } else {
        cli::cli_abort(
          "The table structure in {.file {basename(transcript_file)}} is not recognized as a standard MS Teams transcript."
        )
      }
    } else {
      # Handle paragraph-based Teams transcripts
      # Pattern: Speaker Name [space] Time \n Text
      # or \n Speaker Name [space] Time \n Text
      # Different versions of {officer} and different DOCX exports can change
      # how MS Teams transcript paragraphs are represented:
      # - Some include a newline between "Speaker Time" and "Text".
      # - Others flatten everything into one paragraph (sometimes even without a
      #   space after the time, e.g. "0:10Hello").
      # We support both by matching the multiline form first, then falling back
      # to a single-line capture.
      teams_pattern_multiline <- "^\\s*(.*?)\\s+(\\d{1,2}:\\d{2}(?::\\d{2})?)\\s*\\n\\s*(.*)$"
      teams_pattern_singleline <- "^\\s*(.*?)\\s+(\\d{1,2}:\\d{2}(?::\\d{2})?)\\s*(.*)$"

      # Define a local helper for relative time parsing
      parse_relative_time <- function(t) {
        parts <- as.numeric(stringr::str_split_1(t, ":"))
        if (length(parts) == 2) {
          return(parts[1] * 60 + parts[2])
        } else if (length(parts) == 3) {
          return(parts[1] * 3600 + parts[2] * 60 + parts[3])
        }
        NA_real_
      }

      paragraphs <- content |>
        dplyr::filter(.data$content_type == "paragraph")

      # First, try to match the standard "Speaker Time \n Text" paragraph form.
      matches <- stringr::str_match(paragraphs$text, teams_pattern_multiline)
      # Any paragraph that fails the multiline match might be a flattened
      # export where the newline is lost (e.g., "Speaker 0:10Text").
      missing <- is.na(matches[, 1])
      if (any(missing)) {
        # Re-run matching only for the non-matching rows using the fallback
        # single-line regex, and overwrite just those rows in the match matrix.
        matches[missing, ] <- stringr::str_match(
          paragraphs$text[missing],
          teams_pattern_singleline
        )
      }

      # Keep only paragraphs that matched either regex.
      keep <- !is.na(matches[, 1])

      # Build a clean transcript table from the regex capture groups:
      # [2] speaker, [3] time, [4] text.
      transcript_data <- dplyr::tibble(
        start = purrr::map_dbl(matches[keep, 3], parse_relative_time),
        end = .data$start,
        speaker = if (import_diarization) {
          # Some exports embed an extra numeric token (e.g. an internal ID)
          # before the speaker name; strip it if present.
          matches[keep, 2] |>
            stringr::str_remove("^\\d+\\s*") |>
            stringr::str_squish()
        } else {
          NA_character_
        },
        text = matches[keep, 4]
      ) |>
        dplyr::filter(!is.na(.data$start))
    }

    if (nrow(transcript_data) == 0) {
      cli::cli_abort(
        "The file {.file {basename(transcript_file)}} does not appear to be a valid MS Teams transcript."
      )
    }

    return(transcript_data)
  }

  if (!file_extension %in% c("srt", "vtt")) {
    cli::cli_abort(
      "Unsupported transcript format for {.file {basename(transcript_file)}}. Supported formats are SRT and VTT."
    )
  }

  # Read the file content
  lines <- readLines(transcript_file, warn = FALSE)

  times_pos <- which(stringr::str_detect(lines, "-->"))

  # Check if file has any time cues at all - if not, it's not a valid transcript
  if (length(times_pos) == 0) {
    cli::cli_abort(
      "The file {.file {basename(transcript_file)}} does not contain any time cues and is not a valid transcript file."
    )
  }

  # Iterate over each time position
  transcript_data <- purrr::map(
    times_pos,
    ~ {
      cue_line_content <- lines[.x]

      # Extract the start and end times
      times <- stringr::str_split_1(cue_line_content, "-->") |>
        stringr::str_squish() |>
        lubridate::hms(quiet = TRUE) |>
        as.numeric()

      if (length(times) < 2 || any(!is.finite(times))) {
        warning(
          sprintf(
            "Skipping malformed time cue in '%s'. Line %d: \"%s\"",
            basename(transcript_file),
            .x,
            cue_line_content
          ),
          call. = FALSE
        )
        return(NULL)
      }

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
        line_above_content <- if (line_above_idx >= 1) {
          lines[line_above_idx]
        } else {
          NA_character_
        }

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
    purrr::compact() |>
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
    if (!fs::file_exists(chat_transcript)) {
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
            if (i > 1 && ncol(chat_transcript[[i - 1]]) >= 1) {
              prev <- chat_transcript[[i - 1]]
              chat_transcript[[i - 1]][1, ncol(prev)] <- paste(
                prev[1, ncol(prev)],
                curr[1, ncol(curr)]
              )
              chat_transcript[[i]] <- NULL
            } else {
              cli::cli_warn(
                "Skipping chat line that looks like a continuation but has no previous message."
              )
              chat_transcript[[i]] <- NULL
            }
          }
        }

        chat_transcript <- do.call("rbind", chat_transcript) |>
          as.data.frame()

        if (ncol(chat_transcript) < 4) {
          cli::cli_abort(
            "The chat file {.file {basename(file_name)}} does not appear to be a valid Webex chat export."
          )
        }

        chat_transcript <- chat_transcript |>
          setNames(c("date", "start", "speaker", "text")) |>
          mutate(
            date = NULL,
            start = parse_event_time(.data$start) |>
              as.numeric() -
              start_time,
            end = .data$start,
            speaker = stringr::str_remove(.data$speaker, "(?i)\\sfrom\\s.*") |>
              stringr::str_squish() |>
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

    if (!"end" %in% names(chat_transcript)) {
      chat_transcript <- chat_transcript |>
        dplyr::mutate(end = .data$start, .after = "start")
    } else {
      chat_transcript <- chat_transcript |>
        dplyr::mutate(end = dplyr::coalesce(.data$end, .data$start))
    }
  } else {
    cli::cli_abort(
      c(
        "Unsupported chat data format.",
        "x" = "Expected a file path or a data frame,
      got {.cls {class(chat_transcript)}}."
      )
    )
  }

  # Add the chat messages to the transcript
  dplyr::bind_rows(transcript_data, chat_transcript) |>
    dplyr::arrange(.data$start) |>
    dplyr::as_tibble()
}

#' Standardize and prepare an external transcript for the workflow
#'
#' This function takes an external transcript file (VTT, SRT, or DOCX),
#' standardizes it into the internal format used by the package, and saves it
#' as a JSON file in the transcription output directory. This allows the rest
#' of the workflow to proceed as if the transcript was generated by the
#' package's speech-to-text models.
#'
#' @param file Path to the external transcript file.
#' @param target_dir Path to the target directory where the workflow is running.
#' @param import_diarization A boolean indicating whether the speaker should be
#'   imported from the transcript, if present.
#' @param lines_per_json A positive integer indicating how many transcript rows
#'   should be written to each JSON segment file.
#'
#' @return Invisibly returns the paths to the created JSON segment files.
#'
#' @export
use_transcript_input <- function(
  file,
  target_dir = getwd(),
  import_diarization = TRUE,
  lines_per_json = 200
) {
  cli::cli_alert_info("Preparing external transcript: {.file {basename(file)}}")

  # Accept numeric "whole numbers" like 200 (double) as well as 200L (integer),
  # but reject non-integers like 200.5.
  if (!rlang::is_scalar_integerish(lines_per_json) || lines_per_json < 1) {
    cli::cli_abort("`lines_per_json` must be a positive integer.")
  }

  # Import the transcript data
  transcript_df <- import_transcript_from_file(
    transcript_file = file,
    import_diarization = import_diarization
  )

  if (nrow(transcript_df) == 0) {
    cli::cli_abort(
      "The provided transcript file is empty or could not be parsed."
    )
  }

  # Create the transcription output directory if it doesn't exist
  stt_output_dir <- file.path(target_dir, "transcription_output_data")
  if (!fs::dir_exists(stt_output_dir)) {
    fs::dir_create(stt_output_dir)
  }

  # Ensure consistent ordering before chunking, so segments are written and
  # reconstructed in chronological order.
  transcript_df <- transcript_df |>
    dplyr::arrange(.data$start, .data$end)

  if (
    !all(is.finite(transcript_df$start)) || !all(is.finite(transcript_df$end))
  ) {
    cli::cli_abort(
      c(
        "The provided transcript contains invalid time cues.",
        "x" = "Found non-finite values in {.var start} or {.var end}.",
        "i" = "Ensure the transcript has valid timestamp cues and try again."
      )
    )
  }

  # Clean up NA values in text column for proper JSON serialization
  transcript_df$text <- tidyr::replace_na(transcript_df$text, "")

  # Split the transcript into multiple JSON files to avoid producing one very
  # large prompt payload for downstream LLM steps.
  chunk_id <- ceiling(seq_len(nrow(transcript_df)) / lines_per_json)
  n_chunks <- max(chunk_id)
  # Pad the segment index with zeros so alphabetical ordering matches numeric
  # ordering (and parse_transcript_json() also sorts with numeric = TRUE).
  pad_width <- nchar(as.character(n_chunks))

  output_files <- character(n_chunks)
  # parse_transcript_json() adds an accumulated `last_time` across files.
  # To preserve absolute timestamps while writing chunked JSON, we write each
  # chunk with times relative to the end of the previous chunk.
  offset_time <- 0

  for (chunk_index in seq_len(n_chunks)) {
    rows <- which(chunk_id == chunk_index)
    chunk_df <- transcript_df[rows, , drop = FALSE]

    chunk_end_abs <- max(chunk_df$end, na.rm = TRUE)

    chunk_df$start <- chunk_df$start - offset_time
    chunk_df$end <- chunk_df$end - offset_time

    # Standardize into the JSON format expected by parse_transcript_json().
    transcript_json <- list(
      text = paste(chunk_df$text, collapse = " "),
      segments = purrr::transpose(chunk_df)
    )

    output_file <- file.path(
      stt_output_dir,
      sprintf(paste0("segment_%0", pad_width, "d.json"), chunk_index)
    )
    jsonlite::write_json(
      transcript_json,
      output_file,
      auto_unbox = TRUE,
      pretty = TRUE
    )

    output_files[chunk_index] <- output_file
    offset_time <- chunk_end_abs
  }

  cli::cli_alert_success(
    "External transcript standardized and saved to {.path {basename(stt_output_dir)}}"
  )

  return(invisible(output_files))
}
