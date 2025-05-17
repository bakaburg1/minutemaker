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
      .data$text == lag(.data$text, default = silent()),
      silent(),
      .data$text
    )
  )

  # Remove pieces of text that are sorrounded by > 4 "[...]" as they are
  # likely hallucinations from the speech-to-text model or non-relevant text
  # (e.g. talking outside sessions)
  for (i in seq_len(nrow(transcript_data))) {
    indexes <- pmax(i + c(-4:-1, 1:4), 0)
    indexes <- intersect(indexes, seq_len(nrow(transcript_data)))
    if (transcript_data$text[i] != silent() && length(indexes) > 0) {
      if (all(is_silent(transcript_data$text[indexes]))) {
        transcript_data$text[i] <- silent()
      }
    }
  }

  # Segments with multiple repetitions of the same word are likely
  # hallucinations
  repetitions <- purrr::map_int(
    transcript_data$text,
    ~ stringr::str_split_1(.x, ", *") |> table() |> max()
  )

  transcript_data$text[repetitions > 10] <- silent()

  # Use the Whisper API results statistics to remove segments with low
  # confidence
  if (all(c("avg_logprob", "no_speech_prob") %in% names(transcript_data))) {
    condition <- with(
      transcript_data,
      avg_logprob < -0.5 & no_speech_prob > 0.9
    )
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
  start_time = NULL,
  end_time = NULL,
  import_diarization = TRUE
) {
  # If no parameters are provided, the whole transcript is used and a warning is
  # issued
  if (is.null(agenda_element) && is.null(start_time) && is.null(end_time)) {
    duration <- max(transcript_data$end) - min(transcript_data$start)

    if (duration > 3600) {
      cli::cli_warn(
        c(
          "Summarising a transcript covering more than 1 hour may result
            in a loss of details.",
          "i" = "Consider treating it as a collection of tasks/meetings
            to summarise separately.",
          "i" = "See {.fn summarise_full_meeting} for more details."
        )
      )
    }
  }

  # If an agenda element is provided, its start and end times are used
  if (!is.null(agenda_element)) {
    if (!is.numeric(agenda_element$from) || !is.numeric(agenda_element$to)) {
      cli::cli_abort(
        "Agenda element must contain 'from' and 'to'
        fields in numeric format. Use convert_agenda_times() to convert
        the agenda element times to numeric format."
      )
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
  if (
    !"speaker" %in% names(transcript_data) ||
      all(is.na(transcript_data$speaker)) ||
      n_distinct(transcript_data$speaker, na.rm = T) == 1
  ) {
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
        .data$speaker[1],
        paste(.data$text, collapse = "\n")
      ),
      .by = "speakerID"
    ) |>

    # Extract the text
    pull(.data$text) |>
    paste(collapse = "\n\n")

  # Remove the speaker id import_diarization is FALSE
  if (!import_diarization) {
    transcript <- stringr::str_remove_all(transcript, "Speaker:.*?\n")
  }

  transcript
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
    cli::cli_inform("No empty segments found in the transcript.")
    return(transcript_x)
  }

  transcript_x$seen <- FALSE

  # TODO: this line seems redundant, but I want to be sure before removing it
  # col_names <- intersect(names(transcript_x), names(transcript_y))

  if (import_diarization) {
    # This message is useful only if also diarization is imported
    cli::cli_inform("Merging...")

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
    redundant <- which(
      transcript_x$start >= candidate$start &
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

  cli::cli_inform(c(
    "v" = "Added {added} segments.",
    "i" = "Removed {removed} segments."
  ))

  if (import_diarization) {
    # Train a GloVe model on both transcripts
    glove_model <- c(transcript_x$text, transcript_y$text) |>
      purrr::discard(is_silent) |>
      generate_glove_model()

    cli::cli_inform("Importing diarization...")

    transcript_y <- stored_y
    transcript_x$speaker <- NA

    for (i in seq_len(nrow(transcript_x))) {
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
      y_probes <- transcript_y[y_range, ] |>
        group_by(gID = consecutive_id(.data$speaker)) |>
        summarize(
          text = paste(.data$text, collapse = " "),
          speaker = .data$speaker[1]
        )

      # Calculate the semantic similarity between the segment in the first
      # transcript and the candidate segments in the second transcript
      y_probes$similarity <- compute_text_sim(
        x_text,
        y_probes$text,
        glove_model
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
  if ("speaker" %in% names(transcript_x)) {
    # only try to clean if there are any actual speaker entries to evaluate
    if(sum(!is.na(transcript_x$speaker)) > 0) {
      transcript_x$speaker[is_silent(transcript_x$speaker)] <- NA
    }
  }

  clean_transcript(transcript_x)
}
