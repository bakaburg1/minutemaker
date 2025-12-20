#' Check if an audio file is corrupted using ffmpeg
#'
#' This function uses ffmpeg to check if an audio file is corrupted. It does
#' this by trying to decode the audio and sending it to a null sink. If ffmpeg
#' returns an error, the file is considered corrupted.
#'
#' @param audio_file The path to the audio file to check.
#'
#' @return TRUE if the file is valid, FALSE if it is corrupted.
#'
is_audio_file_sane <- function(audio_file) {
  # Run ffmpeg with a null output to check for errors
  status <- system2(
    "ffmpeg",
    args = c("-v", "error", "-i", audio_file, "-f", "null", "-"),
    stdout = FALSE,
    stderr = FALSE
  )
  return(status == 0)
}

#' Extract Audio Segment
#'
#' This helper function extracts a single audio segment from a larger audio
#' file, retrying once if the initial attempt results in a corrupted file. It
#' serves as the core worker logic for [split_audio()].
#'
#' @param audio_file The path to the source audio file.
#' @param output_file The path where the extracted segment should be saved.
#' @param start_time The start time in seconds for the segment.
#' @param duration The duration of the segment in seconds.
#' @param verbose A logical value indicating whether to print messages.
#'
#' @return The path to the created segment file if successful.
#'
#' @keywords internal
extract_audio_segment <- function(
  audio_file,
  output_file,
  start_time,
  duration,
  verbose = TRUE
) {
  if (verbose) {
    cli::cli_alert("Outputting audio segment: {.file {basename(output_file)}}")
  }

  max_attempts <- 2
  for (attempt in 1:max_attempts) {
    # Ensure the directory exists before writing
    if (!dir.exists(dirname(output_file))) {
      dir.create(dirname(output_file), recursive = TRUE)
    }

    av::av_audio_convert(
      audio_file,
      output_file,
      start_time = start_time,
      total_time = duration
    )

    if (is_audio_file_sane(output_file)) {
      return(output_file) # Success
    }

    if (verbose) {
      if (attempt < max_attempts) {
        cli::cli_warn(
          "Generated segment {.file {basename(output_file)}} is corrupted. Attempt {attempt} of {max_attempts} failed, retrying..."
        )
      } else {
        cli::cli_warn(
          "Generated segment {.file {basename(output_file)}} is corrupted. Attempt {attempt} of {max_attempts} failed."
        )
      }
    }
  }

  # If the loop finishes, all attempts have failed.
  cli::cli_abort(
    "Failed to create a valid segment for {.file {basename(output_file)}} after {max_attempts} attempts."
  )
}

#' Split an audio file into segments of a specified duration
#'
#' Some speech-to-text models have a limit on the size of the audio file. For
#' example, the "Whisper" based models have a limit of 25 MB. This function
#' splits an audio file into segments of a specified duration and saves them as
#' mp3 files.
#'
#' @param audio_file The path to the audio file to split.
#' @param segment_duration The duration of each splitted audio file in minutes.
#'   20 minutes equate to more or less 7-8 MB.
#' @param output_folder The path to the folder where to save the segments in.
#' @param parallel A logical value indicating whether to process segments in
#'   parallel. Defaults to `getOption("minutemaker_split_audio_parallel",
#'   FALSE)`. Parallel processing requires the `mirai` package.
#'
#' @return Nothing, but saves the segments to files.
#'
#' @export
#'
split_audio <- function(
  audio_file,
  segment_duration = 40,
  output_folder = file.path(dirname(audio_file), "recording_parts"),
  parallel = getOption("minutemaker_split_audio_parallel", FALSE)
) {
  if (!rlang::is_string(audio_file) || audio_file == "") {
    cli::cli_abort(
      c(
        "No valid audio file provided to {.fn split_audio}.",
        "x" = "Received {.val {audio_file}}.",
        "i" = "Provide a path to an existing audio file (e.g., .wav, .mp3, .mp4).",
        "i" = "If you are starting from an existing transcript, bypass audio splitting by supplying {.code external_transcript} to {.fn speech_to_summary_workflow}."
      )
    )
  }

  # Check if the av package is installed and ask to install it if not
  rlang::check_installed("av")

  # Use absolute paths so workers don't depend on their working directory
  audio_file <- normalizePath(audio_file, mustWork = FALSE)

  # Calculate segment length in seconds
  segment_length_sec <- segment_duration * 60

  # Bind helper to a local symbol for use inside parallel workers
  extract_audio_segment_func <- extract_audio_segment

  # Get the total duration of the file in seconds
  media_info <- av::av_media_info(audio_file)
  total_duration_sec <- media_info$duration

  if (!is.finite(total_duration_sec) || total_duration_sec <= 0) {
    cli::cli_abort(
      c(
        "Cannot split audio with non-positive duration.",
        "x" = "File {.file {basename(audio_file)}} has duration {total_duration_sec} seconds."
      )
    )
  }

  # Calculate the number of segments
  num_segments <- ceiling(total_duration_sec / segment_length_sec)

  # Ensure the output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  output_folder <- normalizePath(output_folder, mustWork = TRUE)

  if (parallel) {
    # --- PARALLEL EXECUTION ---
    rlang::check_installed("mirai", reason = "for parallel processing.")

    max_parallel_wait <- getOption(
      "minutemaker_split_audio_parallel_timeout",
      120
    )
    parallel_start_time <- Sys.time()

    # Track whether this invocation owns the lifecycle of minutemaker daemons
    started_daemons <- FALSE

    if (mirai::status(.compute = "minutemaker")$daemons == 0) {
      cli::cli_alert(
        "Starting mirai daemons for parallel audio splitting..."
      )
      n_daemons <- max(1, parallel::detectCores() - 1)
      mirai::daemons(
        n = n_daemons,
        dispatcher = FALSE,
        .compute = "minutemaker"
      )
      started_daemons <- TRUE
      # Ensure cleanup only shuts down daemons started by this invocation
      on.exit(
        {
          if (isTRUE(started_daemons)) {
            mirai::daemons(0, .compute = "minutemaker")
          }
        },
        add = TRUE
      )
    } else {
      cli::cli_alert_info(
        "Using {mirai::status(.compute = 'minutemaker')$daemons} existing minutemaker daemons."
      )
    }

    tasks <- lapply(seq_len(num_segments), \(i) {
      start_time <- (i - 1) * segment_length_sec
      output_file <- file.path(output_folder, paste0("segment_", i, ".mp3"))
      duration <- segment_length_sec # Capture in local scope

      mirai::mirai(
        {
          # Bind helper dependency into the worker environment so it is
          # available even when the package namespace is not loaded in the
          # daemon.
          extract_audio_segment_bound <- extract_audio_segment_func
          environment(extract_audio_segment_bound) <- list2env(
            list(is_audio_file_sane = is_audio_file_sane),
            parent = environment(extract_audio_segment_func)
          )

          extract_audio_segment_bound(
            audio_file = audio_file,
            output_file = output_file,
            start_time = start_time,
            duration = duration,
            verbose = FALSE # Less verbose in parallel
          )
        },
        extract_audio_segment_func = extract_audio_segment,
        is_audio_file_sane = is_audio_file_sane,
        audio_file = audio_file,
        output_file = output_file,
        start_time = start_time,
        duration = duration,
        .compute = "minutemaker"
      )
    })

    cli::cli_alert("Started {length(tasks)} parallel splitting tasks.")

    # Track processing status of each parallel task, initially all FALSE (not
    # completed)
    processed_flags <- rep(FALSE, length(tasks))
    # Flag to indicate if a failure has occurred, which should halt task
    # processing
    fail_fast_triggered <- FALSE
    fallback_used <- FALSE

    # Continue loop until all tasks are processed or a failure occurs
    while (!all(processed_flags) && !fail_fast_triggered) {
      Sys.sleep(0.2) # Sleep briefly to avoid busy waiting

      # Get status of all tasks: TRUE = task not finished, FALSE = task finished
      still_running <- mirai::unresolved(tasks)
      # Some mirai versions may return a single logical for the whole vector;
      # expand it to align with tasks so we don't wait forever.
      if (
        is.logical(still_running) &&
          length(still_running) == 1L &&
          length(tasks) > 1
      ) {
        still_running <- rep(still_running, length(tasks))
      }

      # Bail out if tasks take too long to finish to avoid hanging indefinitely
      if (
        as.numeric(difftime(
          Sys.time(),
          parallel_start_time,
          units = "secs"
        )) >
          max_parallel_wait
      ) {
        if (started_daemons) {
          mirai::daemons(0, .compute = "minutemaker")
          started_daemons <- FALSE
        } else {
          cli::cli_warn(
            "Skipping shutdown of pre-existing mirai daemons after timeout."
          )
        }
        cli::cli_alert_warning(
          c(
            "Parallel splitting exceeded timeout of {max_parallel_wait} seconds.",
            i = "Processed {sum(processed_flags)}/{length(tasks)} segments before timing out.",
            i = "Falling back to sequential processing for remaining segments."
          )
        )

        pending_idxs <- which(!processed_flags)
        for (i in pending_idxs) {
          start_time <- (i - 1) * segment_length_sec
          output_file <- file.path(output_folder, paste0("segment_", i, ".mp3"))
          extract_audio_segment(
            audio_file = audio_file,
            output_file = output_file,
            start_time = start_time,
            duration = segment_length_sec
          )
          processed_flags[i] <- TRUE
        }

        fallback_used <- TRUE
        break
      }

      for (i in seq_along(tasks)) {
        if (processed_flags[i]) {
          # This segment has already finished successfully, skip to next
          next
        }

        # Determine if task i is still running. `unresolved()` can return either
        # a logical vector (modern mirai) or a list of unresolved tasks (older
        # versions), so handle both.
        task_unresolved <- TRUE
        if (is.logical(still_running)) {
          task_unresolved <- if (length(still_running) >= i) {
            still_running[i]
          } else {
            TRUE
          }
        } else if (is.list(still_running)) {
          task_unresolved <- any(purrr::map_lgl(
            still_running,
            ~ identical(.x, tasks[[i]])
          ))
        }

        if (isTRUE(task_unresolved)) {
          next
        }

        # Task is resolved; check status
        result <- tasks[[i]][]
        if (mirai::is_error_value(result)) {
          # Worker reported an error, log and activate fail-fast
          err_message <- tryCatch(
            conditionMessage(result),
            error = function(...) {
              fallback <- tryCatch(
                result$message,
                error = function(...) NULL
              )
              if (is.null(fallback) || !nzchar(fallback)) {
                return("Unknown worker error")
              }
              fallback
            }
          )
          failure_message <- paste0("A worker failed: ", err_message)
          cli::cli_alert_danger(failure_message)
          fail_fast_triggered <- TRUE
          break
        }
        # Report successful processing of this segment
        cli::cli_alert_success(
          "Segment {i}/{length(tasks)} processed successfully."
        )
        # Mark this segment as completed
        processed_flags[i] <- TRUE
      }
    }

    if (fail_fast_triggered) {
      cli::cli_alert_danger(
        "Halting due to worker error. Checking source file integrity..."
      )
      if (!is_audio_file_sane(audio_file)) {
        cli::cli_abort(
          c(
            "The source audio file {.path {audio_file}} appears to be corrupted, which may be the cause of the error.",
            i = "Please check the file and try again."
          )
        )
      } else {
        cli::cli_abort(
          c(
            "A segment could not be generated correctly, but the source file appears to be OK.",
            i = "This could be an intermittent issue. Please try running the command again."
          )
        )
      }
    }

    cli::cli_alert_success("All segments processed successfully.")
  } else {
    # --- SEQUENTIAL EXECUTION ---
    purrr::walk(seq_len(num_segments), \(i) {
      start_time <- (i - 1) * segment_length_sec
      output_file <- file.path(output_folder, paste0("segment_", i, ".mp3"))

      extract_audio_segment(
        audio_file = audio_file,
        output_file = output_file,
        start_time = start_time,
        duration = segment_length_sec
      )
    })
  }
}
