#' Builds IDs from each agenda item session and titles
#'
#' @param agenda A list containing the agenda items.
#'
#' @return A character vector containing the IDs for each agenda item.
#'
build_ids_from_agenda <- function(agenda) {
  # Build the id of each talk using the session and title
  purrr::imap_chr(
    agenda,
    ~ {
      # If the title is NULL, use an index as title
      if (is.null(.x$title)) {
        .x$title <- .y
      }

      paste0(
        # If the session is not NULL, prepend it to the title
        if (!is.null(.x$session)) paste0(.x$session, "_"),
        .x$title
      )
    }
  )
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
  if (
    !all(purrr::map_lgl(agenda, ~ is.numeric(.x$from))) &&
      !all(purrr::map_lgl(
        agenda,
        ~ {
          inherits(.x$from, c("POSIXct", "character"))
        }
      ))
  ) {
    cli::cli_abort(
      "Agenda times must be all of the same class across the agenda."
    )
  }

  # Determine if all agenda "from" times are numeric (e.g., seconds from the
  # start)
  agenda_time_is_numeric <- all(
    purrr::map_lgl(agenda, ~ is.numeric(.x$from))
  )
  # Determine if all agenda "from" times are either POSIXct or character (i.e.,
  # in clock time format)
  agenda_time_is_clock <- all(
    purrr::map_lgl(agenda, ~ inherits(.x$from, c("POSIXct", "character")))
  )
  # Identify if an origin (event start time) is needed for the conversion based
  # on the direction of conversion
  needs_origin <- (convert_to == "seconds" && agenda_time_is_clock) ||
    (convert_to == "clocktime" && agenda_time_is_numeric)

  # If an origin is needed, check if it is provided
  if (needs_origin) {
    if (!is.null(event_start_time)) {
      if (inherits(event_start_time, c("POSIXct", "character"))) {
        # Parse the start time if not null
        temp <- parse_event_time(event_start_time)

        if (is.na(temp)) {
          cli::cli_abort(
            "Agenda time conversion failed:
            {.val {event_start_time}}"
          )
        }

        event_start_time <- temp
      } else {
        # Only character and POSIXct event start times are supported
        cli::cli_abort("Invalid event start time format.")
      }
    } else if (convert_to == "seconds" && agenda_time_is_clock) {
      # Use the first agenda time otherwise
      event_start_time <- agenda[[1]]$from |>
        parse_event_time()

      # Check if the event start time is valid
      if (is.na(event_start_time)) {
        cli::cli_abort(
          "Agenda time conversion failed:
          {.val {agenda[[1]]$from}}"
        )
      }

      # Warning handled by callers to avoid repeated messages.
    } else {
      event_start_time <- lubridate::origin
      # Warning handled by callers to avoid repeated messages.
    }
  }

  # Convert the times for each agenda element
  for (i in seq_along(agenda)) {
    # Convert the times
    for (time in c("from", "to")) {
      # If converting to seconds and the time is not numeric, convert it
      if (convert_to == "seconds" && !is.numeric(agenda[[i]][[time]])) {
        # Convert to seconds
        agenda[[i]][[time]] <- agenda[[i]][[time]] |>
          time_to_numeric(origin = event_start_time)
      } else if (convert_to == "clocktime") {
        if (is.numeric(agenda[[i]][[time]])) {
          # Convert to clock time
          cur_time <- agenda[[i]][[time]]

          agenda[[i]][[time]] <- (event_start_time +
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

#' Clean agenda items using transcript data
#'
#' Drops agenda elements whose transcript slice is empty or too small, or aborts
#' depending on the chosen behavior.
#'
#' @param agenda A list containing the agenda items or a file path to one.
#' @param transcript_data A data frame with transcript segments, containing
#'   numeric `start` and `end` columns in seconds.
#' @param event_start_time The start time of the event in the HH:MM(:SS)( AM/PM)
#'   format. Used when agenda times are clock-based and need conversion.
#' @param min_rows Minimum number of transcript rows required to keep an agenda
#'   item. Must be a positive integer.
#' @param on_empty Action when an agenda item has fewer than `min_rows` rows.
#'   One of "drop" or "abort".
#'
#' @return A cleaned agenda list with invalid items removed.
#'
#' @export
#'
clean_agenda <- function(
  agenda,
  transcript_data,
  event_start_time = getOption("minutemaker_event_start_time"),
  min_rows = 1,
  on_empty = c("drop", "abort")
) {
  on_empty <- match.arg(on_empty)

  if (!is.data.frame(transcript_data)) {
    cli::cli_abort("Transcript data must be a data frame.")
  }

  if (!all(c("start", "end") %in% names(transcript_data))) {
    cli::cli_abort(
      "Transcript data must contain {.field start} and {.field end} columns."
    )
  }

  if (
    !is.numeric(transcript_data$start) ||
      !is.numeric(transcript_data$end)
  ) {
    cli::cli_abort(
      "Transcript {.field start} and {.field end} columns must be numeric."
    )
  }

  if (
    !is.numeric(min_rows) ||
      length(min_rows) != 1 ||
      is.na(min_rows) ||
      min_rows < 1 ||
      min_rows %% 1 != 0
  ) {
    cli::cli_abort(
      "{.arg min_rows} must be a positive integer."
    )
  }

  if (is.character(agenda)) {
    agenda <- load_serialized(agenda, "agenda")
  }

  if (!validate_agenda(agenda)) {
    cli::cli_abort("The agenda is not valid.")
  }

  agenda_seconds <- convert_agenda_times(
    agenda,
    convert_to = "seconds",
    event_start_time = event_start_time
  )

  talk_ids <- build_ids_from_agenda(agenda)
  keep <- logical(length(agenda_seconds))

  for (i in seq_along(agenda_seconds)) {
    agenda_element <- agenda_seconds[[i]]
    transcript_rows <- dplyr::between(
      transcript_data$start,
      agenda_element$from,
      agenda_element$to
    )
    row_count <- sum(transcript_rows, na.rm = TRUE)

    if (row_count < min_rows) {
      if (on_empty == "abort") {
        cli::cli_abort(
          c(
            "Agenda item has an empty transcript slice.",
            "x" = "Item {.val {talk_ids[[i]]}} has {row_count} rows.",
            "i" = "from: {.val {agenda_element$from}}, to:
              {.val {agenda_element$to}}."
          )
        )
      }

      cli::cli_alert_warning("Agenda item has an empty transcript slice.")
      cli::cli_alert_info(
        "Dropping {.val {talk_ids[[i]]}} (rows: {row_count}). from:
        {.val {agenda_element$from}}, to: {.val {agenda_element$to}}."
      )
      keep[[i]] <- FALSE
    } else {
      keep[[i]] <- TRUE
    }
  }

  agenda <- agenda[keep]

  if (length(agenda) == 0) {
    cli::cli_abort(
      "All agenda items were removed during cleaning."
    )
  }

  agenda
}
