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
      if (is.null(.x$title)) .x$title <- .y

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

  # TODO: remove this line once I'm sure agenda_orig is not used
  # agenda_orig <- agenda

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
  } else if (
    length(agenda) > 0 &&
    agenda[[1]]$from |>
      inherits(c("POSIXct", "character")) &&
      convert_to == "seconds"
  ) {
    # Use the first agenda time otherwise
    event_start_time <- agenda[[1]]$from |>
      parse_event_time()

    cli::cli_warn(
      c(
        "No start time provided.\n",
        "Using the start time of the first agenda element.",
        "i" = "Consider setting the event start time using
          {.fn set_event_start_time}."
      )
    )
  } else {
    event_start_time <- lubridate::origin

    cli::cli_warn(
      c(
        "No start time provided.\n",
        "Using \"00:00:00\" as start time.",
        "i" = "Consider setting the event start time using
          {.fn set_event_start_time}."
      )
    )
  }

  for (i in seq_along(agenda)) {
    # Convert the times
    for (time in c("from", "to")) {
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
