#' Check if transcript segments are silent
#'
#' Check if transcript segments are silent, e.g., `[...]`, NA, or empty strings.
#'
#' @param segments A vector of transcript segments.
#'
#' @return A logical vector indicating if the segment is silent.
#'
is_silent <- function(segments) {
  segments %in% c("[...]", NA, "")
}

#' Generate text for silent segments
#'
#' @return The "silent" symbol, i.e., `[...]`.
silent <- function() {
  "[...]"
}

#' Parse time if it is a string
#'
#' @param time A time string in the "HH:MM(:SS)( AM/PM)", format.
#' @param format A character vector of formats to try.
#'
#' @return A POSIXct value representing the time.
#'
parse_event_time <- function(time, format = c("R", "T")) {
  if (!inherits(time, c("POSIXct", "character"))) {
    cli::cli_abort(
      c(
        "Invalid time format provided.",
        "x" = "Expected a character string in HH:MM(:SS)
          or HH:MM(:SS) AM/PM format, or a POSIXct object.",
        "i" = "Received object of class: {.cls {class(time)}}"
      )
    )
  }

  if (inherits(time, "POSIXct")) {
    time
  } else {
    lubridate::parse_date_time(time, format)
  }
}

#' Transform a time string to numeric
#'
#' @param time A time string in the "HH:MM(:SS)( AM/PM)" format, a POSIXct
#'   object, or a number (i.e., of seconds).
#' @param origin Same format as `time`; if provided, used to compute the
#'   difference in seconds from `time`.
#'
#' @return A numeric value representing the difference in seconds from `origin`.
#'
time_to_numeric <- function(time, origin = NULL) {
  if (!inherits(time, c("character", "POSIXct", "numeric", "integer"))) {
    cli::cli_abort(
      c(
        "Invalid format for {.arg time} argument.",
        "x" = "Expected character, POSIXct, numeric, or integer.",
        "i" = "Received object of class: {.cls {class(time)}}"
      )
    )
  }

  if (!is.null(origin)) {
    if (!inherits(origin, c("character", "POSIXct", "numeric"))) {
      cli::cli_abort(
        c(
          "Invalid format for {.arg origin} argument.",
          "x" = "Expected character, POSIXct, or numeric.",
          "i" = "Received object of class: {.cls {class(origin)}}"
        )
      )
    }

    if (
      # Using is.numeric is enough since both time and origin types were already
      # checked to be valid
      (is.numeric(time) && !is.numeric(origin)) ||
        (!is.numeric(time) && is.numeric(origin))
    ) {
      cli::cli_abort(
        c(
          "{.arg time} and {.arg origin} arguments have incompatible types.",
          "x" = "Both must be numeric or both must be non-numeric
            (character or POSIXct).",
          "i" = "time class: {.cls {class(time)}}, origin class:
            {.cls {class(origin)}}"
        )
      )
    }
  }

  if (is.null(origin)) {
    if (is.numeric(time)) return(time) else origin <- "00:00:00"
  }

  # Convert to POSIXct if not numeric. Only one check is necessary since type
  # consistency was already checked
  if (!is.numeric(time)) {
    time <- parse_event_time(time)
    origin <- parse_event_time(origin)
  }

  # Necessary to convert to numeric, otherwise the difference could be in other
  # time units
  diff <- as.numeric(time) - as.numeric(origin)

  if (is.na(diff)) {
    # If diff is NA (e.g., due to unparseable input to parse_event_time), return
    # NA_real_ directly. Warnings from parse_event_time would have already
    # occurred.
    return(NA_real_)
  }

  if (diff < 0) {
    cli::cli_abort(
      c(
        "Calculated time difference is negative.",
        "x" = "{.arg time} ({.val {time}}) cannot be
          earlier than {.arg origin} ({.val {origin}}).",
        "i" = "Resulting difference: {diff} seconds."
      )
    )
  }

  diff
}
