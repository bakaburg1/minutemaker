
#' Check if transcript segments are silent
#'
#' Check if transcript segments are silent, e.g., [...], NA, or empty strings.
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
#' @return The "silent" symbol, i.e., "[...]".
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
    stop("Invalid time format. Should be either a valid HH:MM(:SS)( AM/PM) ",
         "character or already in POSIXct format.")
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
    stop("Invalid time format for parameter 'time'")
  }

  if (!is.null(origin)) {
    if (!inherits(origin, c("character", "POSIXct", "numeric"))) {
      stop("Invalid time format for parameter 'origin'")
    }

    if (
      # Using is.numeric is enough since both time and origin types were already
      # checked to be valid
      (is.numeric(time) && !is.numeric(origin)) ||
      (!is.numeric(time) && is.numeric(origin))
    ) {
      stop("time and origin arguments are not compatible")
    }
  }

  if (is.null(origin)) {
    if (is.numeric(time)) return(time)
    else origin <- "00:00:00"
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

  if (diff < 0) {
    stop("Time difference in seconds is negative")
  }

  diff
}
