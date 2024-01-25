#' Check and install dependencies
#'
#' Checks if a list of packages are installed and installs them if not. Trigger
#' an error if the user chooses not to install a package.
#'
#' @param deps A character vector of package names.
#'
#' @return Nothing.
#'
check_and_install_dependencies <- function(deps) {
  for (dep in deps) {
    stop_message <- paste0(dep, " is required but was not installed.")
    # Check if the package is installed
    is_installed = requireNamespace(dep, quietly = TRUE)

    if (!is_installed) {
      # If not, ask the user if they want to install it
      if (interactive()) { # Only ask if the session is interactive otherwise just stop
        is_installed <- utils::menu(
          c("Yes", "No"),
          title = paste0(dep, " is not installed. Install it now?")) == 1
      }
    }

    # Stop if the package is not installed
    if (!is_installed) stop(stop_message)
  }
}

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
silent <- function(){
  "[...]"
}

#' Parse time if it is a string
#'
#' @param time A time string in the "HH:MM(:SS)( AM/PM)", format.
#' @param format A character vector of formats to try.
#'
#' @return A POSIXct value representing the time or the original value if it is
#'   numeric.
parse_event_time <- function(time, format = c("R", "T")) {
  if (is.numeric(time)) {
    time
  } else {
    lubridate::parse_date_time(time, format)
  }
}

#' Transform a time string to numeric
#'
#' @param time A time string in the "HH:MM(:SS)( AM/PM)", format.
#'
#' @return A numeric value representing the time.
time_to_numeric <- function(time) {
  if (is.numeric(time)) {
    time
  } else {
    parse_event_time(time) |> as.numeric()
  }
}
