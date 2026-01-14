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

#' Check if a file path is valid
#'
#' Performs comprehensive validation on file paths including type checking,
#' emptiness, dangerous characters, reserved names, and directory creation
#' testing. The function trims leading and trailing whitespace from the path.
#' Can either raise errors or issue warnings depending on the
#' \code{stop_on_error} parameter.
#'
#' @param path A file path to validate.
#' @param stop_on_error If \code{TRUE}, raises an error when validation fails.
#'   If \code{FALSE}, returns \code{FALSE}.
#' @param fail_msg Optional error/warning message to use when validation fails.
#'   If missing, default messages are used. If \code{stop_on_error = FALSE} and
#'   \code{fail_msg} is provided, it is emitted as a warning. If \code{FALSE} or
#'   \code{NA}, messaging is suppressed and the function simply returns
#'   \code{FALSE}. When \code{stop_on_error = TRUE}, \code{fail_msg = FALSE} or
#'   \code{NA} is treated as if \code{fail_msg} was missing (default error
#'   messages are used). Within \code{fail_msg}, \code{path} expands to the full
#'   path string and \code{basepath} to its basename.
#'
#' @keywords internal
#'
#' @return The trimmed path if valid, or \code{FALSE} when \code{stop_on_error =
#'   FALSE} and validation fails.
#'
check_path <- function(path, stop_on_error = TRUE, fail_msg) {
  caller_env <- parent.frame()
  # When called via path_exists(), we want custom fail_msg expressions to be
  # evaluated in the original caller's environment (not inside path_exists()).
  if (length(sys.calls()) >= 2) {
    caller_call <- sys.call(-2)
    if (
      is.call(caller_call) &&
        identical(rlang::call_name(caller_call), "path_exists")
    ) {
      caller_env <- parent.frame(2)
    }
  }
  path_input <- path
  fail_msg_missing <- missing(fail_msg)
  fail_msg_suppressed <- !fail_msg_missing &&
    (isFALSE(fail_msg) ||
      (rlang::is_scalar_logical(fail_msg) && is.na(fail_msg)))

  path_trimmed <- if (rlang::is_string(path_input) && !is.na(path_input)) {
    trimws(path_input)
  } else {
    path_input
  }
  basepath <- if (rlang::is_string(path_trimmed) && !is.na(path_trimmed)) {
    basename(path_trimmed)
  } else {
    NA_character_
  }

  # Helper for consistent messaging and return handling.
  fail <- function(default_bullets) {
    env <- rlang::env(
      parent = caller_env,
      path = path_trimmed,
      basepath = basepath
    )

    fail_msg_missing_for_error <- fail_msg_missing ||
      (isTRUE(stop_on_error) && fail_msg_suppressed)

    if (isTRUE(stop_on_error)) {
      bullets <- if (fail_msg_missing_for_error) default_bullets else fail_msg
      cli::cli_abort(bullets, .envir = env)
    }

    if (!fail_msg_missing && !fail_msg_suppressed) {
      cli::cli_warn(fail_msg, .envir = env)
    }
    FALSE
  }

  # Check if path is a string and not NA
  if (!rlang::is_string(path)) {
    return(fail(c(
      "Invalid path provided.",
      "x" = "Expected a character string."
    )))
  }

  if (is.na(path)) {
    return(fail(c(
      "Invalid path provided.",
      "x" = "Path cannot be NA."
    )))
  }

  # Trim whitespace
  path <- trimws(path)
  path_trimmed <- path
  basepath <- basename(path)

  # Check if empty after trimming
  if (path == "") {
    return(fail(c(
      "Invalid path provided.",
      "x" = "Path cannot be empty or whitespace-only."
    )))
  }

  # Check for null bytes or dangerous control characters (excluding tab and
  # newline which might be valid in some contexts)
  if (grepl("[\001-\010\013-\014\016-\037]", path)) {
    return(fail(c(
      "Invalid path provided.",
      "x" = "Path contains invalid control characters."
    )))
  }

  # Check for Windows reserved names in the basename
  basename_part <- basename(path)
  reserved_names <- c(
    "CON",
    "PRN",
    "AUX",
    "NUL",
    paste0("COM", 1:9),
    paste0("LPT", 1:9)
  )
  basename_no_ext <- toupper(tools::file_path_sans_ext(basename_part))
  if (basename_no_ext %in% reserved_names) {
    return(fail(c(
      "Invalid path provided.",
      "x" = "Path contains a Windows reserved filename.",
      "i" = "Reserved names: {.val {reserved_names}}"
    )))
  }

  # For relative paths, test if the directory structure can be created
  if (!fs::is_absolute_path(path)) {
    temp_base <- file.path(tempdir(), "path_validation_test")
    test_dir <- file.path(temp_base, dirname(path))

    result <- tryCatch(
      {
        fs::dir_create(test_dir, recurse = TRUE)
        TRUE
      },
      error = function(e) {
        FALSE
      }
    )

    # Clean up test directory
    if (dir.exists(temp_base)) {
      unlink(temp_base, recursive = TRUE)
    }

    if (!result) {
      return(fail(c(
        "Invalid path provided.",
        "x" = "Cannot create directory structure for this path."
      )))
    }
  }

  path
}

#' Check whether a path exists
#'
#' Wraps \code{fs::file_exists()} and \code{fs::dir_exists()} with a
#' \code{check_path()} pre-validation step to avoid errors on invalid path
#' inputs. This function is intended for existence checks only.
#'
#' @param path A path to check for existence.
#' @param type What to check for: \code{"file"} (file only), \code{"any"} (file
#'   or directory), or \code{"dir"} (directory only).
#' @param stop_on_error If \code{TRUE}, invalid path inputs raise an error. If
#'   \code{FALSE}, invalid path inputs return \code{FALSE}.
#' @param fail_msg Optional error/warning message to use when the path is
#'   invalid or does not exist. If missing, default messages are used. If
#'   \code{stop_on_error = FALSE} and \code{fail_msg} is provided, it is emitted
#'   as a warning. If \code{FALSE} or \code{NA}, messaging is suppressed and the
#'   function simply returns \code{FALSE}. When \code{stop_on_error = TRUE},
#'   \code{fail_msg = FALSE} or \code{NA} is treated as if \code{fail_msg} was
#'   missing (default error messages are used). Within \code{fail_msg},
#'   use glue-style interpolation with \code{{path}} for the full path string
#'   and \code{{basepath}} for its basename. For example,
#'   \code{"File {basepath} not found at {path}"}.
#'
#' @return The trimmed path if it exists with the requested \code{type}, or
#'   \code{FALSE} otherwise.
#'
#' @keywords internal
#'
#' @examples
#' # Default behavior - check if a file exists (will abort on invalid input)
#' \dontrun{
#' minutemaker:::path_exists("some_file.txt")  # TRUE if file exists, FALSE otherwise
#' }
#'
#' # Check directory existence
#' temp_dir <- tempdir()
#' minutemaker:::path_exists(temp_dir, type = "dir")  # TRUE
#'
#' # Check with custom fail message using interpolation
#' temp_file <- fs::file_temp()
#' writeLines("test", temp_file)
#' minutemaker:::path_exists(
#'   temp_file,
#'   fail_msg = "Required file {basepath} not found at {path}"
#' )
#'
#' # Suppress messaging and return FALSE on failure
#' minutemaker:::path_exists("nonexistent.txt", stop_on_error = FALSE, fail_msg = FALSE)
#' minutemaker:::path_exists("nonexistent.txt", stop_on_error = FALSE, fail_msg = NA)
#'
path_exists <- function(
  path,
  type = c("file", "any", "dir"),
  stop_on_error = TRUE,
  fail_msg
) {
  type <- match.arg(type)

  checked_path <- if (!missing(fail_msg)) {
    check_path(
      path,
      stop_on_error = stop_on_error,
      fail_msg = fail_msg
    )
  } else {
    check_path(
      path,
      stop_on_error = stop_on_error
    )
  }
  if (isFALSE(checked_path)) {
    return(FALSE)
  }

  exists <- switch(
    type,
    any = fs::file_exists(checked_path),
    dir = fs::dir_exists(checked_path),
    file = fs::file_exists(checked_path) && !fs::dir_exists(checked_path)
  )

  if (!isTRUE(exists)) {
    basepath <- basename(checked_path)
    env <- rlang::env(
      parent = parent.frame(),
      path = checked_path,
      basepath = basepath
    )

    fail_msg_missing_for_error <- missing(fail_msg) ||
      (isTRUE(stop_on_error) &&
        (isFALSE(fail_msg) ||
          (rlang::is_scalar_logical(fail_msg) && is.na(fail_msg))))

    if (isTRUE(stop_on_error)) {
      bullets <- if (fail_msg_missing_for_error) {
        c(
          "Path does not exist.",
          "x" = "No {type} exists at {.path {path}}."
        )
      } else {
        fail_msg
      }
      cli::cli_abort(bullets, .envir = env)
    }

    if (
      !missing(fail_msg) &&
        !(isFALSE(fail_msg) ||
          (rlang::is_scalar_logical(fail_msg) && is.na(fail_msg)))
    ) {
      cli::cli_warn(fail_msg, .envir = env)
    }
    return(FALSE)
  }

  checked_path
}
