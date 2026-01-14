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
#' Can either raise errors or issue warnings depending on the `stop_on_error`
#' parameter.
#'
#' @param path A file path to validate.
#' @param stop_on_error If `TRUE`, raises an error when validation fails. If
#'   `FALSE`, returns `FALSE`.
#' @param fail_msg Optional error/warning message to use when validation fails.
#'   If missing, default messages are used. If `stop_on_error = FALSE` and
#'   `fail_msg` is provided, it is emitted as a warning. If `FALSE` or `NA`,
#'   messaging is suppressed and the function simply returns `FALSE`. When
#'   `stop_on_error = TRUE`, `fail_msg = FALSE` or `NA` is treated as if
#'   `fail_msg` was missing (default error messages are used). Within
#'   `fail_msg`, `path` expands to the full path string and `basepath` to its
#'   basename.
#'
#' @keywords internal
#'
#' @return The trimmed path if valid, or `FALSE` when `stop_on_error = FALSE`
#'   and validation fails.
#'
check_path <- function(path, stop_on_error = TRUE, fail_msg) {
  # Helper: Determine if custom messaging for failure is suppressed (fail_msg =
  # FALSE or NA)
  is_msg_suppressed <- function(fail_msg) {
    isFALSE(fail_msg) ||
      (rlang::is_scalar_logical(fail_msg) && is.na(fail_msg))
  }

  # Select caller environment for error/warning glue interpolation ----

  # Needed so fail_msg passed from path_exists() can expand {path}, {basepath}
  # in the proper (original) user's environment, not just inside path_exists.
  caller_env <- parent.frame()
  if (length(sys.calls()) >= 2) {
    caller_call <- sys.call(-2)
    if (
      is.call(caller_call) &&
        identical(rlang::call_name(caller_call), "path_exists")
    ) {
      # If we came here via path_exists(), set env to grandparent (user call)
      caller_env <- parent.frame(2)
    }
  }

  # Prepare initial variables for glue and validation logic ----

  # Store original input for glue expansion regardless of in-body mutation.
  path_input <- path

  # Track if fail_msg is missing (to select default vs custom messages)
  fail_msg_missing <- missing(fail_msg)

  # Should custom fail_msg be suppressed? (fail_msg = FALSE or NA).
  # If so, fall back to silent FALSE in warning-mode, or use default in error.
  fail_msg_suppressed <- !fail_msg_missing && is_msg_suppressed(fail_msg)

  # Pre-trim path for glue: may be NA or not character
  path_trimmed <- if (rlang::is_string(path_input) && !is.na(path_input)) {
    trimws(path_input)
  } else {
    path_input
  }

  # Compute basename for glue-messages; NA if path is NA/invalid
  basepath <- if (rlang::is_string(path_trimmed) && !is.na(path_trimmed)) {
    basename(path_trimmed)
  } else {
    NA_character_
  }

  # Helper for consistent messaging and return handling. ----

  # Uses the correct environment so glue variables work in cli alerts.
  # Respects messaging suppression flags.
  fail <- function(default_bullets) {
    # Build environment for glue: includes local path/basepath plus caller's
    # parent env.
    env <- rlang::env(
      parent = caller_env,
      path = path_trimmed,
      basepath = basepath
    )
    # When fail_msg is suppressed and we are erroring, use default (not custom).
    fail_msg_missing_for_error <- fail_msg_missing ||
      (isTRUE(stop_on_error) && fail_msg_suppressed)
    if (isTRUE(stop_on_error)) {
      # Raise error, using fail_msg if NOT suppressed, otherwise default.
      cli::cli_abort(
        if (fail_msg_missing_for_error) default_bullets else fail_msg,
        .envir = env
      )
    }
    # If not erroring, emit warning unless suppressed. Otherwise silent FALSE.
    if (!fail_msg_missing && !fail_msg_suppressed) {
      cli::cli_warn(fail_msg, .envir = env)
    }
    FALSE
  }

  # Validation 1: Must be a non-NA string ----
  if (!rlang::is_string(path)) {
    # Not character scalar, immediately fail
    return(fail(c(
      "Invalid path provided.",
      "x" = "Expected a character string."
    )))
  }
  if (is.na(path)) {
    # Disallow NA values
    return(fail(c(
      "Invalid path provided.",
      "x" = "Path cannot be NA."
    )))
  }

  # Validation 2: Whitespace trimming and further checks on trimmed result ----

  path <- trimws(path)

  if (path == "") {
    # Path cannot be empty after trimming (whitespace-only invalid)
    return(fail(c(
      "Invalid path provided.",
      "x" = "Path cannot be empty or whitespace-only."
    )))
  }

  # Validation 3: Disallow embedded control characters (except tab/newline) ----

  # Control chars: ASCII 0x01-0x08, 0x0B, 0x0C, 0x0E-0x1F; allow 0x09(TAB) and
  # 0x0A/0D(NL/CR).
  # Prevent old Mac/Win bugs and malicious input.
  if (grepl("[\001-\010\013-\014\016-\037]", path)) {
    return(fail(c(
      "Invalid path provided.",
      "x" = "Path contains invalid control characters."
    )))
  }

  # Validation 4: Forbid Windows reserved device filenames ----

  # (CON, PRN, AUX, NUL, COM1-9, LPT1-9), applied to the basename minus
  # extension.
  basename_part <- basename(path)
  reserved_names <- c(
    "CON",
    "PRN",
    "AUX",
    "NUL",
    paste0("COM", 1:9),
    paste0("LPT", 1:9)
  )
  # Remove file extension and coerce to upper case for comparison
  basename_no_ext <- toupper(tools::file_path_sans_ext(basename_part))
  if (basename_no_ext %in% reserved_names) {
    return(fail(c(
      "Invalid path provided.",
      "x" = "Path contains a Windows reserved filename.",
      "i" = "Reserved names: {.val {reserved_names}}"
    )))
  }

  # Validation 5: Directory structure creatability for relative paths ----

  # For relative paths only, attempt to create the target directory structure in
  # tempdir().
  # If this fails, filesystem is not accepting the pattern—fail fast.
  if (!fs::is_absolute_path(path)) {
    temp_base <- file.path(tempdir(), "path_validation_test")
    test_dir <- file.path(temp_base, dirname(path))
    result <- tryCatch(
      {
        fs::dir_create(test_dir, recurse = TRUE)
        TRUE
      },
      error = function(e) FALSE
    )
    # Cleanup no matter what, so we don't leave behind folders due to test.
    if (fs::dir_exists(temp_base)) {
      unlink(temp_base, recursive = TRUE)
    }
    # Fail (warn/error) if directory could not be created
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
#' Wraps `fs::file_exists()` and `fs::dir_exists()` with a `check_path()`
#' pre-validation step to avoid errors on invalid path inputs. This function is
#' intended for existence checks only.
#'
#' @param path A path to check for existence.
#' @param type What to check for: `"file"` (file only), `"any"` (file or
#'   directory), or `"dir"` (directory only).
#' @param stop_on_error If `TRUE`, invalid path inputs raise an error. If
#'   `FALSE`, invalid path inputs return `FALSE`.
#' @param fail_msg Optional error/warning message to use when the path is
#'   invalid or does not exist. If missing, default messages are used. If
#'   `stop_on_error = FALSE` and `fail_msg` is provided, it is emitted as a
#'   warning. If `FALSE` or `NA`, messaging is suppressed and the function
#'   simply returns `FALSE`. When `stop_on_error = TRUE`, `fail_msg = FALSE` or
#'   `NA` is treated as if `fail_msg` was missing (default error messages are
#'   used). Within `fail_msg`, use glue-style interpolation with `{path}` for
#'   the full path string and `{basepath}` for its basename. For example, `"File
#'   {basepath} not found at {path}"`.
#'
#' @return The trimmed path if it exists with the requested `type`, or `FALSE`
#'   otherwise.
#'
#' @keywords internal
#'
#' @examples
#' # Default behavior - check if a file exists (will abort on invalid input)
#' minutemaker:::path_exists("some_file.txt")
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
  # Helper: Determine if custom messaging for failure is suppressed (fail_msg =
  # FALSE or NA)
  is_msg_suppressed <- function(fail_msg) {
    isFALSE(fail_msg) ||
      (rlang::is_scalar_logical(fail_msg) && is.na(fail_msg))
  }

  # Validate and standardize the 'type' argument (file/any/dir)
  type <- match.arg(type)

  # Step 1: Path validation using check_path() prior to existence check.
  #         If a custom fail_msg is provided, pass it along.
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

  # If check_path() failed and stop_on_error = FALSE, return FALSE immediately.
  if (isFALSE(checked_path)) {
    return(FALSE)
  }

  # Step 2: Actual existence check depending on `type`.
  #  - "any": file_exists (which also includes directories, per fs spec)
  #  - "dir": dir_exists
  #  - "file": file_exists and not dir_exists (exclude directories named like
  # files)
  exists <- switch(
    type,
    any = fs::file_exists(checked_path),
    dir = fs::dir_exists(checked_path),
    file = fs::file_exists(checked_path) && !fs::dir_exists(checked_path)
  )

  # Step 3: If the path does not exist as requested, handle
  # error/warning/report.
  if (!isTRUE(exists)) {
    # Prepare interpolation environment for cli errors/warnings
    basepath <- basename(checked_path)
    env <- rlang::env(
      parent = parent.frame(),
      path = checked_path,
      basepath = basepath
    )

    # Determine: are we missing a custom fail_msg? Is messaging suppressed?
    fail_msg_missing_for_error <- missing(fail_msg) ||
      (isTRUE(stop_on_error) && is_msg_suppressed(fail_msg))

    # If asked to stop on error, abort with default or custom message as
    # appropriate
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

    # Otherwise, if we're in warning mode and messaging is NOT suppressed, emit
    # warning
    if (!missing(fail_msg) && !is_msg_suppressed(fail_msg)) {
      cli::cli_warn(fail_msg, .envir = env)
    }
    # Always return FALSE to indicate the check failed if we get to here
    return(FALSE)
  }

  checked_path
}
