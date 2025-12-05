#' Load a serialized object with contextual errors
#'
#' Safely read an object from disk using [base::dget()] while providing
#' descriptive error messages. Warnings raised during deserialization are
#' treated as errors to prevent partial or corrupted reads.
#'
#' @param path Character string; file path to the serialized object readable by
#'   [base::dget()].
#' @param label Character string describing the object, used in error messages.
#' @param on_error One of `"abort"`, `"warn"`, or `"silent"`; controls how
#'   read/parse failures are handled.
#'
#' @return The deserialized R object, or `NULL` if `on_error` is `"warn"` or
#'   `"silent"` and a failure occurs.
#'
#' @keywords internal
load_serialized <- function(
  path,
  label,
  on_error = c("abort", "warn", "silent")
) {
  # Argument matching for error handling mode ("abort", "warn", or "silent")
  on_error <- match.arg(on_error)

  # Helper to issue the right feedback and always return NULL
  .fail <- function(msg, cnd = NULL) {
    # Compose cli-style bullets for context-rich error/warning
    bullets <- c(
      # Main error summary for readability
      "Failed to read the {label} file {.file {path}}.",
      # Detailed description of the reason
      "x" = msg
    )

    # Abort if requested (terminate with error)
    if (on_error == "abort") {
      cli::cli_abort(bullets, parent = cnd)
    }

    # Warn (soft failure, continue) if requested
    if (on_error == "warn") {
      cli::cli_warn(bullets, parent = cnd)
    }

    # Silent: no message, always returns NULL
    return(NULL)
  }

  # Validate that path is a single character string
  if (!is.character(path) || length(path) != 1) {
    return(.fail("Provide a single string path to a serialized R object."))
  }

  # Check if file actually exists before attempting read
  if (!file.exists(path)) {
    return(.fail("The file does not exist."))
  }

  # Attempt to read file; trap warnings as errors for consistency
  tryCatch(
    dget(path),
    warning = function(cnd) {
      # Treat any warning during parsing as a failure
      .fail("{.code {conditionMessage(cnd)}}.", cnd)
    },
    error = function(cnd) {
      # Fail with contextual error on dget error
      .fail("{.code {conditionMessage(cnd)}}.", cnd)
    }
  )
}
