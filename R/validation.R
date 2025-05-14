#' Validates an agenda element
#'
#' @param agenda_element A list containing the agenda elements.
#' @param session A boolean indicating whether the `session` item should be
#'   present.
#' @param title A boolean indicating whether the `title` item should be present.
#' @param speakers A boolean indicating whether the `speakers` item should be
#'   present.
#' @param moderators A boolean indicating whether the `moderators` item should
#'   be present.
#' @param type A boolean indicating whether the `type` item should be present.
#' @param from A boolean indicating whether the `from` item should be present.
#' @param to A boolean indicating whether the `to` item should be present.
#'
#' @return A boolean indicating whether the agenda element is valid.
#'
validate_agenda_element <- function(
  agenda_element,
  session = FALSE,
  title = FALSE,
  speakers = FALSE,
  moderators = FALSE,
  type = FALSE,
  from = FALSE,
  to = FALSE
) {
  # Get the arguments as a list
  args <- as.list(environment())

  # Initialize the validation result
  is_valid <- TRUE

  # Remove the 'agenda_element' argument from the list
  args$agenda_element <- NULL

  if (length(agenda_element) == 0) {
    cli::cli_warn(
      c(
        "Agenda element validation failed:",
        "i" = "Element details:
          {.code {deparse(agenda_element) |> paste(collapse = '\\n')}}",
        "x" = "The agenda element is empty."
      ),
      wrap = TRUE
    )

    return(FALSE)
  }

  # Check if the required items are present in the agenda element
  el_checks <- purrr::imap_lgl(
    args,
    ~ {
      !is.null(agenda_element[[.y]]) || isFALSE(.x)
    }
  )

  if (!all(el_checks)) {
    cli::cli_warn(
      c(
        "Agenda element validation failed:",
        "i" = "Element details:
          {.code {deparse(agenda_element) |> paste(collapse = '\\n')}}",
        "x" = "Required items missing: {.field {names(args)[!el_checks]}}"
      ),
      wrap = TRUE
    )
  }

  is_valid <- all(el_checks)

  if (isTRUE(from) || isTRUE(to)) {
    # Check if the times are interpretable
    for (time in c("from", "to")) {
      # Convert integer times to numeric to simplify the validation
      if (inherits(agenda_element[[time]], "integer")) {
        agenda_element[[time]] <- as.numeric(agenda_element[[time]])
      }

      if (
        !inherits(agenda_element[[time]], c("numeric", "POSIXct", "character"))
      ) {
        cli::cli_warn(
          c(
            "Agenda element validation failed:",
            "i" = "Element details:
              {.code {deparse(agenda_element) |> paste(collapse = '\\n')}}",
            "x" = "Agenda element field {.field {time}} must be numeric,
            character, or POSIXct, but it's class
            {.cls {class(agenda_element[[time]])}}."
          ),
          wrap = TRUE
        )

        is_valid <- FALSE
      }

      if (
        !is.numeric(agenda_element[[time]]) &&
          is.na(parse_event_time(agenda_element[[time]]))
      ) {
        cli::cli_warn(
          c(
            "Agenda element validation failed:",
            "i" = "Element details:
              {.code {deparse(agenda_element) |> paste(collapse = '\\n')}}",
            "x" = "Agenda element field {.field {time}} time not interpretable:
            {.val {agenda_element[[time]]}}"
          ),
          wrap = TRUE
        )

        is_valid <- FALSE
      }
    }

    if (!identical(class(agenda_element$from), class(agenda_element$to))) {
      cli::cli_warn(
        c(
          "Agenda element validation failed:",
          "x" = "The agenda element {.field from} and {.field to} times
            are not of the same class:",
          "i" = "from ({.cls {class(agenda_element$from)}}):
            {.val {agenda_element$from}}",
          "i" = "to ({.cls {class(agenda_element$to)}}):
            {.val {agenda_element$to}}"
        ),
        wrap = TRUE
      )

      is_valid <- FALSE
    }

    if (
      time_to_numeric(agenda_element$from) > time_to_numeric(agenda_element$to)
    ) {
      cli::cli_warn(
        c(
          "Agenda element validation failed:",
          "i" = "Element details:
            {.code {deparse(agenda_element) |> paste(collapse = '\\n')}}",
          "x" = "Agenda element 'from' time should precede 'to' time:",
          " " = "from: {.val {agenda_element$from}}",
          " " = "to: {.val {agenda_element$to}}"
        ),
        wrap = TRUE
      )

      is_valid <- FALSE
    }
  }

  # Return the validation result
  is_valid
}


#' Validates an agenda
#'
#' Checks if the agenda is a list (or a file path to a list) and if it is not
#' empty and if all its elements are valid.
#'
#' @param agenda A list containing the agenda or a file path to it.
#' @param ... Additional arguments to be passed to `validate_agenda_element`.
#'
#' @return A boolean indicating whether the agenda is valid.
#'
#' @export
#'
#' @examples
#' validate_agenda(list(
#'  list(
#'   session = "Session 1",
#'   title = "Opening Session",
#'   speakers = "John Doe",
#'   moderators = "Jane Doe",
#'   type = "conference talk",
#'   from = "09:00 AM",
#'   to = "10:00 AM"
#'  ),
#'  list()
#'  ))
#'  #> [1] FALSE # Because the second element is empty
#'
#' validate_agenda(list(
#'   list(
#'     title = "Opening Session",
#'     moderators = "Jane Doe",
#'     type = "conference talk",
#'     from = "09:00 AM",
#'     to = "10:00 AM"
#'   )
#' ), session = TRUE, title = TRUE, speakers = TRUE, moderators = TRUE,
#' type = TRUE, from = TRUE, to = TRUE)
#'
#' #> [1] FALSE # Because the session and speakers are missing
#'
validate_agenda <- function(
  agenda,
  ...
) {
  general_warn <- "Agenda validation failed:"

  # Check if the agenda is FALSE
  if (isFALSE(agenda)) {
    return(FALSE)
  }

  # Initialize the validation result
  is_valid <- TRUE

  if (purrr::is_empty(agenda)) {
    cli::cli_warn(
      c(
        general_warn,
        "x" = "The provided agenda is empty."
      )
    )

    return(FALSE)
  }

  if (!class(agenda) %in% c("list", "character")) {
    cli::cli_warn(
      c(
        general_warn,
        "x" = "The agenda must be a list or a file path, but it's class {.cls {class(agenda)}}."
      )
    )

    return(FALSE)
  }

  # Check if the agenda is a file path
  if (!purrr::is_empty(agenda) && is.character(agenda) && file.exists(agenda)) {
    # Potential error source if dget fails, wrap in tryCatch?
    # For now, assuming dget works or failure is acceptable.
    agenda <- dget(agenda)
  }

  # Check if the agenda is a list
  if (!is.list(agenda)) {
    cli::cli_warn(
      c(
        general_warn,
        "x" = "After potential loading from file, the agenda is not a list,
        it's class {.cls {class(agenda)}}."
      ),
      wrap = TRUE
    )

    return(FALSE)
  }

  # Check if the agenda elements are valid
  for (agenda_element in agenda) {
    # Pass the validation failure up the chain if an element is invalid
    if (!validate_agenda_element(agenda_element, ...)) {
      # The warning is already issued by validate_agenda_element
      return(FALSE)
    }
  }

  return(TRUE)
}

#' Validate summary tree id consistency
#'
#' @param summary_tree A list containing the summary tree or a file path to it.
#'
#' @return Nothing, will throw an error if the summary tree is not consistent.
check_summary_tree_consistency <- function(summary_tree) {
  if (is.character(summary_tree)) {
    # Potential error source if dget fails, wrap in tryCatch?
    summary_tree <- dget(summary_tree)
  }

  if (length(summary_tree) == 0) {
    cli::cli_abort(
      "The summary tree is empty and cannot be validated."
    )
  }

  obs_ids <- names(summary_tree)

  # Need to handle potential errors within build_ids_from_agenda if it can fail
  exp_ids <- tryCatch(
    build_ids_from_agenda(summary_tree),
    error = \(e) {
      cli::cli_abort(
        c(
          "Failed to build expected IDs from summary tree/agenda:",
          "x" = "{e$message}"
        ),
        parent = e
      )
    }
  )
  names(exp_ids) <- NULL # Remove names if they exist

  test <- all.equal(obs_ids, exp_ids)

  if (isTRUE(test)) {
    # Successfully validated
    return(invisible(TRUE))
  }

  # Construct the detailed error message for cli_abort
  error_details <- purrr::map(seq_along(obs_ids), \(i) {
    if (obs_ids[i] != exp_ids[i]) {
      # Use cli's formatting for key-value pairs
      c(
        "!" = "ID mismatch at index {i}:
        {.val {obs_ids[i]}} != {.val {exp_ids[i]}}"
      )
    }
  }) |>
    purrr::compact() |>
    unlist()

  cli::cli_abort(
    c(
      "The summary tree IDs and titles/sessions are not consistent.",
      "i" = "Comparison details: {test}",
      error_details
    )
  )
}

#' Check consistency of agenda and summary tree ids
#'
#' @param agenda A list containing the agenda or a file path to it.
#' @param summary_tree A list containing the summary tree or a file path to it.
#'
#' @return Nothing, will throw an error if the agenda and summary tree are not
#'   consistent.
check_agenda_summary_tree_consistency <- function(agenda, summary_tree) {
  if (is.character(agenda)) {
    agenda <- dget(agenda)
  }

  if (is.character(summary_tree)) {
    summary_tree <- dget(summary_tree)
  }

  check_summary_tree_consistency(summary_tree)

  agenda_ids <- build_ids_from_agenda(agenda)
  summary_ids <- names(summary_tree)

  test <- all.equal(agenda_ids, summary_ids)

  if (isTRUE(test)) {
    return()
  }

  cli::cli_abort(
    c(
      "The agenda and summary tree are not consistent: {test}",
      purrr::map(seq_along(agenda_ids), \(i) {
        if (!agenda_ids[i] %in% summary_ids[i]) {
          sprintf('"%s" != "%s"', agenda_ids[i], summary_ids[i])
        }
      }) |>
        purrr::compact() |>
        unlist() |>
        paste(collapse = "\n")
    )
  )
}
