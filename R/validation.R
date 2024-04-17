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

  general_warn <- paste(
    "Agenda element validation failed:\n",
    styler::style_text(
      deparse(agenda_element)) |> paste(collapse = "\n"), "\n"
    )

  # Remove the 'agenda_element' argument from the list
  args$agenda_element <- NULL

  if (length(agenda_element) == 0) {
    warning(
      general_warn, "The agenda element is empty.",
      call. = FALSE, immediate. = TRUE)

    return(FALSE)
  }

  # Check if the required items are present in the agenda element
  el_checks <- purrr::imap_lgl(args, ~ {
    !is.null(agenda_element[[.y]]) || isFALSE(.x)
  })

  if (!all(el_checks)) {
    warning(
      general_warn, "Some of the required items are missing:\n",
      stringr::str_flatten_comma(names(args)[!el_checks]),
      call. = FALSE, immediate. = TRUE)
  }

  is_valid <- all(el_checks)

  if (isTRUE(from) || isTRUE(to)) {

    # Check if the times are interpretable
    for (time in c("from", "to")) {

      # Convert integer times to numeric to simplify the validation
      if (inherits(agenda_element[[time]], "integer")) {
        agenda_element[[time]] <- as.numeric(agenda_element[[time]])
      }

      if (!inherits(agenda_element[[time]],
                    c("numeric", "POSIXct", "character"))) {
        warning(
          general_warn, stringr::str_glue(
            'Agenda element "{time}" should be numeric, character or POSIXct,',
            "but it's of class {class(agenda_element[[time]])}."
          ), call. = FALSE, immediate. = TRUE)

        is_valid <- FALSE
      }

      if (!is.numeric(agenda_element[[time]]) &&
          is.na(parse_event_time(agenda_element[[time]]))
      ) {
        warning(
          general_warn, "Agenda element \"", time,
          "\" time not interpretable: ", agenda_element[[time]],
          call. = FALSE, immediate. = TRUE)

        is_valid <- FALSE
      }
    }

    if (class(agenda_element$from) != class(agenda_element$to)) {
      warning(
        general_warn, "The agenda element times are not of the same class:",
        " from: ", agenda_element$from,
        " to: ", agenda_element$to,
        call. = FALSE, immediate. = TRUE)

      is_valid <- FALSE
    }

    if (
      time_to_numeric(agenda_element$from) > time_to_numeric(agenda_element$to)
    ) {
      warning(
        general_warn, "Agenda element \"from\" time should preceed \"to\" time:",
        " from: ", agenda_element$from,
        " to: ", agenda_element$to,
        call. = FALSE, immediate. = TRUE)

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

  general_warn <- "Agenda validation failed:\n"

  # Check if the agenda is FALSE
  if (isFALSE(agenda)) {
    return(FALSE)
  }

  # Initialize the validation result
  is_valid <- TRUE

  if (purrr::is_empty(agenda)) {
    warning(
      general_warn, "The agenda is empty.",
      call. = FALSE, immediate. = TRUE)

    return(FALSE)
  }

  if (!class(agenda) %in% c("list", "character")) {
    warning(
      general_warn, "The agenda is not a list or a file path.",
      call. = FALSE, immediate. = TRUE)

    return(FALSE)
  }

  # Check if the agenda is a file path
  if (!purrr::is_empty(agenda) && is.character(agenda) && file.exists(agenda)){
    agenda <- dget(agenda)
  }

  # Check if the agenda is a list
  if (!is.list(agenda)) {
    warning(
      general_warn, "The agenda is not a list.",
      call. = FALSE, immediate. = TRUE)

    return(FALSE)
  }

  # Check if the agenda elements are valid
  for (agenda_element in agenda) {
    if (!validate_agenda_element(agenda_element, ...)) {
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
    summary_tree <- dget(summary_tree)
  }

  if (length(summary_tree) == 0) {
    stop("The summary tree is empty.")
  }

  obs_ids <- names(summary_tree)

  exp_ids <- build_ids_from_agenda(summary_tree)
  names(exp_ids) <- NULL

  test <- all.equal(obs_ids, exp_ids)

  if (isTRUE(test)) {
    return()
  }

  stop("The summary tree IDs and titles/sessions are not consistent: ",
       test, ".\n",
       purrr::map(seq_along(obs_ids), \(i) {
         if (obs_ids[i] != exp_ids[i]) {
           sprintf('"%s" != "%s"', obs_ids[i], exp_ids[i])
         }
       }) |> purrr::compact() |> unlist() |> paste(collapse = "\n")
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

  stop("The agenda and summary tree are not consistent: ", test, ".\n",
       purrr::map(seq_along(agenda_ids), \(i) {
         if (!agenda_ids[i] %in% summary_ids[i]) {
           sprintf('"%s" != "%s"', agenda_ids[i], summary_ids[i])
         }
       }) |> purrr::compact() |> unlist() |> paste(collapse = "\n")
  )
}
