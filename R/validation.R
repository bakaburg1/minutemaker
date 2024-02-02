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

  # Remove the 'agenda_element' argument from the list
  args$agenda_element <- NULL

  # Check if the required items are present in the agenda element
  is_valid <- purrr::imap_lgl(args, ~ {
    !is.null(agenda_element[[.y]]) || isFALSE(.x)
  }) |> all()

  if (isTRUE(from) || isTRUE(to)) {

    # Check if the times are interpretable
    for (time in c("from", "to")) {

      if (!inherits(agenda_element[[time]],
                    c("numeric", "POSIXct", "character"))) {
        stop(stringr::str_glue(
          'Agenda element "{time}" should be numeric, character or POSIXct,',
          "but it's of class {class(agenda_element[[time]])}."
        ))
      }

      if (!is.numeric(agenda_element[[time]]) &&
          is.na(parse_event_time(agenda_element[[time]]))
      ) {
        stop("Agenda element \"", time, "\" time not interpretable: ",
             agenda_element[[time]])
      }
    }

    if (class(agenda_element$from) != class(agenda_element$to)) {
      stop("The agenda element times are not of the same class:",
           " from: ", agenda_element$from,
           " to: ", agenda_element$to)
    }

    if (
      time_to_numeric(agenda_element$from) > time_to_numeric(agenda_element$to)
    ) {
      stop("Agenda element \"from\" time should preceed \"to\" time:",
           " from: ", agenda_element$from,
           " to: ", agenda_element$to)
    }
  }

  # Return the validation result
  is_valid
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

  titles <- purrr::map(summary_tree, \(x) x[["title"]])
  sessions <- purrr::map(summary_tree, \(x) {
    if (!purrr::is_empty(x[["session"]])) {
      x[["session"]]
    } else {
      x[["title"]]
    }
  })

  exp_ids <- paste(sessions, titles, sep = "_")

  test <- all.equal(obs_ids, exp_ids)

  if (isTRUE(test)) {
    return()
  }

  stop("The summary tree is not consistent: ", test, ".\n",
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
