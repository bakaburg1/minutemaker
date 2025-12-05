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
    for (field_name in c("from", "to")) {
      if (isTRUE(args[[field_name]])) {
        current_time_val <- agenda_element[[field_name]]

        if (is.null(current_time_val)) {
          # If NULL, it means a required time field is missing.
          # el_checks should have already set is_valid to FALSE.
          # Skip further processing for this specific NULL field.
          next
        }

        if (inherits(current_time_val, "integer")) {
          current_time_val <- as.numeric(current_time_val)
          # Keep time classes aligned for downstream comparisons in this check
          agenda_element[[field_name]] <- current_time_val
        }

        if (!inherits(current_time_val, c("numeric", "POSIXct", "character"))) {
          cli::cli_warn(
            c(
              "Agenda element validation failed:",
              "i" = "Element details:
                {.code {deparse(agenda_element) |> paste(collapse = '\\n')}}",
              "x" = "Agenda element field {.field {field_name}} must be numeric,
                character, or POSIXct, but it's class
                {.cls {class(current_time_val)}}."
            ),
            wrap = TRUE
          )
          is_valid <- FALSE

          # Skip further parsing for this field if class is fundamentally wrong
          next
        }

        if (is.character(current_time_val)) {
          # parse_event_time warns on failure; we silence that here and rely on
          # the explicit cli warning below when parsing returns NA.
          if (is.na(suppressWarnings(parse_event_time(current_time_val)))) {
            cli::cli_warn(
              c(
                "Agenda element validation failed:",
                "i" = "Element details:
                  {.code {deparse(agenda_element) |> paste(collapse = '\\n')}}",
                "x" = "Agenda element field
                  {.field {field_name}} time not interpretable:
                  {.val {current_time_val}}"
              ),
              wrap = TRUE
            )
            is_valid <- FALSE
          }
        }
      }
    } # End for loop over "from", "to"

    # Comparison logic: only if both 'from' and 'to' were required AND are
    # non-NULL.
    if (
      isTRUE(args[["from"]]) &&
        isTRUE(args[["to"]]) &&
        !is.null(agenda_element[["from"]]) &&
        !is.null(agenda_element[["to"]])
    ) {
      can_compare_types <- inherits(
        agenda_element[["from"]],
        c("numeric", "POSIXct", "character")
      ) &&
        inherits(agenda_element[["to"]], c("numeric", "POSIXct", "character"))

      if (can_compare_types) {
        shared_time_class <- intersect(
          class(agenda_element$from),
          class(agenda_element$to)
        )

        if (length(shared_time_class) == 0) {
          cli::cli_warn(
            c(
              "Agenda element validation failed:",
              "x" = "The agenda element {.field from} and {.field to}
                times are not of the same class:",
              "i" = "from ({.cls {class(agenda_element$from)}}):
                {.val {agenda_element$from}}",
              "i" = "to ({.cls {class(agenda_element$to)}}):
                {.val {agenda_element$to}}"
            ),
            wrap = TRUE
          )
          is_valid <- FALSE
        }

        # Order check: only if both also successfully convert to numeric
        from_numeric <- suppressWarnings(time_to_numeric(agenda_element$from))
        to_numeric <- suppressWarnings(time_to_numeric(agenda_element$to))

        if (
          !is.na(from_numeric) &&
            !is.na(to_numeric) &&
            from_numeric > to_numeric
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

  if (purrr::is_empty(agenda)) {
    cli::cli_warn(
      c(
        general_warn,
        "x" = "The provided agenda is empty."
      )
    )

    return(FALSE)
  }

  if (!(inherits(agenda, "list") || is.character(agenda))) {
    cli::cli_warn(
      c(
        general_warn,
        "x" = "The agenda must be a list or a file path,
        but it's class {.cls {class(agenda)}}."
      )
    )

    return(FALSE)
  }

  # Check if the agenda is a file path
  if (!purrr::is_empty(agenda) && is.character(agenda)) {
    if (!file.exists(agenda)) {
      cli::cli_warn(
        c(
          general_warn,
          "x" = "The agenda file path does not exist."
        )
      )

      return(FALSE)
    }

    agenda_from_file <- tryCatch(
      dget(agenda),
      warning = function(cnd) {
        cli::cli_warn(
          c(
            general_warn,
            "x" = paste0(
              "Failed to read the agenda file {.file {agenda}}: ",
              conditionMessage(cnd),
              "."
            )
          ),
          wrap = TRUE
        )

        return(NULL)
      },
      error = function(cnd) {
        cli::cli_warn(
          c(
            general_warn,
            "x" = paste0(
              "Failed to read the agenda file '{agenda}': ",
              conditionMessage(cnd),
              "."
            )
          ),
          wrap = TRUE
        )

        return(NULL)
      }
    )

    if (is.null(agenda_from_file)) {
      return(FALSE)
    }

    if (purrr::is_empty(agenda_from_file)) {
      cli::cli_warn(
        c(
          general_warn,
          "x" = "The loaded agenda is empty."
        )
      )

      return(FALSE)
    }

    agenda <- agenda_from_file
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

# Load a serialized R object from a file path and abort with context on failure.
load_serialized_object_or_abort <- function(path, label) {
  tryCatch(
    dget(path),
    warning = function(cnd) {
      cli::cli_abort(
        c(
          "Failed to read the {label} file '{path}'.",
          "x" = conditionMessage(cnd)
        ),
        parent = cnd
      )
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Failed to read the {label} file '{path}'.",
          "x" = conditionMessage(cnd)
        ),
        parent = cnd
      )
    }
  )
}

#' Validate summary tree id consistency
#'
#' @param summary_tree A list containing the summary tree or a file path to it.
#'
#' @return Nothing, will throw an error if the summary tree is not consistent.
check_summary_tree_consistency <- function(summary_tree) {
  if (is.character(summary_tree)) {
    summary_tree <- load_serialized_object_or_abort(
      summary_tree,
      "summary tree"
    )
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
  max_len <- max(length(obs_ids), length(exp_ids))
  error_details <- purrr::map(seq_len(max_len), \(i) {
    obs <- if (i <= length(obs_ids)) obs_ids[i] else NA
    exp <- if (i <= length(exp_ids)) exp_ids[i] else NA

    if (is.na(obs) || is.na(exp) || !identical(obs, exp)) {
      c(
        "!" = cli::format_inline(
          "ID mismatch at index {i}:
          {.val {obs}} != {.val {exp}}",
          .envir = rlang::env(i = i, obs = obs, exp = exp)
        )
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
    agenda <- load_serialized_object_or_abort(
      agenda,
      "agenda"
    )
  }

  if (is.character(summary_tree)) {
    summary_tree <- load_serialized_object_or_abort(
      summary_tree,
      "summary tree"
    )
  }

  check_summary_tree_consistency(summary_tree)

  agenda_ids <- build_ids_from_agenda(agenda)
  summary_ids <- names(summary_tree)

  test <- all.equal(agenda_ids, summary_ids)

  if (isTRUE(test)) {
    return()
  }

  # Build a detailed mismatch report even when the vectors have different lengths
  max_len <- max(length(agenda_ids), length(summary_ids))
  # Prepare detailed mismatch messages across both vectors
  error_details <- purrr::map_chr(seq_len(max_len), \(i) {
    agenda_id <- NA_character_
    summary_id <- NA_character_

    # If the current index exists in agenda_ids, extract it; otherwise, leave as
    # NA
    if (i <= length(agenda_ids)) {
      agenda_id <- agenda_ids[i]
    }

    # If the current index exists in summary_ids, extract it; otherwise, leave
    # as NA
    if (i <= length(summary_ids)) {
      summary_id <- summary_ids[i]
    }

    # If either agenda_id or summary_id is missing, or if they are not
    # identical, report the mismatch with a formatted message. Otherwise, return
    # an empty string.
    if (
      is.na(agenda_id) ||
        is.na(summary_id) ||
        !identical(agenda_id, summary_id)
    ) {
      cli::format_inline(
        "ID mismatch at index {i}: {.val {agenda_id}} != {.val {summary_id}}"
      )
    } else {
      ""
    }
  }) |>
    purrr::discard(~ !nzchar(.x))

  cli::cli_abort(
    c(
      "The agenda and summary tree are not consistent.",
      "i" = "Comparison details: {test}",
      error_details
    )
  )
}
