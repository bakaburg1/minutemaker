
#' Extracts a sepecifoc talk text from a given transcript data
#'
#' The specific talk text can be identify using the relative agenda element or
#' manually inputting the start and end times.
#'
#' @param transcript_data A data frame containing the transcript data. It must
#'   have the following columns: 'start', 'end' and 'text'
#' @param agenda_element A list with 'from' and 'to' fields indicating the start
#'   and end times of the agenda element.
#' @param start_time The start time for the extraction.
#' @param end_time The end time for the extraction.
#'
#' @return A string containing the extracted text.
#'
#' @importFrom rlang .data
#'
#' @export
#'
extract_text_from_transcript <- function(
    transcript_data,
    agenda_element = NULL,
    start_time = NULL, end_time = NULL
) {

  # If no parameters are provided, the whole transcript is used and a warning is issued
  if (is.null(agenda_element) && is.null(start_time) && is.null(end_time)) {
    warning("The whole transcript will be used (may be very long)."
            , call. = FALSE, immediate. = TRUE)
  }

  # If an agenda element is provided, its start and end times are used
  if (!is.null(agenda_element)) {
    start_time <- agenda_element$from
    end_time <- agenda_element$to
  }

  # If no start time is provided, start from the beginning of the transcript
  if (is.null(start_time)) {
    start_time <- min(transcript_data$start)
  }

  # If no end time is provided, end at the end of the transcript
  if (is.null(end_time)) {
    start_time <- max(transcript_data$end)
  }

  # The data is filtered based on the start and end times, and the text is extracted
  transcript_data %>%
    filter(.data$start >= start_time, .data$end <= end_time) |>
    filter(.data$text != "[...]") |>
    with(paste(text, collapse = "\n"))
}

#' Builds IDs from each agenda item session and titles
#'
#' @param agenda A list containing the agenda items.
#'
#' @return A character vector containing the IDs for each agenda item.
#'
build_ids_from_agenda <- function(agenda) {
  # Build the id of each talk using the session and title
  talks_ids <- purrr::imap_chr(agenda, ~ {
    # If the title is NULL, use an index as title
    if (is.null(.x$title)) .x$title <- .y

    paste0(
      # If the session is not NULL, prepend it to the title
      if (!is.null(.x$session)) paste0(.x$session, "_"),
      .x$title
    )
  })
}

#' Formats a summary tree into a human-readable text
#'
#' The summary functions returns a machine-readable summary tree. This function
#' takes the summary tree and formats it into a human-readable text.
#'
#' @param summary_tree A list containing the summary tree, or a string with the
#'   path to a file containing the summary tree.
#' @param agenda A list containing the agenda items.
#' @param output_file A string with the path to the output file. If NULL, the
#'   output is returned invisibly and not written to a file.
#'
#' @return A string containing the formatted summary tree, invisibly. This is printed to
#'   the output file if one is provided.

#' @export
format_summary_tree <- function(
    summary_tree,
    agenda,
    output_file = NULL
) {

  # If summary_tree is a file path, load the data from the file
  if (is.character(summary_tree) && file.exists(summary_tree)) {
    summary_tree <- dget(summary_tree)
  }

  # Initialize the output string
  output <- ""

  # Build the id of each talk using the session and title
  talk_ids <- build_ids_from_agenda(agenda)

  # Loop over each talk id
  for (id in talk_ids) {
    # Extract the summary for the current talk
    talk_summary <- summary_tree[[id]]$summary
    # Extract the agenda element for the current talk
    agenda_element <- agenda[[which(talk_ids == id)]]

    # If no summary is found for the current talk, issue a warning and skip to the next talk
    if (is.null(talk_summary)) {
      warning("No summary found for ", id, ". Skipping.", call. = FALSE)
      next
    }

    # Generate a text version of the summary, with session, title, speakers, moderators and summary
    output_piece <- with(agenda_element, {
      speakers <- stringr::str_flatten_comma(speakers)
      moderators <- stringr::str_flatten_comma(moderators)

      stringr::str_glue("Session: {session};
    Title: {title};
    Speakers: {speakers};
    Moderators: {moderators};
    Summary:

    {talk_summary}")
    }) |>
      stringr::str_remove_all("\\n?\\w+:\\s*;") # Remove empty fields

    # Append the output piece to the output string
    output <- paste0(output, output_piece, "\n\n####################\n\n")
  }

  # If an output file is specified, write the output to the file
  if (!is.null(output_file)) {
    readr::write_lines(output, output_file)
  }

  # Return the output string invisibly
  invisible(output)
}


#' Validates an agenda element
#'
#' @param agenda_element A list containing the agenda element.
#' @param session A boolean indicating whether the `session` item should be present.
#' @param title A boolean indicating whether the `title` item should be present.
#' @param speakers A boolean indicating whether the `speakers` item should be present.
#' @param moderators A boolean indicating whether the `moderators` item should be present.
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

  # Return the validation result
  is_valid
}
