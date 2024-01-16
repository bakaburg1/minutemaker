#' Set technical prompts that power the LLM model
#'
#' This function helps setting the prompts for the LLM that is tasked with
#' summarising meetings and conference presentations. The prompts are set as
#' options in the R environment. If `NULL` is passed as an argument, the default
#' prompts are used.`
#'
#' @param base_task The base task of the AI secretary.
#' @param transcript The transcript to be summarised.
#' @param vocabulary The vocabulary to be used.
#' @param diarization_instructions Instructions for diarization.
#' @param extra_diarization_instructions Extra instructions for diarization.
#' @param audience_instructions Instructions for the audience and the summary
#'   focus.
#' @param audience The audience for the summary.
#' @param summarisation_focus The focus of the summarisation.
#' @param output_format The format of the output.
#' @param presentation_details The details of the presentation.
#' @param meeting_description The description of the meeting.
#' @param force If TRUE, overwrite existing prompts.
#'
#' @return Nothing. The prompts are set as options in the R environment.
#'
#' @importFrom stats setNames
#'
#' @export
#'
set_prompts <- function(
    base_task = NULL,
    transcript = NULL,
    vocabulary = NULL,
    diarization_instructions = NULL,
    extra_diarization_instructions = NULL,
    audience_instructions = NULL,
    audience = NULL,
    summarisation_focus = NULL,
    output_format = NULL,
    presentation_details = NULL,
    meeting_description = NULL,

    force = TRUE
) {

  # Get the arguments as a list
  args <- as.list(environment())

  # Remove the 'force' argument from the list
  args$force <- NULL

  # Define a function to collapse the prompt pieces into a single string
  collapse <- purrr::partial(paste, sep = "\n")


  defaults <- list(
    base_task = collapse(
      "You are an AI secretary, expert in summarising meetings and conference presentations.",
      "Your task is to provide a summary of the transcripts that will be provided to you.",
      "Take into account that multiple speakers may be present in the transcript {diarization_instructions}."
    ),

    diarization_instructions = ", each idenfitied by a specific (i.e., a name) or generic (e.g. Guest 1) label. Keep into account that the diarization is never perfect, therefore the same speaker may be identified with different labels in different parts of the transcript",

    meeting_description = collapse(
      "The following is a description of the meeting:",
      "<meeting_description>", "{event_description}", "</meeting_description>"
    ),

    vocabulary = "Mind that the transcript is not perfect and the following and other terms and names may have been wrongly transcribed: {vocabulary}. Please correct them.",

    transcript = collapse(
      "Here is the transcript:",
      "<transcript>", "{transcript}", "</transcript>"
      ),

    audience_instructions = "Your summary must focus on aspects relevant for an audience with the following characteristics:\n<audience>{audience}</audience>",

    audience = "An audience with understanding of the topic",

    summarisation_focus = collapse(
      "Your summary must include the following sections:",
      "<summary_sections>",
      "- Topic of the talk/meeting: Identify the main topic(s) discussed. Understanding the objective will guide you in determining which parts of the presentation are most crucial to include in your summary.",
      "- Key points: Identify all possible key points/results/take home messages of the presentation/meeting. These are the main ideas that the speaker wants to convey to the audience.",
      "- Questions and Discussions: If there were interventions from the audience, include key questions and answers or points of discussion. This often provides additional insight and clarifies any ambiguities in the presentation. Try to infer the author of the question from the transcript even if they don't mention their name {extra_diarization_instructions}.",
      "- Conclusions: Summarize the main conclusions of the presentation/meeting. If the speaker doesn't mention the conclusions explicitly, try to infer them from the context of the presentation/meeting. List any discussed action points if mentioned during the presentation/meeting.",
      "</summary_sections>"
    ),

    extra_diarization_instructions = "; if the speaker label is generic (e.g. Guest 1), try to infer the name from the context",

    output_format = collapse(
      "<output_instructions>",
      "When summarizing, aim to be concise but extremely comprehensive.",
      "Use only the information provided in the transcript, do not add any details not mentioned in the transcript.",
      "Speak at the present tense when describing what was said during the talk/meeting.",
      "The summary must provide a clear and extremely information-dense representation of the transcript, without unnecessary details and formalisms.",
      "Avoid reporting circumstantial details, jokes, non-content related comments, etc.",
      "It must allow the reader to internalize most of the information of the original recording without reading the full transcript.",
      "Remember to maintain the original meaning and context of the presentation/discussion.",
      "Your summary must be one-two pages long.",
      "</output_instructions>"
    )
  )

  # Walk through the names of the default prompts
  purrr::walk(names(defaults), function(prompt_name) {
    # Create the option label
    opt_label <- paste0("minutemaker_prompt_", prompt_name)

    # Get the current prompt
    current_prompt <- getOption(opt_label)

    # Get the prompt from the arguments
    prompt <- args[[prompt_name]]

    # If the prompt is NULL, use the default prompt
    if (is.null(prompt)) {
      prompt <- defaults[[prompt_name]]
    }

    # If the current prompt is NULL or 'force' is TRUE, set the prompt
    if (is.null(current_prompt) || force) {
      prompt |>
        list() |>
        setNames(opt_label) |>
        options()
    }
  })
}

#' Get the current prompts
#'
#' @return A list containing the current prompts.
#'
#' @export
get_prompts <- function() {
  options()[grepl("minutemaker_prompt_", names(options()))]

}

#' Generate Recording Details
#'
#' This function generates a formatted string containing details about a
#' recording. The details include the type of talk/meeting, the session in which
#' it took place, the title of the talk/meeting, the main speakers, and the
#' moderators. All parameters are optional.
#'
#' @param agenda_element A list containing the details of the recording. If this
#'   is provided, the other parameters are ignored.
#' @param type The type of the talk/meeting, e.g., "conference talk",
#'   "workshop", "panel discussion", "conference welcome message", etc.
#' @param session The session in which the talk/meeting took place.
#' @param title The title of the talk/meeting.
#' @param speakers The main speakers of the talk/meeting as a vector.
#' @param moderators The moderators of the talk/meeting as a vector.
#'
#' @return A formatted string containing the details of the recording, or NULL
#'   if all parameters are NULL.
#'
#' @export
generate_recording_details <- function(
    agenda_element = NULL,
    type = NULL,
    session = NULL,
    title = NULL,
    speakers = NULL,
    moderators = NULL
) {

  # If agenda_element is not NULL, extract the information from it
  if (!is.null(agenda_element)) {
    type <- agenda_element$type
    session <- agenda_element$session
    title <- agenda_element$title
    speakers <- agenda_element$speakers
    moderators <- agenda_element$moderators
  }

  # Return NULL if all the arguments are NULL
  if (is.null(type) && is.null(session) &&
      is.null(title) && is.null(speakers) && is.null(moderators)) {
    return(NULL)
  }

  # Helper function to add a section to the details string if it is not NULL
  add_section <- function(section, section_name) {
    if (!is.null(section)) {
      paste0(
        "- ", stringr::str_flatten_comma(section_name),
        ": ", stringr::str_flatten_comma(section), ";\n")
    }
  }

  # Build the details prompt based on which arguments are not NULL
  paste0(
    "The following are the information on the recording: <recording_details>\n",
    add_section(type, "Type (the type of this specific talk/meeting)"),
    add_section(session, "Session (the conference/larger meeting session in which this talk/meeting took place)"),
    add_section(title, "Title (the title of the talk/meeting)"),
    add_section(speakers, "Speakers (the main speakers of this talk/meeting)"),
    add_section(moderators, "Moderators (the moderators of this talk/meeting)"),
    "</recording_details>"
  )
}

#' Summarise a recording transcript
#'
#' This function takes a transcript and various optional parameters, and returns
#' a summarised version of the transcript. All parameters apart from the
#' transcript are optional, and if not provided they will be ignored in the
#' final prompt.
#'
#' @param transcript A character vector representing the transcript to be
#'   summarised.
#' @param event_description A description of the event.
#' @param event_details Additional details about the event. We suggest using
#'   `generate_recording_details` to generate this string.
#' @param audience A description the intended audience and the focus of the
#'   summarisation.
#' @param vocabulary A character vector of specific vocabulary words, names,
#'   definitions, to help the LLM recognise misspellings and abbreviations.
#' @param consider_diarization A logical indicating whether the summarisation should take
#'  into account the diarization (e.g. the speakers) of the transcript. Default is TRUE
#' @param extra_diarization_instructions Additional diarization instructions,
#'   e.g., "Do not report the name of the speaker", "Use only the second name of
#'   the speaker", etc...
#' @param prompt_only If TRUE, only the prompt is returned, the LLM is not
#'   interrogated. Default is FALSE.
#' @param ... Additional arguments passed to the `interrogate_llm` function,
#'   such as the LLM provider.
#'
#' @return A summary of the transcript.
#'
#' @export
#'
summarise_transcript <- function(
    transcript,
    event_description = NULL,
    event_details = NULL,
    audience = NULL,
    vocabulary = NULL,
    consider_diarization = TRUE,
    extra_diarization_instructions = NULL,

    prompt_only = FALSE,
    ...
) {

  transcript <- paste(transcript, collapse = "\n")

  # Helper function to get the prompt options
  get_ops <- function(ops) {
    ops <- paste0("minutemaker_prompt_", ops)

    getOption(ops, NULL)
  }

  # Set the prompts if they are not set
  if (is.null(get_ops("base_task"))) {
    set_prompts()
  }

  diarization_instructions <- NULL

  if (consider_diarization) {
    diarization_instructions <- get_ops("diarization_instructions")
  }

  # Build the prompt
  prompt <- paste(
    get_ops("base_task"),

    if (!is.null(event_description)) {
      get_ops("event_description")
    },

    event_details,

    get_ops("transcript"),

    if (!is.null(vocabulary)) {
      vocabulary <- stringr::str_flatten_comma(vocabulary)
      get_ops("vocabulary")
    },

    get_ops("summarisation_focus"),

    if (!is.null(audience)) {
      audience <- stringr::str_flatten_comma(audience)
      get_ops("audience_instructions")
    },

    get_ops("output_format"),

    sep = "\n"
  ) |>
    stringr::str_replace_all("\n\n+", "\n\n") |> # remove multiple newlines
    stringr::str_glue(.null = NULL) # .null default makes the output character(0) if any of the {vars} is NULL

  if (prompt_only) {
    return(prompt)
  }

  interrogate_llm(
    c(user = prompt),
    ...
  )
}

#' Summarise a full meeting
#'
#' This function summarises a full meeting by processing the transcript data and
#' the agenda. It generates a summary for each talk in the meeting and saves the
#' results in an output file. It's built on top of `summarise_transcript`.
#'
#' @param transcript_data The transcript data of the meeting as a data frame.
#' @param agenda The agenda of the meeting, that is, a list of agenda elements
#'   each with a session name, a title, speaker and moderator lists, type of
#'   talk and start and end times. Alternatively, the path to an R file
#'   containing such a list.
#' @param output_file The file to save the results to as a list. Default is
#'   "event_summary.R".
#' @param meeting_description The description of the meeting. See
#'   `summarise_transcript` for more details.
#' @param meeting_audience The audience of the meeting. See
#'   `summarise_transcript` for more details.
#' @param vocabulary The vocabulary used in the meeting. See
#'   `summarise_transcript` for more details.
#' @param consider_diarization Whether to take into account the diarization of
#'   the transcript. Default is TRUE See `summarise_transcript` for more
#'   details.
#' @param extra_diarization_instructions Extra instructions for diarization. See
#'   `summarise_transcript` for more details.
#' @param overwrite Whether to overwrite existing summaries. Default is FALSE.
#' @param ... Additional arguments passed to `interrogate_llm` function, such as
#'   the LLM provider.
#'
#' @return The result tree of the meeting summary. Also saves the results in the
#'   output file as side effect.
#'
#' @export
summarise_full_meeting <- function(
    transcript_data,
    agenda,
    output_file = "event_summary.R",

    meeting_description = NULL,
    meeting_audience = NULL,
    vocabulary = NULL,
    consider_diarization = TRUE,
    extra_diarization_instructions = NULL,

    overwrite = FALSE,
    ...
) {

  # Import agenda from file
  if (is.character(agenda)) {
    agenda <- dget(agenda)
  }

  # Generate the output container if it doesn't exist
  if (!file.exists(output_file)) {
    result_tree <- list()
  } else {
    result_tree <- dget(output_file)
  }

  # Build the id of each talk using the session and title
  talks_ids <- build_ids_from_agenda(agenda)

  for (id in talks_ids) {

    # Skip if the talk has already been summarised
    if (!is.null(result_tree[[id]]) && !overwrite) {
      next
    }

    message("Talk: ", id)

    # Get the index of the current talk in the agenda
    i <- which(talks_ids == id)

    # Get the agenda element of the current talk
    agenda_element <- agenda[[i]]

    # Validate the agenda element to make sure it has a start and end time
    is_valid_agenda <- validate_agenda_element(
      agenda_element,
      from = TRUE, to = TRUE
    )

    if (!is_valid_agenda) {
      warning("The agenda element ", id, " is not valid. Skipping.",
              call. = FALSE, immediate. = TRUE)
      next
    }

    # Extract the transcript of the talk
    transcript_text <- extract_text_from_transcript(
      transcript_data,
      agenda_element = agenda_element
    )

    # Extract the details of the talk
    event_details <- generate_recording_details(agenda_element)

    # Extract the vocabulary of the talk
    talk_summary <- summarise_transcript(
      transcript = transcript_text,
      event_description = meeting_description,
      event_details = event_details,
      vocabulary = vocabulary,
      audience = meeting_audience,
      ...
    )

    # Create the output object for the current talk
    output <- list(
      session = agenda_element$session,
      title = agenda_element$title,
      summary = talk_summary
    )

    # Add the output to the results tree
    result_tree[[id]] <- output

    # Update the results file
    dput(result_tree, file = output_file)
  }

  # Return the result tree invisibly
  invisible(result_tree)
}
