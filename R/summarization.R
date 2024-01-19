#' Set the technical prompts that power the LLM model
#'
#' This function helps setting the prompts for the LLM that is tasked with
#' summarising meetings and conference presentations. The prompts are set as
#' options in the R environment. If `NULL` is passed as an argument, the default
#' prompts are used.`
#'
#' @param base_task The base task of the AI secretary.
#' @param transcript_template The template for the transcript.
#' @param event_description_template The template for the event description.
#' @param recording_details_template The template for the recording details.
#' @param vocabulary_template The template for the vocabulary.
#' @param diarization_template The template for the diarization instructions.
#' @param diarization_instructions Default instructions for the diarization.
#' @param audience_template The template for the audience.
#' @param audience Description of the audience and the summary focus.
#' @param summarisation_template The template for the summarisation
#'   instructions.
#' @param summarisation_sections The sections to include in the summary.
#' @param output_template The template for the output instructions.
#' @param output_instructions Instructions for the output, i.e., how to produce
#'   and format the summary.
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
    transcript_template = NULL,
    event_description_template = NULL,
    recording_details_template = NULL,
    vocabulary_template = NULL,
    diarization_template = NULL,
    diarization_instructions = NULL,
    audience_template = NULL,
    audience = NULL,
    summarisation_template = NULL,
    summarisation_sections = NULL,
    output_template = NULL,
    output_instructions = NULL,

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
      "Your task is to provide a summary of the transcripts that will be provided to you."
    ),

    event_description_template = collapse(
      "The following is a description of the event, which will provide you with context.",
      "<event_description>", "{event_description}", "</event_description>"
    ),

    recording_details_template = collapse(
      "The following are the information on the recording:",
      "<recording_details>",
      "{recording_details}",
      "</recording_details>"
    ),

    transcript_template = collapse(
      "Here is the transcript:",
      "<transcript>", "{transcript}", "</transcript>"
    ),

    vocabulary_template = "Mind that the transcript is not perfect and the following and other terms and names may have been wrongly transcribed: {vocabulary}. Use your knowledge to recognize and correct misspelled versions of these terms.",

    diarization_template = collapse(
      "<speaker_recognition_instructions>",
      "Take into account that multiple speakers may be present in the transcript, each idenfitied by a specific (i.e., a name) or generic (e.g. Guest 1) label.",
      "Keep into account that the diarization is never perfect, therefore the same speaker label may be wring, or the same speaker may be identified with different labels in different parts of the transcript.",
      "{diarization_instructions}",
      "</speaker_recognition_instructions>"
      ),

    diarization_instructions = "; if the speaker label is generic (e.g. Guest 1), try to infer the name from the context",

    audience_template = "Your summary must focus on aspects relevant for an audience with the following characteristics:\n<audience>{audience}</audience>",

    audience_instructions = "An audience with understanding of the topic",

    summarisation_template = collapse(
      "Your summary must include the following sections:",
      "<summary_sections>",
      "{summarisation_sections}",
      "</summary_sections>"
    ),

    summarisation_sections = collapse(
      "- Topic of the talk/meeting: Identify the main topic(s) discussed. Understanding the objective will guide you in determining which parts of the presentation are most crucial to include in your summary.",
      "- Key points: Identify all possible key points/results/take home messages of the presentation/meeting. These are the main ideas that the speaker wants to convey to the audience.",
      "- Questions and Discussions: If there were interventions from the audience, include key questions and answers or points of discussion. This often provides additional insight and clarifies any ambiguities in the presentation. Try to infer who asked the question from the transcript even if the name is not clearly mentioned.",
      "- Conclusions: Summarize the main conclusions of the presentation/meeting. If the speaker doesn't mention the conclusions explicitly, try to infer them from the context of the presentation/meeting. List any discussed action points if mentioned during the presentation/meeting."
    ),

    output_template = collapse(
      "The following instructions will guide you on how to write the summary. Be sure to adhere to them:",
      "<output_instructions>",
      "{output_instructions}",
      "</output_instructions>",
      "\nNow provide your summary."
    ),

    output_instructions = collapse(
      "- When summarizing, aim to be concise but extremely comprehensive.",
      "- Summarise only the information provided in the <transcript>, and use the <event_description> and <recording_details> to understand the context. Do not add any information that is not present in the transcript.",
      "- Speak at the present tense when describing what was said during the talk/meeting.",
      "- The summary must provide a clear and extremely information-dense representation of the transcript, without unnecessary details and formalisms.",
      "- Avoid reporting circumstantial details, jokes, non-content related comments, etc.",
      "- The summary must allow the reader to internalize most of the information of the original recording without reading the full transcript.",
      "- Remember to maintain the original meaning and context of the presentation/discussion.",
      "- Your summary must be two to three pages long."
    )
  )

  # Walk through the names of the default prompts
  purrr::walk(names(defaults), function(prompt_name) {
    # Create the option label
    opt_label <- paste0("minutemaker_prompt_", prompt_name)

    # Get the current prompt. Cannot use get_prompts() since it would give
    # errors if the option is not set yet
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
get_prompts <- function(which = NULL) {

  # Get the names of the valid options
  valid_options <- names(options()) |>
    stringr::str_subset("minutemaker_prompt_") |>
    stringr::str_remove("minutemaker_prompt_")

  # Check if the requested prompts are valid
  if (!is.null(which) && any(!which %in% valid_options)) {
    stop("Invalid prompt name: ", which[!which %in% valid_options])
  }

  # If no prompts are requested, return all the prompts
  if (is.null(which)) {
    which <- valid_options
  }

  # Get the prompts
  prompts <- purrr::map_chr(which, ~ {
    getOption(paste0("minutemaker_prompt_", .x))
  }) |> purrr::set_names(which)

  # If only one prompt is requested, return it as an unnamed vector
  if (length(which) == 1) {
    prompts <- as.vector(prompts)
  }

  prompts

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
        ": ", stringr::str_flatten_comma(section), ";\n") # Moving \n in paste doesn't work for some reason
    }
  }

  # Build the details prompt based on which arguments are not NULL
  paste0(
    add_section(type, "Type (the type of this specific talk/meeting)"),
    add_section(session, "Session (the conference/larger meeting session in which this talk/meeting took place)"),
    add_section(title, "Title (the title of the talk/meeting)"),
    add_section(speakers, "Speakers (the main speakers of this talk/meeting)"),
    add_section(moderators, "Moderators (the moderators of this talk/meeting)")
  ) |> trimws()
}

#' Summarise a recording transcript
#'
#' This function takes a transcript and various optional parameters, and uses an
#' LLM to generate a summary. For `diarization_instructions` and
#' `output_instructions`, the default prompts are used if they are not provided
#' (check them using `get_prompts()`). All the other parameters apart from
#' `transcript` are optional, and if not provided they will be ignored in the
#' final prompt.
#'
#' @param transcript A character vector representing the transcript to be
#'   summarised.
#' @param event_description A description of the event.
#' @param recordings_details Details on the specific recording. We suggest using
#'   `generate_recording_details` to generate this string.
#' @param audience A description the intended audience and the focus of the
#'   summarisation.
#' @param vocabulary A character vector of specific vocabulary words, names,
#'   definitions, to help the LLM recognise misspellings and abbreviations.
#' @param consider_diarization A logical indicating whether the summarisation
#'   should take into account the diarization (e.g. the speakers) of the
#'   transcript. Default is TRUE
#' @param summarisation_sections A character vector of the sections to include
#'   in the summary. See `get_prompts("summarisation_sections")` for the
#'   defaults.
#' @param diarization_instructions Diarization instructions, e.g., "Do not
#'   report the name of the speaker", "Use only the second name of the speaker",
#'   etc... See get_prompts("diarization_instructions") for the default
#'   instructions.
#' @param output_instructions Instructions for the output, i.e., how to produce
#'   and format the summary. See get_prompts("output_instructions") for the
#'   defaults.
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
    recording_details = NULL,
    audience = NULL,
    vocabulary = NULL,
    consider_diarization = TRUE,

    summarisation_sections = NULL,
    diarization_instructions = NULL,
    output_instructions = NULL,

    prompt_only = FALSE,
    ...
) {

  transcript <- paste(transcript, collapse = "\n")


  # Set the default prompts if not already set
  set_prompts()

  # Check if the arguments are NULL and if so, get the prompts
  if (is.null(diarization_instructions)) {
    diarization_instructions <- get_prompts("diarization_instructions")
  }

  if (is.null(output_instructions)) {
    output_instructions <- get_prompts("output_instructions")
  }

  if (is.null(summarisation_sections)) {
    summarisation_sections <- get_prompts("summarisation_sections")
  }

  if (is.null(audience)) {
    audience <- get_prompts("audience")
  }

  # Build the prompt
  prompt <- paste(
    get_prompts("base_task"),

    if (!is.null(event_description)) {
      get_prompts("event_description_template")
    },

    #  Uses the {recording_details} as generated by generate_recording_details()
    if (!is.null(recording_details)) {
      get_prompts("recording_details_template")
    },

    # Uses the {transcript} argument
    get_prompts("transcript_template"),

    if (!is.null(vocabulary)) {
      vocabulary <- stringr::str_flatten_comma(vocabulary)
      get_prompts("vocabulary_template")
    },

    # Uses the {summarisation_sections} argument
    if (!is.null(summarisation_sections)) {
      get_prompts("summarisation_template")
    },

    # Uses the {diarization_instructions} argument
    if (consider_diarization && !is.null(diarization_instructions)) {
      get_prompts("diarization_template")
    },

    # Uses the {audience} argument
    if (!is.null(audience)) {
      audience <- stringr::str_flatten_comma(audience)
      get_prompts("audience_template")
    },

    # Uses the {output_instructions} argument
    if (!is.null(output_instructions)) {
      get_prompts("output_template")
    },

    sep = "\n\n"
  ) |>
    stringr::str_replace_all("\n\n+", "\n\n") |> # remove multiple newlines
    stringr::str_glue(.null = NULL) # leaving .null default produces character(0) if any of the {vars} is NULL

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
#' This function summarises a full event comprising multiple meetings/talks
#' described in an `agenda`. It generates a summary for each talk in the meeting
#' and saves the results in an output file. It's built on top of
#' `summarise_transcript`.
#'
#' @param transcript_data The transcript data of the meeting as a data frame.
#' @param agenda The agenda of the meeting, that is, a list of agenda elements
#'   each with a session name, a title, speaker and moderator lists, type of
#'   talk and start and end times. Alternatively, the path to an R file
#'   containing such a list.
#' @param output_file The file to save the results to as a list. Default is
#'   "event_summary.R".
#' @param event_description The description of the event See
#'   `summarise_transcript` for more details.
#' @param event_audience The audience of the event See `summarise_transcript`
#'   for more details.
#' @param vocabulary The vocabulary used in the meeting. See
#'   `summarise_transcript` for more details.
#' @param consider_diarization Whether to take into account the diarization of
#'   the transcript. Default is TRUE See `summarise_transcript` for more
#'   details.
#' @param summarisation_sections,diarization_instructions,output_instructions
#'   Specific instructions necessary to build the summarisation prompt. See
#'   `summarise_transcript` for more details and run `get_prompts()` to see the
#'   defaults.
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

    event_description = NULL,
    event_audience = NULL,
    vocabulary = NULL,
    consider_diarization = TRUE,

    summarisation_sections = NULL,
    diarization_instructions = NULL,
    output_instructions = NULL,

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
    recording_details <- generate_recording_details(agenda_element)

    # Extract the vocabulary of the talk
    talk_summary <- summarise_transcript(
      transcript = transcript_text,
      event_description = event_description,
      recording_details = recording_details,
      vocabulary = vocabulary,
      audience = event_audience,

      summarisation_sections = summarisation_sections,
      diarization_instructions = diarization_instructions,
      output_instructions = output_instructions,
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
