#' Set the technical prompts that power the LLM model
#'
#' This function helps setting the prompts for the LLM that is tasked with
#' summarising meetings and conference presentations. The prompts are set as
#' options in the R environment. If `NULL` is passed as an argument, the default
#' prompts are used.`
#'
#' @param persona The general persona description of the AI secretary.
#' @param base_task The base task for a trasncript summarisation.
#' @param aggregate_task_rolling The task for the rolling window summary
#'   aggregation
#' @param transcript_template The template for inserting the transcript .
#' @param aggregate_template_rolling The template for inserting the summaries
#'   for the rolling window aggregation.
#' @param event_description_template The template for inserting the event
#'   description.
#' @param recording_details_template The template for inserting the recording
#'   details.
#' @param vocabulary_template The template for inserting the vocabulary.
#' @param diarization_template The template with standard diarization
#'   instructions and the placeholder for extra custom instructions.
#' @param audience_template The template for inserting the audience and summary
#'   focus description.
#' @param summary_template The template for inserting the summary structure
#'   instructions.
#' @param summary_structure The default summary structure instructions.
#' @param output_template The general output instruction template, with
#'   placeholders for trascript summarisation/summaries' aggregation output
#'   instruction and extra, user-defined, instructions.
#' @param output_summarisation The default output instructions for the
#'   transcript summarisation.
#' @param output_rolling_aggregation The default output instructions for the
#'   "rolling window" generated summaries' aggregation.
#' @param force If TRUE, overwrite existing prompts.
#'
#' @return Nothing. The prompts are set as options in the R environment.
#'
#' @importFrom stats setNames
#'
#' @export
#'
set_prompts <- function(
  persona = NULL,
  base_task = NULL,
  aggregate_task_rolling = NULL,
  event_description_template = NULL,
  recording_details_template = NULL,
  transcript_template = NULL,
  aggregate_template_rolling = NULL,
  vocabulary_template = NULL,
  diarization_template = NULL,
  audience_template = NULL,
  summary_template = NULL,
  summary_structure = NULL,
  output_template = NULL,
  output_summarisation = NULL,
  output_rolling_aggregation = NULL,

  force = TRUE
) {
  # Get the arguments as a list
  args <- as.list(environment())

  # Remove the 'force' argument from the list
  args$force <- NULL

  # Define a function to collapse the prompt pieces into a single string
  collapse <- purrr::partial(paste, sep = "\n")

  defaults <- list(
    persona = "You are an AI secretary, expert in summarising meetings and conference presentations, with expert knowledge in the field of the meeting/conference summarization.",

    base_task = "Your task is to provide a summary of the transcript segments that will be given to you.",

    aggregate_task_rolling = "Your task is to aggregate the summaries generated from the segments of a meeting/talk into a single, comprehensive text, without loss of information.",
    agenda_inference_task = collapse(
      "Your task is to extract individual talks from this transcript, creating an agenda. You can identify them from a change of speaker, and or, a change of topic. Try to detect broad changes of topics so to avoid splitting the transcript into an excessively large number of small talks; a talk usually last at least 10-15 minutes to one hour, so join into one talk very short change of topics, even if the speaker change. Aggregate both the talk itself and possible Q&A sessions in the same talk.",
      "You will be FIRST producing a step by step reasoning of what could be a good subdivision of the transcript into different talks, considering different competing subdivisions, and THEN suggest the agenda. Take speakers, topics, and timings into consideration in your reasoning.",
      "Your output will be a JSON object with two components: your reasoning and the actual agenda. The agenda must be an array of \"talk\" objects, each with the talk title, a short description (1 sentence), a label with the type of talk (e.g. welcome talk, conference outline, conference talk, meeting discussion, Q&A session, etc...), an array with one of more speakers, another array with moderators (if detectable) and the starting and end time in seconds. Add also the \"session\" object if it make sense as grouping.",
      "Here's an example of the output structure:",
      "###
       {
        reasoning = \"Your reasoning goes here\",
        agenda = [
           {
            title = \"The talk title\",
            type = \"Conference talk\",
            description = \"A description of this talk\",
            speakers = [\"speaker 1\", \"speaker 2\"],
            moderators = [\"moderator 1\"] # If detectable, otherwise ignore this field
            from = 1231, to = 2023
           },
            {...}, /* another talk element */
           ...
        ]
       }
       ###",
      "Important: process the whole transcript, do not be lazy: your agenda should cover the entirety of the transcript."
    ),

    event_description_template = collapse(
      "The following is a description of the event in which the talk/meeting took place, which will provide you with context.",
      "Use this information only to understand the context of the transcript, do not include it in the summary.",
      "<event_description>",
      "{event_description}",
      "</event_description>"
    ),

    recording_details_template = collapse(
      "The following are the information on the specific recording:",
      "<recording_details>",
      "{recording_details}",
      "</recording_details>"
    ),

    transcript_template = collapse(
      "Here is the transcript segment you need to summarise:",
      "<transcript>",
      "{transcript}",
      "</transcript>"
    ),

    aggregate_template_rolling = "Here are the segment summaries to aggregate:",

    agenda_inference_template = collapse(
      "This is the transcript of an event/meeting:\n<transcript>",
      "{transcript}",
      "</transcript>\n",
      "The transcript is formatted as a csv with the start and end time of each segment and the segment text."
    ),

    vocabulary_template = "Mind that the transcript is not perfect and the following and other terms and names may have been wrongly transcribed. Here's a list of technical terms, acronyms and names you may find in the trascript and that may have been wrongly transcribed:\n{vocabulary}.\nRecognize and correct misspelled versions of these and other related terms and names.",

    diarization_template = collapse(
      "<speaker_recognition_instructions>",
      "Take into account that multiple speakers may be present in the transcript, each identified by a specific (i.e., a name) or generic (e.g., Guest 1, Room 2) label.",
      "Keep into account that the diarization is never perfect, therefore the same speaker label may be wrong, or the same speaker may be identified with different labels in different parts of the transcript.",
      "If the speaker label is generic (e.g. Guest 1), try to infer the name from the context.",
      "{extra_diarization_instructions}",
      "</speaker_recognition_instructions>"
    ),

    audience_template = "Your summary must focus on aspects relevant for an audience with the following characteristics:\n<audience>{audience}</audience>",

    summarisation_template = collapse(
      "Your summary must include the following sections:",
      "<summary_sections>",
      "{summary_structure}",
      "</summary_sections>"
    ),

    summary_structure = collapse(
      "- Topic of the talk/meeting: Identify the main topic(s) discussed. Understanding the objective will guide you in determining which parts of the presentation are most crucial to include in your summary.",
      "- Key points: Identify all possible key points/results/take home messages of the presentation/meeting. These are the main ideas that the speaker wants to convey to the audience.",
      "- Questions and Discussions: If there were interventions from the audience, include key questions and answers or points of discussion. This often provides additional insight and clarifies any ambiguities in the presentation. Try to infer who asked the question from the transcript even if the name is not clearly mentioned.",
      "- Conclusions: Summarize the main conclusions of the presentation/meeting. If the speaker doesn't mention the conclusions explicitly, try to infer them from the context of the presentation/meeting. List any discussed action points if mentioned during the presentation/meeting."
    ),

    output_template = collapse(
      "The following instructions will guide you on how to write the output. Be sure to adhere to them:",
      "<output_instructions>",
      "{output_instructions}",
      "- Speak at the present tense when describing what was said during the talk/meeting.",
      "- Your summary must be around {output_length} pages long.",
      "- Remove structural html tags such as <summary_sections>",
      "</output_instructions>",
      "\nNow provide your output."
    ),

    output_summarisation = c(
      "When summarizing, aim to be concise but extremely comprehensive.",
      "Important: Summarise only the information provided in the <transcript>, and use the <event_description> and <recording_details> to understand the context. Do not include a summary of the <event_description> and <recording_details> in your output.",
      "Do not include in your summary any concept clearly not reported in the transcript.",
      "The summary must provide a clear and extremely information-dense representation of the transcript, without unnecessary details and formalisms.",
      "Avoid reporting circumstantial details, jokes, out-of-topic comments, etc.",
      "DO NOT REPORT personal opinions on other people's work, attitude, private life, interpersonal relationships, etc., if not central to the meeting. The speakers should fell free to express their personal feelings and out-of-topic thoughts without fear of being recorded in the summary.",
      "The summary must allow the reader to internalize most of the information of the original recording without reading the full transcript."
    ),

    output_rolling_aggregation = c(
      "Aggregate the individual summaries in a long text wihout loss of informations.",
      "Aim to be concise but extremely comprehensive.",
      "Important: Use only the information provided in the <segment_summary_*>, and use the <event_description> and <recording_details> to understand the context. Do not include a summary of the <event_description> and <recording_details> in your output.",
      "Do not include in your output any concept clearly not reported in the summaries.",
      "The output must provide a clear and extremely information-dense representation of the summaries, without unnecessary details and formalisms.",
      "The output must allow the reader to internalize most of the information of the original recording without reading the full transcript."
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
      # Raise a warning if the new prompt is different from the current one
      if (!is.null(current_prompt) && !identical(current_prompt, prompt)) {
        cli::cli_warn(
          "Overwriting existing prompt for {.val {prompt_name}} because
            {.code force = TRUE}."
        )
      }

      prompt |>
        list() |>
        setNames(opt_label) |>
        options()
    }
  })
}

#' Get the current prompts
#'
#' This function returns the current prompts as a list. If no argument is
#' provided, all the prompts are returned. If a vector of prompt names is
#' provided, only the requested prompts are returned.
#'
#' @param which A vector of one or more prompt names.
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
    invalid_prompts <- which[!which %in% valid_options]
    cli::cli_abort(
      c(
        "Invalid prompt name(s) requested:",
        "x" = "Unknown prompt name{?s}: {.val {invalid_prompts}}.",
        "i" = "Valid prompt names are: {.val {valid_options}}."
      )
    )
  }

  # If no prompts are requested, return all the prompts
  if (is.null(which)) {
    which <- valid_options
  }

  # Get the prompts
  prompts <- purrr::map(
    which,
    ~ {
      getOption(paste0("minutemaker_prompt_", .x))
    }
  ) |>
    purrr::set_names(which)

  # If only one prompt is requested, return it as an unnamed vector
  if (length(which) == 1) {
    prompts <- prompts[[1]]
  }

  prompts
}


#' Generate a transcript summarisation prompt
#'
#' This function is used by `summarise_transcript()` to generate a summarisation
#' prompt for a single transcript.
#'
#' @param transcript A transcript text, for example generated by
#'   `extract_text_from_transcript()`.
#' @param args A list of arguments to be passed to the prompt template.
#'
#' @return A summarisation prompt used by `summarise_transcript()`.
#'
generate_summarisation_prompt <- function(
  transcript,
  args
) {
  maybe <- function(x, value, cond = !is.null(x)) {
    if (isTRUE(cond)) value else NULL
  }

  # Expose template placeholders as variables for glue substitution
  base_task <- get_prompts("base_task")
  transcript_template <- get_prompts("transcript_template")
  event_description_prompt <- maybe(
    args$event_description,
    get_prompts("event_description_template")
  )
  recording_details_prompt <- maybe(
    args$recording_details,
    get_prompts("recording_details_template")
  )
  vocab_prompt <- maybe(args$vocabulary, get_prompts("vocabulary_template"))
  audience_prompt <- maybe(args$audience, get_prompts("audience_template"))
  summarisation_template <- get_prompts("summarisation_template")
  diarization_template <- get_prompts("diarization_template")
  output_template <- get_prompts("output_template")
  args$output_summarisation <- get_prompts("output_summarisation") |>
    paste(collapse = "\n")

  vocab_block <- NULL
  if (!is.null(args$vocabulary)) {
    # Format the vocabulary argument if a vector is provided
    args$vocabulary <- paste0(
      "- ",
      args$vocabulary,
      collapse = "\n"
    )
    vocab_block <- vocab_prompt
  }

  summarisation_block <- maybe(args$summary_structure, summarisation_template)
  diarization_block <- maybe(
    NULL,
    diarization_template,
    cond = isTRUE(args$consider_diarization)
  )
  audience_block <- maybe(args$audience, audience_prompt)

  args$output_instructions <- c(
    get_prompts("output_summarisation"),
    args$extra_output_instructions
  )

  # Remove extra_output_instructions since it would trigger the length check
  # later
  args$extra_output_instructions <- NULL

  # Collapse output instruction into a bullet point list
  args$output_instructions <- paste0(
    "- ",
    args$output_instructions,
    collapse = "\n"
  )

  if (is.null(args$output_length)) {
    args$output_length <- "3"
  }

  # Aggregate arguments if length > 1 vectors
  if (length(args$extra_diarization_instructions) > 1) {
    args$extra_diarization_instructions <- paste(
      args$extra_diarization_instructions,
      collapse = "\n"
    )
  }

  if (length(args$summary_structure) > 1) {
    args$summary_structure <- paste(
      args$summary_structure,
      collapse = "\n"
    )
  }

  prompt <- paste(
    base_task,
    event_description_prompt,
    recording_details_prompt,
    transcript_template,
    vocab_block,
    summarisation_block,
    diarization_block,
    audience_block,
    output_template,
    sep = "\n\n"
  ) |>
    stringr::str_replace_all("\n\n+", "\n\n") # remove multiple newlines

  long_arguments <- purrr::map_lgl(args, ~ length(.x) > 1)

  if (any(long_arguments)) {
    cli::cli_abort(
      "All arguments in args should have length
      1:{.arg {names(args)[long_arguments]}}"
    )
  }

  # leaving .null as default produces character(0) if any of the {vars} is
  # NULL
  stringr::str_glue_data(prompt, .x = args, .null = NULL)
}

#' Generate one rolling window summarisation prompt
#'
#' This function is used by `summarise_transcript()` to aggregate the summaries
#' generated by the "rolling window" summarisation of a single transcript.
#'
#' @param summaries A character vector of summaries of the transcript segments
#'   generated through a "rolling window" approach
#' @param args A list of arguments to be passed to the prompt template.
#'
#' @return A summarisation prompt used by `summarise_transcript()`.
#'
generate_rolling_aggregation_prompt <- function(
  summaries,
  args
) {
  maybe <- function(x, value, cond = !is.null(x)) {
    if (isTRUE(cond)) value else NULL
  }

  # Expose template placeholders as variables for glue substitution
  base_task <- get_prompts("base_task")
  transcript_template <- get_prompts("transcript_template")
  event_description_prompt <- maybe(
    args$event_description,
    get_prompts("event_description_template")
  )
  recording_details_prompt <- maybe(
    args$recording_details,
    get_prompts("recording_details_template")
  )
  vocab_prompt <- maybe(args$vocabulary, get_prompts("vocabulary_template"))
  audience_prompt <- maybe(args$audience, get_prompts("audience_template"))
  aggregate_task_rolling <- get_prompts("aggregate_task_rolling")
  aggregate_template_rolling <- get_prompts("aggregate_template_rolling")
  summarisation_template <- get_prompts("summarisation_template")
  output_template <- get_prompts("output_template")
  args$output_summarisation <- get_prompts("output_summarisation") |>
    paste(collapse = "\n")

  vocab_block <- NULL
  if (!is.null(args$vocabulary)) {
    args$vocabulary <- paste0(
      "- ",
      args$vocabulary,
      collapse = "\n"
    )
    vocab_block <- vocab_prompt
  }

  summarisation_block <- maybe(args$summary_structure, summarisation_template)
  audience_block <- maybe(args$audience, audience_prompt)

  args$output_instructions <- c(
    get_prompts("output_rolling_aggregation"),
    args$extra_output_instructions
  )

  # Remove extra_output_instructions since it would trigger the length check
  # later
  args$extra_output_instructions <- NULL

  # Collapse output instruction into a bullet point list
  args$output_instructions <- paste0(
    "- ",
    args$output_instructions,
    collapse = "\n"
  )

  if (is.null(args$output_length)) {
    args$output_length <- "3"
  }

  # Aggregate arguments if length > 1 vectors
  if (length(args$summary_structure) > 1) {
    args$summary_structure <- paste(
      args$summary_structure,
      collapse = "\n"
    )
  }

  summary_seq <- seq_along(summaries)

  summaries <- stringr::str_glue(
    "<segment_summary_{summary_seq}>\n{summaries}\n</segment_summary_{summary_seq}>"
  ) |>
    paste(collapse = "\n\n")

  prompt <- paste(
    aggregate_task_rolling,
    event_description_prompt,
    recording_details_prompt,
    aggregate_template_rolling,

    # Append the summaries
    summaries,

    vocab_block,
    summarisation_block,
    audience_block,
    output_template,

    sep = "\n\n"
  )

  long_arguments <- purrr::map_lgl(args, ~ length(.x) > 1)

  if (any(long_arguments)) {
    cli::cli_abort(
      "All arguments in args should have length
      1:{.arg {names(args)[long_arguments]}}"
    )
  }

  # leaving .null as default produces character(0) if any of the {vars} is
  # NULL
  stringr::str_glue_data(prompt, .x = args, .null = NULL)
}


#' Generate the agenda inference prompt
#'
#' This function is used by `infer_agenda_from_transcript()` to generate a
#' prompt for inferring the agenda from a transcript.
#'
#' @param transcript_segment A segment of the transcript to be used for
#'   inferring the agenda. Can be a character vector representing the data in CSV
#'   format or a data frame.
#' @param args A list of arguments to be passed to the prompt template. They can
#'   include: event_description, vocabulary and expected_agenda.
#'
#' @return A prompt used by `infer_agenda_from_transcript()`.
#'
generate_agenda_inference_prompt <- function(
  transcript_segment,
  args
) {
  if (is.data.frame(transcript_segment)) {
    transcript_segment <- readr::format_csv(transcript_segment)
  }

  if (!is.null(args$vocabulary)) {
    # Format the vocabulary argument if a vector is provided
    args$vocabulary <- paste0(
      "- ",
      args$vocabulary,
      collapse = "\n"
    )
  }

  # Aggregate instructions if length > 1 vectors and convert into the
  # extra_diarization_instructions argument
  if (length(args$diarization_instructions) > 0) {
    args$extra_diarization_instructions <- paste(
      args$diarization_instructions,
      collapse = "\n"
    )
  }

  long_arguments <- purrr::map_lgl(args, ~ length(.x) > 1)

  if (any(long_arguments)) {
    cli::cli_abort(
      "All arguments in args should have length
      1:{.arg {names(args)[long_arguments]}}"
    )
  }

  prompt <- paste(
    "Your task is to extract individual talks from a transcript, creating an agenda.",

    if (!is.null(args$event_description)) {
      # Uses the {event_description} argument
      get_prompts("event_description_template")
    },

    if (!is.null(args$vocabulary)) {
      # Uses the {vocabulary} argument
      get_prompts("vocabulary_template")
    },

    # Uses the {extra_diarization_instructions} argument
    if (!is.null(args$diarization_instructions)) {
      get_prompts("diarization_template")
    },

    "This is the transcript of the event/meeting from which you need to infer the agenda items:\n<transcript>\n{transcript_segment}\n</transcript>\n\nThe transcript is formatted as a csv with the start and end time of each segment, the segment text and possibly, the speakers.",

    sep = "\n\n"
  ) |>
    stringr::str_glue_data(.x = args, .null = NULL) |>
    paste(
      'You can identify the talks from a change of speakers, and or, a change of topic. Try to detect broad changes of topics so to avoid splitting the transcript into an excessively large number of small talks; a talk usually last at least 10-15 minutes to one hour, so join into one talk very short change of topics, even if the speaker change. Aggregate talks and the related Q&A sessions in the same talk.

You wil be FIRST producing an INFORMATION DENSE, step by step reasoning of what could be a good subdivision of the transcript into different talks, considering different competing subdivisions, listing each identified talk start time and topics. THEN you will extract the starting times of each talk.

Take speakers, topics, and timings into consideration in your reasoning. The reasoning doesn\'t have to be human readable. Favor a high information over length ratio.',

      if (!is.null(args$expected_agenda)) {
        stringr::str_glue_data(
          .x = args,
          .null = NULL,
          "The agenda is expected to have the following talks: ###
{expected_agenda}
###
Try to match the agenda you generated to this structure."
        )
      },

      'Your output will be a JSON object with two components: your reasoning and the start times of each identified talks. Here\'s an example of the output structure:
###
 {
  reasoning = "Your reasoning goes here",
  start_times = [1, 232, 1242, 2343, 5534, 7023, ...]
 }
 ###

Important: process the whole transcript, do not be lazy: your agenda WILL cover the entirety of the transcript, FROM START TO END WITHOUT TIME HOLES.',

      sep = "\n"
    )
}

#' Generate the prompt to extract an agenda element details from a transcript
#'
#' This function is used by `infer_agenda_from_transcript()` to generate a
#' prompt for extracting the details of an agenda element from a transcript.
#'
#' @param transcript_segment A segment of the transcript to be used for
#'   extracting the details of an agenda element. Can be a character vector
#'   representing the data in CSV format or a data frame.
#' @param args A list of arguments to be passed to the prompt template. They can
#'   include: event_description and vocabulary.
#'
#' @return A prompt used by `infer_agenda_from_transcript()`.
#'
generate_agenda_element_prompt <- function(
  transcript_segment,
  args
) {
  if (is.data.frame(transcript_segment)) {
    transcript_segment <- readr::format_csv(transcript_segment)
  }

  if (!is.null(args$vocabulary)) {
    # Format the vocabulary argument if a vector is provided
    args$vocabulary <- paste0(
      "- ",
      args$vocabulary,
      collapse = "\n"
    )
  }

  # Aggregate instructions if length > 1 vectors and convert into the
  # extra_diarization_instructions argument
  if (length(args$diarization_instructions) > 0) {
    args$extra_diarization_instructions <- paste(
      args$diarization_instructions,
      collapse = "\n"
    )
  }

  long_arguments <- purrr::map_lgl(args, ~ length(.x) > 1)

  if (any(long_arguments)) {
    cli::cli_abort(
      "All arguments in args should have length
      1:{.arg {names(args)[long_arguments]}}"
    )
  }

  prompt <- paste(
    "This is a segment of the transcript of an event/meeting:

<transcript>\n{transcript_segment}\n</transcript>

The transcript is formatted as a csv with the start and end time of each segment, the segment text and possibly, the speakers.",

    if (!is.null(args$event_description)) {
      # Uses the {event_description} argument
      get_prompts("event_description_template")
    },

    if (!is.null(args$vocabulary)) {
      # Uses the {vocabulary} argument
      get_prompts("vocabulary_template")
    },

    # Uses the {extra_diarization_instructions} argument
    if (!is.null(args$diarization_instructions)) {
      get_prompts("diarization_template")
    },

    sep = "\n\n"
  ) |>
    stringr::str_glue_data(.x = args, .null = NULL) |>
    paste(
      'Your task is to extract a title and a short description (1-2 sentences max) for this talk, considering that it\'s part of a larger event. Assign also a label, e.g., welcome talk, conference outline, conference talk, meeting discussion, Q&A session, etc... (the start/end times can be helpful for this). Extract also the speakers and the moderators (if any). Format your output as a JSON object with the following structure: ###
        {
            title = "The talk title",
            type = "A label to define the talk",
            description = "A description of this talk",
            speakers = ["a list of speakers"] # If detectable, otherwise ignore this,
            moderators = ["a list of moderatora"] # If detectable/appropriate, otherwise ignore this field
           }
        ###',

      if (!is.null(args$expected_agenda_element)) {
        paste(
          "The event expected agenda is the following, so try to match the extracted talk to this structure. But feel free to describe a novel element if you cannot find a logical match, since there could have been unexpected changes in the agenda: ###\n",
          args$expected_agenda_element,
          "\n###"
        )
      },

      "Provide your output.",

      sep = "\n\n"
    )
}
