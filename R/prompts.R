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

    event_description_template = collapse(
      "The following is a description of the event in which the talk/meeting took place, which will provide you with context.",
      "Use this information only to understand the context of the transcript, do not include it in the summary.",
      "<event_description>", "{event_description}", "</event_description>"
    ),

    recording_details_template = collapse(
      "The following are the information on the specific recording:",
      "<recording_details>",
      "{recording_details}",
      "</recording_details>"
    ),

    transcript_template = collapse(
      "Here is the transcript segment you need to summarise:",
      "<transcript>", "{transcript}", "</transcript>"
    ),

    # transcript_template_one_shot = collapse(
    #   "Here is the transcript segment you need to summarise:",
    #   "<transcript>", "{transcript}", "</transcript>"
    # ),
    #
    # transcript_template_rolling = collapse(
    #   "Here is the transcript of the segment you need to summarise:",
    #   "<transcript>", "[...]\n{transcript}\n[...]", "</transcript>"
    # ),

    aggregate_template_rolling = "Here are the segment summaries to aggregate:",

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
    stop("Invalid prompt name: ", which[!which %in% valid_options])
  }

  # If no prompts are requested, return all the prompts
  if (is.null(which)) {
    which <- valid_options
  }

  # Get the prompts
  prompts <- purrr::map(which, ~ {
    getOption(paste0("minutemaker_prompt_", .x))
  }) |> purrr::set_names(which)

  # If only one prompt is requested, return it as an unnamed vector
  if (length(which) == 1) {
    prompts <- prompts[[1]]
  }

  prompts

}
