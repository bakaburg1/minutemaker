# Helper function to clear minutemaker prompt options
.clear_minutemaker_prompts <- function() {
  opts <- options()
  mm_opts_names <- names(opts)[startsWith(names(opts), "minutemaker_prompt_")]
  if (length(mm_opts_names) > 0) {
    null_list <- stats::setNames(
      vector("list", length(mm_opts_names)),
      mm_opts_names
    )
    options(null_list)
  }
}

# Tests for set_prompts() and get_prompts() ---

test_that("set_prompts() sets default prompts and get_prompts() retrieves them", {
  .clear_minutemaker_prompts()

  # Set prompts with default values
  set_prompts()

  # Retrieve all prompts
  all_prompts <- get_prompts()

  # Check that all_prompts is a list and has the expected names
  expect_type(all_prompts, "list")
  expected_prompt_names <- c(
    "persona",
    "base_task",
    "aggregate_task_rolling",
    "agenda_inference_task",
    "event_description_template",
    "recording_details_template",
    "transcript_template",
    "aggregate_template_rolling",
    "agenda_inference_template",
    "vocabulary_template",
    "diarization_template",
    "audience_template",
    "summarisation_template",
    "summary_structure",
    "output_template",
    "output_summarisation",
    "output_rolling_aggregation"
  )
  expect_setequal(names(all_prompts), expected_prompt_names)

  # Check that individual prompts can be retrieved
  persona_prompt <- get_prompts("persona")
  expect_identical(persona_prompt, all_prompts$persona)

  # Check that multiple specific prompts can be retrieved
  specific_prompts <- get_prompts(c("base_task", "transcript_template"))
  expect_type(specific_prompts, "list")
  expect_length(specific_prompts, 2)
  expect_identical(specific_prompts$base_task, all_prompts$base_task)
  expect_identical(
    specific_prompts$transcript_template,
    all_prompts$transcript_template
  )
})

test_that("set_prompts() overwrites prompts when force = TRUE", {
  .clear_minutemaker_prompts()
  # Set initial prompts without expecting a warning here, as state is clean
  set_prompts(persona = "initial persona", force = TRUE)
  initial_persona <- get_prompts("persona")
  expect_identical(initial_persona, "initial persona")

  # Overwrite with force = TRUE (default)
  # Expect a warning when overwriting
  expect_warning(
    set_prompts(persona = "new persona", force = TRUE),
    regexp = "Overwriting existing prompt for .*persona.* because .*force = TRUE"
  )
  new_persona <- get_prompts("persona")
  expect_identical(new_persona, "new persona")

  # Test that default is used if NULL is passed
  expect_warning(
    set_prompts(persona = NULL, force = TRUE),
    regexp = "Overwriting existing prompt for .*persona.* because .*force = TRUE"
  )
  default_persona <- getOption("minutemaker_prompt_persona") # get default directly
  current_persona <- get_prompts("persona")
  expect_false(identical(current_persona, "new persona")) # should be default now

  # Reset prompts to default for subsequent tests
  set_prompts(force = TRUE)
})

test_that("set_prompts() does not overwrite prompts when force = FALSE and prompt exists", {
  .clear_minutemaker_prompts()
  # Set initial prompts without expecting a warning here, as state is clean
  set_prompts(persona = "initial persona for no force test", force = TRUE)
  initial_persona <- get_prompts("persona")
  expect_identical(initial_persona, "initial persona for no force test")

  # Attempt to overwrite with force = FALSE
  set_prompts(persona = "new persona, should not be set", force = FALSE)
  current_persona <- get_prompts("persona")
  expect_identical(current_persona, "initial persona for no force test")

  # Reset prompts to default for subsequent tests
  # This might warn if it overwrites "initial persona for no force test"
  expect_warning(
    set_prompts(force = TRUE),
    regexp = "Overwriting existing prompt"
  )
})

test_that("get_prompts() handles requests for invalid prompt names", {
  .clear_minutemaker_prompts()
  set_prompts() # Ensure defaults are set

  # Request an invalid prompt name
  expect_error(
    get_prompts("invalid_prompt_name"),
    "Invalid prompt name\\(s\\) requested"
  )

  # Request a mix of valid and invalid prompt names
  expect_error(
    get_prompts(c("persona", "invalid_prompt_name")),
    "Invalid prompt name\\(s\\) requested"
  )
})

# Tests for generate_summarisation_prompt() ---

test_that("generate_summarisation_prompt() works with minimal arguments", {
  # Set default prompts
  set_prompts(force = TRUE)

  transcript_text <- "This is a test transcript."
  args <- list(
    transcript = transcript_text,
    event_description = NULL,
    recording_details = NULL,
    vocabulary = NULL,
    summary_structure = NULL,
    consider_diarization = FALSE,
    audience = NULL,
    extra_output_instructions = NULL,
    output_length = "1" # Specify to avoid NULL default issues in glue
  )

  prompt <- generate_summarisation_prompt(transcript_text, args)

  expect_type(prompt, "character")
  expect_length(prompt, 1)
  expect_match(prompt, get_prompts("base_task"), fixed = TRUE)
  expect_match(prompt, transcript_text, fixed = TRUE)
  expect_match(
    prompt,
    stringr::str_glue(
      "- Your summary must be around {args$output_length} pages long."
    ),
    fixed = TRUE
  )
  expect_match(prompt, get_prompts("output_summarisation")[1], fixed = TRUE)
})

test_that(
  "generate_summarisation_prompt() adds transcript when missing from args",
  {
    set_prompts(force = TRUE)
    transcript_text <- "Transcript provided only as argument."
    args <- list(
      event_description = NULL,
      recording_details = NULL,
      vocabulary = NULL,
      summary_structure = NULL,
      consider_diarization = FALSE,
      audience = NULL,
      extra_output_instructions = NULL,
      output_length = "1"
    )

    prompt <- generate_summarisation_prompt(transcript_text, args)

    expect_match(prompt, transcript_text, fixed = TRUE)
    expect_type(prompt, "character")
    expect_length(prompt, 1)
  }
)

test_that("generate_summarisation_prompt() includes all optional sections", {
  set_prompts(force = TRUE)

  transcript_text <- "Another test transcript."
  args <- list(
    transcript = transcript_text,
    event_description = "A very important meeting.",
    recording_details = "Recorded on Tuesday.",
    vocabulary = c("term1", "acronym2"),
    summary_structure = "Custom summary structure.",
    consider_diarization = TRUE,
    extra_diarization_instructions = "Speaker 1 is John.",
    audience = "Technical experts.",
    extra_output_instructions = "Be very formal.",
    output_length = "2"
  )

  prompt <- generate_summarisation_prompt(transcript_text, args)

  expect_type(prompt, "character")
  expect_length(prompt, 1)

  # Check for presence of all templates and content
  expect_match(prompt, get_prompts("base_task"), fixed = TRUE)
  expect_match(
    prompt,
    "<event_description>",
    fixed = TRUE
  )
  expect_match(prompt, args$event_description, fixed = TRUE)
  expect_match(
    prompt,
    "<recording_details>",
    fixed = TRUE
  )
  expect_match(prompt, args$recording_details, fixed = TRUE)
  expect_match(
    prompt,
    "<transcript>",
    fixed = TRUE
  )
  expect_match(prompt, transcript_text, fixed = TRUE)
  expect_match(
    prompt,
    "wrongly transcribed",
    fixed = TRUE
  )
  expect_match(prompt, "term1", fixed = TRUE)
  expect_match(prompt, "acronym2", fixed = TRUE)
  expect_match(
    prompt,
    "<summary_sections>",
    fixed = TRUE
  )
  expect_match(prompt, args$summary_structure, fixed = TRUE)
  expect_match(
    prompt,
    "<speaker_recognition_instructions>",
    fixed = TRUE
  )
  expect_match(prompt, args$extra_diarization_instructions, fixed = TRUE)
  expect_match(
    prompt,
    "<audience>",
    fixed = TRUE
  )
  expect_match(prompt, args$audience, fixed = TRUE)
  expect_match(
    prompt,
    "<output_instructions>",
    fixed = TRUE
  )
  expect_match(prompt, args$extra_output_instructions, fixed = TRUE)
  expect_match(
    prompt,
    stringr::str_glue(
      "- Your summary must be around {args$output_length} pages long."
    ),
    fixed = TRUE
  )
})

test_that("generate_summarisation_prompt() handles multi-line args correctly", {
  set_prompts(force = TRUE)
  transcript_text <- "Transcript with multi-line args."
  args <- list(
    transcript = transcript_text,
    event_description = NULL,
    recording_details = NULL,
    vocabulary = c("item1", "item2"), # multi-line
    summary_structure = c("Section 1", "Section 2"), # multi-line
    consider_diarization = TRUE,
    extra_diarization_instructions = c("Instr 1", "Instr 2"), # multi-line
    audience = NULL,
    extra_output_instructions = c("Output 1", "Output 2"), # multi-line
    output_length = "1"
  )

  prompt <- generate_summarisation_prompt(transcript_text, args)

  expect_match(prompt, "- item1\n- item2", fixed = TRUE)
  expect_match(prompt, "Section 1\nSection 2", fixed = TRUE)
  expect_match(prompt, "Instr 1\nInstr 2", fixed = TRUE)
  expect_match(prompt, "- Output 1\n- Output 2", fixed = TRUE)
})


test_that("generate_summarisation_prompt() aborts if args elements have length > 1 after processing", {
  set_prompts(force = TRUE)
  transcript_text <- "test transcript"
  # This test checks the internal validation within
  # generate_summarisation_prompt. The function itself collapses some
  # multi-element vectors (like vocabulary). We need to ensure the final check
  # for `length(args$element) > 1` is robust. Let's test a normally single-value
  # arg with multiple values.
  args_bad <- list(
    transcript = transcript_text,
    event_description = c("desc1", "desc2"), # This should cause an error
    recording_details = NULL,
    vocabulary = NULL,
    summary_structure = NULL,
    consider_diarization = FALSE,
    audience = NULL,
    extra_output_instructions = NULL,
    output_length = "1"
  )

  expect_error(
    generate_summarisation_prompt(transcript_text, args_bad),
    "All arguments in args should have length 1:`event_description`"
  )
})

# Tests for generate_rolling_aggregation_prompt() ---

test_that("generate_rolling_aggregation_prompt() works with minimal arguments", {
  set_prompts(force = TRUE)
  summaries_vec <- c("Summary 1.", "Summary 2.")
  args <- list(
    event_description = NULL,
    recording_details = NULL,
    summary_structure = NULL,
    audience = NULL,
    extra_output_instructions = NULL,
    output_length = "3" # default if NULL in func, but explicit here
  )

  prompt <- generate_rolling_aggregation_prompt(summaries_vec, args)

  expect_type(prompt, "character")
  expect_length(prompt, 1)
  expect_match(prompt, get_prompts("aggregate_task_rolling"), fixed = TRUE)
  expect_match(
    prompt,
    "<segment_summary_1>\nSummary 1.\n</segment_summary_1>",
    fixed = TRUE
  )
  expect_match(
    prompt,
    "<segment_summary_2>\nSummary 2.\n</segment_summary_2>",
    fixed = TRUE
  )
  expect_match(
    prompt,
    stringr::str_glue(
      "- Your summary must be around {args$output_length} pages long."
    ),
    fixed = TRUE
  )
  expect_match(
    prompt,
    get_prompts("output_rolling_aggregation")[1],
    fixed = TRUE
  )
})

test_that("generate_rolling_aggregation_prompt() includes all optional sections", {
  set_prompts(force = TRUE)
  summaries_vec <- c("First summary.", "Second summary, very detailed.")
  args <- list(
    event_description = "Another important meeting.",
    recording_details = "Recorded on Wednesday by AI.",
    summary_structure = "Aggregated custom structure.",
    vocabulary = c("Term1", "Name Two"),
    audience = "General audience.",
    extra_output_instructions = "Use simple language.",
    output_length = "5"
  )

  prompt <- generate_rolling_aggregation_prompt(summaries_vec, args)

  expect_type(prompt, "character")
  expect_length(prompt, 1)

  expect_match(prompt, get_prompts("aggregate_task_rolling"), fixed = TRUE)
  expect_match(
    prompt,
    "<segment_summary_1>\nFirst summary.\n</segment_summary_1>",
    fixed = TRUE
  )
  expect_match(
    prompt,
    "<segment_summary_2>\nSecond summary, very detailed.\n</segment_summary_2>",
    fixed = TRUE
  )

  expect_match(
    prompt,
    "<event_description>",
    fixed = TRUE
  )
  expect_match(prompt, args$event_description, fixed = TRUE)
  expect_match(
    prompt,
    "<recording_details>",
    fixed = TRUE
  )
  expect_match(prompt, args$recording_details, fixed = TRUE)
  expect_match(
    prompt,
    "<summary_sections>",
    fixed = TRUE
  )
  expect_match(prompt, args$summary_structure, fixed = TRUE)
  expect_match(
    prompt,
    stringr::str_glue_data(
      get_prompts("vocabulary_template"),
      .x = list(
        vocabulary = paste0("- ", args$vocabulary, collapse = "\n")
      ),
      .null = NULL
    ),
    fixed = TRUE
  )
  expect_match(prompt, "- Term1", fixed = TRUE)
  expect_match(prompt, "- Name Two", fixed = TRUE)
  expect_match(
    prompt,
    "<audience>",
    fixed = TRUE
  )
  expect_match(prompt, args$audience, fixed = TRUE)
  expect_match(
    prompt,
    "<output_instructions>",
    fixed = TRUE
  )
  expect_match(prompt, args$extra_output_instructions, fixed = TRUE)
  expect_match(
    prompt,
    stringr::str_glue(
      "- Your summary must be around {args$output_length} pages long."
    ),
    fixed = TRUE
  )
})

test_that("generate_rolling_aggregation_prompt() handles multi-line args", {
  set_prompts(force = TRUE)
  summaries_vec <- c("S1", "S2")
  args <- list(
    event_description = NULL,
    recording_details = NULL,
    summary_structure = c("Struct A", "Struct B"), # multi-line
    audience = NULL,
    extra_output_instructions = c("Out A", "Out B"), # multi-line
    output_length = "2"
  )

  prompt <- generate_rolling_aggregation_prompt(summaries_vec, args)
  expect_match(prompt, "Struct A\nStruct B", fixed = TRUE)
  expect_match(prompt, "- Out A\n- Out B", fixed = TRUE)
})

test_that("generate_rolling_aggregation_prompt() aborts if args elements have length > 1 after processing", {
  set_prompts(force = TRUE)
  summaries_vec <- c("S1")
  args_bad <- list(
    event_description = c("desc1", "desc2"), # This should cause an error
    recording_details = NULL,
    summary_structure = NULL,
    audience = NULL,
    extra_output_instructions = NULL,
    output_length = "1"
  )

  expect_error(
    generate_rolling_aggregation_prompt(summaries_vec, args_bad),
    "All arguments in args should have length 1:`event_description`"
  )
})

# Tests for generate_agenda_inference_prompt() ---

test_that("generate_agenda_inference_prompt() works with minimal args (character transcript)", {
  set_prompts(force = TRUE)
  transcript_csv <- "start,end,text\n0,10,Hello world"
  args <- list(
    event_description = NULL,
    vocabulary = NULL,
    diarization_instructions = NULL,
    expected_agenda = NULL
  )

  prompt <- generate_agenda_inference_prompt(transcript_csv, args)
  expect_type(prompt, "character")
  expect_length(prompt, 1)
  expect_match(prompt, "Your task is to extract individual talks", fixed = TRUE)
  expect_match(prompt, transcript_csv, fixed = TRUE)
  expect_no_match(prompt, get_prompts("vocabulary_template"), fixed = TRUE)
  expect_no_match(prompt, get_prompts("diarization_template"), fixed = TRUE)
  expect_no_match(prompt, "The agenda is expected to have", fixed = TRUE)
})

test_that("generate_agenda_inference_prompt() works with data.frame transcript", {
  set_prompts(force = TRUE)
  transcript_df <- data.frame(
    start = 0,
    end = 10,
    text = "Hello from data.frame"
  )
  transcript_csv_expected <- readr::format_csv(transcript_df)
  args <- list(
    event_description = NULL,
    vocabulary = NULL,
    diarization_instructions = NULL,
    expected_agenda = NULL
  )

  prompt <- generate_agenda_inference_prompt(transcript_df, args)
  expect_match(prompt, transcript_csv_expected, fixed = TRUE)
})

test_that("generate_agenda_inference_prompt() includes all optional sections", {
  set_prompts(force = TRUE)
  transcript_csv <- "start,end,text\n0,10,Hello again"
  args <- list(
    event_description = "An event.",
    vocabulary = c("vocab1", "term2"),
    diarization_instructions = "Speaker A is primary.\nNote Speaker B.",
    expected_agenda = "Talk 1: Intro\nTalk 2: Main Topic"
  )

  prompt <- generate_agenda_inference_prompt(transcript_csv, args)

  expect_match(
    prompt,
    "<event_description>",
    fixed = TRUE
  )
  expect_match(prompt, args$event_description, fixed = TRUE)

  expect_match(
    prompt,
    "wrongly transcribed",
    fixed = TRUE
  )
  expect_match(prompt, "- vocab1", fixed = TRUE)
  expect_match(prompt, "- term2", fixed = TRUE)

  expect_match(
    prompt,
    "<speaker_recognition_instructions>",
    fixed = TRUE
  )
  expect_match(prompt, "Speaker A is primary.", fixed = TRUE)
  expect_match(prompt, "Note Speaker B.", fixed = TRUE)

  expect_match(
    prompt,
    "The agenda is expected to have the following talks:",
    fixed = TRUE
  )
  expect_match(prompt, args$expected_agenda, fixed = TRUE)
})

test_that("generate_agenda_inference_prompt() aborts if args elements have length > 1 after processing", {
  set_prompts(force = TRUE)
  transcript_csv <- "start,end,text\n0,1,test"
  args_bad <- list(
    event_description = c("desc1", "desc2"), # This should cause an error
    vocabulary = NULL,
    diarization_instructions = NULL,
    expected_agenda = NULL
  )

  expect_error(
    generate_agenda_inference_prompt(transcript_csv, args_bad),
    "All arguments in args should have length 1:`event_description`"
  )
})


# Tests for generate_agenda_element_prompt() ---

test_that("generate_agenda_element_prompt() works with minimal args (character transcript)", {
  set_prompts(force = TRUE)
  transcript_csv <- "start,end,text,speaker\n0,10,Hello element,Speaker1"
  args <- list(
    event_description = NULL,
    vocabulary = NULL,
    diarization_instructions = NULL,
    expected_agenda_element = NULL
  )

  prompt <- generate_agenda_element_prompt(transcript_csv, args)
  expect_type(prompt, "character")
  expect_length(prompt, 1)
  expect_match(prompt, "This is a segment of the transcript", fixed = TRUE)
  expect_match(prompt, transcript_csv, fixed = TRUE)
  expect_no_match(prompt, get_prompts("vocabulary_template"), fixed = TRUE)
  expect_no_match(prompt, get_prompts("diarization_template"), fixed = TRUE)
  expect_no_match(prompt, "The event expected agenda is", fixed = TRUE)
})

test_that("generate_agenda_element_prompt() works with data.frame transcript", {
  set_prompts(force = TRUE)
  transcript_df <- data.frame(
    start = 0,
    end = 10,
    text = "Hello from df element",
    speaker = "Speaker2"
  )
  transcript_csv_expected <- readr::format_csv(transcript_df)
  args <- list(
    event_description = NULL,
    vocabulary = NULL,
    diarization_instructions = NULL,
    expected_agenda_element = NULL
  )

  prompt <- generate_agenda_element_prompt(transcript_df, args)
  expect_type(prompt, "character")
  expect_length(prompt, 1)
  expect_match(prompt, transcript_csv_expected, fixed = TRUE)
})

test_that("generate_agenda_element_prompt() includes all optional sections", {
  set_prompts(force = TRUE)
  transcript_csv <- "start,end,text,speaker\n0,10,Hello full element,SPK_A"
  args <- list(
    event_description = "An element event.",
    vocabulary = c("element_vocab", "term_elem"),
    diarization_instructions = "SPK_A is primary.\nNote other voices.",
    expected_agenda_element = "Element Title: Details"
  )

  prompt <- generate_agenda_element_prompt(transcript_csv, args)

  expect_match(
    prompt,
    "<event_description>",
    fixed = TRUE
  )
  expect_match(prompt, args$event_description, fixed = TRUE)

  expect_match(
    prompt,
    "wrongly transcribed",
    fixed = TRUE
  )
  expect_match(prompt, "- element_vocab", fixed = TRUE)
  expect_match(prompt, "- term_elem", fixed = TRUE)

  expect_match(
    prompt,
    "<speaker_recognition_instructions>",
    fixed = TRUE
  )
  expect_match(prompt, "SPK_A is primary.", fixed = TRUE)
  expect_match(prompt, "Note other voices.", fixed = TRUE)

  expect_match(
    prompt,
    "The event expected agenda is the following",
    fixed = TRUE
  )
  expect_match(prompt, args$expected_agenda_element, fixed = TRUE)
})

test_that("generate_agenda_element_prompt() aborts if args elements have length > 1 after processing", {
  set_prompts(force = TRUE)
  transcript_csv <- "start,end,text\n0,1,test_elem"
  args_bad <- list(
    event_description = c("desc_e1", "desc_e2"), # This should cause an error
    vocabulary = NULL,
    diarization_instructions = NULL,
    expected_agenda_element = NULL
  )

  expect_error(
    generate_agenda_element_prompt(transcript_csv, args_bad),
    "All arguments in args should have length 1:`event_description`"
  )
})

# Final newline
