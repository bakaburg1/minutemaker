# Tests for generate_recording_details() ---

test_that("returns NULL when all arguments are NULL", {
  expect_null(generate_recording_details())
  expect_null(generate_recording_details(
    type = NULL,
    session = NULL,
    title = NULL,
    description = NULL,
    speakers = NULL,
    moderators = NULL
  ))
})

test_that("uses agenda_element when provided", {
  sample_agenda_element <- list(
    type = "conference talk",
    session = "Morning Session",
    title = "The Future of AI",
    description = "A talk about AI.",
    speakers = c("John Doe", "Jane Smith"),
    moderators = "Alice Brown"
  )
  expected_output <- paste0(
    "- Type (the type of this specific talk/meeting): conference talk;\n",
    "- Session (the conference/larger meeting session in which this talk/meeting took place): Morning Session;\n",
    "- Title (the title of the talk/meeting): The Future of AI;\n",
    "- Description (a description of the talk/meeting topic): A talk about AI.;\n",
    "- Speakers (the main speakers of this talk/meeting): John Doe, Jane Smith;\n",
    "- Moderators (the moderators of this talk/meeting): Alice Brown;"
  )
  expect_identical(
    generate_recording_details(agenda_element = sample_agenda_element),
    expected_output
  )
})

test_that("uses individual arguments when provided", {
  expected_output <- paste0(
    "- Type (the type of this specific talk/meeting): workshop;\n",
    "- Session (the conference/larger meeting session in which this talk/meeting took place): Afternoon Session;\n",
    "- Title (the title of the talk/meeting): R Programming;\n",
    "- Description (a description of the talk/meeting topic): An R workshop.;\n",
    "- Speakers (the main speakers of this talk/meeting): Bob Green;\n",
    "- Moderators (the moderators of this talk/meeting): Carol White;"
  )
  expect_identical(
    generate_recording_details(
      type = "workshop",
      session = "Afternoon Session",
      title = "R Programming",
      description = "An R workshop.",
      speakers = "Bob Green",
      moderators = "Carol White"
    ),
    expected_output
  )
})

test_that("agenda_element takes precedence over individual arguments", {
  sample_agenda_element <- list(
    type = "panel discussion",
    session = "Evening Session",
    title = "Data Science Trends",
    description = "Discussion on trends.",
    speakers = c("Eve Black", "Frank Blue"),
    moderators = "Grace Red"
  )
  expected_output <- paste0(
    "- Type (the type of this specific talk/meeting): panel discussion;\n",
    "- Session (the conference/larger meeting session in which this talk/meeting took place): Evening Session;\n",
    "- Title (the title of the talk/meeting): Data Science Trends;\n",
    "- Description (a description of the talk/meeting topic): Discussion on trends.;\n",
    "- Speakers (the main speakers of this talk/meeting): Eve Black, Frank Blue;\n",
    "- Moderators (the moderators of this talk/meeting): Grace Red;"
  )
  expect_identical(
    generate_recording_details(
      agenda_element = sample_agenda_element,
      type = "lecture", # This should be ignored
      session = "Night Session", # This should be ignored
      title = "Old Tech", # This should be ignored
      description = "Old stuff.", # This should be ignored
      speakers = "Old Speaker", # This should be ignored
      moderators = "Old Mod" # This should be ignored
    ),
    expected_output
  )
})

test_that("handles single and multiple speakers/moderators correctly", {
  expected_output_single <- paste0(
    "- Speakers (the main speakers of this talk/meeting): Speaker One;\n",
    "- Moderators (the moderators of this talk/meeting): Moderator One;"
  )
  expect_identical(
    generate_recording_details(
      speakers = "Speaker One",
      moderators = "Moderator One"
    ),
    expected_output_single
  )

  expected_output_multiple <- paste0(
    "- Speakers (the main speakers of this talk/meeting): Speaker Alpha, Speaker Beta;\n",
    "- Moderators (the moderators of this talk/meeting): Moderator Alpha, Moderator Beta;"
  )
  expect_identical(
    generate_recording_details(
      speakers = c("Speaker Alpha", "Speaker Beta"),
      moderators = c("Moderator Alpha", "Moderator Beta")
    ),
    expected_output_multiple
  )
})

test_that("handles missing (NULL) parts within agenda_element", {
  sample_agenda_element <- list(
    type = "meeting",
    title = "Quick Sync",
    speakers = "Team Lead"
    # session, description, moderators are NULL
  )
  expected_output <- paste0(
    "- Type (the type of this specific talk/meeting): meeting;\n",
    "- Title (the title of the talk/meeting): Quick Sync;\n",
    "- Speakers (the main speakers of this talk/meeting): Team Lead;"
  )
  expect_identical(
    generate_recording_details(agenda_element = sample_agenda_element),
    expected_output
  )
})

test_that("handles missing (NULL) individual arguments", {
  expected_output <- paste0(
    "- Title (the title of the talk/meeting): Important Update;\n",
    "- Speakers (the main speakers of this talk/meeting): CEO;"
  )
  expect_identical(
    generate_recording_details(
      title = "Important Update",
      speakers = "CEO"
      # type, session, description, moderators are NULL
    ),
    expected_output
  )
})

test_that("output ends with a semicolon and not a newline if last element present", {
  # Case where 'moderators' is the last non-null element
  details_mod <- generate_recording_details(moderators = "Mod X")
  expect_true(stringr::str_ends(details_mod, ";"))

  # Case where 'speakers' is the last non-null, 'moderators' is NULL
  details_spk <- generate_recording_details(
    speakers = "Speaker Y",
    moderators = NULL
  )
  expect_true(stringr::str_ends(details_spk, ";"))

  # Case where 'title' is the last non-null
  details_title <- generate_recording_details(title = "A Title")
  expect_true(stringr::str_ends(details_title, ";"))
})

test_that("output elements are separated by newline correctly", {
  # Test with a few elements to check newline separation
  expected_output <- paste0(
    "- Type (the type of this specific talk/meeting): type1;\n",
    "- Title (the title of the talk/meeting): title1;"
  )
  expect_identical(
    generate_recording_details(type = "type1", title = "title1"),
    expected_output
  )

  expected_output_rev <- paste0(
    "- Title (the title of the talk/meeting): title2;\n",
    "- Speakers (the main speakers of this talk/meeting): speaker2;"
  )
  expect_identical(
    generate_recording_details(title = "title2", speakers = "speaker2"),
    expected_output_rev
  )
})

# Tests for summarise_transcript() ---

# Helper functions and data ---

# Mock llmR::prompt_llm
# This function will be called by summarise_transcript
# It needs to return a character string (the summary)
mock_prompt_llm <- function(prompt, model = "mock_model", ...) {
  # llmR::prompt_llm can accept a raw string for `prompt` or a pre-formatted
  # list.
  # If it's a string, it internally calls prepare_chat_completions.
  # This mock needs to simulate that to correctly get prepared_prompt_set.

  prepared_prompt_set <- if (is.character(prompt)) {
    # Simplified simulation of llmR::prepare_chat_completions
    # Uses the persona from mock_get_prompts if available, or a default.
    persona_prompt <- tryCatch(
      mock_get_prompts("persona"),
      error = function(e) "You are a helpful assistant."
    )
    list(
      model = model,
      messages = list(
        list(role = "system", content = persona_prompt),
        list(role = "user", content = prompt)
      )
      # Add other elements like temperature, max_tokens if SUT passes them and mock needs to see them
    )
  } else {
    prompt # Assume it's already a prepared list (output of prepare_chat_completions)
  }

  # Now, the rest of the logic uses prepared_prompt_set
  last_user_message_content <- "" # Default to empty string
  if (
    !is.null(prepared_prompt_set$messages) &&
      length(prepared_prompt_set$messages) > 0
  ) {
    user_messages <- Filter(
      function(m) m$role == "user",
      prepared_prompt_set$messages
    )
    if (length(user_messages) > 0) {
      # If there are any user messages
      # Get content of the LAST user message
      last_user_msg_obj <- user_messages[[length(user_messages)]]
      if (!is.null(last_user_msg_obj$content)) {
        last_user_message_content <- last_user_msg_obj$content
      }
    }
  }
  # Ensure last_user_message_content is a single string
  last_user_message_content <- paste(last_user_message_content, collapse = "\n")
  # last_user_message_content is now guaranteed to be a single string (possibly "")

  aggregation_trigger_text <- mock_get_prompts("output_rolling_aggregation")
  is_aggregation_call <- any(stringr::str_detect(
    last_user_message_content,
    aggregation_trigger_text
  ))

  if (is_aggregation_call) {
    return("Aggregated summary from mock_prompt_llm")
  } else {
    if (is.null(last_user_message_content) || last_user_message_content == "") {
      return("Mock summary for empty or NULL input")
    } else {
      return(paste(
        "Mock summary for input:",
        substr(last_user_message_content, 1, 100)
      ))
    }
  }
}

# Mock get_prompts
# This function is used to fetch various prompt templates
mock_get_prompts <- function(prompt_name) {
  prompts <- list(
    # Standard prompts that might be called directly by SUT or other mocks
    persona = "You are a helpful assistant.",
    summary_structure = "Structure: Key points, Action items, Detailed summary.",
    output_summarisation = "Please provide a concise summary.",
    output_rolling_aggregation = "Please aggregate these summaries into one.",
    aggregate_task_rolling = "Your task is to aggregate the summaries generated from the segments of a meeting",
    aggregate_template_rolling = "Here are the segment summaries to aggregate:",
    diarization_template = "Take into account that multiple speakers",
    base_task = "You are an AI assistant. Your task is to summarise the provided text.",
    transcript_template = "Transcript: {transcript}",
    summarisation_template = paste(
      "Task: {base_task}",
      "Transcript context: {transcript_template}",
      "Event details: {event_description_prompt}",
      "Recording details: {recording_details_prompt}",
      "Vocabulary hints: {vocab_prompt}",
      "Audience: {audience_prompt}",
      "Desired output structure: {summary_structure}",
      "Output instructions: {output_summarisation}",
      sep = "\n"
    ),
    output_template = "Dummy output template used only inside unit tests.",

    # Prompts used by the generate_summarisation_prompt path
    event_description_prompt = "Event description: {event_description}",
    recording_details_prompt = "Recording details: {recording_details}",
    vocab_prompt = "Vocabulary: {vocabulary}",
    audience_prompt = "Audience: {audience}",
    # simple_bullets, simple_diarised_bullets etc. are more like templates or instructions passed as args,
    # not separate full prompts fetched by get_prompts. Let's remove them from here if they are part of summary_structure arg.
    # Re-checking usage, some might be fetched if summary_structure is complex.
    # For now, keeping the ones that were there from previous edits.
    simple_bullets = "Use bullets.",
    simple_diarised_bullets = "Use bullets and note speakers.",
    default_diarisation_instructions = "Default dia instructions.",
    default_output_instructions = "Default output instructions.",

    # Prompts used by the generate_rolling_aggregation_prompt path
    aggregate_bullets = "Aggregate with bullets.",
    aggregate_diarised_bullets = "Aggregate with bullets and note speakers.",

    # Added to fix test errors
    audience_template = "Audience: {audience}" # Placeholder
  )

  # Check if all requested prompt names exist
  unknown_prompts <- setdiff(prompt_name, names(prompts))
  if (length(unknown_prompts) > 0) {
    stop(paste(
      "mock_get_prompts called for unmocked prompt(s):",
      paste(unknown_prompts, collapse = ", ")
    ))
  }

  # Return single prompt string or list of prompt strings
  if (length(prompt_name) == 1) {
    return(prompts[[prompt_name]])
  } else {
    return(prompts[prompt_name])
  }
}

# Mock set_prompts to do nothing as it interacts with options
mock_set_prompts <- function() {
  invisible(NULL)
}

# Mock extract_text_from_transcript
mock_extract_text_from_transcript <- function(
  transcript_df,
  import_diarization
) {
  # Fallback: if the expected 'text' column is missing, return empty.
  if (!"text" %in% names(transcript_df)) {
    return("")
  }

  # ------------------------------------------------------------------
  # 1. Build each line
  # ------------------------------------------------------------------
  if (
    isTRUE(import_diarization) &&
      "speaker" %in% names(transcript_df) &&
      any(!is.na(transcript_df$speaker))
  ) {
    # With diarization: keep the speaker label
    line_vec <- paste0(
      "Speaker: ",
      transcript_df$speaker,
      "\n",
      transcript_df$text
    )
  } else {
    # Without diarization: just the raw text
    line_vec <- transcript_df$text
  }

  # ------------------------------------------------------------------
  # 2. Collapse into the final transcript string
  #    Two newlines between blocks mirrors the real helper.
  # ------------------------------------------------------------------
  paste(line_vec, collapse = "\n\n")
}

# Sample data for summarise_transcript tests
sample_transcript_char <- "Hello world. This is a test. Another sentence here."
sample_transcript_df_for_summarisation <- data.frame(
  text = c(
    "Hello world.",
    "This is a test.",
    "Another sentence here.",
    "And one more."
  ),
  start = c(0, 10, 50, 60),
  end = c(5, 15, 55, 65),
  speaker = c("S1", "S2", "S1", "S2") # Ensured speaker column is present
)

# For rolling window, needs to be longer than 1.5 * window_size (15 min default)
# window_size = 15 min = 900 seconds. 1.5 * 900 = 1350 seconds
sample_transcript_df_long <- data.frame(
  start = seq(0, 1500, by = 30),
  end = seq(29, 1529, by = 30),
  text = paste("Segment number", 1:(1500 / 30 + 1)),
  speaker = rep(c("SpeakerA", "SpeakerB"), length.out = (1500 / 30 + 1))
)

# Test cases start here

test_that("summarise_transcript simple method works with character input", {
  withr::with_options(list(minutemaker_prompts = list()), {
    testthat::local_mocked_bindings(
      prompt_llm = mock_prompt_llm,
      .package = "llmR"
    )
    testthat::local_mocked_bindings(
      set_prompts = mock_set_prompts,
      get_prompts = mock_get_prompts,
      extract_text_from_transcript = mock_extract_text_from_transcript,
      .package = "minutemaker"
    )

    summary_result <- summarise_transcript(
      transcript_data = sample_transcript_char,
      method = "simple"
    )
    expect_type(summary_result, "character")
    expect_true(startsWith(summary_result, "Mock summary for input:"))
  })
})

test_that("summarise_transcript simple method works with data.frame input", {
  withr::with_options(list(minutemaker_prompts = list()), {
    testthat::local_mocked_bindings(
      prompt_llm = mock_prompt_llm,
      .package = "llmR"
    )
    testthat::local_mocked_bindings(
      set_prompts = mock_set_prompts,
      get_prompts = mock_get_prompts,
      extract_text_from_transcript = mock_extract_text_from_transcript,
      .package = "minutemaker"
    )

    summary_result <- summarise_transcript(
      transcript_data = sample_transcript_df_for_summarisation,
      method = "simple"
    )
    expect_type(summary_result, "character")
    expect_true(startsWith(summary_result, "Mock summary for input:"))
  })
})

test_that("prompt_only = TRUE returns prompt for simple method", {
  withr::with_options(list(minutemaker_prompts = list()), {
    testthat::local_mocked_bindings(
      set_prompts = mock_set_prompts,
      get_prompts = mock_get_prompts,
      extract_text_from_transcript = mock_extract_text_from_transcript,
      .package = "minutemaker"
    )
    # No llmR mocks needed here as prompt_only = TRUE

    prompt <- summarise_transcript(
      transcript_data = sample_transcript_char,
      method = "simple",
      prompt_only = TRUE
    )
    expect_type(prompt, "character")
    expect_match(
      prompt,
      sample_transcript_char,
      fixed = TRUE
    )
  })
})

test_that("consider_diarization influences transcript processing", {
  # Spy on extract_text_from_transcript to check its arguments
  extract_calls <- list()
  spy_extract_text_from_transcript <- function(
    transcript_df,
    import_diarization
  ) {
    extract_calls <<- append(
      extract_calls,
      list(list(
        df_names = names(transcript_df),
        import_diarization = import_diarization
      ))
    )
    return(mock_extract_text_from_transcript(transcript_df, import_diarization))
  }

  withr::with_options(list(minutemaker_prompts = list()), {
    testthat::local_mocked_bindings(
      prompt_llm = mock_prompt_llm,
      .package = "llmR"
    )
    testthat::local_mocked_bindings(
      set_prompts = mock_set_prompts,
      get_prompts = mock_get_prompts,
      extract_text_from_transcript = spy_extract_text_from_transcript,
      .package = "minutemaker"
    )

    # Test with consider_diarization = TRUE (default)
    summarise_transcript(
      transcript_data = sample_transcript_df_for_summarisation,
      method = "simple",
      consider_diarization = TRUE
    )
    expect_length(extract_calls, 1)
    expect_true(extract_calls[[1]]$import_diarization)

    # Reset spy for next call
    extract_calls <- list()
    summarise_transcript(
      transcript_data = sample_transcript_df_for_summarisation,
      method = "simple",
      consider_diarization = FALSE
    )
    expect_length(extract_calls, 1)
    expect_false(extract_calls[[1]]$import_diarization)
  })
})

test_that("summarise_transcript rolling method works with long data.frame", {
  # Capture calls to prompt_llm to verify number of segments and aggregation
  llm_calls <- list()
  capturing_mock_prompt_llm <- function(prompt_set, ...) {
    llm_calls <<- append(llm_calls, list(prompt_set = prompt_set))

    # Return different values for segment summaries vs aggregation summary
    if (
      is.character(prompt_set["user"]) &&
        startsWith(prompt_set["user"], "USER:")
    ) {
      # Heuristic for segment prompt
      return(paste(
        "Mock segment summary for:",
        substr(prompt_set["user"], 1, 30)
      ))
    } else if (
      is.character(prompt_set["user"]) &&
        length(prompt_set["user"]) == 1 &&
        (startsWith(prompt_set["user"], "Based on the following summaries") ||
          startsWith(
            prompt_set["user"],
            "Your task is to aggregate the summaries"
          ) ||
          grepl("<segment_summary_\\d+>", prompt_set["user"]))
    ) {
      return("Aggregated summary from rolling mock")
    }
    return("Generic mock LLM response") # Fallback
  }

  # Mocking cli_alert to avoid printing to console during tests
  mock_cli_alert <- function(...) {
    invisible(NULL)
  }

  withr::with_options(list(minutemaker_prompts = list()), {
    testthat::local_mocked_bindings(
      prompt_llm = capturing_mock_prompt_llm,
      .package = "llmR"
    )
    testthat::local_mocked_bindings(
      set_prompts = mock_set_prompts,
      get_prompts = mock_get_prompts,
      extract_text_from_transcript = mock_extract_text_from_transcript,
      .package = "minutemaker"
    )
    testthat::local_mocked_bindings(
      cli_alert = mock_cli_alert,
      .package = "cli"
    )

    summary_result <- summarise_transcript(
      transcript_data = sample_transcript_df_long, # long enough for 2 segments
      method = "rolling",
      window_size = 10 # 10 mins = 600s. sample_transcript_df_long is 1500s long
    )

    # sample_transcript_df_long duration: max(start) - min(start) = 1500 - 0 = 1500s
    # window_size = 10 min = 600s
    # Expected breakpoints: 0, 600, 1200, 1500 (max start)
    # Segments: [0, 600), [600, 1200), [1200, 1500+epsilon)
    # So, 3 segments for summarization, then 1 for aggregation.
    expect_length(llm_calls, 3 + 1) # 3 segment summaries + 1 aggregation
    expect_identical(summary_result, "Aggregated summary from rolling mock")
  })
})


test_that("rolling method reverts to simple if transcript too short", {
  warn_env <- rlang::current_env()
  warn_env$warn_list <- c()

  # Mocking cli_warn to capture warnings
  mock_cli_warn <- function(message, ...) {
    warn_env$warn_list <- c(warn_env$warn_list, paste(message, collapse = " "))
    invisible(NULL)
  }

  withr::with_options(list(minutemaker_prompts = list()), {
    testthat::local_mocked_bindings(
      prompt_llm = mock_prompt_llm,
      .package = "llmR"
    )
    testthat::local_mocked_bindings(
      set_prompts = mock_set_prompts,
      get_prompts = mock_get_prompts,
      extract_text_from_transcript = mock_extract_text_from_transcript,
      .package = "minutemaker"
    )
    testthat::local_mocked_bindings(cli_warn = mock_cli_warn, .package = "cli")

    summary_result <- summarise_transcript(
      transcript_data = sample_transcript_df_for_summarisation, # Too short for rolling
      method = "rolling",
      window_size = 15 # Default
    )

    expect_match(
      warn_env$warn_list,
      "Transcript too short",
      fixed = TRUE
    )

    # Check that it behaved like 'simple' method
    expect_match(summary_result, "^Mock summary for input:")
  })
})

test_that("prompt_only = TRUE returns list of prompts for rolling method", {
  # Mocking cli_alert to avoid printing to console during tests
  mock_cli_alert <- function(...) {
    invisible(NULL)
  }

  withr::with_options(list(minutemaker_prompts = list()), {
    testthat::local_mocked_bindings(
      set_prompts = mock_set_prompts,
      get_prompts = mock_get_prompts,
      extract_text_from_transcript = mock_extract_text_from_transcript,
      .package = "minutemaker"
    )
    testthat::local_mocked_bindings(
      cli_alert = mock_cli_alert,
      .package = "cli"
    )

    prompts_list <- summarise_transcript(
      transcript_data = sample_transcript_df_long,
      method = "rolling",
      window_size = 10, # Ensure it splits
      prompt_only = TRUE
    )
    # Expected 3 segments based on sample_transcript_df_long and window_size = 10 min (600s)
    # 1500s total / 600s window = 2.5 -> 3 segments (0-600, 600-1200, 1200-1500)
    expect_type(prompts_list, "character") # purrr::map_chr returns a character vector
    expect_length(prompts_list, 3)

    expect_type(prompts_list, "character")
    expect_true(stringr::str_detect(prompts_list[1], "Segment number 1"))
  })
})

test_that("summarise_transcript error handling for invalid inputs", {
  # Restore original expect_error calls and apply withr::with_options to the one causing summary_structure issue.

  # Invalid transcript_data for rolling method (data frame without 'from')
  # This was the one that triggered the get_prompts("summary_structure") error path.
  invalid_df_rolling <- data.frame(text = "test", some_other_col = 10)
  withr::with_options(
    list(
      minutemaker_prompts = list(
        summary_structure = "Mocked summary structure for this specific error test."
      )
    ),
    {
      expect_error(
        summarise_transcript(invalid_df_rolling, method = "rolling"),
        regexp = "Invalid input for rolling window method"
      )
    }
  )

  # Test invalid transcript_data (e.g., NULL, numeric) - these were added by the model and are good to keep.
  expect_error(
    summarise_transcript(NULL),
    "`transcript_data` must be a character vector or a data frame."
  )
  expect_error(
    summarise_transcript(123),
    "`transcript_data` must be a character vector or a data frame."
  )

  # Test invalid method - also good to keep.
  # Using fixed = TRUE as it is an exact string match.
  expect_error(
    summarise_transcript("text", method = "invalid_method"),
    "'arg' should be one of \"simple\", \"rolling\"",
    fixed = TRUE
  )

  # Test invalid output_length - also good to keep.
  expect_error(
    summarise_transcript(
      sample_transcript_df_for_summarisation,
      method = "rolling",
      output_length = -1
    ),
    "`output_length` must be a positive integer."
  )

  # Original tests for simple method invalid input (if different from above generic character/df check)
  expect_error(
    summarise_transcript(list("text1", "text2"), method = "simple"),
    regexp = "`transcript_data` to be a single data frame or a single character string"
  )
  expect_error(
    summarise_transcript(c("text1", "text2"), method = "simple"),
    regexp = "Invalid input for simple summarisation method.*single data frame or a single character string"
  )
})

test_that("arguments are correctly passed to generate_summarisation_prompt", {
  # Spy on generate_summarisation_prompt
  gsp_calls <- list()
  spy_generate_summarisation_prompt <- function(transcript, args) {
    gsp_calls <<- append(
      gsp_calls,
      list(list(transcript = transcript, args = args))
    )
    return(paste0(
      "System: Persona\\nUser: Summarize: ",
      transcript,
      "\\nEvent: ",
      args$event_description,
      "\\nAudience: ",
      args$audience
    ))
  }

  withr::with_options(list(minutemaker_prompts = list()), {
    testthat::local_mocked_bindings(
      set_prompts = mock_set_prompts,
      get_prompts = mock_get_prompts,
      extract_text_from_transcript = mock_extract_text_from_transcript,
      generate_summarisation_prompt = spy_generate_summarisation_prompt,
      .package = "minutemaker"
    )

    custom_event_desc <- "A special event."
    custom_audience <- "Experts only."
    custom_vocab <- c("term1", "term2")
    custom_sum_struct <- "Abstract and Details."
    custom_output_len <- 5
    custom_extra_diar_instr <- "Ignore speakers."
    custom_extra_out_instr <- "Be very formal."

    # Test with simple method
    summarise_transcript(
      transcript_data = sample_transcript_char,
      method = "simple",
      event_description = custom_event_desc,
      audience = custom_audience,
      vocabulary = custom_vocab,
      summary_structure = custom_sum_struct,
      output_length = custom_output_len,
      extra_diarization_instructions = custom_extra_diar_instr,
      extra_output_instructions = custom_extra_out_instr,
      prompt_only = TRUE # So we inspect the prompt args, not call LLM
    )

    expect_length(gsp_calls, 1)
    called_args <- gsp_calls[[1]]$args
    expect_identical(called_args$event_description, custom_event_desc)
    expect_identical(called_args$audience, custom_audience)
    expect_identical(called_args$vocabulary, custom_vocab)
    expect_identical(called_args$summary_structure, custom_sum_struct)
    expect_identical(called_args$output_length, custom_output_len)
    expect_identical(
      called_args$extra_diarization_instructions,
      custom_extra_diar_instr
    )
    expect_identical(
      called_args$extra_output_instructions,
      custom_extra_out_instr
    )
  })
})

test_that("arguments are correctly passed for rolling method (to aggregation)", {
  # Spy on generate_rolling_aggregation_prompt
  grap_calls <- list()
  spy_generate_rolling_aggregation_prompt <- function(summaries, args) {
    grap_calls <<- append(
      grap_calls,
      list(list(summaries = summaries, args = args))
    )
    return("Dummy aggregation prompt")
  }

  # Mocking cli_alert to avoid printing to console during tests
  mock_cli_alert <- function(...) {
    invisible(NULL)
  }

  withr::with_options(list(minutemaker_prompts = list()), {
    testthat::local_mocked_bindings(
      prompt_llm = mock_prompt_llm,
      .package = "llmR"
    )
    testthat::local_mocked_bindings(
      set_prompts = mock_set_prompts,
      get_prompts = mock_get_prompts, # Used by various internal prompt generation
      extract_text_from_transcript = mock_extract_text_from_transcript,
      generate_summarisation_prompt = function(transcript, args) {
        paste("Prompt for:", transcript)
      },
      generate_rolling_aggregation_prompt = spy_generate_rolling_aggregation_prompt,
      .package = "minutemaker"
    )
    testthat::local_mocked_bindings(
      cli_alert = mock_cli_alert,
      .package = "cli"
    )

    custom_event_desc <- "A rolling event."
    custom_audience <- "General audience."
    custom_sum_struct <- "Key Takeaways."
    custom_output_len <- 2
    custom_extra_out_instr <- "Use bullet points."

    summarise_transcript(
      transcript_data = sample_transcript_df_long,
      method = "rolling",
      window_size = 10, # Ensure splitting
      event_description = custom_event_desc,
      audience = custom_audience,
      summary_structure = custom_sum_struct,
      output_length = custom_output_len,
      extra_output_instructions = custom_extra_out_instr
      # Not testing vocab or extra_diar_instr for aggregation prompt pass-through here
      # as they are not used by generate_rolling_aggregation_prompt
    )

    expect_length(grap_calls, 1)
    called_args_agg <- grap_calls[[1]]$args
    expect_identical(called_args_agg$event_description, custom_event_desc)
    expect_identical(called_args_agg$audience, custom_audience)
    expect_identical(called_args_agg$summary_structure, custom_sum_struct)
    expect_identical(called_args_agg$output_length, custom_output_len)
    expect_identical(
      called_args_agg$extra_output_instructions,
      custom_extra_out_instr
    )
  })
})

# Tests for summarise_full_meeting() ---

# Helper data for summarise_full_meeting tests
sample_agenda_list <- list(
  list(
    session = "Session 1",
    title = "Talk A",
    type = "Talk",
    from = 0,
    to = 100,
    speakers = "Speaker A",
    moderators = "Mod A",
    description = "Desc A"
  ),
  list(
    session = "Session 1",
    title = "Talk B",
    type = "Presentation",
    from = 101,
    to = 200,
    speakers = "Speaker B",
    moderators = "Mod B",
    description = "Desc B"
  ),
  list(
    # This one might be skipped by validation or empty transcript
    session = "Session 2",
    title = "Talk C",
    type = "Workshop",
    from = 201,
    to = 300,
    speakers = "Speaker C",
    moderators = "Mod C",
    description = "Desc C"
  )
)

sample_transcript_for_meeting <- data.frame(
  start = c(0, 10, 50, 101, 110, 150, 205), # Times in seconds
  end = c(9, 49, 99, 109, 149, 199, 210),
  text = paste("Meeting segment", 1:7),
  speaker = rep(c("P1", "P2"), length.out = 7)
)

# Mocks for summarise_full_meeting dependencies
mock_summarise_transcript_meeting <- function(
  transcript_data,
  recording_details,
  ...
) {
  return(paste(
    "Summary for:",
    gsub(".*Title:(.*?);.*", "\\1", recording_details)
  ))
}

mock_validate_agenda <- function(agenda) TRUE
mock_validate_agenda_element <- function(elem, from = TRUE, to = TRUE) {
  if (!is.null(elem$title) && elem$title == "Talk C") {
    return(FALSE)
  }
  return(TRUE)
}

mock_convert_agenda_times <- function(agenda, convert_to, event_start_time) {
  return(agenda)
}

mock_generate_recording_details <- function(agenda_element) {
  # Use paste0 to avoid extra spaces that cause issues with gsub extraction later
  paste0("Title:", agenda_element$title, ";Session:", agenda_element$session)
}

mock_build_ids_from_agenda <- function(agenda) {
  sapply(agenda, function(x) paste0(x$session, " - ", x$title))
}

test_that("summarise_full_meeting basic workflow, new output file", {
  withr::with_tempdir({
    work_dir <- getwd()
    temp_output_file_path <- file.path(work_dir, "test_summary_output.R")

    # Spy on summarise_transcript
    st_calls <- list()
    spy_summarise_transcript <- function(
      transcript_data,
      method,
      window_size,
      output_length,
      event_description,
      recording_details,
      vocabulary,
      audience,
      consider_diarization,
      summary_structure,
      extra_diarization_instructions,
      extra_output_instructions,
      ...
    ) {
      call_args <- list(
        transcript_nrow = nrow(transcript_data),
        recording_details = recording_details
      )
      st_calls <<- append(st_calls, list(call_args))
      return(mock_summarise_transcript_meeting(
        transcript_data,
        recording_details
      ))
    }

    # We are not mocking dput, file.exists, or dget (for the output file) anymore.
    # We are also not mocking cli_inform or cli_warn.
    testthat::local_mocked_bindings(
      summarise_transcript = spy_summarise_transcript,
      validate_agenda = mock_validate_agenda,
      validate_agenda_element = mock_validate_agenda_element, # Talk C will be invalid
      convert_agenda_times = mock_convert_agenda_times,
      generate_recording_details = mock_generate_recording_details,
      build_ids_from_agenda = mock_build_ids_from_agenda,
      # dget will use the real dget for reading the output,
      # and mock_dget_agenda_input for agenda files (if that test needs it)
      .package = "minutemaker"
    )
    # No base R mocks for dput, file.exists, dget here for output file handling
    # No cli package mocks here
    testthat::local_mocked_bindings(
      # styler package functions
      style_file = function(path, ...) invisible(NULL), # Mock styler to do nothing
      .package = "styler"
    )

    expect_warning(
      result_tree <- summarise_full_meeting(
        transcript_data = sample_transcript_for_meeting,
        agenda = sample_agenda_list,
        output_file = temp_output_file_path # Use the path in tempdir
      ),
      '"Session 2 - Talk C" is not valid. Skipping'
    )

    # Talk A and Talk B should be processed. Talk C is marked invalid by mock_validate_agenda_element.
    expect_length(st_calls, 2)
    expect_equal(st_calls[[1]]$transcript_nrow, 3)
    expect_true(stringr::str_detect(st_calls[[1]]$recording_details, "Talk A"))
    expect_equal(st_calls[[2]]$transcript_nrow, 3)
    expect_true(stringr::str_detect(st_calls[[2]]$recording_details, "Talk B"))

    # Check the actual written file
    expect_true(file.exists(temp_output_file_path))
    final_output_from_file <- dget(temp_output_file_path)

    expect_length(final_output_from_file, 2)
    expect_named(
      final_output_from_file,
      c("Session 1 - Talk A", "Session 1 - Talk B")
    )
    expect_identical(
      final_output_from_file[["Session 1 - Talk A"]]$summary,
      "Summary for: Talk A"
    )
    expect_identical(
      final_output_from_file[["Session 1 - Talk B"]]$summary,
      "Summary for: Talk B"
    )
    expect_identical(result_tree, final_output_from_file)
  })
})

test_that("summarise_full_meeting overwrite=FALSE skips existing talks", {
  withr::with_tempdir({
    work_dir <- getwd()
    temp_output_file_path <- file.path(
      work_dir,
      "test_summary_overwrite_false.R"
    )

    # Simulate an existing output file with Talk A already summarized
    # by actually writing it to the temp directory.
    existing_summary_content <- list(
      `Session 1 - Talk A` = list(
        session = "Session 1",
        title = "Talk A",
        type = "Talk",
        from = 0,
        to = 100,
        speakers = "Speaker A",
        moderators = "Mod A",
        description = "Desc A",
        summary = "Existing summary for Talk A"
      )
    )
    # Need to use the real dput to create this file for the test setup.
    # No, dput() is for R objects. The file should contain R code that produces the list.
    # So, we manually create a string that represents the R code and write it.
    # For simplicity in test setup, let's assume the SUT expects a file readable by dget().
    # So we can use dput() to create this file in the temp dir for setup.
    dput(existing_summary_content, file = temp_output_file_path)

    st_calls <- list()
    spy_summarise_transcript_overwrite <- function(
      transcript_data,
      recording_details,
      ...
    ) {
      call_args <- list(
        title = gsub(".*Title:(.*?);.*", "\\1", recording_details)
      )
      st_calls <<- append(st_calls, list(call_args))
      return(paste("New summary for:", call_args$title))
    }

    # Ensure Talk C is considered valid for this test to check skipping of A and processing of B & C
    mock_validate_agenda_element_all_valid <- function(elem, ...) TRUE

    testthat::local_mocked_bindings(
      summarise_transcript = spy_summarise_transcript_overwrite,
      validate_agenda = mock_validate_agenda,
      validate_agenda_element = mock_validate_agenda_element_all_valid,
      convert_agenda_times = mock_convert_agenda_times,
      generate_recording_details = mock_generate_recording_details,
      build_ids_from_agenda = mock_build_ids_from_agenda,
      .package = "minutemaker"
    )
    # No base R mocks for dput, file.exists, dget for output file handling
    # No cli mocks
    testthat::local_mocked_bindings(
      style_file = function(path, ...) invisible(NULL),
      .package = "styler"
    )

    result_tree <- summarise_full_meeting(
      transcript_data = sample_transcript_for_meeting,
      agenda = sample_agenda_list, # Contains Talk A, B, C
      output_file = temp_output_file_path,
      overwrite = FALSE
    )

    # summarise_transcript should only be called for Talk B and Talk C
    expect_length(st_calls, 2)
    processed_titles <- sapply(st_calls, function(x) x$title)
    expect_false("Talk A" %in% processed_titles)
    expect_true("Talk B" %in% processed_titles)
    expect_true("Talk C" %in% processed_titles)

    # Check the actual written file
    final_output_from_file <- dget(temp_output_file_path)
    expect_length(final_output_from_file, 3)
    expect_identical(
      final_output_from_file[["Session 1 - Talk A"]]$summary,
      "Existing summary for Talk A"
    )
    expect_identical(
      final_output_from_file[["Session 1 - Talk B"]]$summary,
      "New summary for: Talk B"
    )
    expect_identical(
      final_output_from_file[["Session 2 - Talk C"]]$summary,
      "New summary for: Talk C"
    )
    expect_identical(result_tree, final_output_from_file)
  })
})

test_that("summarise_full_meeting overwrite=TRUE re-summarizes existing talks", {
  withr::with_tempdir({
    work_dir <- getwd()
    temp_output_file_path <- file.path(
      work_dir,
      "test_summary_overwrite_true.R"
    )

    # Simulate an existing output file, which should be overwritten.
    existing_summary_content <- list(
      `Session 1 - Talk A` = list(
        session = "Session 1",
        title = "Talk A",
        type = "Talk",
        from = 0,
        to = 100,
        summary = "Old summary for Talk A"
      )
    )
    dput(existing_summary_content, file = temp_output_file_path)

    st_calls <- list()
    spy_summarise_transcript_overwrite_true <- function(
      transcript_data,
      recording_details,
      ...
    ) {
      call_args <- list(
        title = gsub(".*Title:(.*?);.*", "\\1", recording_details)
      )
      st_calls <<- append(st_calls, list(call_args))
      return(paste("Fresh summary for:", call_args$title))
    }

    # Make Talk C valid for this test
    mock_validate_agenda_element_all_valid <- function(elem, ...) TRUE

    testthat::local_mocked_bindings(
      summarise_transcript = spy_summarise_transcript_overwrite_true,
      validate_agenda = mock_validate_agenda,
      validate_agenda_element = mock_validate_agenda_element_all_valid,
      convert_agenda_times = mock_convert_agenda_times,
      generate_recording_details = mock_generate_recording_details,
      build_ids_from_agenda = mock_build_ids_from_agenda,
      .package = "minutemaker"
    )
    testthat::local_mocked_bindings(
      style_file = function(path, ...) invisible(NULL),
      .package = "styler"
    )

    result_tree <- summarise_full_meeting(
      transcript_data = sample_transcript_for_meeting,
      agenda = sample_agenda_list, # Talk A, B, C
      output_file = temp_output_file_path,
      overwrite = TRUE
    )

    # summarise_transcript should be called for all 3 talks
    expect_length(st_calls, 3)
    processed_titles <- sapply(st_calls, function(x) x$title)
    expect_true(all(c("Talk A", "Talk B", "Talk C") %in% processed_titles))

    # Check the actual written file
    final_output_from_file <- dget(temp_output_file_path)
    expect_length(final_output_from_file, 3)
    expect_identical(
      final_output_from_file[["Session 1 - Talk A"]]$summary,
      "Fresh summary for: Talk A"
    )
    expect_identical(
      final_output_from_file[["Session 1 - Talk B"]]$summary,
      "Fresh summary for: Talk B"
    )
    expect_identical(
      final_output_from_file[["Session 2 - Talk C"]]$summary,
      "Fresh summary for: Talk C"
    )
    expect_identical(result_tree, final_output_from_file)
  })
})

test_that("summarise_full_meeting reads agenda from file path", {
  withr::with_tempdir({
    work_dir <- getwd()
    temp_agenda_file_path <- file.path(work_dir, "test_agenda_input.R")
    # Create the agenda file in the temp directory
    dput(sample_agenda_list, file = temp_agenda_file_path)

    temp_output_file_path <- file.path(
      work_dir,
      "test_summary_agenda_from_file.R"
    )

    st_calls <- list()
    spy_summarise_transcript_agenda_file <- function(
      transcript_data,
      recording_details,
      ...
    ) {
      title_extracted <- gsub(".*Title:(.*?);.*", "\\1", recording_details)
      st_calls <<- append(
        st_calls,
        list(list(title = title_extracted, details = recording_details))
      )
      return(paste("Summary for file agenda:", title_extracted))
    }

    # Using the original mock_validate_agenda_element where Talk C is invalid

    testthat::local_mocked_bindings(
      summarise_transcript = spy_summarise_transcript_agenda_file,
      validate_agenda = mock_validate_agenda,
      validate_agenda_element = mock_validate_agenda_element,
      convert_agenda_times = mock_convert_agenda_times,
      generate_recording_details = mock_generate_recording_details,
      build_ids_from_agenda = mock_build_ids_from_agenda,
      .package = "minutemaker"
    )
    # No base R mocks for dput, file.exists, dget needed.
    # No cli mocks needed.
    testthat::local_mocked_bindings(
      style_file = function(path, ...) invisible(NULL),
      .package = "styler"
    )

    expect_warning(
      result_tree <- summarise_full_meeting(
        transcript_data = sample_transcript_for_meeting,
        agenda = temp_agenda_file_path, # Pass the file path here
        output_file = temp_output_file_path
      ),
      '"Session 2 - Talk C" is not valid. Skipping'
    )

    # Expect Talk A and B to be processed, Talk C invalid by mock_validate_agenda_element
    expect_length(st_calls, 2)
    processed_titles <- sapply(st_calls, function(x) x$title)
    expect_true("Talk A" %in% processed_titles)
    expect_true("Talk B" %in% processed_titles)

    # Check the actual written output file
    final_output_from_file <- dget(temp_output_file_path)
    expect_length(final_output_from_file, 2)
    expect_identical(
      final_output_from_file[["Session 1 - Talk A"]]$summary,
      "Summary for file agenda: Talk A"
    )
    expect_identical(result_tree, final_output_from_file)
  })
})

test_that("summarise_full_meeting skips item with empty transcript subset and warns", {
  withr::with_tempdir({
    work_dir <- getwd()
    temp_output_file_path <- file.path(work_dir, "test_summary_empty_subset.R")

    agenda_for_empty_subset <- list(
      sample_agenda_list[[1]],
      list(
        session = "Session 1",
        title = "Talk B Empty",
        type = "Talk",
        from = 500,
        to = 600,
        speakers = "S",
        moderators = "M",
        description = "D"
      )
    )
    custom_build_ids <- function(agenda) {
      sapply(agenda, function(x) paste0(x$session, " - ", x$title))
    }

    st_calls_empty <- list()
    spy_summarise_transcript_empty <- function(
      transcript_data,
      recording_details,
      ...
    ) {
      title_extracted <- gsub(".*Title:(.*?);.*", "\\1", recording_details)
      st_calls_empty <<- append(
        st_calls_empty,
        list(list(title = title_extracted, details = recording_details))
      )
      return(paste("Summary for:", title_extracted))
    }

    mock_validate_agenda_element_all_valid <- function(elem, ...) TRUE

    # Mock dget for this test to simulate reading an *output file* if overwrite=FALSE path were taken (not currently the prime path for this test)
    # However, this test as written doesn't rely on reading a pre-existing output file,
    # so dget mocking for output is not strictly needed here unless we change the test focus.
    # For now, no dget mock for output file reading.

    testthat::local_mocked_bindings(
      summarise_transcript = spy_summarise_transcript_empty,
      validate_agenda = mock_validate_agenda,
      validate_agenda_element = mock_validate_agenda_element_all_valid,
      convert_agenda_times = mock_convert_agenda_times,
      generate_recording_details = mock_generate_recording_details,
      build_ids_from_agenda = custom_build_ids,
      .package = "minutemaker"
    )
    # No base R mocks for dput, file.exists, dget here (SUT will write to temp_output_file_path)
    # No cli_inform mock. We will use expect_warning for cli_warn.
    testthat::local_mocked_bindings(
      style_file = function(path, ...) invisible(NULL),
      .package = "styler"
    )

    # Expect a warning when a talk's transcript subset is empty
    expect_warning(
      result_tree <- summarise_full_meeting(
        transcript_data = sample_transcript_for_meeting,
        agenda = agenda_for_empty_subset,
        output_file = temp_output_file_path
      ),
      regexp = "The transcript subset for the talk \"Session 1 - Talk B Empty\" is empty\\."
    )

    # Only Talk A should be processed, Talk B Empty has no transcript data in range
    expect_length(st_calls_empty, 1)
    expect_true("Talk A" %in% sapply(st_calls_empty, function(x) x$title))

    # Check the output file: it should contain only Talk A
    final_output_from_file <- dget(temp_output_file_path)
    expect_length(final_output_from_file, 1)
    expect_named(final_output_from_file, "Session 1 - Talk A")
    expect_identical(result_tree, final_output_from_file) # result_tree should also contain only Talk A
  })
})

test_that("summarise_full_meeting aborts if no talks are summarized", {
  withr::with_tempdir({
    work_dir <- getwd()
    temp_output_file_path <- file.path(work_dir, "test_summary_abort_empty.R")

    agenda_all_bad <- list(
      list(session = "S1", title = "Bad Talk 1", from = 700, to = 800),
      list(session = "S2", title = "Bad Talk 2", from = 801, to = 900)
    )
    custom_build_ids_bad <- function(agenda) {
      sapply(agenda, function(x) paste0(x$session, " - ", x$title))
    }
    mock_validate_all_valid <- function(...) TRUE

    testthat::local_mocked_bindings(
      summarise_transcript = function(...) stop("Should not be called"),
      validate_agenda = mock_validate_agenda,
      validate_agenda_element = mock_validate_all_valid,
      convert_agenda_times = mock_convert_agenda_times,
      generate_recording_details = mock_generate_recording_details,
      build_ids_from_agenda = custom_build_ids_bad,
      # No cli_warn mock needed here as we expect an error, not a warning that continues execution.
      .package = "minutemaker"
    )
    # No base R mocks for dput, file.exists, dget (output file won't be written if it aborts)
    testthat::local_mocked_bindings(
      style_file = function(path, ...) invisible(NULL),
      .package = "styler"
    )

    summarise_full_meeting(
      transcript_data = sample_transcript_for_meeting,
      agenda = agenda_all_bad,
      output_file = temp_output_file_path
    ) |>
      expect_warning('"S1 - Bad Talk 1" is empty') |>
      expect_warning('"S2 - Bad Talk 2" is empty') |>
      expect_error(
        regexp = "The final result tree has length zero. No talks were summarised."
      )
    # Ensure the output file was NOT created because of the abort
    expect_false(file.exists(temp_output_file_path))
  })
})
