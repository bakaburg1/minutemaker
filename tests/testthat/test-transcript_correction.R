# Helper function to create a temporary JSON file for apply_llm_correction tests
create_temp_transcript_file <- function(
  content_list,
  temp_dir,
  file_name = "temp_transcript.json"
) {
  file_path <- file.path(temp_dir, file_name)
  if (!"corrected" %in% names(content_list)) {
    content_list$corrected <- FALSE
  }
  jsonlite::write_json(
    content_list,
    file_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  return(file_path)
}

# Define content for JSON files to be created on-the-fly
simple_uncorrected_content <- list(
  text = "Ths is a tst transcript.",
  corrected = FALSE
)
simple_corrected_content <- list(
  text = "Ths is a tst transcript.",
  corrected = TRUE
)

test_that("apply_single_correction_set works with empty inputs", {
  expect_identical(apply_single_correction_set("", list(a = "b")), "")
  expect_identical(apply_single_correction_set("text", list()), "text")
  expect_identical(apply_single_correction_set("", list()), "")
})

test_that("apply_single_correction_set performs simple replacements", {
  corrections <- list("wrld" = "world", "Hllo" = "Hello")
  text <- "Hllo wrld!"
  expect_identical(
    apply_single_correction_set(text, corrections),
    "Hello world!"
  )
})

test_that("apply_single_correction_set respects order for overlapping keys (longer first)", {
  corrections <- list("Nana" = "Grape", "Banana" = "Orange")
  text <- "Banana Nana"
  expect_identical(
    apply_single_correction_set(text, corrections),
    "Orange Grape"
  )

  corrections_rev <- list("Banana" = "Orange", "Nana" = "Grape")
  expect_identical(
    apply_single_correction_set(text, corrections_rev),
    "Orange Grape"
  )
})

test_that("apply_single_correction_set handles special regex characters in keys", {
  corrections <- list("*.txt" = "glob.file", "test." = "testdot")
  text <- "This is a test. and a *.txt file."
  expect_identical(
    apply_single_correction_set(text, corrections),
    "This is a testdot and a glob.file file."
  )
})

mock_llm_response_success <- function(correction_json_str) {
  paste0(
    "<json_corrections_output>",
    correction_json_str,
    "</json_corrections_output>"
  )
}
no_changes_signal_response <- "<json_corrections_output>NO_CHANGES_NEEDED</json_corrections_output>"

test_that("correct_transcription_errors handles no text input", {
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res <- correct_transcription_errors("", terms = NULL)
    expect_identical(res$status, "no_text_to_correct")
    expect_false(res$made_changes)

    res_ws <- correct_transcription_errors("   ", terms = NULL)
    expect_identical(res_ws$status, "no_text_to_correct")
    expect_false(res_ws$made_changes)
  })
})

test_that("correct_transcription_errors handles LLM call failure", {
  testthat::local_mocked_bindings(
    prompt_llm = function(...) stop("LLM API down!"),
    set_llmr_model = function(...) invisible(NULL),
    .package = "llmR"
  )
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    expect_warning(
      res <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "LLM call failed"
    )
    expect_identical(res$status, "llm_call_failed")
    expect_false(res$made_changes)
    expect_null(res$corrections_map)
  })
})

test_that("correct_transcription_errors handles empty/NULL LLM response", {
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      prompt_llm = function(...) NULL,
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res_null <- correct_transcription_errors("Some text", terms = NULL)
    expect_identical(res_null$status, "llm_call_failed")
  })

  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      prompt_llm = function(...) "   ",
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res_ws <- correct_transcription_errors("Some text", terms = NULL)
    expect_identical(res_ws$status, "llm_call_failed")
  })
})

test_that("correct_transcription_errors handles malformed XML tags", {
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    # Test incomplete opening tag
    testthat::local_mocked_bindings(
      prompt_llm = function(...) "<json_corrections_output{\"key\": \"value\"}",
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res_incomplete <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "did not contain the expected XML wrapper tags"
    )
    expect_identical(res_incomplete$status, "parsing_failed")

    # Test incomplete closing tag
    testthat::local_mocked_bindings(
      prompt_llm = function(...)
        "<json_corrections_output>{\"key\": \"value\"}</json_corrections",
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res_incomplete_close <- correct_transcription_errors(
        "Some text",
        terms = NULL
      ),
      regexp = "did not contain the expected XML wrapper tags"
    )
    expect_identical(res_incomplete_close$status, "parsing_failed")

    # Test wrong tag names
    testthat::local_mocked_bindings(
      prompt_llm = function(...) "<wrong_tag>{\"key\": \"value\"}</wrong_tag>",
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res_wrong_tag <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "did not contain the expected XML wrapper tags"
    )
    expect_identical(res_wrong_tag$status, "parsing_failed")
  })
})

test_that("correct_transcription_errors handles NO_CHANGES_NEEDED signal", {
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      prompt_llm = function(...) no_changes_signal_response,
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res <- correct_transcription_errors("Some text", terms = NULL)
    expect_identical(res$status, "no_changes_signal")
    expect_false(res$made_changes)
    expect_null(res$corrections_map)
  })
})

test_that("correct_transcription_errors handles valid JSON corrections", {
  corrections_json <- "{\"Hllo\": \"Hello\", \"wrld\": \"world\"}"
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      prompt_llm = function(...) mock_llm_response_success(corrections_json),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res <- correct_transcription_errors("Hllo wrld, Hllo.", terms = NULL)
    expect_identical(res$status, "corrections_applied")
    expect_true(res$made_changes)
    expect_identical(res$corrected_text, "Hello world, Hello.")
    expect_length(res$corrections_map, 2)
    expect_identical(res$corrections_map[["Hllo"]], "Hello")
  })
})

test_that("correct_transcription_errors warns on unmatched correction keys", {
  corrections_json <- "{\"NotInText\": \"Fixed\"}"
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      prompt_llm = function(...) mock_llm_response_success(corrections_json),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "not found in the input text"
    )
    expect_identical(res$status, "no_changes_signal")
  })
})

test_that("correct_transcription_errors handles empty JSON object {} as no changes", {
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      prompt_llm = function(...) mock_llm_response_success("{}"),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "LLM returned a JSON object that parsed to an empty R object"
    )
    expect_identical(res$status, "no_changes_signal")
    expect_false(res$made_changes)
  })
})

test_that("correct_transcription_errors handles syntactically invalid JSON", {
  invalid_json <- "{\"key\": \"value\",,}"
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      prompt_llm = function(...) mock_llm_response_success(invalid_json),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "JSON parsing failed"
    )
    expect_identical(res$status, "parsing_failed")
    expect_match(
      as.character(res$error_message),
      "lexical error|parse error|trailing comma",
      ignore.case = TRUE
    )
  })
})

test_that("correct_transcription_errors handles JSON that is not a JSON object (array/scalar)", {
  array_json <- "[\"item1\", \"item2\"]"
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      prompt_llm = function(...) mock_llm_response_success(array_json),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res_array <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "no JSON object substring was found"
    )
    expect_identical(res_array$status, "parsing_failed")
    expect_match(
      res_array$error_message,
      "Content neither 'NO_CHANGES_NEEDED' signal nor a JSON object",
      fixed = TRUE
    )
  })

  scalar_json <- "\"just a string\""
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      prompt_llm = function(...) mock_llm_response_success(scalar_json),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res_scalar <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "no JSON object substring was found"
    )
    expect_identical(res_scalar$status, "parsing_failed")
    expect_match(
      res_scalar$error_message,
      "Content neither 'NO_CHANGES_NEEDED' signal nor a JSON object",
      fixed = TRUE
    )
  })
})

test_that("correct_transcription_errors model selection logic", {
  withr::with_options(
    list(
      minutemaker_correction_llm_model = "specific_correction_model",
      llmr_current_model = "general_model"
    ),
    {
      set_model_calls <- character(0)
      # Mock llmR::set_llmr_model to record calls - This test specifically checks set_llmr_model behavior
      testthat::local_mocked_bindings(
        prompt_llm = function(...) no_changes_signal_response, # Avoid actual call, focus on set_llmr_model
        set_llmr_model = function(model_label) {
          set_model_calls <<- c(set_model_calls, model_label)
          invisible(NULL)
        },
        .package = "llmR"
      )
      correct_transcription_errors("text", terms = NULL)
      expect_true("specific_correction_model" %in% set_model_calls)
      expect_true("general_model" %in% set_model_calls)
    }
  )

  withr::with_options(
    list(
      minutemaker_correction_llm_model = NULL,
      llmr_current_model = "general_fallback_model"
    ),
    {
      set_model_calls_2 <- character(0)
      testthat::local_mocked_bindings(
        prompt_llm = function(...) no_changes_signal_response,
        set_llmr_model = function(model_label) {
          # This one also checks set_llmr_model behavior
          set_model_calls_2 <<- c(set_model_calls_2, model_label)
          invisible(NULL)
        },
        .package = "llmR"
      )
      expect_message(
        correct_transcription_errors("text", terms = NULL),
        "reverting to general model"
      )

      grep("general_fallback_model", set_model_calls_2) |>
        length() |>
        expect_gt(0)
    }
  )

  withr::with_options(
    list(minutemaker_correction_llm_model = NULL, llmr_current_model = NULL),
    {
      # No set_llmr_model mock here as it should error before trying to set
      # model if none are configured.
      expect_error(
        correct_transcription_errors("text", terms = NULL),
        "LLM model for correction not set"
      )
    }
  )
})

test_that("correct_transcription_errors input validation for include_reasoning", {
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    testthat::local_mocked_bindings(
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_error(
      correct_transcription_errors("text", include_reasoning = "true_string"),
      "must be a single logical value"
    )
  })
})

mock_successful_correction <- function(
  text_to_correct,
  terms,
  include_reasoning,
  llm_extra_params
) {
  list(
    corrected_text = paste0(text_to_correct, " [corrected]"),
    corrections_map = if (nzchar(text_to_correct)) list(old = "new") else NULL,
    status = "corrections_applied",
    made_changes = nzchar(text_to_correct)
  )
}

mock_no_changes_correction <- function(
  text_to_correct,
  terms,
  include_reasoning,
  llm_extra_params
) {
  list(
    corrected_text = text_to_correct,
    corrections_map = NULL,
    status = "no_changes_signal",
    made_changes = FALSE
  )
}

mock_failed_correction <- function(
  text_to_correct,
  terms,
  include_reasoning,
  llm_extra_params,
  fail_status = "parsing_failed"
) {
  list(
    corrected_text = text_to_correct,
    corrections_map = NULL,
    status = fail_status,
    made_changes = FALSE
  )
}

test_that("apply_llm_correction handles non-existent input path", {
  expect_error(
    apply_llm_correction(input_path = "/non/existent/path.json", terms = NULL),
    "Path does not exist"
  )
})

test_that("apply_llm_correction handles non-JSON file input", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  non_json_file <- file.path(temp_dir, "not_a_json.txt")
  writeLines("hello", non_json_file)
  expect_error(
    apply_llm_correction(input_path = non_json_file, terms = NULL),
    "Input file is not a JSON file"
  )
})

test_that("apply_llm_correction handles empty directory", {
  # Create a temporary empty directory for this test
  temp_empty_dir <- withr::local_tempdir(pattern = "test_empty_dir")

  expect_message(
    apply_llm_correction(input_path = temp_empty_dir, terms = NULL),
    "No JSON files found"
  )
  processed <- apply_llm_correction(input_path = temp_empty_dir, terms = NULL)
  expect_length(processed, 0)
})

test_that("apply_llm_correction processes a single file, overwrite=TRUE, changes made", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  test_file_path <- create_temp_transcript_file(
    simple_uncorrected_content,
    temp_dir,
    "temp_uncorrected.json"
  )

  testthat::local_mocked_bindings(
    correct_transcription_errors = mock_successful_correction,
    .package = "minutemaker"
  )

  processed_files <- apply_llm_correction(
    input_path = test_file_path,
    terms = c("tst"),
    overwrite = TRUE
  )
  expect_identical(basename(processed_files), basename(test_file_path))

  final_data <- jsonlite::read_json(test_file_path)
  expect_true(final_data$corrected)
  expect_identical(final_data$text, "Ths is a tst transcript. [corrected]")
})

test_that("apply_llm_correction processes a single file, overwrite=TRUE, no changes made by LLM", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  test_file_path <- create_temp_transcript_file(
    simple_uncorrected_content,
    temp_dir,
    "temp_uncorrected.json"
  )

  testthat::local_mocked_bindings(
    correct_transcription_errors = mock_no_changes_correction,
    .package = "minutemaker"
  )

  processed_files <- apply_llm_correction(
    input_path = test_file_path,
    terms = NULL,
    overwrite = TRUE
  )
  expect_identical(basename(processed_files), basename(test_file_path))

  final_data <- jsonlite::read_json(test_file_path)
  expect_true(final_data$corrected)
  expect_identical(final_data$text, "Ths is a tst transcript.")
})

test_that("apply_llm_correction processes a single file, overwrite=FALSE, changes made", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  test_file_path <- create_temp_transcript_file(
    simple_uncorrected_content,
    temp_dir,
    "temp_uncorrected.json"
  )

  testthat::local_mocked_bindings(
    correct_transcription_errors = mock_successful_correction,
    .package = "minutemaker"
  )

  apply_llm_correction(
    input_path = test_file_path,
    terms = c("tst"),
    overwrite = FALSE
  )
  final_data <- jsonlite::read_json(test_file_path)
  expect_true(final_data$corrected)
  expect_identical(final_data$text, "Ths is a tst transcript. [corrected]")
})

test_that("apply_llm_correction processes a single file, overwrite=FALSE, no changes made by LLM", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  test_file_path <- create_temp_transcript_file(
    simple_uncorrected_content,
    temp_dir,
    "temp_uncorrected.json"
  )
  original_content <- jsonlite::read_json(test_file_path) # Read the temp file content for comparison

  testthat::local_mocked_bindings(
    correct_transcription_errors = mock_no_changes_correction,
    .package = "minutemaker"
  )

  apply_llm_correction(
    input_path = test_file_path,
    terms = NULL,
    overwrite = FALSE
  )
  final_data <- jsonlite::read_json(test_file_path)
  expect_false(final_data$corrected)
  expect_identical(final_data$text, original_content$text)
})

test_that("apply_llm_correction skips already corrected file if overwrite=FALSE", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  test_file_path <- create_temp_transcript_file(
    simple_corrected_content,
    temp_dir,
    "temp_corrected.json"
  )
  original_content <- jsonlite::read_json(test_file_path) # Read the temp file content for comparison

  cte_call_count <- 0
  mock_cte_for_skip <- function(...) {
    cte_call_count <<- cte_call_count + 1
    list() # Should not be called
  }
  testthat::local_mocked_bindings(
    correct_transcription_errors = mock_cte_for_skip,
    .package = "minutemaker"
  )

  apply_llm_correction(
    input_path = test_file_path,
    terms = NULL,
    overwrite = FALSE
  )

  final_data <- jsonlite::read_json(test_file_path)
  expect_identical(final_data, original_content)
  expect_equal(cte_call_count, 0)
})

test_that("apply_llm_correction retries on specified failures", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  test_file_path <- create_temp_transcript_file(
    simple_uncorrected_content,
    temp_dir,
    "temp_uncorrected.json"
  )

  mock_calls <- 0
  cte_retry_mock <- function(
    text_to_correct,
    terms,
    include_reasoning,
    llm_extra_params
  ) {
    mock_calls <<- mock_calls + 1
    if (mock_calls == 1) {
      return(mock_failed_correction(
        text_to_correct,
        terms,
        include_reasoning,
        llm_extra_params,
        fail_status = "parsing_failed"
      ))
    } else {
      return(mock_successful_correction(
        text_to_correct,
        terms,
        include_reasoning,
        llm_extra_params
      ))
    }
  }
  testthat::local_mocked_bindings(
    correct_transcription_errors = cte_retry_mock,
    .package = "minutemaker"
  )

  apply_llm_correction(
    input_path = test_file_path,
    terms = NULL,
    overwrite = TRUE,
    max_retries = 1
  )

  expect_equal(mock_calls, 2)
  final_data <- jsonlite::read_json(test_file_path)
  expect_true(final_data$corrected)
  expect_identical(final_data$text, "Ths is a tst transcript. [corrected]")
})

test_that("apply_llm_correction backs off by splitting segments on failure", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  segmented_content <- list(
    text = "Ths is a tst transcript.",
    corrected = FALSE,
    segments = list(
      list(text = "Ths is"),
      list(text = "a tst"),
      list(text = "transcript."),
      list(text = "extra one"),
      list(text = "extra two"),
      list(text = "")
    )
  )
  test_file_path <- create_temp_transcript_file(
    segmented_content,
    temp_dir,
    "temp_segmented.json"
  )

  cte_calls <- 0
  cte_backoff_mock <- function(
    text_to_correct,
    terms,
    include_reasoning,
    llm_extra_params
  ) {
    cte_calls <<- cte_calls + 1
    if (
      length(text_to_correct) == 1 &&
        identical(text_to_correct, "Ths is a tst transcript.")
    ) {
      return(mock_failed_correction(
        text_to_correct,
        terms,
        include_reasoning,
        llm_extra_params,
        fail_status = "llm_call_failed"
      ))
    }
    if (length(text_to_correct) > 5) {
      return(mock_failed_correction(
        text_to_correct,
        terms,
        include_reasoning,
        llm_extra_params,
        fail_status = "parsing_failed"
      ))
    }
    list(
      corrected_text = text_to_correct,
      corrections_map = list("tst" = "test"),
      status = "corrections_applied",
      made_changes = TRUE
    )
  }
  testthat::local_mocked_bindings(
    correct_transcription_errors = cte_backoff_mock,
    .package = "minutemaker"
  )

  apply_llm_correction(
    input_path = test_file_path,
    terms = NULL,
    overwrite = TRUE,
    max_retries = 1
  )

  final_data <- jsonlite::read_json(test_file_path)
  expect_true(final_data$corrected)
  expect_identical(final_data$text, "Ths is a test transcript.")
  expect_identical(final_data$segments[[2]]$text, "a test")
  expect_true(cte_calls >= 3)
})

test_that("apply_llm_correction gives up after max_retries", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  test_file_path <- create_temp_transcript_file(
    simple_uncorrected_content,
    temp_dir,
    "temp_uncorrected.json"
  )
  original_content <- jsonlite::read_json(test_file_path, simplifyVector = TRUE) # Read the temp file content

  mock_calls <- 0
  cte_always_fail_mock <- function(...) {
    mock_calls <<- mock_calls + 1
    return(mock_failed_correction(
      original_content$text,
      NULL,
      TRUE,
      list(),
      fail_status = "llm_call_failed"
    ))
  }
  testthat::local_mocked_bindings(
    correct_transcription_errors = cte_always_fail_mock,
    .package = "minutemaker"
  )

  processed_output_object <- NULL
  expect_warning(
    processed_output_object <- apply_llm_correction(
      input_path = test_file_path,
      terms = NULL,
      overwrite = TRUE,
      max_retries = 2
    ),
    "Correction failed after 2 retries"
  )
  expect_length(processed_output_object, 0)
  expect_equal(mock_calls, 3)
  final_data <- jsonlite::read_json(test_file_path)
  expect_identical(final_data$text, original_content$text)
  expect_false(final_data$corrected)
})

test_that("apply_llm_correction handles invalid terms input", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  test_file_path <- create_temp_transcript_file(
    simple_uncorrected_content,
    temp_dir,
    "temp_uncorrected.json"
  )
  expect_error(
    apply_llm_correction(input_path = test_file_path, terms = 123),
    regexp = "Parameter.*terms.*must be a character vector or NULL"
  )
})

test_that("apply_llm_correction processes files in a directory", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")
  file1_content <- list(text = "File one tst.", corrected = FALSE)
  file2_content <- list(text = "File two tst.", corrected = FALSE)
  create_temp_transcript_file(file1_content, temp_dir, "f1.json")
  create_temp_transcript_file(file2_content, temp_dir, "f2.json")
  writeLines("ignore me", file.path(temp_dir, "ignore.txt"))

  mock_calls <- 0
  mock_cte_dir <- function(text_to_correct, ...) {
    mock_calls <<- mock_calls + 1
    return(mock_successful_correction(text_to_correct, NULL, TRUE, list()))
  }
  testthat::local_mocked_bindings(
    correct_transcription_errors = mock_cte_dir,
    .package = "minutemaker"
  )

  processed_files <- apply_llm_correction(
    input_path = temp_dir,
    terms = NULL,
    overwrite = TRUE
  )
  expect_equal(mock_calls, 2)
  expect_length(processed_files, 2)
  expect_true(all(endsWith(processed_files, ".json")))

  f1_data <- jsonlite::read_json(file.path(temp_dir, "f1.json"))
  f2_data <- jsonlite::read_json(file.path(temp_dir, "f2.json"))
  expect_true(f1_data$corrected)
  expect_true(f2_data$corrected)
  expect_identical(f1_data$text, "File one tst. [corrected]")
  expect_identical(f2_data$text, "File two tst. [corrected]")
})

test_that("correct_transcription_errors handles mixed XML content", {
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    # Test content before XML tags
    testthat::local_mocked_bindings(
      prompt_llm = function(...)
        paste0(
          "Some text before ",
          mock_llm_response_success("{\"key\": \"value\"}")
        ),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res_before <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "not found in the input text"
    )
    expect_identical(res_before$status, "no_changes_signal")

    # Test content after XML tags
    testthat::local_mocked_bindings(
      prompt_llm = function(...)
        paste0(
          mock_llm_response_success("{\"key\": \"value\"}"),
          " Some text after"
        ),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res_after <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "not found in the input text"
    )
    expect_identical(res_after$status, "no_changes_signal")

    # Test multiple XML tag sets
    testthat::local_mocked_bindings(
      prompt_llm = function(...)
        paste0(
          mock_llm_response_success("{\"key1\": \"value1\"}"),
          mock_llm_response_success("{\"key2\": \"value2\"}")
        ),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    expect_warning(
      res_multiple <- correct_transcription_errors("Some text", terms = NULL),
      regexp = "not found in the input text"
    )
    expect_identical(res_multiple$status, "no_changes_signal")
  })
})

test_that("correct_transcription_errors handles invalid JSON values", {
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    # Test non-string values
    testthat::local_mocked_bindings(
      prompt_llm = function(...)
        mock_llm_response_success(
          "{\"key\": \"123\", \"another_key\": \"true\"}"
        ),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res_non_string <- correct_transcription_errors(
      "key another_key",
      terms = NULL
    )
    expect_identical(res_non_string$status, "corrections_applied")
    expect_identical(res_non_string$corrected_text, "123 true")

    # Test nested objects
    testthat::local_mocked_bindings(
      prompt_llm = function(...)
        mock_llm_response_success(
          "{\"key\": \"value\"}"
        ),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res_nested <- correct_transcription_errors("key", terms = NULL)
    expect_identical(res_nested$status, "corrections_applied")
    expect_identical(res_nested$corrected_text, "value")

    # Test arrays as values
    testthat::local_mocked_bindings(
      prompt_llm = function(...)
        mock_llm_response_success(
          "{\"key\": \"value1,value2\"}"
        ),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res_array <- correct_transcription_errors("key", terms = NULL)
    expect_identical(res_array$status, "corrections_applied")
    expect_identical(res_array$corrected_text, "value1,value2")

    # Test empty string keys/values
    testthat::local_mocked_bindings(
      prompt_llm = function(...)
        mock_llm_response_success(
          "{\"nonempty\": \"value\"}"
        ),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res_empty <- correct_transcription_errors("nonempty", terms = NULL)
    expect_identical(res_empty$status, "corrections_applied")
    expect_identical(res_empty$corrected_text, "value")
  })
})

test_that("correct_transcription_errors handles Unicode and special characters", {
  withr::with_options(list(minutemaker_correction_llm_model = "mock_model"), {
    # Test Unicode escapes
    testthat::local_mocked_bindings(
      prompt_llm = function(...)
        mock_llm_response_success(
          "{\"\\u00E9t\\u00E9\": \"\\u00E9t\\u00E9 fixed\"}"
        ),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res_unicode <- correct_transcription_errors("été", terms = NULL)
    expect_identical(res_unicode$status, "corrections_applied")
    expect_identical(res_unicode$corrected_text, "été fixed")

    # Test special characters in keys/values
    testthat::local_mocked_bindings(
      prompt_llm = function(...)
        mock_llm_response_success(
          "{\"key\\nwith\\nnewlines\": \"value\\twith\\ttabs\", \"key\\\"with\\\"quotes\": \"value\\\\with\\\\backslashes\"}"
        ),
      set_llmr_model = function(...) invisible(NULL),
      .package = "llmR"
    )
    res_special <- correct_transcription_errors(
      "key\nwith\nnewlines key\"with\"quotes",
      terms = NULL
    )
    expect_identical(res_special$status, "corrections_applied")
    expect_true(res_special$made_changes)
    expect_identical(
      res_special$corrected_text,
      "value\twith\ttabs value\\with\\backslashes"
    )
  })
})

test_that("apply_llm_correction properly handles corrections_applied field", {
  temp_dir <- withr::local_tempdir(pattern = "test_apply_llm")

  # Test case 1: Changes made - corrections_applied should be present
  test_file_path <- create_temp_transcript_file(
    simple_uncorrected_content,
    temp_dir,
    "temp_uncorrected.json"
  )

  mock_corrections_map <- list(
    "Ths" = "This",
    "tst" = "test"
  )

  mock_cte_with_corrections <- function(...) {
    list(
      corrected_text = "This is a test transcript.",
      corrections_map = mock_corrections_map,
      status = "corrections_applied",
      made_changes = TRUE
    )
  }

  testthat::local_mocked_bindings(
    correct_transcription_errors = mock_cte_with_corrections,
    .package = "minutemaker"
  )

  processed_files <- apply_llm_correction(
    input_path = test_file_path,
    terms = NULL,
    overwrite = TRUE
  )

  final_data <- jsonlite::read_json(test_file_path)
  expect_true(final_data$corrected)
  expect_identical(final_data$corrections_applied, mock_corrections_map)
  expect_identical(final_data$text, "This is a test transcript.")

  # Test case 2: No changes made - corrections_applied should be NULL/absent
  test_file_path2 <- create_temp_transcript_file(
    simple_uncorrected_content,
    temp_dir,
    "temp_uncorrected2.json"
  )

  mock_cte_no_corrections <- function(...) {
    list(
      corrected_text = "Ths is a tst transcript.",
      corrections_map = NULL,
      status = "no_changes_signal",
      made_changes = FALSE
    )
  }

  testthat::local_mocked_bindings(
    correct_transcription_errors = mock_cte_no_corrections,
    .package = "minutemaker"
  )

  processed_files2 <- apply_llm_correction(
    input_path = test_file_path2,
    terms = NULL,
    overwrite = TRUE
  )

  final_data2 <- jsonlite::read_json(test_file_path2)
  expect_true(final_data2$corrected) # Should be TRUE due to overwrite=TRUE
  expect_null(final_data2$corrections_applied)
  expect_identical(final_data2$text, "Ths is a tst transcript.")

  # Test case 3: Empty corrections map - corrections_applied should be NULL/absent
  test_file_path3 <- create_temp_transcript_file(
    simple_uncorrected_content,
    temp_dir,
    "temp_uncorrected3.json"
  )

  mock_cte_empty_corrections <- function(...) {
    list(
      corrected_text = "Ths is a tst transcript.",
      corrections_map = list(),
      status = "corrections_applied",
      made_changes = FALSE
    )
  }

  testthat::local_mocked_bindings(
    correct_transcription_errors = mock_cte_empty_corrections,
    .package = "minutemaker"
  )

  processed_files3 <- apply_llm_correction(
    input_path = test_file_path3,
    terms = NULL,
    overwrite = TRUE
  )

  final_data3 <- jsonlite::read_json(test_file_path3)
  expect_true(final_data3$corrected) # Should be TRUE due to overwrite=TRUE
  expect_null(final_data3$corrections_applied)
  expect_identical(final_data3$text, "Ths is a tst transcript.")
})

test_that("correct_transcription_errors uses temperature for non-reasoning models even when include_reasoning=TRUE", {
  withr::with_options(
    list(
      minutemaker_correction_llm_model = "mm_gpt-4.1_azure",
      llmr_current_model = "mm_gpt-4.1_azure"
    ),
    {
      captured_params <- NULL
      captured_messages <- NULL

      testthat::local_mocked_bindings(
        prompt_llm = function(messages, params, force_json, ...) {
          captured_messages <<- messages
          captured_params <<- params
          "<json_corrections_output>NO_CHANGES_NEEDED</json_corrections_output>"
        },
        set_llmr_model = function(...) invisible(NULL),
        .package = "llmR"
      )

      res <- correct_transcription_errors(
        text_to_correct = "Some transcript text",
        terms = NULL,
        include_reasoning = TRUE
      )

      expect_identical(res$status, "no_changes_signal")
      expect_true("temperature" %in% names(captured_params))
      expect_identical(captured_params$temperature, 0)
      expect_false("reasoning_effort" %in% names(captured_params))
      expect_true(
        stringr::str_detect(
          captured_messages[["system"]],
          "REASON STEP BY STEP"
        )
      )
    }
  )
})

test_that("correct_transcription_errors uses reasoning_effort when include_reasoning=FALSE and omits temperature", {
  withr::with_options(
    list(
      minutemaker_correction_llm_model = "mm_any_reasoning_model",
      llmr_current_model = "mm_any_reasoning_model"
    ),
    {
      captured_params <- NULL
      captured_messages <- NULL

      testthat::local_mocked_bindings(
        prompt_llm = function(messages, params, force_json, ...) {
          captured_messages <<- messages
          captured_params <<- params
          "<json_corrections_output>NO_CHANGES_NEEDED</json_corrections_output>"
        },
        set_llmr_model = function(...) invisible(NULL),
        .package = "llmR"
      )

      res <- correct_transcription_errors(
        text_to_correct = "Some transcript text",
        terms = NULL,
        include_reasoning = FALSE
      )

      expect_identical(res$status, "no_changes_signal")
      expect_true("reasoning_effort" %in% names(captured_params))
      expect_identical(captured_params$reasoning_effort, "medium")
      expect_false("temperature" %in% names(captured_params))
      expect_false(
        stringr::str_detect(
          captured_messages[["system"]],
          "REASON STEP BY STEP"
        )
      )
    }
  )
})

cat("\nAll testthat tests for transcript_correction.R defined.\n")
