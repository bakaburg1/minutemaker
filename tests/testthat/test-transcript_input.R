# Helper functions ---

segment_files <- function(dir_path) {
  files <- list.files(
    dir_path,
    pattern = "segment_[0-9]+\\.json$",
    full.names = TRUE
  )
  files[stringr::str_order(basename(files), numeric = TRUE)]
}

# Tests for use_transcript_input() ---

test_that("use_transcript_input standardizes VTT correctly", {
  vtt_path <- testthat::test_path("material", "webex.vtt")
  skip_if_not(file.exists(vtt_path))

  target_dir <- withr::local_tempdir()

  result_files <- use_transcript_input(vtt_path, target_dir = target_dir)

  expect_true(all(file.exists(result_files)))

  expected_dir <- testthat::test_path(
    "material",
    "webex_json_out",
    "transcription_output_data"
  )
  expected_files <- segment_files(expected_dir)

  expect_length(result_files, length(expected_files))
  expect_identical(basename(result_files), basename(expected_files))

  json_data <- jsonlite::read_json(result_files[[1]])
  expect_named(json_data, c("text", "segments"))
  expect_true(length(json_data$segments) > 0)

  # Check segment structure
  first_seg <- json_data$segments[[1]]
  expect_named(first_seg, c("start", "end", "speaker", "text"))

  for (i in seq_along(result_files)) {
    expect_equal(
      jsonlite::read_json(result_files[[i]]),
      jsonlite::read_json(expected_files[[i]])
    )
  }
})

test_that("use_transcript_input standardizes DOCX correctly", {
  docx_path <- testthat::test_path("material", "teams.docx")
  skip_if_not(file.exists(docx_path))
  testthat::skip_if_not_installed("officer")

  target_dir <- withr::local_tempdir()

  result_files <- use_transcript_input(docx_path, target_dir = target_dir)

  expect_true(all(file.exists(result_files)))
  expected_dir <- testthat::test_path(
    "material",
    "teams_json_out",
    "transcription_output_data"
  )
  expected_files <- segment_files(expected_dir)
  expect_length(result_files, length(expected_files))
  expect_identical(basename(result_files), basename(expected_files))

  json_data <- jsonlite::read_json(result_files[[1]])
  expect_named(json_data, c("text", "segments"))

  for (i in seq_along(result_files)) {
    expect_equal(
      jsonlite::read_json(result_files[[i]]),
      jsonlite::read_json(expected_files[[i]])
    )
  }
})

test_that("import_transcript_from_file aborts on invalid DOCX", {
  withr::with_tempdir({
    rlang::check_installed("officer")
    doc <- officer::read_docx()
    doc <- officer::body_add_par(doc, "Random text")
    # Use the print method for rdocx objects
    print(doc, target = "garbage.docx")

    expect_error(
      import_transcript_from_file("garbage.docx"),
      "does not appear to be a valid MS Teams transcript"
    )
  })
})

# Tests for speech_to_summary_workflow() with external transcript ---

test_that("workflow bypasses STT when external_transcript is provided", {
  withr::with_tempdir({
    target_dir <- "."
    vtt_path <- "test.vtt"
    # Create a minimal valid VTT content
    writeLines(
      c("WEBVTT", "", "00:00:01.000 --> 00:00:02.000", "Hello world"),
      vtt_path
    )

    # Mocks
    testthat::local_mocked_bindings(
      perform_speech_to_text = function(...) stop("STT should not be called"),
      apply_llm_correction = function(...) invisible(NULL),
      summarise_transcript = function(...) "Mock summary",
      get_prompts = function(...) "Mock prompt"
    )

    result <- withr::with_options(
      list(
        llmr_llm_provider = "mock",
        minutemaker_event_start_time = NULL
      ),
      {
        speech_to_summary_workflow(
          target_dir = target_dir,
          external_transcript = vtt_path,
          source_audio = NULL,
          use_agenda = "no",
          llm_provider = "mock",
          formatted_output_file = "summary.txt"
        )
      }
    )

    expect_named(result, c("transcript_data", "formatted_summary"))
    expect_identical(result$formatted_summary, "Mock summary")
    expect_true(file.exists(
      "transcription_output_data/segment_1.json"
    ))
  })
})
