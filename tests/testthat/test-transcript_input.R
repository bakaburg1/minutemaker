# Helper functions ---

# Tests for use_transcript_input() ---

test_that("use_transcript_input standardizes VTT correctly", {
  vtt_path <- testthat::test_path("material", "webex.vtt")
  skip_if_not(file.exists(vtt_path))

  target_dir <- withr::local_tempdir()

  imported <- import_transcript_from_file(vtt_path, import_diarization = TRUE)
  result_files <- use_transcript_input(
    vtt_path,
    target_dir = target_dir,
    lines_per_json = 2
  )

  expect_true(all(file.exists(result_files)))

  expected_chunks <- ceiling(nrow(imported) / 2)
  pad_width <- nchar(as.character(expected_chunks))
  expected_basenames <- sprintf(
    paste0("segment_%0", pad_width, "d.json"),
    seq_len(expected_chunks)
  )
  expect_length(result_files, expected_chunks)
  expect_identical(basename(result_files), expected_basenames)

  json_data <- jsonlite::read_json(result_files[[1]])
  expect_named(json_data, c("text", "segments"))
  expect_true(length(json_data$segments) > 0)

  # Check segment structure
  first_seg <- json_data$segments[[1]]
  expect_named(first_seg, c("start", "end", "speaker", "text"))

  parsed <- parse_transcript_json(
    file.path(target_dir, "transcription_output_data"),
    pretty_times = FALSE,
    event_start_time = NULL
  )
  imported <- imported |>
    dplyr::arrange(.data$start, .data$end)
  parsed <- parsed |>
    dplyr::arrange(.data$start, .data$end)

  expect_equal(parsed$start, imported$start)
  expect_equal(parsed$end, imported$end)
})

test_that("use_transcript_input standardizes DOCX correctly", {
  docx_path <- testthat::test_path("material", "teams.docx")
  skip_if_not(file.exists(docx_path))
  testthat::skip_if_not_installed("officer")

  target_dir <- withr::local_tempdir()

  imported <- import_transcript_from_file(docx_path, import_diarization = TRUE)
  result_files <- use_transcript_input(
    docx_path,
    target_dir = target_dir,
    lines_per_json = 2
  )

  expect_true(all(file.exists(result_files)))
  expect_length(result_files, ceiling(nrow(imported) / 2))

  json_data <- jsonlite::read_json(result_files[[1]])
  expect_named(json_data, c("text", "segments"))
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
