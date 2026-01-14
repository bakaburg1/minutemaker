# Helper functions ----

# Tests for speech_to_summary_workflow() ----

test_that("workflow runs end-to-end with extensive mocking and minimal options", {
  withr::with_tempdir({
    target_dir <- "." # The temp dir is the working dir

    # Create necessary subdirectories
    dir.create(file.path(target_dir, "audio_to_transcribe"))
    dir.create(file.path(target_dir, "transcription_output_data"))

    # Create a dummy audio file for STT input
    file.create(file.path(target_dir, "audio_to_transcribe", "dummy.wav"))

    # Mocks for current package functions (no .package needed)
    testthat::local_mocked_bindings(
      perform_speech_to_text = function(audio_path, output_dir, model, ...) {
        # Simulate STT creating an output file
        file.create(file.path(output_dir, "dummy_stt_output.json"))
        invisible(NULL)
      },
      apply_llm_correction = function(...) invisible(NULL),
      parse_transcript_json = function(transcription_output_dir, ...) {
        # Return a minimal valid transcript tibble
        dplyr::tibble(
          text = "mocked transcript text",
          start_time = 0,
          end_time = 1000,
          speaker = "SPEAKER_00"
        )
      },
      summarise_transcript = function(transcript_data, ...) {
        # Return a simple mock summary
        return("mock summary content")
      },
      get_prompts = function(prompt_name, ...) {
        if (prompt_name == "summary_structure") {
          return("mocked summary structure prompt")
        }
        # Fallback for safety, though not expected to be hit in this test
        stop(paste("Unexpected prompt name in get_prompts mock:", prompt_name))
      }
    )

    # Call the workflow with minimal and controlled arguments
    result <- withr::with_options(
      list(
        minutemaker_stt_model = "mock_stt_model",
        llmr_llm_provider = "mock_provider",
        minutemaker_event_start_time = NULL,
        minutemaker_include_llm_reasoning = FALSE
      ),
      {
        expect_no_error({
          speech_to_summary_workflow(
            target_dir = target_dir,
            source_audio = NULL, # Use pre-existing audio in stt_audio_dir
            split_audio = FALSE,
            # If TRUE and source_audio is NULL, it aborts.
            # If FALSE, it uses existing audio in stt_audio_dir.
            overwrite_stt_audio = FALSE,
            stt_audio_dir = file.path(target_dir, "audio_to_transcribe"),
            transcription_output_dir = file.path(
              target_dir,
              "transcription_output_data"
            ),
            stt_model = "mock_stt_model_arg",
            # To ensure perform_speech_to_text mock is called
            overwrite_transcription_files = TRUE,
            enable_llm_correction = FALSE,
            transcript_file = file.path(target_dir, "transcript.csv"),
            overwrite_transcript = TRUE,
            external_transcript = NULL,
            chat_file = NULL,
            generate_context = FALSE,
            use_agenda = "no", # Simplifies flow, avoids agenda generation/use
            llm_provider = "mock_provider_arg",
            formatted_output_file = file.path(target_dir, "event_summary.txt"),
            overwrite_formatted_output = TRUE,
            # Ensure other overwrites are true to test those paths
            overwrite_summary_tree = TRUE # Not used if use_agenda="no", but good practice
          )
        })
      }
    )

    # Validate the results
    expect_type(result, "list")
    expect_named(result, c("transcript_data", "formatted_summary"))
    expect_s3_class(result$transcript_data, "tbl_df")

    expect_identical(result$transcript_data$text, "mocked transcript text")
    expect_identical(result$formatted_summary, "mock summary content")
  })
})

test_that("workflow errors clearly when neither audio nor transcript are available", {
  withr::with_tempdir({
    expect_error(
      withr::with_options(
        list(
          minutemaker_stt_model = "mock_stt_model",
          llmr_llm_provider = "mock_provider"
        ),
        speech_to_summary_workflow(
          target_dir = ".",
          external_transcript = NULL,
          generate_context = FALSE,
          source_audio = NULL,
          split_audio = FALSE,
          overwrite_stt_audio = TRUE,
          stt_audio_dir = file.path(".", "audio_to_transcribe"),
          transcription_output_dir = file.path(
            ".",
            "transcription_output_data"
          ),
          stt_model = "mock_stt_model",
          use_agenda = "no",
          llm_provider = "mock_provider",
          formatted_output_file = file.path(".", "event_summary.txt"),
          enable_llm_correction = FALSE
        )
      ),
      regexp = "No audio found for speech-to-text"
    )
  })
})

test_that("workflow accepts a file path as target_dir when a transcript is present", {
  withr::with_tempdir({
    script_path <- file.path(getwd(), "script.R")
    file.create(script_path)

    vtt_path <- file.path(getwd(), "transcript.vtt")
    writeLines(
      c("WEBVTT", "", "00:00:01.000 --> 00:00:02.000", "Hello from VTT"),
      vtt_path
    )

    testthat::local_mocked_bindings(
      perform_speech_to_text = function(...) {
        stop("STT should be bypassed when transcript is available")
      },
      apply_llm_correction = function(...) {
        stop("LLM correction should not run in this test")
      },
      summarise_transcript = function(...) {
        "Mock summary from transcript-only path"
      },
      get_prompts = function(...) "Mock prompt"
    )

    result <- withr::with_options(
      list(
        llmr_llm_provider = "mock_provider",
        minutemaker_event_start_time = NULL
      ),
      speech_to_summary_workflow(
        target_dir = script_path,
        use_agenda = "no",
        generate_context = FALSE,
        llm_provider = "mock_provider",
        formatted_output_file = "summary.txt",
        enable_llm_correction = FALSE
      )
    )

    expect_named(result, c("transcript_data", "formatted_summary"))
    expect_identical(
      result$formatted_summary,
      "Mock summary from transcript-only path"
    )
    expect_true(file.exists("transcription_output_data/segment_1.json"))
  })
})

test_that("workflow errors clearly on NA target_dir", {
  expect_error(
    speech_to_summary_workflow(
      target_dir = NA_character_,
      use_agenda = "no",
      generate_context = FALSE,
      enable_llm_correction = FALSE,
      llm_provider = NULL
    ),
    regexp = "Invalid `target_dir`"
  )
})

test_that("workflow errors when stt_audio_dir is a file", {
  withr::with_tempdir({
    target_dir <- "."
    source_audio <- file.path(target_dir, "source.wav")
    stt_audio_dir <- file.path(target_dir, "audio_to_transcribe")
    dir.create(file.path(target_dir, "transcription_output_data"))
    file.create(source_audio)
    writeLines("not a directory", stt_audio_dir)

    expect_error(
      speech_to_summary_workflow(
        target_dir = target_dir,
        source_audio = source_audio,
        split_audio = FALSE,
        overwrite_stt_audio = TRUE,
        stt_audio_dir = stt_audio_dir,
        transcription_output_dir = file.path(
          target_dir,
          "transcription_output_data"
        ),
        stt_model = "mock_stt_model",
        use_agenda = "no",
        generate_context = FALSE,
        llm_provider = "mock_provider",
        formatted_output_file = file.path(target_dir, "event_summary.txt"),
        enable_llm_correction = FALSE
      ),
      regexp = "Destination path for speech-to-text is not a directory"
    )
  })
})

test_that("workflow forwards overwrite to LLM correction", {
  withr::with_tempdir({
    target_dir <- "."

    dir.create(file.path(target_dir, "audio_to_transcribe"))
    dir.create(file.path(target_dir, "transcription_output_data"))

    file.create(file.path(target_dir, "audio_to_transcribe", "dummy.wav"))

    overwrite_seen <- NULL

    testthat::local_mocked_bindings(
      perform_speech_to_text = function(audio_path, output_dir, model, ...) {
        file.create(file.path(output_dir, "dummy_stt_output.json"))
        invisible(NULL)
      },
      apply_llm_correction = function(..., overwrite) {
        overwrite_seen <<- overwrite
        invisible(NULL)
      },
      parse_transcript_json = function(transcription_output_dir, ...) {
        dplyr::tibble(
          text = "mocked transcript text",
          start_time = 0,
          end_time = 1000,
          speaker = "SPEAKER_00"
        )
      },
      summarise_transcript = function(transcript_data, ...) {
        "mock summary content"
      },
      get_prompts = function(...) "mock prompt"
    )

    withr::with_options(
      list(
        minutemaker_stt_model = "mock_stt_model",
        llmr_llm_provider = "mock_provider",
        minutemaker_event_start_time = NULL,
        minutemaker_include_llm_reasoning = FALSE
      ),
      {
        speech_to_summary_workflow(
          target_dir = target_dir,
          source_audio = NULL,
          split_audio = FALSE,
          overwrite_stt_audio = FALSE,
          stt_audio_dir = file.path(target_dir, "audio_to_transcribe"),
          transcription_output_dir = file.path(
            target_dir,
            "transcription_output_data"
          ),
          stt_model = "mock_stt_model_arg",
          overwrite_transcription_files = TRUE,
          enable_llm_correction = TRUE,
          transcript_file = file.path(target_dir, "transcript.csv"),
          overwrite_transcript = TRUE,
          external_transcript = NULL,
          chat_file = NULL,
          use_agenda = "no",
          generate_context = FALSE,
          llm_provider = "mock_provider_arg",
          formatted_output_file = file.path(target_dir, "event_summary.txt"),
          overwrite_formatted_output = TRUE,
          overwrite_summary_tree = TRUE
        )
      }
    )

    expect_identical(overwrite_seen, TRUE)
  })
})

test_that("workflow warns when using deprecated stt_output_dir", {
  withr::with_tempdir({
    target_dir <- "."
    deprecated_output_dir <- "deprecated_transcription_output_data"
    output_dir_seen <- NULL

    dir.create(file.path(target_dir, "audio_to_transcribe"))
    dir.create(file.path(target_dir, "transcription_output_data"))
    dir.create(deprecated_output_dir)

    file.create(file.path(target_dir, "audio_to_transcribe", "dummy.wav"))

    testthat::local_mocked_bindings(
      perform_speech_to_text = function(audio_path, output_dir, model, ...) {
        output_dir_seen <<- output_dir
        file.create(file.path(output_dir, "dummy_stt_output.json"))
        invisible(NULL)
      },
      apply_llm_correction = function(...) invisible(NULL),
      parse_transcript_json = function(transcription_output_dir, ...) {
        dplyr::tibble(
          text = "mocked transcript text",
          start_time = 0,
          end_time = 1000,
          speaker = "SPEAKER_00"
        )
      },
      summarise_transcript = function(transcript_data, ...) {
        "mock summary content"
      },
      get_prompts = function(...) "mock prompt"
    )

    withr::with_options(
      list(
        minutemaker_stt_model = "mock_stt_model",
        llmr_llm_provider = "mock_provider",
        minutemaker_event_start_time = NULL,
        minutemaker_include_llm_reasoning = FALSE
      ),
      {
        expect_warning(
          speech_to_summary_workflow(
            target_dir = target_dir,
            source_audio = NULL,
            split_audio = FALSE,
            overwrite_stt_audio = FALSE,
            stt_audio_dir = file.path(target_dir, "audio_to_transcribe"),
            stt_output_dir = "deprecated_transcription_output_data",
            stt_model = "mock_stt_model_arg",
            overwrite_transcription_files = TRUE,
            enable_llm_correction = FALSE,
            transcript_file = file.path(target_dir, "transcript.csv"),
            overwrite_transcript = TRUE,
            external_transcript = NULL,
            chat_file = NULL,
            use_agenda = "no",
            generate_context = FALSE,
            llm_provider = "mock_provider_arg",
            formatted_output_file = file.path(target_dir, "event_summary.txt"),
            overwrite_formatted_output = TRUE,
            overwrite_summary_tree = TRUE
          ),
          regexp = "stt_output_dir"
        )
      }
    )

    expect_identical(output_dir_seen, deprecated_output_dir)
  })
})

test_that("workflow sets generate_initial_prompt correctly based on external transcript", {
  withr::with_tempdir({
    target_dir <- "."
    dir.create(file.path(target_dir, "documentation"))
    writeLines("Test material", file.path(target_dir, "documentation", "test.txt"))

    # Track generate_context calls
    generate_context_calls <- list()

    spy_generate_context <- function(..., generate_initial_prompt) {
      generate_context_calls <<- append(
        generate_context_calls,
        list(list(generate_initial_prompt = generate_initial_prompt))
      )
      # Return empty context to avoid file generation
      list()
    }

    testthat::local_mocked_bindings(
      generate_context = spy_generate_context,
      perform_speech_to_text = function(...) {
        file.create(file.path(
          "transcription_output_data",
          "dummy_stt_output.json"
        ))
        invisible(NULL)
      },
      apply_llm_correction = function(...) invisible(NULL),
      parse_transcript_json = function(...) {
        dplyr::tibble(
          text = "mocked transcript text",
          start_time = 0,
          end_time = 1000,
          speaker = "SPEAKER_00"
        )
      },
      summarise_transcript = function(...) "mock summary content",
      get_prompts = function(...) "mock prompt",
      .package = "minutemaker"
    )

    # Test 1: No external transcript -> generate_initial_prompt should be TRUE
    withr::with_options(
      list(
        minutemaker_stt_model = "mock_stt_model",
        llmr_llm_provider = "mock_provider",
        minutemaker_event_start_time = NULL
      ),
      {
        generate_context_calls <- list()
        dir.create(file.path(target_dir, "audio_to_transcribe"))
        dir.create(file.path(target_dir, "transcription_output_data"))
        file.create(file.path(target_dir, "audio_to_transcribe", "dummy.wav"))

        speech_to_summary_workflow(
          target_dir = target_dir,
          source_audio = NULL,
          split_audio = FALSE,
          overwrite_stt_audio = FALSE,
          stt_audio_dir = file.path(target_dir, "audio_to_transcribe"),
          transcription_output_dir = file.path(
            target_dir,
            "transcription_output_data"
          ),
          stt_model = "mock_stt_model",
          overwrite_transcription_files = TRUE,
          enable_llm_correction = FALSE,
          transcript_file = file.path(target_dir, "transcript.csv"),
          overwrite_transcript = TRUE,
          external_transcript = NULL, # No external transcript
          chat_file = NULL,
          generate_context = TRUE,
          use_agenda = "no",
          llm_provider = "mock_provider",
          formatted_output_file = file.path(target_dir, "event_summary.txt"),
          overwrite_formatted_output = TRUE
        )

        expect_length(generate_context_calls, 1)
        expect_true(generate_context_calls[[1]]$generate_initial_prompt)
      }
    )

    # Test 2: External transcript exists -> generate_initial_prompt should be FALSE
    withr::with_options(
      list(
        minutemaker_stt_model = "mock_stt_model",
        llmr_llm_provider = "mock_provider",
        minutemaker_event_start_time = NULL
      ),
      {
        generate_context_calls <- list()
        vtt_path <- file.path(target_dir, "transcript.vtt")
        writeLines(
          c("WEBVTT", "", "00:00:01.000 --> 00:00:02.000", "Hello world"),
          vtt_path
        )

        speech_to_summary_workflow(
          target_dir = target_dir,
          source_audio = NULL,
          split_audio = FALSE,
          overwrite_stt_audio = FALSE,
          stt_audio_dir = file.path(target_dir, "audio_to_transcribe"),
          transcription_output_dir = file.path(
            target_dir,
            "transcription_output_data"
          ),
          stt_model = "mock_stt_model",
          overwrite_transcription_files = TRUE,
          enable_llm_correction = FALSE,
          transcript_file = file.path(target_dir, "transcript.csv"),
          overwrite_transcript = TRUE,
          external_transcript = vtt_path, # External transcript exists
          chat_file = NULL,
          generate_context = TRUE,
          use_agenda = "no",
          llm_provider = "mock_provider",
          formatted_output_file = file.path(target_dir, "event_summary.txt"),
          overwrite_formatted_output = TRUE
        )

        expect_length(generate_context_calls, 1)
        expect_false(generate_context_calls[[1]]$generate_initial_prompt)
      }
    )
  })
})
