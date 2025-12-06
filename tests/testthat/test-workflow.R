# Helper functions ---

# Tests for speech_to_summary_workflow() ---

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
      parse_transcript_json = function(stt_output_dir, ...) {
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
            stt_output_dir = file.path(target_dir, "transcription_output_data"),
            stt_model = "mock_stt_model_arg",
            # To ensure perform_speech_to_text mock is called
            stt_overwrite_output_files = TRUE,
            enable_llm_correction = FALSE,
            transcript_file = file.path(target_dir, "transcript.csv"),
            overwrite_transcript = TRUE,
            transcript_to_merge = NULL,
            chat_file = NULL,
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
