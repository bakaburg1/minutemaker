# Tests for perform_speech_to_text() -----------------------------------------

test_that("throws error for invalid path", {
  expect_error(
    perform_speech_to_text("nonexistent/file.wav"),
    "is not a valid file or folder",
    fixed = TRUE
  )
})

test_that("throws error when directory has no audio files", {
  withr::with_tempdir({
    dir.create("empty_dir")

    expect_error(
      perform_speech_to_text("empty_dir"),
      "No audio files found",
      fixed = TRUE
    )
  })
})

# -------------------------------------------------------------------------
# Setup and Helper Functions ----------------------------------------------
# -------------------------------------------------------------------------

# Helper to create a dummy WAV file for tests that need a file to exist
write_dummy_wav <- function(path) {
  # This doesn't need to be a valid WAV, just a file.
  writeBin(raw(10), path)
  invisible(path)
}

# -------------------------------------------------------------------------
# Tests for perform_speech_to_text() --------------------------------------
# -------------------------------------------------------------------------

test_that("perform_speech_to_text handles invalid paths and empty dirs", {
  expect_error(
    perform_speech_to_text("nonexistent/file.wav"),
    "is not a valid file or folder"
  )
  withr::with_tempdir({
    dir.create("empty_dir")
    expect_error(
      perform_speech_to_text("empty_dir"),
      "No audio files found"
    )
  })
})

test_that("perform_speech_to_text processes files and handles overwrite", {
  skip_if_not_installed("jsonlite")
  dummy_json <- list(text = "test", segments = list())
    model_calls <- 0

  # Mock the STT model to track calls and return dummy data
    local_mocked_bindings(
    use_whisper_local_stt = function(...) {
        model_calls <<- model_calls + 1
        dummy_json
      },
    # Mock the final parsing step
    parse_transcript_json = function(...) "parsed",
      .package = "minutemaker"
    )

  withr::with_tempdir({
    # Create a dummy audio file
    write_dummy_wav("test.wav")

    # First run should call the model once
    perform_speech_to_text("test.wav", model = "whisper_local")
    expect_equal(model_calls, 1)
    expect_true(file.exists("transcription_output_data/test.json"))

    # Second run with overwrite=FALSE should NOT call the model
    perform_speech_to_text("test.wav", model = "whisper_local", overwrite = FALSE)
    expect_equal(model_calls, 1)

    # Third run with overwrite=TRUE SHOULD call the model again
    perform_speech_to_text("test.wav", model = "whisper_local", overwrite = TRUE)
    expect_equal(model_calls, 2)
  })
})
