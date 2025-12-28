# Tests for perform_speech_to_text() -----------------------------------------

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
    perform_speech_to_text(
      "test.wav",
      model = "whisper_local",
      overwrite = FALSE
    )
    expect_equal(model_calls, 1)

    # Third run with overwrite=TRUE SHOULD call the model again
    perform_speech_to_text(
      "test.wav",
      model = "whisper_local",
      overwrite = TRUE
    )
    expect_equal(model_calls, 2)
  })
})

test_that("use_parakeet_mlx_stt accepts NULL text when sentences are present", {
  skip_if_not_installed("jsonlite")

  strip_quotes <- function(x) {
    gsub("^['\"]|['\"]$", "", x)
  }

  audio_file <- tempfile(fileext = ".wav")
  write_dummy_wav(audio_file)

  local_mocked_bindings(
    Sys.which = function(command) {
      if (identical(command, "parakeet-mlx")) {
        return("/fake/parakeet-mlx")
      }
      base::Sys.which(command)
    },
    system2 = function(command, args, stdout = "", stderr = "", ...) {
      audio_arg <- strip_quotes(args[1])
      output_idx <- which(args == "--output-dir")
      output_dir <- strip_quotes(args[output_idx + 1])
      output_name <- paste0(
        tools::file_path_sans_ext(basename(audio_arg)),
        ".json"
      )
      output_path <- file.path(output_dir, output_name)

      jsonlite::write_json(
        list(
          text = NULL,
          sentences = list(
            list(
              id = 0,
              start = 0,
              end = 1,
              text = "Segment text"
            )
          )
        ),
        output_path,
        auto_unbox = TRUE,
        null = "null"
      )

      if (nzchar(stdout)) {
        file.create(stdout)
      }
      if (nzchar(stderr)) {
        file.create(stderr)
      }

      0
    },
    .package = "base"
  )

  result <- use_parakeet_mlx_stt(audio_file = audio_file)

  expect_equal(result$text, "")
  expect_equal(length(result$segments), 1)
  expect_equal(result$segments[[1]]$text, "Segment text")
})

# Tests for use_openai_whisper_stt() --------------------------------------

test_that("use_openai_whisper_stt stops after max retries", {
  withr::with_tempdir({
    audio_path <- write_dummy_wav("dummy.wav")
    call_count <- 0

    local_mocked_bindings(
      POST = function(...) {
        call_count <<- call_count + 1
        list(status_code = 429)
      },
      content = function(x, type = NULL, ...) {
        if (!is.null(type) && type == "text") {
          return("temporarily unable to process")
        }
        list()
      },
      upload_file = function(path, ...) path,
      .package = "httr"
    )

    local_mocked_bindings(
      Sys.sleep = function(...) invisible(NULL),
      .package = "base"
    )

    {
      use_openai_whisper_stt(
        audio_file = audio_path,
        language = "en",
        api_key = "key",
        max_retries = 1
      )
    } |>
      expect_warning(
        "Error in OpenAI Whisper API request: temporarily unable to process",
        fixed = TRUE
      ) |>
      expect_warning(
        "Error in OpenAI Whisper API request: temporarily unable to process",
        fixed = TRUE
      ) |>
      expect_error("Max retries reached", fixed = TRUE)
    expect_equal(call_count, 2)
  })
})
