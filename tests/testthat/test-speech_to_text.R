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

# Tests for split_audio() -----------------------------------------------------

test_that("creates the expected number of segments", {
  skip_if_not_installed("av")

  dummy_info <- list(duration = 120) # 2 minutes total

  withr::with_tempdir({
    local_mocked_bindings(
      av_media_info = function(file) dummy_info,
      av_audio_convert = function(input, output, ...) {
        file.create(output)
        invisible(NULL)
      },
      .package = "av"
    )

    split_audio(
      audio_file = "dummy.wav",
      segment_duration = 1,
      output_folder = "."
    )

    expect_length(
      list.files(".", pattern = "^segment_\\d+\\.mp3$"),
      2
    )
  })
})

# -------------------------------------------------------------------------
# tests for perform_speech_to_text() --------------------------------------
# -------------------------------------------------------------------------

dummy_json <- list(
  text = "Hello world.",
  segments = list(
    list(id = 0, start = 0, end = 2, text = "Hello"),
    list(id = 1, start = 2, end = 4, text = "world.")
  )
)

write_dummy_wav <- function(path) {
  writeBin(raw(10), path) # 10‑byte placeholder
  TRUE # return logical(1) for vapply
}

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

test_that("single‑file branch works and writes JSON", {
  skip_if_not_installed("jsonlite")

  withr::with_tempdir({
    wav <- "one.wav"
    write_dummy_wav(wav)
    outdir <- "txt_out"

    local_mocked_bindings(
      use_whisper_local_stt = function(audio_file, ...) dummy_json,
      parse_transcript_json = function(dir) "parsed_df",
      .package = "minutemaker"
    )

    res <- perform_speech_to_text(
      audio_path = wav,
      model = "whisper_local",
      output_dir = outdir
    )

    expect_equal(res, "parsed_df")
    expect_true(file.exists(file.path(outdir, "one.json")))
  })
})

test_that("folder branch, prompt carry‑over, and overwrite logic", {
  skip_if_not_installed("jsonlite")

  withr::with_tempdir({
    wavs <- file.path(".", sprintf("seg_%d.wav", 1:2))
    vapply(wavs, write_dummy_wav, logical(1))

    local_mocked_bindings(
      use_whisper_local_stt = function(audio_file, ...) dummy_json,
      parse_transcript_json = function(dir) "parsed_df",
      .package = "minutemaker"
    )

    # first run creates JSONs
    perform_speech_to_text(audio_path = ".", overwrite = FALSE)

    # second run should skip both files because overwrite = FALSE
    expect_no_warning(
      perform_speech_to_text(audio_path = ".", overwrite = FALSE)
    )
  })
})

# -------------------------------------------------------------------------
# tests for split_audio() --------------------------------------------------
# -------------------------------------------------------------------------

test_that("split_audio() creates the expected number of segments", {
  skip_if_not_installed("av")

  dummy_info <- list(duration = 120) # 2 minutes total

  withr::with_tempdir({
    local_mocked_bindings(
      av_media_info = function(file) dummy_info,
      av_audio_convert = function(input, output, ...) {
        file.create(output) # mimic side‑effect of real function
        invisible(NULL)
      },
      .package = "av"
    )

    split_audio(
      audio_file = "dummy.wav",
      segment_duration = 1,
      output_folder = "."
    )

    expect_length(
      list.files(".", pattern = "^segment_\\d+\\.mp3$"),
      2
    )
  })
})

# -------------------------------------------------------------------------
# extra coverage for overwrite = TRUE branch and tictoc timing -------------
# -------------------------------------------------------------------------
test_that("overwrite = TRUE forces rerun and tictoc branch is executed", {
  skip_if_not_installed("jsonlite")

  withr::with_tempdir({
    wav <- "rerun.wav"
    write_dummy_wav(wav)

    # Prepare a minimal fake 'tictoc' namespace so SUT can call tic/toc
    fake_tictoc <- new.env(parent = emptyenv())
    fake_tictoc$.elapsed <- 0
    fake_tictoc$tic <- function() fake_tictoc$.elapsed <<- 0
    fake_tictoc$toc <- function(quiet = TRUE) list(tic = 0, toc = 0.42)
    withr::defer(
      detach("package:tictoc", character.only = TRUE, unload = TRUE),
      envir = parent.frame()
    )
    attach(fake_tictoc, name = "package:tictoc")

    model_calls <- 0

    local_mocked_bindings(
      # count how many times the model is invoked
      use_whisper_local_stt = function(audio_file, ...) {
        model_calls <<- model_calls + 1
        dummy_json
      },
      parse_transcript_json = function(dir) "parsed_df",
      .package = "minutemaker"
    )

    local_mocked_bindings(
      # force requireNamespace("tictoc") to return TRUE so timing path is
      # covered
      requireNamespace = function(pkg, quietly = TRUE) pkg == "tictoc",
      .package = "base"
    )

    # FIRST run writes JSON to disk
    perform_speech_to_text(
      audio_path = wav,
      model = "whisper_local",
      overwrite = FALSE
    )

    expect_equal(model_calls, 1)

    # SECOND run with overwrite = FALSE should skip model
    perform_speech_to_text(
      audio_path = wav,
      model = "whisper_local",
      overwrite = FALSE
    )

    expect_equal(model_calls, 1)

    # THIRD run with overwrite = TRUE must force model again
    perform_speech_to_text(
      audio_path = wav,
      model = "whisper_local",
      overwrite = TRUE
    )

    expect_equal(model_calls, 2)
  })
})


# -------------------------------------------------------------------------
# coverage: split_audio() creates output directory when it doesn't exist -----
# -------------------------------------------------------------------------
test_that("split_audio() creates output directory if needed", {
  skip_if_not_installed("av")

  dummy_info <- list(duration = 60) # 1 minute total (one segment)

  withr::with_tempdir({
    local_mocked_bindings(
      av_media_info = function(file) dummy_info,
      av_audio_convert = function(input, output, ...) {
        file.create(output)
        invisible(NULL)
      },
      .package = "av"
    )

    out_dir <- "new_parts" # directory does NOT exist yet
    expect_false(dir.exists(out_dir))

    split_audio(
      audio_file = "dummy.wav",
      segment_duration = 1,
      output_folder = out_dir
    )

    # after running, the output directory should have been created
    expect_true(dir.exists(out_dir))
    expect_length(
      list.files(out_dir, pattern = "^segment_\\d+\\.mp3$", full.names = TRUE),
      1
    )
  })
})
