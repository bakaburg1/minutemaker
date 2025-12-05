# This test file focuses on audio processing functions, including both
# unit tests and integration tests for split_audio(), is_audio_file_sane(),
# create_and_validate_segment(), and their dependencies like av and mirai.

# -------------------------------------------------------------------------
# Setup and Helper Functions ----------------------------------------------
# -------------------------------------------------------------------------

# These tests spin up and tear down mirai daemons in temp directories; you may
# see shell-init/getcwd warnings like "error retrieving current directory".
# They come from daemons shutting down after temp dirs are removed and are
# harmless in this test context.
cli::cli_alert_info(
  "Audio processing tests start/stop mirai daemons; shell-init/getcwd warnings are expected and harmless here."
)

# Helper function to create test audio files
helper_create_test_audio <- function(path, valid = TRUE) {
  if (valid) {
    # Create a valid 2-second silent audio file using ffmpeg
    system2(
      "ffmpeg",
      c("-f", "lavfi", "-i", "anullsrc=r=44100:cl=mono", "-t", "2", path),
      stdout = FALSE,
      stderr = FALSE
    )
  } else {
    # Create a corrupted file by writing text
    writeLines(c("This is not", "an audio file"), path)
  }
  invisible(path)
}

# -------------------------------------------------------------------------
# Integration Tests for split_audio() -------------------------------------
# -------------------------------------------------------------------------

test_that("split_audio works sequentially with real audio", {
  skip_if_not_installed("av")
  skip_on_cran()

  withr::with_tempdir({
    # Create a valid 2-second audio file
    audio_file <- "test.wav"
    helper_create_test_audio(audio_file, valid = TRUE)

    # Verify it was created and is valid
    expect_true(file.exists(audio_file))
    expect_true(is_audio_file_sane(audio_file))

    # Split into 1-second segments (should create 2 segments)
    split_audio(
      audio_file = audio_file,
      segment_duration = 1 / 60, # 1 second in minutes
      output_folder = "segments",
      parallel = FALSE
    )

    # Check results
    segments <- list.files("segments", pattern = "\\.mp3$", full.names = TRUE)
    expect_length(segments, 2)

    # Verify segments are valid
    sane_checks <- purrr::map_lgl(segments, is_audio_file_sane)
    expect_true(all(sane_checks))
  })
})

test_that("split_audio works in parallel with real audio", {
  skip_if_not_installed(c("av", "mirai"))
  skip_on_cran()

  withr::with_tempdir({
    # Clean up any stray daemons from previous tests to avoid shell-init noise
    if (mirai::status(.compute = "minutemaker")$daemons > 0) {
      mirai::daemons(0, .compute = "minutemaker")
    }

    # Set up mirai daemons and ensure cleanup
    withr::defer({
      if (mirai::status(.compute = "minutemaker")$daemons > 0) {
        mirai::daemons(0, .compute = "minutemaker")
      }
    })

    # Create a valid 2-second audio file
    audio_file <- "test.wav"
    helper_create_test_audio(audio_file, valid = TRUE)

    # Verify it was created and is valid
    expect_true(file.exists(audio_file))
    expect_true(is_audio_file_sane(audio_file))

    # With explicit function passing, parallel processing should now work
    expect_no_error(
      split_audio(
        audio_file = audio_file,
        segment_duration = 1 / 60, # 1 second in minutes
        output_folder = "segments",
        parallel = TRUE
      )
    )

    # Check that segments were created
    segments <- list.files("segments", pattern = "\\.mp3$", full.names = TRUE)
    expect_length(segments, 2)

    # Verify segments are valid
    sane_checks <- purrr::map_lgl(segments, is_audio_file_sane)
    expect_true(all(sane_checks))
  })
})

test_that("split_audio handles corrupted source file properly", {
  skip_if_not_installed(c("av", "mirai"))
  skip_on_cran()

  withr::with_tempdir({
    # Create a corrupted audio file
    audio_file <- "corrupt.wav"
    helper_create_test_audio(audio_file, valid = FALSE)

    # Verify it's corrupted
    expect_true(file.exists(audio_file))
    expect_false(is_audio_file_sane(audio_file))

    # Should fail when trying to get media info
    expect_error(
      split_audio(
        audio_file = audio_file,
        segment_duration = 1 / 60,
        output_folder = "segments",
        parallel = TRUE
      ),
      "Invalid data found when processing input"
    )
  })
})

test_that("parallel processing works with proper worker setup", {
  skip_if_not_installed(c("av", "mirai"))
  skip_on_cran()
  # This test demonstrates that parallel processing works when
  # the package is properly available to workers

  withr::with_tempdir({
    # Set up daemons with package access and ensure cleanup
    withr::defer({
      if (mirai::status(.compute = "test_workers")$daemons > 0) {
        mirai::daemons(0, .compute = "test_workers")
      }
    })

    # Start daemons that have the package loaded
    mirai::daemons(2, .compute = "test_workers")

    # Create a valid 2-second audio file
    audio_file <- "test.wav"
    helper_create_test_audio(audio_file, valid = TRUE)

    # Test that extract_audio_segment works in isolation
    # (this simulates what the parallel version should do)
    expect_no_error({
      segment_path <- extract_audio_segment(
        audio_file = audio_file,
        output_file = "segments/segment_1.mp3",
        start_time = 0,
        duration = 60,
        verbose = FALSE
      )
    })

    # Verify the segment was created and is valid
    expect_true(file.exists(segment_path))
    expect_true(is_audio_file_sane(segment_path))
  })
})

test_that("parallel timeout does not stop external daemons", {
  skip_if_not_installed(c("av", "mirai"))

  withr::with_tempdir({
    withr::local_options(
      list(minutemaker_split_audio_parallel_timeout = 0)
    )

    file.create("fake.wav")

    call_log <- list(stop = 0L, start = 0L)

    testthat::local_mocked_bindings(
      av_media_info = function(...) list(duration = 90),
      .package = "av"
    )
    testthat::local_mocked_bindings(
      status = function(...) list(daemons = 2),
      daemons = function(n, ...) {
        if (identical(n, 0)) {
          call_log$stop <<- call_log$stop + 1L
        } else {
          call_log$start <<- call_log$start + 1L
        }
        invisible(NULL)
      },
      mirai = function(...) "task",
      unresolved = function(tasks) rep(TRUE, length(tasks)),
      is_error_value = function(...) FALSE,
      .package = "mirai"
    )
    testthat::local_mocked_bindings(
      extract_audio_segment = function(...) "segment_path",
      is_audio_file_sane = function(...) TRUE,
      .package = "minutemaker"
    )

    split_audio(
      audio_file = "fake.wav",
      segment_duration = 1,
      output_folder = "segments",
      parallel = TRUE
    )

    expect_identical(call_log$start, 0L)
    expect_identical(call_log$stop, 0L)
  })
})

test_that("parallel workers receive helper bindings", {
  skip_if_not_installed(c("av", "mirai"))
  skip_on_cran()

  withr::with_tempdir({
    # Ensure we start and stop with a clean pool of daemons
    withr::defer({
      if (mirai::status(.compute = "minutemaker")$daemons > 0) {
        mirai::daemons(0, .compute = "minutemaker")
      }
    })

    # Minimal mocks to avoid real audio/ffmpeg work
    local_mocked_bindings(
      av_media_info = function(...) list(duration = 60),
      .package = "av"
    )
    local_mocked_bindings(
      detectCores = function() 2,
      .package = "parallel"
    )
    local_mocked_bindings(
      is_audio_file_sane = function(...) TRUE,
      .package = "minutemaker"
    )

    # Worker that needs is_audio_file_sane but has an empty environment,
    # so it relies on the binding being injected into the worker session.
    worker <- function(...) is_audio_file_sane("dummy.wav")
    environment(worker) <- emptyenv()
    local_mocked_bindings(
      extract_audio_segment = worker,
      .package = "minutemaker"
    )

    expect_no_error(
      split_audio(
        audio_file = "dummy.wav",
        segment_duration = 1,
        output_folder = "segments",
        parallel = TRUE
      )
    )
  })
})

# -------------------------------------------------------------------------
# Individual Function Tests -----------------------------------------------
# -------------------------------------------------------------------------

test_that("is_audio_file_sane detects valid audio files", {
  skip_on_cran()

  withr::with_tempdir({
    # Create a valid audio file
    audio_file <- "valid.wav"
    helper_create_test_audio(audio_file, valid = TRUE)

    expect_true(file.exists(audio_file))
    expect_true(is_audio_file_sane(audio_file))
  })
})

test_that("is_audio_file_sane detects corrupted audio files", {
  skip_on_cran()

  withr::with_tempdir({
    # Create a corrupted file
    corrupt_file <- "corrupt.wav"
    helper_create_test_audio(corrupt_file, valid = FALSE)

    expect_true(file.exists(corrupt_file))
    expect_false(is_audio_file_sane(corrupt_file))
  })
})

test_that("is_audio_file_sane handles non-existent files", {
  expect_false(is_audio_file_sane("non_existent_file.wav"))
})

test_that("is_audio_file_sane handles paths with spaces and special chars", {
  withr::with_tempdir({
    audio_path <- file.path(getwd(), "path with space$special.wav")
    writeLines("dummy", audio_path)

    local_mocked_bindings(
      system2 = function(command, args, stdout = FALSE, stderr = FALSE, ...) {
        target_idx <- which(args == "-i") + 1L
        target <- args[target_idx]
        if (file.exists(target)) {
          return(0L)
        }
        return(1L)
      },
      .package = "base"
    )

    expect_true(is_audio_file_sane(audio_path))
  })
})

test_that("extract_audio_segment creates valid segments", {
  skip_if_not_installed("av")
  skip_on_cran()

  withr::with_tempdir({
    # Create a valid 6-second audio file (enough for 2 segments of 2 seconds each)
    audio_file <- "test.wav"
    system2(
      "ffmpeg",
      c("-f", "lavfi", "-i", "anullsrc=r=44100:cl=mono", "-t", "6", audio_file),
      stdout = FALSE,
      stderr = FALSE
    )

    expect_true(is_audio_file_sane(audio_file))

    # Create first segment (0-2 seconds)
    segment_path <- extract_audio_segment(
      audio_file = audio_file,
      output_file = "segments/segment_1.mp3",
      start_time = 0,
      duration = 2,
      verbose = FALSE
    )

    # Verify segment was created and is valid
    expect_true(file.exists(segment_path))
    expect_true(is_audio_file_sane(segment_path))
    expect_match(segment_path, "segment_1\\.mp3$")

    # Create second segment (2-4 seconds)
    segment_path_2 <- extract_audio_segment(
      audio_file = audio_file,
      output_file = "segments/segment_2.mp3",
      start_time = 2,
      duration = 2,
      verbose = FALSE
    )

    # Verify second segment was created and is valid
    expect_true(file.exists(segment_path_2))
    expect_true(is_audio_file_sane(segment_path_2))
    expect_match(segment_path_2, "segment_2\\.mp3$")
  })
})

test_that("extract_audio_segment fails with corrupted source file", {
  skip_if_not_installed("av")
  skip_on_cran()

  withr::with_tempdir({
    # Create a corrupted source file
    corrupt_file <- "corrupt.wav"
    helper_create_test_audio(corrupt_file, valid = FALSE)

    expect_false(is_audio_file_sane(corrupt_file))

    # Should fail when trying to process corrupted source (av throws the error first)
    expect_error(
      extract_audio_segment(
        audio_file = corrupt_file,
        output_file = "segments/segment_1.mp3",
        start_time = 0,
        duration = 60,
        verbose = FALSE
      ),
      "Invalid data found when processing input|Failed to create a valid segment.*after 2 attempts"
    )
  })
})

test_that("extract_audio_segment retry logic works", {
  skip_if_not_installed("av")
  skip_on_cran()

  withr::with_tempdir({
    # Create a valid audio file
    audio_file <- "test.wav"
    helper_create_test_audio(audio_file, valid = TRUE)

    expect_true(is_audio_file_sane(audio_file))

    # Mock is_audio_file_sane to fail first attempt, succeed second
    call_count <- 0
    testthat::local_mocked_bindings(
      is_audio_file_sane = function(file) {
        call_count <<- call_count + 1
        # Fail first attempt, succeed second
        return(call_count > 1)
      }
    )

    # Should succeed after retry (with warning about first attempt)
    expect_warning(
      segment_path <- extract_audio_segment(
        audio_file = audio_file,
        output_file = "segments/segment_1.mp3",
        start_time = 0,
        duration = 60,
        verbose = TRUE
      ),
      "Generated segment.*is corrupted.*Attempt .*failed, retrying"
    )

    # Should return the path even though our mock validation "failed" first time
    expect_match(segment_path, "segment_1\\.mp3$")
    expect_equal(call_count, 2) # Verify retry happened
  })
})

test_that("extract_audio_segment fails after max attempts", {
  skip_if_not_installed("av")
  skip_on_cran()

  withr::with_tempdir({
    # Create a valid audio file
    audio_file <- "test.wav"
    helper_create_test_audio(audio_file, valid = TRUE)

    expect_true(is_audio_file_sane(audio_file))

    # Mock is_audio_file_sane to always fail
    testthat::local_mocked_bindings(
      is_audio_file_sane = function(file) FALSE
    )

    # Should fail after 2 attempts
    expect_error(
      extract_audio_segment(
        audio_file = audio_file,
        output_file = "segments/segment_1.mp3",
        start_time = 0,
        duration = 60,
        verbose = FALSE
      ),
      "Failed to create a valid segment.*after 2 attempts"
    )
  })
})

test_that("split_audio parallel mode validates source file when workers fail", {
  skip_if_not_installed(c("av", "mirai"))
  skip_on_cran()

  withr::with_tempdir({
    # Set up mirai daemons and ensure cleanup
    withr::defer({
      if (mirai::status(.compute = "minutemaker")$daemons > 0) {
        mirai::daemons(0, .compute = "minutemaker")
      }
    })

    # Create a valid audio file
    audio_file <- "test.wav"
    helper_create_test_audio(audio_file, valid = TRUE)

    expect_true(is_audio_file_sane(audio_file))

    # Mock extract_audio_segment to always fail
    testthat::local_mocked_bindings(
      extract_audio_segment = function(...) {
        cli::cli_abort("Simulated worker failure")
      }
    )

    # Should check source file and report it's OK when workers fail
    expect_error(
      split_audio(
        audio_file = audio_file,
        segment_duration = 1 / 60,
        output_folder = "segments",
        parallel = TRUE
      ),
      "A segment could not be generated correctly, but the source file appears to be OK"
    )
  })
})

test_that("split_audio parallel mode detects corrupted source when workers fail", {
  skip_if_not_installed(c("av", "mirai"))
  skip_on_cran()

  withr::with_tempdir({
    # Set up mirai daemons and ensure cleanup
    withr::defer({
      if (mirai::status(.compute = "minutemaker")$daemons > 0) {
        mirai::daemons(0, .compute = "minutemaker")
      }
    })

    # Create a valid audio file first
    audio_file <- "test.wav"
    helper_create_test_audio(audio_file, valid = TRUE)

    # Simple mocking: make workers fail and source appear corrupted
    testthat::local_mocked_bindings(
      extract_audio_segment = function(...) {
        cli::cli_abort("Simulated worker failure")
      },
      is_audio_file_sane = function(file) {
        # Always return FALSE to simulate corruption detected
        FALSE
      }
    )

    # Should detect source corruption when workers fail
    expect_error(
      split_audio(
        audio_file = audio_file,
        segment_duration = 1 / 60,
        output_folder = "segments",
        parallel = TRUE
      ),
      "The source audio file.*appears to be corrupted"
    )
  })
})

# -------------------------------------------------------------------------
# Unit Tests (Mocked Dependencies) ----------------------------------------
# -------------------------------------------------------------------------

# Helper to create a dummy WAV file for tests that need a file to exist
write_dummy_wav <- function(path) {
  # This doesn't need to be a valid WAV, just a file.
  writeBin(raw(10), path)
  invisible(path)
}

test_that("split_audio creates the expected number of segments", {
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
    local_mocked_bindings(
      is_audio_file_sane = function(...) TRUE,
      .package = "minutemaker"
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

test_that("split_audio logs mirai errors with conditionMessage", {
  skip_if_not_installed(c("av", "mirai"))

  withr::with_tempdir({
    write_dummy_wav("dummy.wav")

    condition_called <- FALSE
    assign(
      "conditionMessage.fake_mirai_error",
      function(c) {
        condition_called <<- TRUE
        "worker boom"
      },
      envir = .GlobalEnv
    )
    base::registerS3method(
      "conditionMessage",
      "fake_mirai_error",
      get("conditionMessage.fake_mirai_error", envir = .GlobalEnv)
    )
    withr::defer({
      rm("conditionMessage.fake_mirai_error", envir = .GlobalEnv)
    })
    assign(
      "[.fake_mirai",
      function(x, ...) x$value,
      envir = .GlobalEnv
    )
    withr::defer(rm("[.fake_mirai", envir = .GlobalEnv))

    error_value <- structure(
      list(),
      class = c("fake_mirai_error", "error", "condition")
    )

    log_messages <- character(0)

    local_mocked_bindings(
      av_media_info = function(file) list(duration = 60),
      .package = "av"
    )
    local_mocked_bindings(
      status = function(...) list(daemons = 1),
      daemons = function(...) invisible(NULL),
      mirai = function(...) {
        structure(list(value = error_value), class = "fake_mirai")
      },
      unresolved = function(x) rep(FALSE, length(x)),
      is_error_value = function(x) inherits(x, "condition"),
      .package = "mirai"
    )
    local_mocked_bindings(
      is_audio_file_sane = function(...) TRUE,
      .package = "minutemaker"
    )
    local_mocked_bindings(
      cli_alert_danger = function(msg, ...) {
        log_messages <<- c(log_messages, paste(msg, collapse = " "))
      },
      .package = "cli"
    )

    expect_error(
      split_audio(
        audio_file = "dummy.wav",
        segment_duration = 1,
        output_folder = ".",
        parallel = TRUE
      ),
      "A segment could not be generated correctly"
    )

    expect_true(condition_called)
    expect_true(any(grepl("worker boom", log_messages, fixed = TRUE)))
  })
})

test_that("is_audio_file_sane() correctly identifies sane/corrupt files", {
  # Mock system2 to simulate ffmpeg's exit codes
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      # We identify the corrupted file by a keyword in its path
      if (any(grepl("corrupted", args))) {
        return(1L) # Non-zero status for corrupted
      } else {
        return(0L) # Zero status for sane
      }
    },
    .package = "base"
  )

  expect_true(is_audio_file_sane("path/to/sane_file.mp3"))
  expect_false(is_audio_file_sane("path/to/corrupted_file.mp3"))
})

test_that("extract_audio_segment succeeds on first try", {
  skip_if_not_installed("av")
  withr::with_tempdir({
    # Mock dependencies: av conversion always works, file is always sane
    local_mocked_bindings(
      av_audio_convert = function(input, output, ...) {
        invisible(file.create(output))
      },
      .package = "av"
    )
    local_mocked_bindings(
      is_audio_file_sane = function(...) TRUE,
      .package = "minutemaker"
    )

    # Expect no errors and the output file to be created
    expect_no_error(
      extract_audio_segment(
        "dummy.mp3",
        "segment_1.mp3",
        0,
        60,
        verbose = FALSE
      )
    )
    expect_true(file.exists("segment_1.mp3"))
  })
})

test_that("extract_audio_segment retries and succeeds", {
  skip_if_not_installed("av")
  withr::with_tempdir({
    sane_calls <- 0
    # Mock dependencies: av works, sanity check fails on the first call
    local_mocked_bindings(
      av_audio_convert = function(input, output, ...) {
        invisible(file.create(output))
      },
      .package = "av"
    )
    local_mocked_bindings(
      is_audio_file_sane = function(...) {
        sane_calls <<- sane_calls + 1
        sane_calls > 1 # Fail first time, succeed second time
      },
      .package = "minutemaker"
    )

    # We expect a warning about the first attempt failing
    expect_warning(
      extract_audio_segment("dummy.mp3", "segment_1.mp3", 0, 60),
      "corrupted.*Attempt .*failed, retrying"
    )

    # It should have called the sanity check twice
    expect_equal(sane_calls, 2)
    # The file should still exist
    expect_true(file.exists("segment_1.mp3"))
  })
})

test_that("extract_audio_segment fails after max retries", {
  skip_if_not_installed("av")
  withr::with_tempdir({
    # Mock dependencies: av works, but sanity check ALWAYS fails
    local_mocked_bindings(
      av_audio_convert = function(input, output, ...) {
        invisible(file.create(output))
      },
      .package = "av"
    )
    local_mocked_bindings(
      is_audio_file_sane = function(...) FALSE,
      .package = "minutemaker"
    )

    # It should throw an error after trying twice, with warnings for each retry
    extract_audio_segment("dummy.mp3", "segment_1.mp3", 0, 60) |>
      expect_warning(
        "Generated segment.*is corrupted.*Attempt 1 of 2 failed, retrying"
      ) |>
      expect_warning(
        "Generated segment.*is corrupted.*Attempt 2 of 2 failed\\."
      ) |>
      expect_error("Failed to create a valid segment")
  })
})

test_that("split_audio sequential mode calls worker function correctly", {
  skip_if_not_installed("av")
  withr::with_tempdir({
    worker_calls <- 0
    # Mock the dependencies of split_audio
    local_mocked_bindings(
      av_media_info = function(...) list(duration = 120), # 2 minutes
      .package = "av"
    )
    local_mocked_bindings(
      # We mock the *worker*, not its dependencies.
      extract_audio_segment = function(...) {
        worker_calls <<- worker_calls + 1
      },
      .package = "minutemaker"
    )

    # Split a 2-min file into 1-min segments (=> 2 segments)
    split_audio("dummy.mp3", segment_duration = 1, parallel = FALSE)

    # Expect the worker to have been called twice
    expect_equal(worker_calls, 2)
  })
})

test_that("split_audio parallel mode fail-fast works as expected", {
  skip_if_not_installed(c("av", "mirai"))
  withr::with_tempdir({
    rlang::local_options(cli.default_handler = function(...) invisible(NULL))
    cli::cli_alert_info(
      "Expect worker failure messages below; this test simulates a crash."
    )

    local_mocked_bindings(
      av_media_info = function(...) list(duration = 120),
      .package = "av"
    )
    local_mocked_bindings(
      detectCores = function() 2,
      .package = "parallel"
    )
    local_mocked_bindings(
      extract_audio_segment = function(...) stop("worker failed!"),
      is_audio_file_sane = function(...) TRUE,
      .package = "minutemaker"
    )
    local_mocked_bindings(
      status = function(...) list(daemons = 0),
      daemons = function(...) invisible(),
      mirai = function(expr, ...) {
        env <- list2env(list(...), parent = environment())
        res <- try(eval(substitute(expr), envir = env), silent = TRUE)
        if (inherits(res, "try-error")) {
          return(list(error = TRUE, message = as.character(res)))
        }
        list(error = FALSE, result = res)
      },
      unresolved = function(...) list(),
      is_error_value = function(x) isTRUE(x$error),
      `[.mirai` = function(x, ...) if (isTRUE(x$error)) x else x$result,
      .package = "mirai"
    )

    # Ensure a cli alert is emitted, then the expected error surfaces.
    expect_condition(
      try(
        split_audio("dummy.mp3", segment_duration = 1, parallel = TRUE),
        silent = TRUE
      ),
      class = "cli_message"
    )
    expect_error(
      split_audio("dummy.mp3", segment_duration = 1, parallel = TRUE),
      "intermittent issue"
    )
  })
})

test_that("split_audio parallel mode checks for corrupted source on failure", {
  skip_if_not_installed(c("av", "mirai"))
  withr::with_tempdir({
    rlang::local_options(cli.default_handler = function(...) invisible(NULL))
    cli::cli_alert_info(
      "Expect worker failure messages below; this test simulates a crash with corrupted source."
    )

    local_mocked_bindings(
      av_media_info = function(...) list(duration = 120),
      .package = "av"
    )
    local_mocked_bindings(
      detectCores = function() 2,
      .package = "parallel"
    )
    local_mocked_bindings(
      extract_audio_segment = function(...) stop("worker failed!"),
      is_audio_file_sane = function(...) FALSE,
      .package = "minutemaker"
    )
    local_mocked_bindings(
      status = function(...) list(daemons = 0),
      daemons = function(...) invisible(),
      mirai = function(expr, ...) {
        env <- list2env(list(...), parent = environment())
        res <- try(eval(substitute(expr), envir = env), silent = TRUE)
        if (inherits(res, "try-error")) {
          return(list(error = TRUE, message = as.character(res)))
        }
        list(error = FALSE, result = res)
      },
      unresolved = function(...) list(),
      is_error_value = function(x) isTRUE(x$error),
      `[.mirai` = function(x, ...) if (isTRUE(x$error)) x else x$result,
      .package = "mirai"
    )

    expect_condition(
      try(
        split_audio("dummy.mp3", segment_duration = 1, parallel = TRUE),
        silent = TRUE
      ),
      class = "cli_message"
    )
    expect_error(
      split_audio("dummy.mp3", segment_duration = 1, parallel = TRUE),
      "source audio file.*corrupted"
    )
  })
})

test_that("split_audio marks processed flags after timeout fallback", {
  skip_if_not_installed(c("av", "mirai"))
  skip_on_cran()

  withr::with_tempdir({
    # Force a very short timeout so the loop falls back to sequential processing
    withr::local_options(minutemaker_split_audio_parallel_timeout = 0.01)

    # Keep mirai tasks unresolved so timeout triggers
    local_mocked_bindings(
      status = function(...) list(daemons = 0),
      daemons = function(...) invisible(NULL),
      mirai = function(expr, ...) structure(list(...), class = "mirai"),
      unresolved = function(tasks) rep(TRUE, length(tasks)),
      is_error_value = function(...) FALSE,
      .package = "mirai"
    )
    # Avoid real audio work; just track calls
    local_mocked_bindings(
      av_media_info = function(...) list(duration = 120),
      .package = "av"
    )
    calls <- 0
    local_mocked_bindings(
      extract_audio_segment = function(...) {
        calls <<- calls + 1
      },
      .package = "minutemaker"
    )

    traced <- new.env(parent = emptyenv())
    trace(
      what = "split_audio",
      exit = function() {
        traced$processed <<- get0(
          "processed_flags",
          envir = parent.frame(),
          inherits = FALSE
        )
      },
      print = FALSE
    )
    withr::defer(untrace("split_audio"))

    split_audio("dummy.wav", segment_duration = 1, parallel = TRUE)

    expect_equal(calls, 2)
    expect_equal(traced$processed, rep(TRUE, 2))
  })
})
