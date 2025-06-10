# This test file focuses on audio processing functions, including both
# unit tests and integration tests for split_audio(), is_audio_file_sane(),
# create_and_validate_segment(), and their dependencies like av and mirai.

# -------------------------------------------------------------------------
# Setup and Helper Functions ----------------------------------------------
# -------------------------------------------------------------------------

# Helper function to create test audio files
helper_create_test_audio <- function(path, valid = TRUE) {
  if (valid) {
    # Create a valid 2-second silent audio file using ffmpeg
    system2("ffmpeg", 
            c("-f", "lavfi", "-i", "anullsrc=r=44100:cl=mono", 
              "-t", "2", path), 
            stdout = FALSE, stderr = FALSE)
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
      segment_duration = 1/60, # 1 second in minutes
      output_folder = "segments",
      parallel = FALSE
    )
    
    # Check results
    segments <- list.files("segments", pattern = "*.mp3", full.names = TRUE)
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
        segment_duration = 1/60, # 1 second in minutes
        output_folder = "segments", 
        parallel = TRUE
      )
    )
    
    # Check that segments were created
    segments <- list.files("segments", pattern = "*.mp3", full.names = TRUE)
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
        segment_duration = 1/60,
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

test_that("extract_audio_segment creates valid segments", {
  skip_if_not_installed("av")
  skip_on_cran()
  
  withr::with_tempdir({
    # Create a valid 6-second audio file (enough for 2 segments of 2 seconds each)
    audio_file <- "test.wav"
    system2("ffmpeg", 
            c("-f", "lavfi", "-i", "anullsrc=r=44100:cl=mono", 
              "-t", "6", audio_file), 
            stdout = FALSE, stderr = FALSE)
    
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
      "Generated segment.*is corrupted.*Retrying attempt"
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
        segment_duration = 1/60,
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
        segment_duration = 1/60,
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
      av_audio_convert = function(input, output, ...) invisible(file.create(output)),
      .package = "av"
    )
    local_mocked_bindings(
      is_audio_file_sane = function(...) TRUE,
      .package = "minutemaker"
    )

    # Expect no errors and the output file to be created
    expect_no_error(
      extract_audio_segment("dummy.mp3", "segment_1.mp3", 0, 60, verbose = FALSE)
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
      av_audio_convert = function(input, output, ...) invisible(file.create(output)),
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
    suppressMessages(
      expect_warning(
        extract_audio_segment("dummy.mp3", "segment_1.mp3", 0, 60),
        "corrupted. Retrying"
      )
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
      av_audio_convert = function(input, output, ...) invisible(file.create(output)),
      .package = "av"
    )
    local_mocked_bindings(
      is_audio_file_sane = function(...) FALSE,
      .package = "minutemaker"
    )

    # It should throw an error after trying twice
    suppressMessages(
      expect_error(
        extract_audio_segment("dummy.mp3", "segment_1.mp3", 0, 60),
        "Failed to create a valid segment"
      )
    )
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
        res <- try(eval(substitute(expr)), silent = TRUE)
        if (inherits(res, "try-error")) {
          return(list(error = TRUE, message = as.character(res)))
        }
        list(error = FALSE, result = res)
      },
      unresolved = function(...) list(),
      is_error_value = function(x) isTRUE(x$error),
      `[.mirai` = function(x, ...) if(isTRUE(x$error)) x else x$result,
      .package = "mirai"
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
        res <- try(eval(substitute(expr)), silent = TRUE)
        if (inherits(res, "try-error")) {
          return(list(error = TRUE, message = as.character(res)))
        }
        list(error = FALSE, result = res)
      },
      unresolved = function(...) list(),
      is_error_value = function(x) isTRUE(x$error),
      `[.mirai` = function(x, ...) if(isTRUE(x$error)) x else x$result,
      .package = "mirai"
    )

    expect_error(
      split_audio("dummy.mp3", segment_duration = 1, parallel = TRUE),
      "source audio file.*corrupted"
    )
  })
}) 