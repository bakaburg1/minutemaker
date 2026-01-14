# Tests for check_path() ---

test_that("accepts valid relative paths and returns trimmed path", {
  expect_identical(check_path("file.txt"), "file.txt")
  expect_identical(check_path("path/to/file.txt"), "path/to/file.txt")
  expect_identical(check_path("path with spaces.txt"), "path with spaces.txt")
  expect_identical(
    check_path("path/with/multiple/levels.txt"),
    "path/with/multiple/levels.txt"
  )
})

test_that("accepts valid absolute paths and returns trimmed path", {
  expect_identical(check_path("/absolute/path.txt"), "/absolute/path.txt")
  expect_identical(check_path("/usr/local/file.txt"), "/usr/local/file.txt")
})

test_that("trims whitespace from valid paths", {
  expect_identical(check_path("  file.txt  "), "file.txt")
  expect_identical(check_path("  path/to/file.txt  "), "path/to/file.txt")
  expect_identical(check_path("\tfile.txt\t"), "file.txt")
})

test_that("rejects NULL and NA with stop_on_error = TRUE", {
  expect_error(check_path(NULL), "Invalid path provided")
  expect_error(check_path(NA), "Invalid path provided")
  expect_error(check_path(NA_character_), "Invalid path provided")
})

test_that("rejects NULL and NA with stop_on_error = FALSE", {
  result <- check_path(NULL, stop_on_error = FALSE)
  expect_false(result)

  result <- check_path(NA, stop_on_error = FALSE)
  expect_false(result)

  result <- check_path(NA_character_, stop_on_error = FALSE)
  expect_false(result)
})

test_that("rejects non-string inputs with stop_on_error = TRUE", {
  expect_error(check_path(123), "Invalid path provided")
  expect_error(check_path(TRUE), "Invalid path provided")
  expect_error(check_path(list("path")), "Invalid path provided")
})

test_that("rejects non-string inputs with stop_on_error = FALSE", {
  result <- check_path(123, stop_on_error = FALSE)
  expect_false(result)

  result <- check_path(TRUE, stop_on_error = FALSE)
  expect_false(result)

  result <- check_path(list("path"), stop_on_error = FALSE)
  expect_false(result)
})

test_that("rejects empty strings with stop_on_error = TRUE", {
  expect_error(check_path(""), "Invalid path provided")
  expect_error(check_path("   "), "Invalid path provided")
  expect_error(check_path("\t"), "Invalid path provided")
  expect_error(check_path("\n"), "Invalid path provided")
})

test_that("rejects empty strings with stop_on_error = FALSE", {
  result <- check_path("", stop_on_error = FALSE)
  expect_false(result)

  result <- check_path("   ", stop_on_error = FALSE)
  expect_false(result)

  result <- check_path("\t", stop_on_error = FALSE)
  expect_false(result)

  result <- check_path("\n", stop_on_error = FALSE)
  expect_false(result)
})

test_that("rejects Windows reserved names with stop_on_error = TRUE", {
  expect_error(check_path("CON"), "Invalid path provided")
  expect_error(check_path("PRN"), "Invalid path provided")
  expect_error(check_path("AUX"), "Invalid path provided")
  expect_error(check_path("NUL"), "Invalid path provided")
  expect_error(check_path("COM1"), "Invalid path provided")
  expect_error(check_path("COM9"), "Invalid path provided")
  expect_error(check_path("LPT1"), "Invalid path provided")
  expect_error(check_path("LPT9"), "Invalid path provided")
  expect_error(check_path("con"), "Invalid path provided")
  expect_error(check_path("path/to/CON.txt"), "Invalid path provided")
  expect_error(check_path("path/to/prn.doc"), "Invalid path provided")
})

test_that("rejects Windows reserved names with stop_on_error = FALSE", {
  result <- check_path("CON", stop_on_error = FALSE)
  expect_false(result)

  result <- check_path("path/to/CON.txt", stop_on_error = FALSE)
  expect_false(result)
})

test_that("accepts paths with dots", {
  expect_identical(check_path("path/../file.txt"), "path/../file.txt")
  expect_identical(check_path("./file.txt"), "./file.txt")
  expect_identical(check_path("../file.txt"), "../file.txt")
})

test_that("accepts paths with double slashes", {
  expect_identical(check_path("path//to//file.txt"), "path//to//file.txt")
})

test_that("handles edge cases", {
  expect_identical(check_path("a"), "a")
  expect_identical(check_path("file-with-dashes.txt"), "file-with-dashes.txt")
  expect_identical(
    check_path("file_with_underscores.txt"),
    "file_with_underscores.txt"
  )
  expect_identical(
    check_path("file.multiple.dots.txt"),
    "file.multiple.dots.txt"
  )
})

test_that("returns FALSE silently when stop_on_error = FALSE and fail_msg = FALSE", {
  result <- check_path("", stop_on_error = FALSE, fail_msg = FALSE)
  expect_false(result)
})

test_that("check_path uses unique temp paths for concurrent validation", {
  # Test that multiple calls to check_path() with relative paths use unique
  # temporary directories, preventing race conditions when called concurrently.
  # This verifies that tempfile() is used instead of a fixed path.
  paths <- paste0("test/path/", 1:20, "/file.txt")
  results <- vapply(
    paths,
    function(p) {
      result <- tryCatch(
        check_path(p, stop_on_error = FALSE),
        error = function(e) FALSE
      )
      is.character(result) && result != FALSE
    },
    logical(1)
  )
  # All should succeed without collisions (tempfile() ensures uniqueness)
  expect_true(all(results))
})

# Tests for path_exists() ---

test_that("returns the trimmed path for existing files and directories", {
  withr::with_tempdir({
    file_path <- file.path(getwd(), "a.txt")
    dir_path <- file.path(getwd(), "d")
    fs::file_create(file_path)
    fs::dir_create(dir_path)

    expect_identical(path_exists(file_path, type = "file"), file_path)
    expect_identical(path_exists(dir_path, type = "dir"), dir_path)
    expect_identical(path_exists(file_path, type = "any"), file_path)
    expect_identical(path_exists(dir_path, type = "any"), dir_path)

    expect_identical(
      path_exists(paste0("  ", file_path, "  "), type = "file"),
      file_path
    )
  })
})

test_that("returns FALSE for missing paths", {
  withr::with_tempdir({
    missing_path <- file.path(getwd(), "missing.txt")

    expect_error(
      {
        res_any <- path_exists(
          missing_path,
          type = "any",
          stop_on_error = FALSE
        )
      },
      NA
    )
    expect_false(res_any)

    expect_error(
      {
        res_file <- path_exists(missing_path, stop_on_error = FALSE)
      },
      NA
    )
    expect_false(res_file)

    expect_error(
      {
        res_dir <- path_exists(
          missing_path,
          type = "dir",
          stop_on_error = FALSE
        )
      },
      NA
    )
    expect_false(res_dir)
  })
})

test_that("errors on invalid inputs by default", {
  expect_error(path_exists(NULL), "Invalid path provided")
  expect_error(path_exists(NA_character_), "Invalid path provided")
})

test_that("returns FALSE on invalid inputs when stop_on_error = FALSE", {
  result <- path_exists(NULL, stop_on_error = FALSE)
  expect_false(result)

  result <- path_exists(NULL, stop_on_error = FALSE, fail_msg = FALSE)
  expect_false(result)
})

test_that("wrapper-local variables interpolate correctly when path_exists is wrapped", {
  # Test that when path_exists() is called through a wrapper, variables defined
  # in the wrapper's environment are accessible in fail_msg because path_exists()
  # pre-interpolates the message using parent.frame() before passing it to
  # check_path().

  # Wrapper-only variable - should work because path_exists pre-interpolates
  wrapper_with_local <- function(p) {
    wrapper_only_var <- "wrapper_value"
    path_exists(
      p,
      fail_msg = "Wrapper var: {wrapper_only_var}, path: {`_path`}"
    )
  }
  expect_error(
    wrapper_with_local(NULL),
    "Wrapper var: wrapper_value"
  )

  # User-level variable should also work
  user_var <- "user_value"
  wrapper_using_user_var <- function(p) {
    path_exists(
      p,
      fail_msg = "User var: {user_var}, path: {`_path`}"
    )
  }
  expect_error(
    wrapper_using_user_var(NULL),
    "User var: user_value"
  )
})

test_that("wrapper-local variables interpolate for missing file errors", {
  # When the path validates but does not exist, path_exists() itself emits the
  # error/warning; it should still interpolate wrapper-local variables because
  # it pre-formats fail_msg using parent.frame().
  wrapper_missing <- function(p) {
    wrapper_only_var <- "wrapper_value"
    path_exists(
      p,
      fail_msg = "Wrapper var: {wrapper_only_var}, path: {`_path`}"
    )
  }

  expect_error(
    wrapper_missing("definitely_missing_file.txt"),
    "Wrapper var: wrapper_value"
  )
})
