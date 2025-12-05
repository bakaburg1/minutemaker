test_that("load_serialized aborts on missing file by default", {
  withr::with_tempdir({
    missing_path <- file.path(getwd(), "missing.R")

    expect_error(
      minutemaker:::load_serialized(missing_path, "agenda"),
      "Failed to read the agenda file"
    )
  })
})

test_that("load_serialized warns and returns NULL when requested", {
  withr::with_tempdir({
    missing_path <- file.path(getwd(), "missing.R")

    expect_warning(
      result <- minutemaker:::load_serialized(
        missing_path,
        "agenda",
        on_error = "warn"
      ),
      "Failed to read the agenda file"
    )

    expect_null(result)
  })
})

test_that("load_serialized is silent on error when requested", {
  withr::with_tempdir({
    missing_path <- file.path(getwd(), "missing.R")

    expect_silent(
      result <- minutemaker:::load_serialized(
        missing_path,
        "agenda",
        on_error = "silent"
      )
    )

    expect_null(result)
  })
})

test_that("load_serialized returns object on success", {
  withr::with_tempdir({
    path <- file.path(getwd(), "obj.R")
    dput(list(a = 1), file = path)

    result <- minutemaker:::load_serialized(path, "object")

    expect_identical(result, list(a = 1))
  })
})
