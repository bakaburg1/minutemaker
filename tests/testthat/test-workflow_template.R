# Tests for generate_workflow_template() -----------------------------------

test_that("writes template into a directory", {
  withr::with_tempdir({
    dest_path <- generate_workflow_template(".")
    template_path <- system.file(
      "templates",
      "meeting_summary_template.R",
      package = "minutemaker"
    )

    expect_identical(
      dest_path,
      fs::path_abs(file.path(getwd(), "meeting_summary_template.R"))
    )
    expect_true(file.exists(dest_path))
    expect_identical(readLines(dest_path), readLines(template_path))
  })
})

test_that("errors when destination exists and overwrite is FALSE", {
  withr::with_tempdir({
    generate_workflow_template(".")

    expect_error(
      generate_workflow_template("."),
      "Template already exists",
      fixed = TRUE
    )
  })
})

test_that("overwrites when overwrite is TRUE", {
  withr::with_tempdir({
    dest_path <- generate_workflow_template(".")
    template_path <- system.file(
      "templates",
      "meeting_summary_template.R",
      package = "minutemaker"
    )

    writeLines("custom", dest_path)
    expect_identical(readLines(dest_path), "custom")

    generate_workflow_template(".", overwrite = TRUE)
    expect_identical(readLines(dest_path), readLines(template_path))
  })
})
