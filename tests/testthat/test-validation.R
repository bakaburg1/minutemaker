# Tests for check_summary_tree_consistency() ---

test_that("reports mismatching ids for nameless summary tree", {
  summary_tree <- list(
    list(title = NULL, summary = "First summary"),
    list(title = NULL, summary = "Second summary")
  )

  expect_error(
    check_summary_tree_consistency(summary_tree),
    "ID mismatch at index 1",
    fixed = TRUE
  )
})

test_that("fails gracefully when summary tree file is malformed", {
  withr::with_tempdir({
    malformed_path <- file.path(getwd(), "malformed_summary_tree.txt")
    writeLines("not valid R code", con = malformed_path)

    expect_error(
      check_summary_tree_consistency(malformed_path),
      "Failed to read the summary tree file",
      fixed = TRUE
    )
  })
})

test_that("validate_agenda_element handles empty agenda element", {
  {
    # from = TRUE to trigger deeper checks if not empty
    res <- validate_agenda_element(list(), from = TRUE)
  } |>
    expect_warning(regexp = "The agenda element is empty.", fixed = TRUE)
  expect_false(res)
})

test_that("validate_agenda_element flags missing required items", {
  # Case 1: 'from' is required but missing
  {
    res1 <- validate_agenda_element(list(title = "A talk"), from = TRUE)
  } |>
    expect_warning(regexp = "Required items missing: from", fixed = TRUE)
  expect_false(res1)

  # Case 2: 'from' and 'to' required, only 'from' present
  {
    res2 <- validate_agenda_element(
      list(from = "09:00"),
      from = TRUE,
      to = TRUE
    )
  } |>
    expect_warning(regexp = "Required items missing: to", fixed = TRUE)
  expect_false(res2)

  # Case 3: 'session' and 'title' required, neither present
  {
    res3 <- validate_agenda_element(
      list(from = "09:00"),
      session = TRUE,
      title = TRUE
    )
  } |>
    # The order in the warning might vary, so check for both being mentioned
    # Reprex showed: "Required items missing: session and title " (note the
    # trailing space from cli formatting)
    expect_warning(
      regexp = "Required items missing: session and title",
      fixed = TRUE
    )
  expect_false(res3)
})

test_that("validate_agenda_element handles invalid time field types and values", {
  # Case 1: 'from' field has an invalid class (e.g., a data.frame)
  agenda_item_invalid_class <- list(
    from = data.frame(time = "09:00"),
    to = "10:00"
  )
  {
    res1 <- validate_agenda_element(
      agenda_item_invalid_class,
      from = TRUE,
      to = TRUE
    )
  } |>
    # Reprex showed: "... POSIXct, but it's class <data.frame>. " (note trailing
    # space and < >)
    expect_warning(
      regexp = "must be numeric, character, or POSIXct, but it's class <data.frame>.",
      fixed = TRUE
    )
  expect_false(res1)

  # Case 2: 'to' field is an uninterpretable time string
  agenda_item_uninterpretable <- list(
    from = "09:00",
    to = "invalid_time_string"
  )
  {
    res2 <- validate_agenda_element(
      agenda_item_uninterpretable,
      from = TRUE,
      to = TRUE
    )
  } |>
    # Reprex showed: "... not interpretable: "invalid_time_string" " (note quotes
    # and trailing space)
    expect_warning(
      regexp = 'field to time not interpretable: "invalid_time_string"',
      fixed = TRUE
    )
  expect_false(res2)
})

test_that("validate_agenda_element handles time class mismatch and order issues", {
  # Case 1: 'from' and 'to' times have different (but valid) classes
  # 36000 is 10:00
  agenda_item_class_mismatch <- list(from = "09:00", to = 36000)
  {
    res1 <- validate_agenda_element(
      agenda_item_class_mismatch,
      from = TRUE,
      to = TRUE
    )
  } |>
    expect_warning(
      regexp = "from and to times are not of the same class",
      fixed = TRUE
    )
  expect_false(res1)

  # Case 2: 'from' time does not precede 'to' time (both are character strings)
  agenda_item_order_issue_char <- list(from = "10:00", to = "09:00")
  {
    res2 <- validate_agenda_element(
      agenda_item_order_issue_char,
      from = TRUE,
      to = TRUE
    )
  } |>
    expect_warning(
      regexp = "'from' time should precede 'to' time",
      fixed = TRUE
    )
  expect_false(res2)

  # Case 3: 'from' time does not precede 'to' time (both are numeric)
  agenda_item_order_issue_num <- list(from = 3600, to = 0)
  {
    res3 <- validate_agenda_element(
      agenda_item_order_issue_num,
      from = TRUE,
      to = TRUE
    )
  } |>
    expect_warning(
      regexp = "'from' time should precede 'to' time",
      fixed = TRUE
    )
  expect_false(res3)
})

test_that("validate_agenda_element accepts a fully valid element with all checks", {
  valid_item <- list(
    session = "S1",
    title = "My Awesome Talk",
    speakers = "Speaker A",
    moderators = "Moderator B",
    type = "Plenary",
    from = "09:00",
    to = "10:00"
  )

  # No warnings expected, should return TRUE
  expect_no_warning(
    result <- validate_agenda_element(
      valid_item,
      session = TRUE,
      title = TRUE,
      speakers = TRUE,
      moderators = TRUE,
      type = TRUE,
      from = TRUE,
      to = TRUE
    )
  )
  expect_true(result)
})

test_that("validate_agenda_element allows NULL for non-required, non-time fields", {
  item_with_nulls <- list(
    session = NULL, # Allowed if session = FALSE (default)
    title = "Talk Title", # Required if title = TRUE
    from = "09:00", # Required if from = TRUE
    to = "10:00" # Required if to = TRUE
  )

  # Only title, from, to are strictly required by args here
  expect_no_warning(
    result <- validate_agenda_element(
      item_with_nulls,
      title = TRUE,
      from = TRUE,
      to = TRUE
    )
  )
  expect_true(result)

  # If session IS required, NULL should fail
  {
    res_fail <- validate_agenda_element(
      item_with_nulls,
      session = TRUE,
      title = TRUE,
      from = TRUE,
      to = TRUE
    )
  } |>
    expect_warning(
      regexp = "Required items missing: session",
      fixed = TRUE
    )
  expect_false(res_fail)
})

# Tests for validate_agenda() --------------------------------------------------

test_that("validate_agenda handles agenda = FALSE input", {
  expect_false(validate_agenda(FALSE))
  # No warning expected for this specific case based on current implementation
})

test_that("validate_agenda handles empty agenda input", {
  {
    res <- validate_agenda(list())
  } |>
    expect_warning(
      regexp = "The provided agenda is empty.",
      fixed = TRUE
    )
  expect_false(res)
})

test_that("validate_agenda handles incorrect agenda input types", {
  # Numeric input
  {
    res_num <- validate_agenda(123)
  } |>
    expect_warning(
      regexp = "The agenda must be a list or a file path, but it's class <numeric>.",
      fixed = TRUE
    )
  expect_false(res_num)

  # data.frame input
  {
    res_df <- validate_agenda(data.frame(a = 1))
  } |>
    expect_warning(
      regexp = "The agenda must be a list or a file path, but it's class <data.frame>.",
      fixed = TRUE
    )
  expect_false(res_df)

  # Multi-class input
  multi_class_agenda <- structure(
    list(element = "value"),
    class = c("foo", "bar")
  )
  {
    res_multi <- validate_agenda(multi_class_agenda)
  } |>
    expect_warning(
      regexp = paste(
        "The agenda must be a list or a file path, but it's class",
        "<foo/bar>."
      ),
      fixed = TRUE
    )
  expect_false(res_multi)
})

test_that("validate_agenda handles list with invalid elements", {
  # Agenda is a list, but contains an empty list as an element
  # validate_agenda_element will warn "The agenda element is empty."
  {
    res1 <- validate_agenda(list(list(title = "Good"), list()))
  } |>
    expect_warning(regexp = "The agenda element is empty.", fixed = TRUE)
  expect_false(res1)

  # Agenda is a list, one element fails a 'from' time check (e.g. invalid class
  # for 'from').
  # validate_agenda_element will warn about the bad 'from' field.
  invalid_element_agenda <- list(
    list(from = "09:00", to = "10:00"),
    list(from = data.frame(x = 1), to = "11:00") # Invalid 'from'
  )
  {
    res2 <- validate_agenda(invalid_element_agenda, from = TRUE, to = TRUE)
  } |>
    # Only the first failing element's critical warning
    expect_warning(
      regexp = "must be numeric, character, or POSIXct, but it's class <data.frame>.",
      fixed = TRUE
    )
  expect_false(res2)
})

test_that("validate_agenda accepts a valid list of valid elements", {
  valid_agenda_list <- list(
    list(title = "Talk 1", from = "09:00", to = "09:30"),
    list(title = "Talk 2", from = "09:30", to = "10:00", session = "Morning")
  )
  expect_no_warning(
    result <- validate_agenda(
      valid_agenda_list,
      title = TRUE,
      from = TRUE,
      to = TRUE,
      session = FALSE
    ) # session = FALSE to allow NULLs unless specified
  )
  expect_true(result)

  # Requiring session, but the first element of valid_agenda_list is missing it.
  # So, validate_agenda should return FALSE and a warning for that first
  # element.
  {
    res_req_session <- validate_agenda(
      valid_agenda_list,
      title = TRUE,
      from = TRUE,
      to = TRUE,
      session = TRUE
    )
  } |>
    expect_warning(
      regexp = "Required items missing: session",
      fixed = TRUE
    )
  expect_false(res_req_session)
})

test_that("validate_agenda handles file path inputs correctly", {
  temp_dir <- tempdir()
  # Helper to create a dget file
  create_dget_file <- function(obj, file_name) {
    file_path <- file.path(temp_dir, file_name)
    dput(obj, file = file_path)
    return(file_path)
  }

  # Case 1: Valid agenda in a file
  valid_agenda_for_file <- list(
    list(title = "File Talk 1", from = "10:00", to = "10:30")
  )
  valid_file_path <- create_dget_file(
    valid_agenda_for_file,
    "valid_agenda.txt"
  )
  expect_no_warning(
    result_valid_file <- validate_agenda(
      valid_file_path,
      title = TRUE,
      from = TRUE,
      to = TRUE
    )
  )
  expect_true(result_valid_file)

  # Case 2: Invalid agenda in a file (e.g., one element is empty)
  invalid_agenda_for_file <- list(
    list(title = "File Talk Good"),
    list() # Empty element
  )
  invalid_file_path <- create_dget_file(
    invalid_agenda_for_file,
    "invalid_agenda.txt"
  )

  {
    # title=TRUE so first element is okay
    res_invalid_file <- validate_agenda(invalid_file_path, title = TRUE)
  } |>
    expect_warning(
      regexp = "The agenda element is empty.",
      fixed = TRUE
    )
  expect_false(res_invalid_file)

  # Case 3: File path exists but file does not contain a list (e.g. contains a
  # number)
  not_a_list_content <- 123
  not_list_file_path <- create_dget_file(
    not_a_list_content,
    "not_list_agenda.txt"
  )
  {
    res_not_list <- validate_agenda(not_list_file_path)
  } |>
    expect_warning(
      regexp = "the agenda is not a list, it's class <numeric>.",
      fixed = TRUE
    )
  expect_false(res_not_list)

  # Case 4: Non-existent file path validate_agenda uses file.exists(), so this
  # won't try to dget. It will treat it as a character string that isn't a list,
  # then fail. Or rather, it will not attempt to dget it and then the later
  # `is.list(agenda)` check will fail because agenda is still the character
  # path. The check `!class(agenda) %in% c("list", "character")` handles initial
  # bad types. If it's character but not a file, it passes that. Then `dget` is
  # not called. Then `if (!is.list(agenda))` fails. This matches the existing
  # structure.
  {
    res_no_file <- validate_agenda("non_existent_file.RData")
  } |>
    expect_warning(
      regexp = "the agenda is not a list, it's class <character>.",
      fixed = TRUE
    )
  expect_false(res_no_file)

  malformed_file_path <- file.path(temp_dir, "malformed_agenda.txt")
  writeLines("not valid R code", con = malformed_file_path)

  {
    res_malformed_file <- validate_agenda(malformed_file_path)
  } |>
    expect_warning(
      regexp = "Failed to read the agenda file",
      fixed = TRUE
    )
  expect_false(res_malformed_file)

  # Clean up temp files (optional, as tempdir() handles it, but good practice
  # for clarity)
  unlink(c(
    valid_file_path,
    invalid_file_path,
    not_list_file_path,
    malformed_file_path
  ))
})
