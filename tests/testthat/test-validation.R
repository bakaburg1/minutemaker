test_that("validate_agenda_element handles empty agenda element", {
  env <- rlang::current_env()
  env$warn_list <- c()

  expect_false(
    withCallingHandlers(
      # from = TRUE to trigger deeper checks if not empty
      validate_agenda_element(list(), from = TRUE),
      warning = function(w) {
        env$warn_list <- c(env$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env$warn_list, 1)
  grep("The agenda element is empty.", env$warn_list, fixed = TRUE) |>
    expect_length(1)
})

test_that("validate_agenda_element flags missing required items", {
  # Case 1: 'from' is required but missing
  env1 <- rlang::current_env()
  env1$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda_element(list(title = "A talk"), from = TRUE),
      warning = function(w) {
        env1$warn_list <- c(env1$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env1$warn_list, 1)
  grep("Required items missing: from", env1$warn_list, fixed = TRUE) |>
    expect_length(1)

  # Case 2: 'from' and 'to' required, only 'from' present
  env2 <- rlang::current_env()
  env2$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda_element(list(from = "09:00"), from = TRUE, to = TRUE),
      warning = function(w) {
        env2$warn_list <- c(env2$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env2$warn_list, 1)
  grep("Required items missing: to", env2$warn_list, fixed = TRUE) |>
    expect_length(1)

  # Case 3: 'session' and 'title' required, neither present
  env3 <- rlang::current_env()
  env3$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda_element(
        list(from = "09:00"),
        session = TRUE,
        title = TRUE
      ),
      warning = function(w) {
        env3$warn_list <- c(env3$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env3$warn_list, 1)
  # The order in the warning might vary, so check for both being mentioned
  # Reprex showed: "Required items missing: session and title " (note the
  # trailing space from cli formatting)
  grep(
    "Required items missing: session and title",
    env3$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)
})

test_that("validate_agenda_element handles invalid time field types and values", {
  # Case 1: 'from' field has an invalid class (e.g., a data.frame)
  env1 <- rlang::current_env()
  env1$warn_list <- c()
  agenda_item_invalid_class <- list(
    from = data.frame(time = "09:00"),
    to = "10:00"
  )
  expect_false(
    withCallingHandlers(
      validate_agenda_element(
        agenda_item_invalid_class,
        from = TRUE,
        to = TRUE
      ),
      warning = function(w) {
        env1$warn_list <- c(env1$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  # Expect 1 warning: invalid class for 'from'.
  expect_length(env1$warn_list, 1)
  # Reprex showed: "... POSIXct, but it's class <data.frame>. " (note trailing
  # space and < >)
  grep(
    "must be numeric, character, or POSIXct, but it's class <data.frame>.",
    env1$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)

  # Case 2: 'to' field is an uninterpretable time string
  env2 <- rlang::current_env()
  env2$warn_list <- c()
  agenda_item_uninterpretable <- list(
    from = "09:00",
    to = "invalid_time_string"
  )
  expect_false(
    withCallingHandlers(
      validate_agenda_element(
        agenda_item_uninterpretable,
        from = TRUE,
        to = TRUE
      ),
      warning = function(w) {
        env2$warn_list <- c(env2$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  # Expect 1 warning: "field to time not interpretable".
  expect_length(env2$warn_list, 1)
  # Reprex showed: "... not interpretable: "invalid_time_string" " (note quotes
  # and trailing space)
  grep(
    'field to time not interpretable: "invalid_time_string"',
    env2$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)
})

test_that("validate_agenda_element handles time class mismatch and order issues", {
  # Case 1: 'from' and 'to' times have different (but valid) classes
  env1 <- rlang::current_env()
  env1$warn_list <- c()

  # 36000 is 10:00
  agenda_item_class_mismatch <- list(from = "09:00", to = 36000)
  expect_false(
    withCallingHandlers(
      validate_agenda_element(
        agenda_item_class_mismatch,
        from = TRUE,
        to = TRUE
      ),
      warning = function(w) {
        env1$warn_list <- c(env1$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env1$warn_list, 1)
  grep(
    "from and to times are not of the same class",
    env1$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)

  # Case 2: 'from' time does not precede 'to' time (both are character strings)
  env2 <- rlang::current_env()
  env2$warn_list <- c()
  agenda_item_order_issue_char <- list(from = "10:00", to = "09:00")
  expect_false(
    withCallingHandlers(
      validate_agenda_element(
        agenda_item_order_issue_char,
        from = TRUE,
        to = TRUE
      ),
      warning = function(w) {
        env2$warn_list <- c(env2$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env2$warn_list, 1)
  grep("'from' time should precede 'to' time", env2$warn_list, fixed = TRUE) |>
    expect_length(1)

  # Case 3: 'from' time does not precede 'to' time (both are numeric)
  env3 <- rlang::current_env()
  env3$warn_list <- c()
  agenda_item_order_issue_num <- list(from = 3600, to = 0)
  expect_false(
    withCallingHandlers(
      validate_agenda_element(
        agenda_item_order_issue_num,
        from = TRUE,
        to = TRUE
      ),
      warning = function(w) {
        env3$warn_list <- c(env3$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env3$warn_list, 1)
  grep("'from' time should precede 'to' time", env3$warn_list, fixed = TRUE) |>
    expect_length(1)
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
  env <- rlang::current_env()
  env$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda_element(
        item_with_nulls,
        session = TRUE,
        title = TRUE,
        from = TRUE,
        to = TRUE
      ),
      warning = function(w) {
        env$warn_list <- c(env$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env$warn_list, 1)
  grep("Required items missing: session", env$warn_list, fixed = TRUE) |>
    expect_length(1)
})

# Tests for validate_agenda() --------------------------------------------------

test_that("validate_agenda handles agenda = FALSE input", {
  expect_false(validate_agenda(FALSE))
  # No warning expected for this specific case based on current implementation
})

test_that("validate_agenda handles empty agenda input", {
  env <- rlang::current_env()
  env$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda(list()),
      warning = function(w) {
        env$warn_list <- c(env$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env$warn_list, 1)
  grep("The provided agenda is empty.", env$warn_list, fixed = TRUE) |>
    expect_length(1)
})

test_that("validate_agenda handles incorrect agenda input types", {
  # Numeric input
  env_num <- rlang::current_env()
  env_num$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda(123),
      warning = function(w) {
        env_num$warn_list <- c(env_num$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env_num$warn_list, 1)
  grep(
    "The agenda must be a list or a file path, but it's class <numeric>.",
    env_num$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)

  # data.frame input
  env_df <- rlang::current_env()
  env_df$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda(data.frame(a = 1)),
      warning = function(w) {
        env_df$warn_list <- c(env_df$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env_df$warn_list, 1)
  grep(
    "The agenda must be a list or a file path, but it's class <data.frame>.",
    env_df$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)
})

test_that("validate_agenda handles list with invalid elements", {
  # Agenda is a list, but contains an empty list as an element
  # validate_agenda_element will warn "The agenda element is empty."
  env1 <- rlang::current_env()
  env1$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda(list(list(title = "Good"), list())),
      warning = function(w) {
        env1$warn_list <- c(env1$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env1$warn_list, 1)
  grep("The agenda element is empty.", env1$warn_list, fixed = TRUE) |>
    expect_length(1)

  # Agenda is a list, one element fails a 'from' time check (e.g. invalid class
  # for 'from').
  # validate_agenda_element will warn about the bad 'from' field.
  env2 <- rlang::current_env()
  env2$warn_list <- c()
  invalid_element_agenda <- list(
    list(from = "09:00", to = "10:00"),
    list(from = data.frame(x = 1), to = "11:00") # Invalid 'from'
  )
  expect_false(
    withCallingHandlers(
      validate_agenda(invalid_element_agenda, from = TRUE, to = TRUE),
      warning = function(w) {
        env2$warn_list <- c(env2$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )

  # Only the first failing element's critical warning
  expect_length(env2$warn_list, 1)
  grep(
    "must be numeric, character, or POSIXct, but it's class <data.frame>.",
    env2$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)
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
  env_req_session <- rlang::current_env()
  env_req_session$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda(
        valid_agenda_list,
        title = TRUE,
        from = TRUE,
        to = TRUE,
        session = TRUE
      ),
      warning = function(w) {
        env_req_session$warn_list <- c(
          env_req_session$warn_list,
          rlang::cnd_message(w)
        )
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env_req_session$warn_list, 1)
  grep(
    "Required items missing: session",
    env_req_session$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)
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
    "valid_agenda.RData"
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
    "invalid_agenda.RData"
  )

  env_invalid_file <- rlang::current_env()
  env_invalid_file$warn_list <- c()
  expect_false(
    withCallingHandlers(
      # title=TRUE so first element is okay
      validate_agenda(invalid_file_path, title = TRUE),
      warning = function(w) {
        env_invalid_file$warn_list <- c(
          env_invalid_file$warn_list,
          rlang::cnd_message(w)
        )
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env_invalid_file$warn_list, 1)
  grep(
    "The agenda element is empty.",
    env_invalid_file$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)

  # Case 3: File path exists but file does not contain a list (e.g. contains a
  # number)
  not_a_list_content <- 123
  not_list_file_path <- create_dget_file(
    not_a_list_content,
    "not_list_agenda.RData"
  )
  env_not_list <- rlang::current_env()
  env_not_list$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda(not_list_file_path),
      warning = function(w) {
        env_not_list$warn_list <- c(
          env_not_list$warn_list,
          rlang::cnd_message(w)
        )
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env_not_list$warn_list, 1)
  grep(
    "the agenda is not a list, it's class <numeric>.",
    env_not_list$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)

  # Case 4: Non-existent file path validate_agenda uses file.exists(), so this
  # won't try to dget. It will treat it as a character string that isn't a list,
  # then fail. Or rather, it will not attempt to dget it and then the later
  # `is.list(agenda)` check will fail because agenda is still the character
  # path. The check `!class(agenda) %in% c("list", "character")` handles initial
  # bad types. If it's character but not a file, it passes that. Then `dget` is
  # not called. Then `if (!is.list(agenda))` fails. This matches the existing
  # structure.
  env_no_file <- rlang::current_env()
  env_no_file$warn_list <- c()
  expect_false(
    withCallingHandlers(
      validate_agenda("non_existent_file.RData"),
      warning = function(w) {
        env_no_file$warn_list <- c(env_no_file$warn_list, rlang::cnd_message(w))
        rlang::cnd_muffle(w)
      }
    )
  )
  expect_length(env_no_file$warn_list, 1)
  grep(
    "the agenda is not a list, it's class <character>.",
    env_no_file$warn_list,
    fixed = TRUE
  ) |>
    expect_length(1)

  # Clean up temp files (optional, as tempdir() handles it, but good practice
  # for clarity)
  unlink(c(valid_file_path, invalid_file_path, not_list_file_path))
})
