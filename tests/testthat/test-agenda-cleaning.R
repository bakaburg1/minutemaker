# convert_agenda_times ----
test_that("convert_agenda_times is silent when event_start_time is NULL", {
  numeric_agenda <- list(
    list(from = 0, to = 60),
    list(from = 60, to = 120)
  )

  result_numeric <- expect_silent(
    convert_agenda_times(
      numeric_agenda,
      convert_to = "seconds",
      event_start_time = NULL
    )
  )
  expect_equal(result_numeric[[1]]$from, 0)
  expect_equal(result_numeric[[1]]$to, 60)
  expect_equal(result_numeric[[2]]$from, 60)
  expect_equal(result_numeric[[2]]$to, 120)

  clock_agenda <- list(
    list(from = "09:00", to = "10:00"),
    list(from = "10:00", to = "11:00")
  )

  result_clock <- expect_silent(
    convert_agenda_times(
      clock_agenda,
      convert_to = "seconds",
      event_start_time = NULL
    )
  )
  expect_equal(result_clock[[1]]$from, 0)
  expect_equal(result_clock[[1]]$to, 3600)
  expect_equal(result_clock[[2]]$from, 3600)
  expect_equal(result_clock[[2]]$to, 7200)

  result_numeric_to_clock <- expect_silent(
    convert_agenda_times(
      numeric_agenda,
      convert_to = "clocktime",
      event_start_time = NULL
    )
  )
  expect_equal(result_numeric_to_clock[[1]]$from, "00:00:00")
  expect_equal(result_numeric_to_clock[[1]]$to, "00:01:00")
  expect_equal(result_numeric_to_clock[[2]]$from, "00:01:00")
  expect_equal(result_numeric_to_clock[[2]]$to, "00:02:00")
})

test_that("convert_agenda_times handles null, NA, empty, and invalid inputs", {
  capture_warnings <- function(expr) {
    warnings <- character(0)
    value <- withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warnings)
  }

  empty_agenda <- list()
  expect_error(
    convert_agenda_times(
      empty_agenda,
      convert_to = "seconds",
      event_start_time = NULL
    )
  )

  null_agenda <- list(list(from = NULL, to = 60))
  expect_warning(
    expect_error(
      convert_agenda_times(
        null_agenda,
        convert_to = "seconds",
        event_start_time = NULL
      ),
      regexp = "Agenda times must be all of the same class across the agenda.",
      fixed = TRUE
    ),
    regexp = "Required items missing: from",
    fixed = TRUE
  )

  na_agenda <- list(list(from = NA_real_, to = 60))
  result_na <- expect_silent(
    convert_agenda_times(
      na_agenda,
      convert_to = "seconds",
      event_start_time = NULL
    )
  )
  expect_true(is.na(result_na[[1]]$from))
  expect_equal(result_na[[1]]$to, 60)

  invalid_agenda <- list(list(from = "bad", to = "10:00"))
  invalid_result <- capture_warnings(
    expect_error(
      convert_agenda_times(
        invalid_agenda,
        convert_to = "seconds",
        event_start_time = NULL
      ),
      regexp = "Agenda time conversion failed: \"bad\"",
      fixed = TRUE
    )
  )
  expect_true(
    any(
      grepl(
        "Agenda element validation failed",
        invalid_result$warnings,
        fixed = TRUE
      )
    )
  )
  expect_true(
    any(
      grepl(
        "All formats failed to parse",
        invalid_result$warnings,
        fixed = TRUE
      )
    )
  )

  mixed_agenda <- list(
    list(from = "09:00", to = "10:00"),
    list(from = "bad", to = "11:00")
  )
  mixed_result <- capture_warnings(
    convert_agenda_times(
      mixed_agenda,
      convert_to = "seconds",
      event_start_time = NULL
    )
  )
  expect_true(
    any(
      grepl(
        "Agenda element validation failed",
        mixed_result$warnings,
        fixed = TRUE
      )
    )
  )
  expect_true(
    any(
      grepl(
        "All formats failed to parse",
        mixed_result$warnings,
        fixed = TRUE
      )
    )
  )
  result_mixed <- mixed_result$value
  expect_equal(result_mixed[[1]]$from, 0)
  expect_equal(result_mixed[[1]]$to, 3600)
  expect_true(is.na(result_mixed[[2]]$from))
  expect_equal(result_mixed[[2]]$to, 7200)
})

# clean_agenda ----
test_that("clean_agenda drops or aborts on empty transcript slices", {
  agenda <- list(
    list(session = "S1", title = "Talk 1", from = 0, to = 60),
    list(session = "S2", title = "Talk 2", from = 60, to = 120),
    list(session = "S3", title = "Talk 3", from = 200, to = 240)
  )
  transcript_data <- data.frame(
    start = c(10, 70),
    end = c(20, 80),
    text = c("a", "b")
  )

  cleaned <- NULL
  expect_message(
    {
      cleaned <- clean_agenda(
        agenda = agenda,
        transcript_data = transcript_data,
        on_empty = "drop"
      )
    },
    "Dropping",
    fixed = TRUE
  )

  expect_length(cleaned, 2)
  expect_identical(cleaned[[1]]$title, "Talk 1")
  expect_identical(cleaned[[2]]$title, "Talk 2")

  expect_error(
    clean_agenda(
      agenda = agenda,
      transcript_data = transcript_data,
      on_empty = "abort"
    ),
    "Agenda item has an empty transcript slice",
    fixed = TRUE
  )
})

# extract_text_from_transcript ----
test_that("extract_text_from_transcript skips empty transcript data", {
  empty_df <- data.frame(
    start = numeric(0),
    end = numeric(0),
    text = character(0)
  )

  out <- NULL
  expect_message(
    {
      out <- extract_text_from_transcript(empty_df)
    },
    "Transcript segment is empty. Skipping.",
    fixed = TRUE
  )

  expect_true(is.na(out))
})
