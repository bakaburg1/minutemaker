test_that("convert_agenda_times only warns when conversion needs an origin", {
  numeric_agenda <- list(
    list(from = 0, to = 60),
    list(from = 60, to = 120)
  )

  expect_silent(
    convert_agenda_times(
      numeric_agenda,
      convert_to = "seconds",
      event_start_time = NULL
    )
  )

  clock_agenda <- list(
    list(from = "09:00", to = "10:00"),
    list(from = "10:00", to = "11:00")
  )

  expect_silent(
    convert_agenda_times(
      clock_agenda,
      convert_to = "seconds",
      event_start_time = NULL
    )
  )

  expect_silent(
    convert_agenda_times(
      numeric_agenda,
      convert_to = "clocktime",
      event_start_time = NULL
    )
  )
})

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

test_that("extract_text_from_transcript skips empty transcript data", {
  empty_df <- data.frame(start = numeric(0), end = numeric(0), text = character(0))

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
