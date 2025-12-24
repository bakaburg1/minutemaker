test_that("pause pruning is robust to removals while iterating", {
  breakpoints <- c(0, 2000, 4000, 5100, 6000, 8000, 10000, 12000, 12200)
  pauses <- c(5100, 12200)

  cleaned <- minutemaker:::prune_pause_breakpoints(
    breakpoints,
    pauses,
    pause_duration = 1200
  )

  expect_identical(cleaned, c(0, 2000, 5100, 8000, 10000, 12000))
})

# Tests for infer_agenda_from_transcript() -------------------------------

test_that("infer_agenda_from_transcript falls back when title regeneration fails", {
  withr::local_options(
    minutemaker_temp_agenda = NULL,
    minutemaker_temp_agenda_last_bp = NULL,
    minutemaker_temp_agenda_hash = NULL,
    minutemaker_prompts = list()
  )

  transcript_df <- data.frame(
    start = c(0, 60, 120, 180, 240),
    end = c(30, 90, 150, 210, 270),
    text = paste("Segment", 1:5)
  )

  llm_call <- 0
  # Mock LLM replies keyed by prompt content to avoid order dependence
  mock_prompt_llm <- function(prompt_set, ...) {
    llm_call <<- llm_call + 1

    if (any(grepl("agenda inference prompt", prompt_set, fixed = TRUE))) {
      return('{"start_times": [0, 180]}')
    }

    if (any(grepl("agenda element prompt", prompt_set, fixed = TRUE))) {
      return('{"title": "Repeated", "session": "Session"}')
    }

    if (any(grepl("This agenda item title", prompt_set, fixed = TRUE))) {
      return("not json")
    }

    stop("Unexpected LLM call")
  }

  warnings <- character(0)
  mock_cli_warn <- function(message, ...) {
    warnings <<- c(warnings, paste(message, collapse = " "))
    invisible(NULL)
  }

  testthat::local_mocked_bindings(
    prompt_llm = mock_prompt_llm,
    .package = "llmR"
  )
  testthat::local_mocked_bindings(
    set_prompts = function(...) invisible(NULL),
    get_prompts = function(...) "persona prompt",
    generate_agenda_inference_prompt = function(transcript_segment, args) {
      "agenda inference prompt"
    },
    generate_agenda_element_prompt = function(transcript_segment, args) {
      "agenda element prompt"
    },
    .package = "minutemaker"
  )
  testthat::local_mocked_bindings(
    cli_warn = mock_cli_warn,
    cli_alert = function(...) invisible(NULL),
    cli_inform = function(...) invisible(NULL),
    cli_verbatim = function(...) invisible(NULL),
    .package = "cli"
  )

  agenda <- infer_agenda_from_transcript(
    transcript = transcript_df,
    window_size = 400
  )

  expect_length(agenda, 2)
  expect_identical(agenda[[1]]$title, "Repeated")
  expect_identical(agenda[[2]]$title, "Repeated (2)")
  expect_true(any(grepl("Could not regenerate title", warnings)))
})

test_that("warns and skips empty agenda start times", {
  withr::local_options(
    minutemaker_temp_agenda = NULL,
    minutemaker_temp_agenda_last_bp = NULL,
    minutemaker_temp_agenda_hash = NULL,
    minutemaker_prompts = list()
  )

  transcript_df <- data.frame(
    start = c(0, 60, 120, 180, 240, 300),
    end = c(30, 90, 150, 210, 270, 330),
    text = paste("Segment", 1:6)
  )

  inference_calls <- 0
  mock_prompt_llm <- function(prompt_set, ...) {
    if (any(grepl("agenda inference prompt", prompt_set, fixed = TRUE))) {
      inference_calls <<- inference_calls + 1

      if (inference_calls == 1) {
        return('{"start_times": []}')
      }

      return('{"start_times": [0]}')
    }

    if (any(grepl("agenda element prompt", prompt_set, fixed = TRUE))) {
      return('{"title": "Item", "session": "Session"}')
    }

    stop("Unexpected LLM call")
  }

  testthat::local_mocked_bindings(
    prompt_llm = mock_prompt_llm,
    .package = "llmR"
  )
  testthat::local_mocked_bindings(
    set_prompts = function(...) invisible(NULL),
    get_prompts = function(...) "persona prompt",
    generate_agenda_inference_prompt = function(transcript_segment, args) {
      "agenda inference prompt"
    },
    generate_agenda_element_prompt = function(transcript_segment, args) {
      "agenda element prompt"
    },
    .package = "minutemaker"
  )
  testthat::local_mocked_bindings(
    cli_alert = function(...) invisible(NULL),
    cli_inform = function(...) invisible(NULL),
    cli_verbatim = function(...) invisible(NULL),
    .package = "cli"
  )

  {
    agenda <- infer_agenda_from_transcript(
      transcript = transcript_df,
      window_size = 150
    )
  } |>
    expect_message(
      "Agenda start times are empty. Skipping segment 0-150.",
      fixed = TRUE
    )

  expect_length(agenda, 1)
  expect_identical(agenda[[1]]$title, "Item")
})

test_that("errors on malformed agenda start times", {
  withr::local_options(
    minutemaker_temp_agenda = NULL,
    minutemaker_temp_agenda_last_bp = NULL,
    minutemaker_temp_agenda_hash = NULL,
    minutemaker_prompts = list()
  )

  transcript_df <- data.frame(
    start = c(0, 60, 120),
    end = c(30, 90, 150),
    text = paste("Segment", 1:3)
  )

  mock_prompt_llm <- function(prompt_set, ...) {
    if (any(grepl("agenda inference prompt", prompt_set, fixed = TRUE))) {
      return('{"start_times": ["foo"]}')
    }

    stop("Unexpected LLM call")
  }

  testthat::local_mocked_bindings(
    prompt_llm = mock_prompt_llm,
    .package = "llmR"
  )
  testthat::local_mocked_bindings(
    set_prompts = function(...) invisible(NULL),
    get_prompts = function(...) "persona prompt",
    generate_agenda_inference_prompt = function(transcript_segment, args) {
      "agenda inference prompt"
    },
    .package = "minutemaker"
  )
  testthat::local_mocked_bindings(
    cli_alert = function(...) invisible(NULL),
    cli_inform = function(...) invisible(NULL),
    cli_verbatim = function(...) invisible(NULL),
    .package = "cli"
  )

  expect_error(
    infer_agenda_from_transcript(
      transcript = transcript_df,
      window_size = 200
    ),
    "Agenda start times must be numeric",
    fixed = TRUE
  )
})

test_that("accepts numeric vector agenda start times", {
  withr::local_options(
    minutemaker_temp_agenda = NULL,
    minutemaker_temp_agenda_last_bp = NULL,
    minutemaker_temp_agenda_hash = NULL,
    minutemaker_prompts = list()
  )

  transcript_df <- data.frame(
    start = c(0, 60, 120, 180, 240, 300),
    end = c(30, 90, 150, 210, 270, 330),
    text = paste("Segment", 1:6)
  )

  mock_prompt_llm <- function(prompt_set, ...) {
    if (any(grepl("agenda inference prompt", prompt_set, fixed = TRUE))) {
      return('{"start_times": [0, 180]}')
    }

    if (any(grepl("agenda element prompt", prompt_set, fixed = TRUE))) {
      return('{"title": "Item", "session": "Session"}')
    }

    stop("Unexpected LLM call")
  }

  testthat::local_mocked_bindings(
    prompt_llm = mock_prompt_llm,
    .package = "llmR"
  )
  testthat::local_mocked_bindings(
    set_prompts = function(...) invisible(NULL),
    get_prompts = function(...) "persona prompt",
    generate_agenda_inference_prompt = function(transcript_segment, args) {
      "agenda inference prompt"
    },
    generate_agenda_element_prompt = function(transcript_segment, args) {
      "agenda element prompt"
    },
    .package = "minutemaker"
  )
  testthat::local_mocked_bindings(
    cli_alert = function(...) invisible(NULL),
    cli_inform = function(...) invisible(NULL),
    cli_verbatim = function(...) invisible(NULL),
    .package = "cli"
  )

  {
    agenda <- infer_agenda_from_transcript(
      transcript = transcript_df,
      window_size = 400
    )
  } |>
    expect_warning(
      "Could not regenerate title",
      fixed = TRUE
    )

  expect_length(agenda, 2)
  expect_identical(agenda[[1]]$title, "Item")
  expect_identical(agenda[[2]]$title, "Item (2)")
})
