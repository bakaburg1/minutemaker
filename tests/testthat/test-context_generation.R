# Helper functions ---

.ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    utils::install.packages(pkg)
  }
}

# Tests for generate_context() ------------------------------------------------

test_that("plain text detection reads custom extensions", {
  withr::with_tempdir({
    material_dir <- file.path(getwd(), "documentation")
    dir.create(material_dir)

    text_path <- file.path(material_dir, "notes.vtt")
    writeLines(c("WEBVTT", "Hello there"), text_path)

    # Write a UTF-16LE CSV with BOM to emulate meeting exports.
    utf16_path <- file.path(material_dir, "participants.csv")
    utf16_text <- "Name,Role\nAlice,Lead"
    utf16_raw <- as.raw(c(
      0xFF,
      0xFE,
      unlist(lapply(utf8ToInt(utf16_text), \(cp) {
        c(cp %% 256, cp %/% 256)
      }))
    ))
    writeBin(utf16_raw, utf16_path)

    binary_path <- file.path(material_dir, "binary.bin")
    writeBin(as.raw(c(0, 1, 2, 3, 4)), binary_path)

    expect_true(minutemaker:::.gen_cntx_is_plain_text(text_path))
    expect_true(minutemaker:::.gen_cntx_is_plain_text(utf16_path))

    materials <- NULL
    expect_warning(
      materials <- minutemaker:::.gen_cntx_read_materials(material_dir),
      regexp = "Skipping unsupported non-text file"
    )

    expect_true("notes.vtt" %in% names(materials))
    expect_true("participants.csv" %in% names(materials))
    expect_false("binary.bin" %in% names(materials))
    expect_true(stringr::str_detect(materials[["participants.csv"]], "Alice"))
  })
})

test_that("docx tables are preserved as markdown-like blocks", {
  .ensure_pkg("officer")

  withr::with_tempdir({
    material_dir <- file.path(getwd(), "documentation")
    dir.create(material_dir)

    doc_path <- file.path(material_dir, "agenda.docx")
    agenda_table <- data.frame(
      Time = c("10:00", "10:30"),
      Topic = c("Welcome", "Updates"),
      stringsAsFactors = FALSE
    )

    doc <- officer::read_docx()
    doc <- officer::body_add_par(doc, "Agenda")
    doc <- officer::body_add_table(doc, value = agenda_table)
    print(doc, target = doc_path)

    text <- minutemaker:::.gen_cntx_read_file(doc_path)

    expect_true(stringr::str_detect(text, "Agenda"))
    expect_true(stringr::str_detect(text, "\\| --- \\| --- \\|"))
    expect_true(stringr::str_detect(text, "Welcome"))
    expect_true(stringr::str_detect(text, "Updates"))
  })
})

test_that("generate_context writes files from a single LLM response", {
  withr::with_tempdir({
    material_dir <- file.path(getwd(), "documentation")
    dir.create(material_dir)
    writeLines("Agenda: One, Two", file.path(material_dir, "agenda.txt"))

    llm_response <- paste0(
      "{",
      "\"expected_agenda\": \"Item 1\\nItem 2\",",
      "\"event_description\": \"Short description.\",",
      "\"audience\": \"Project leads.\",",
      "\"vocabulary\": [\"ECDC\", \"MRSA: methicillin-resistant Staphylococcus aureus\"],",
      "\"initial_prompt\": \"Ensure accurate transcription for ECDC, MRSA.\"",
      "}"
    )

    testthat::local_mocked_bindings(
      prompt_llm = function(messages, ...) {
        llm_response
      },
      .package = "llmR"
    )

    context <- generate_context(
      target_dir = getwd(),
      material_dir = "documentation",
      strategy = "one_pass",
      generate_expected_agenda = TRUE,
      generate_event_description = TRUE,
      generate_audience = TRUE,
      generate_vocabulary = TRUE,
      generate_initial_prompt = TRUE,
      overwrite = TRUE
    )

    expect_true(file.exists(file.path("context", "expected_agenda.txt")))
    expect_true(file.exists(file.path("context", "vocabulary.json")))
    expect_true(file.exists(file.path("context", "initial_prompt.txt")))

    agenda_text <- paste(
      readLines(file.path(
        "context",
        "expected_agenda.txt"
      )),
      collapse = "\n"
    )
    expect_identical(agenda_text, "Item 1\nItem 2")

    vocab <- jsonlite::fromJSON(file.path("context", "vocabulary.json"))
    expect_true("ECDC" %in% vocab)

    expect_identical(context$audience, "Project leads.")
  })
})

test_that("generate_context falls back to provided context on null output", {
  withr::with_tempdir({
    material_dir <- file.path(getwd(), "documentation")
    dir.create(material_dir)
    writeLines("Materials only", file.path(material_dir, "notes.txt"))

    llm_response <- "{\"event_description\": null}"

    testthat::local_mocked_bindings(
      prompt_llm = function(messages, ...) {
        llm_response
      },
      .package = "llmR"
    )

    context <- NULL
    expect_message(
      context <- generate_context(
        target_dir = getwd(),
        material_dir = "documentation",
        strategy = "one_pass",
        generate_expected_agenda = FALSE,
        generate_event_description = TRUE,
        generate_audience = FALSE,
        generate_vocabulary = FALSE,
        generate_initial_prompt = FALSE,
        event_description = "Provided description.",
        overwrite = TRUE
      ),
      regexp = "LLM returned null"
    )

    desc_path <- file.path("context", "event_description.txt")
    expect_true(file.exists(desc_path))
    expect_identical(readLines(desc_path), "Provided description.")
    expect_identical(context$event_description, "Provided description.")
  })
})

test_that("parse_json handles fenced arrays for vocabulary", {
  json_text <- "```json\n[\"ECDC\", \"MRSA: methicillin-resistant Staphylococcus aureus\"]\n```"
  parsed <- minutemaker:::.gen_cntx_parse_json(
    json_text,
    "vocabulary output",
    expected = "array"
  )
  expect_true(is.list(parsed))
  expect_true("ECDC" %in% unlist(parsed))
})

test_that("parse_json handles XML-wrapped arrays for vocabulary", {
  json_text <- "<json_context_output>[\"ECDC\", \"MRSA: methicillin-resistant Staphylococcus aureus\"]</json_context_output>"
  parsed <- minutemaker:::.gen_cntx_parse_json(
    json_text,
    "vocabulary output",
    expected = "array"
  )
  expect_true(is.list(parsed))
  expect_true("ECDC" %in% unlist(parsed))
})

test_that("generate_context skips generation when context files exist", {
  withr::with_tempdir({
    context_dir <- file.path(getwd(), "context")
    dir.create(context_dir)
    audience_path <- file.path(context_dir, "audience.txt")
    writeLines("Existing audience", audience_path)

    testthat::local_mocked_bindings(
      prompt_llm = function(...) {
        stop("LLM should not be called")
      },
      .package = "llmR"
    )

    context <- NULL
    expect_message(
      context <- generate_context(
        target_dir = getwd(),
        material_dir = "documentation",
        generate_expected_agenda = FALSE,
        generate_event_description = FALSE,
        generate_audience = TRUE,
        generate_vocabulary = FALSE,
        generate_initial_prompt = FALSE,
        audience = "Manual audience",
        overwrite = FALSE
      ),
      regexp = "Ignoring provided audience"
    )

    expect_identical(context$audience, "Existing audience")
  })
})

test_that("generate_context uses minutemaker_context_gen_llm_model and restores the original model", {
  withr::with_tempdir({
    material_dir <- file.path(getwd(), "documentation")
    dir.create(material_dir)
    writeLines("Materials only", file.path(material_dir, "notes.txt"))

    captured_models <- character(0)

    testthat::local_mocked_bindings(
      set_llmr_model = function(model) {
        captured_models <<- c(captured_models, model)
        options(llmr_current_model = model)
        invisible(NULL)
      },
      prompt_llm = function(messages, ...) {
        "{\"event_description\": \"Short description.\"}"
      },
      .package = "llmR"
    )

    context <- NULL
    withr::with_options(
      list(
        minutemaker_context_gen_llm_model = "mm_gpt-4.1_azure",
        llmr_current_model = "orig_model"
      ),
      {
        context <- generate_context(
          target_dir = getwd(),
          material_dir = "documentation",
          strategy = "one_pass",
          generate_expected_agenda = FALSE,
          generate_event_description = TRUE,
          generate_audience = FALSE,
          generate_vocabulary = FALSE,
          generate_initial_prompt = FALSE,
          overwrite = TRUE
        )
      }
    )

    expect_true(length(captured_models) >= 2)
    expect_identical(captured_models[[1]], "mm_gpt-4.1_azure")
    expect_identical(tail(captured_models, 1)[[1]], "orig_model")
    expect_identical(context$event_description, "Short description.")
  })
})

test_that("expand_mm_include supports one-sided bounds and falls back to whole file on empty slices", {
  materials <- list(
    "agenda.txt" = paste(
      c(
        "Header",
        "<<<START>>>",
        "Line A",
        "Line B",
        "<<<END>>>",
        "Footer"
      ),
      collapse = "\n"
    )
  )

  # Two-sided bounds exclude the marker lines.
  two_sided <- minutemaker:::.gen_cntx_expand_mm_include(
    "<mm_include source_id=\"agenda.txt\" start=\"<<<START>>>\" end=\"<<<END>>>\" />",
    materials = materials
  )
  expect_identical(two_sided, "Line A\nLine B")

  # One-sided bounds work.
  start_only <- minutemaker:::.gen_cntx_expand_mm_include(
    "<mm_include source_id=\"agenda.txt\" start=\"<<<START>>>\" />",
    materials = materials
  )
  expect_match(start_only, "Line A", fixed = TRUE)
  expect_match(start_only, "Footer", fixed = TRUE)

  end_only <- minutemaker:::.gen_cntx_expand_mm_include(
    "<mm_include source_id=\"agenda.txt\" end=\"<<<END>>>\" />",
    materials = materials
  )
  expect_match(end_only, "Header", fixed = TRUE)
  expect_match(end_only, "Line B", fixed = TRUE)

  # Empty slice falls back to including the whole file.
  empty_slice <- minutemaker:::.gen_cntx_expand_mm_include(
    "<mm_include source_id=\"agenda.txt\" start=\"<<<END>>>\" end=\"<<<END>>>\" />",
    materials = materials
  )
  expect_match(empty_slice, "Header", fixed = TRUE)
  expect_match(empty_slice, "Footer", fixed = TRUE)
})

test_that("write_transcript_chunks persists line-bounded chunk files and index", {
  withr::with_tempdir({
    chunks_dir <- file.path(getwd(), "chunks")
    transcript <- paste(rep("hello world", 2000), collapse = "\n")

    index <- minutemaker:::.gen_cntx_write_transcript_chunks(
      transcript_text = transcript,
      chunks_dir = chunks_dir,
      target_tokens = 50,
      overwrite = TRUE
    )

    expect_true(file.exists(file.path(chunks_dir, "chunks_index.csv")))
    expect_true(nrow(index) >= 1)
    expect_true(all(file.exists(index$chunk_path)))

    # Chunks are written as full lines and never empty.
    chunk_text <- readLines(index$chunk_path[[1]])
    expect_true(length(chunk_text) >= 1)
    expect_identical(chunk_text[[1]], "hello world")
  })
})

test_that("generate_context runs agentic strategy and writes final files at the end", {
  withr::with_tempdir({
    material_dir <- file.path(getwd(), "documentation")
    dir.create(material_dir)
    writeLines("Agenda:\n- Welcome", file.path(material_dir, "agenda.txt"))

    pass1 <- paste(
      c(
        "<mm_context target=\"event_description\">Short desc.</mm_context>",
        "<mm_context target=\"audience\">Leads.</mm_context>",
        "<mm_context target=\"vocabulary\">[\"ECDC\"]</mm_context>",
        "<mm_context target=\"expected_agenda\"><mm_include source_id=\"agenda.txt\" /></mm_context>",
        "<mm_context target=\"initial_prompt\">Prompt.</mm_context>"
      ),
      collapse = "\n"
    )

    pass3 <- paste(
      c(
        "<mm_final target=\"event_description\">Final desc.</mm_final>",
        "<mm_final target=\"audience\">Final audience.</mm_final>",
        "<mm_final target=\"vocabulary\">[\"ECDC\",\"MRSA\"]</mm_final>",
        "<mm_final target=\"expected_agenda\">- Welcome</mm_final>",
        "<mm_final target=\"initial_prompt\">Final prompt.</mm_final>"
      ),
      collapse = "\n"
    )

    call_count <- 0L
    testthat::local_mocked_bindings(
      prompt_llm = function(messages, ...) {
        call_count <<- call_count + 1L
        user_prompt <- messages[["user"]]
        if (stringr::str_detect(user_prompt, "Pass 1:")) {
          return(pass1)
        }
        if (stringr::str_detect(user_prompt, "Pass 3:")) {
          return(pass3)
        }
        return("<mm_diff target=\"vocabulary\" />")
      },
      .package = "llmR"
    )

    context <- generate_context(
      target_dir = getwd(),
      material_dir = "documentation",
      strategy = "agentic",
      overwrite = TRUE
    )

    expect_true(file.exists(file.path("context", "event_description.txt")))
    expect_true(file.exists(file.path("context", "vocabulary.json")))
    expect_true(file.exists(file.path("context", "expected_agenda.txt")))

    expect_identical(context$event_description, "Final desc.")
    vocab <- jsonlite::fromJSON(file.path("context", "vocabulary.json"))
    expect_true("MRSA" %in% vocab)
    expect_true(call_count >= 2)
  })
})
