#' Generate meeting context from documentation materials
#'
#' This function scans documentation materials (and optional transcripts) to
#' generate agenda expectations, event description, audience, vocabulary, and
#' optional speech-to-text prompts. Generated context is cached in per-field
#' files under a `context` folder.
#'
#' @param target_dir A directory containing meeting materials and workflow
#'   inputs.
#' @param material_dir Folder name (relative to `target_dir`) containing
#'   documentation materials. Defaults to the
#'   `minutemaker_context_material_dir` option or `"documentation"`.
#' @param overwrite Logical value indicating whether existing context files
#'   should be overwritten. Defaults to the
#'   `minutemaker_overwrite_context` option or `FALSE`.
#' @param strategy One of `one_pass` or `agentic`. `one_pass` uses a
#'   single-agent generation flow that requests all outputs in one LLM call.
#'   `agentic` runs a multi-agent pipeline that first drafts context from
#'   documentation materials, then proposes transcript-driven edits in batches,
#'   and finally consolidates everything into final context files written at
#'   the end.
#' @param generate_expected_agenda Logical value indicating whether to generate
#'   the expected agenda.
#' @param generate_event_description Logical value indicating whether to
#'   generate the event description.
#' @param generate_audience Logical value indicating whether to generate the
#'   audience description.
#' @param generate_vocabulary Logical value indicating whether to generate the
#'   vocabulary list.
#' @param generate_initial_prompt Logical value indicating whether to generate
#'   the speech-to-text initial prompt.
#' @param expected_agenda Optional pre-existing expected agenda content.
#' @param event_description Optional pre-existing event description.
#' @param audience Optional pre-existing audience description.
#' @param vocabulary Optional pre-existing vocabulary.
#' @param stt_initial_prompt Optional pre-existing initial prompt.
#' @param external_transcript Optional path to an external transcript file to
#'   use as additional material.
#' @param llm_provider Optional provider passed to `llmR::prompt_llm`.
#' @param ... Additional arguments passed to `llmR::prompt_llm`.
#'
#' @details
#' By default, the active llmR model (option `llmr_current_model`) is used.
#' To force a specific model for context generation, set the option
#' `minutemaker_context_gen_llm_model` to a registered llmR model label
#' (e.g., `"mm_gpt-4.1_azure"`).
#'
#' @return A list containing generated context values.
#'
#' @examples
#' \dontrun{
#' generate_context(target_dir = "meeting_folder")
#' }
#'
#' @seealso `speech_to_summary_workflow()`
#'
#' @export
generate_context <- function(
  target_dir = getwd(),
  material_dir = getOption("minutemaker_context_material_dir", "documentation"),
  overwrite = getOption("minutemaker_overwrite_context", FALSE),
  strategy = getOption("minutemaker_context_gen_strategy", "agentic"),
  generate_expected_agenda = TRUE,
  generate_event_description = TRUE,
  generate_audience = TRUE,
  generate_vocabulary = TRUE,
  generate_initial_prompt = TRUE,
  expected_agenda = NULL,
  event_description = NULL,
  audience = NULL,
  vocabulary = NULL,
  stt_initial_prompt = NULL,
  external_transcript = NULL,
  llm_provider = getOption("llmr_llm_provider"),
  ...
) {
  # Input validation ----
  # Ensure target_dir exists and points to a directory (not a file).
  # Normalize path to handle relative paths consistently throughout.
  if (!rlang::is_string(target_dir) || !nzchar(target_dir)) {
    cli::cli_abort(
      c(
        "Invalid {.arg target_dir} provided.",
        "x" = "Expected a non-empty string."
      )
    )
  }

  # Normalize the target path early so downstream checks are consistent.
  target_dir <- fs::path_norm(target_dir)
  if (fs::file_exists(target_dir) && !fs::dir_exists(target_dir)) {
    # If user provides a file path, use its parent directory instead.
    # This allows flexible input while maintaining directory-based operations.
    cli::cli_alert_warning(
      "The provided {.path {target_dir}} points to a file. Using its parent directory: {.path {fs::path_dir(target_dir)}}"
    )
    target_dir <- fs::path_dir(target_dir)
  }

  if (!fs::dir_exists(target_dir)) {
    cli::cli_abort(
      c(
        "Target directory not found.",
        "x" = "The path supplied to {.path target_dir} does not exist: {.path {target_dir}}"
      )
    )
  }

  # Validate material_dir is a non-empty string (absolute or relative path).
  if (!rlang::is_string(material_dir) || !nzchar(material_dir)) {
    cli::cli_abort("The {.arg material_dir} must be a non-empty string.")
  }

  # Ensure overwrite is a logical value for cache control decisions.
  if (!rlang::is_bool(overwrite)) {
    cli::cli_abort("The {.arg overwrite} must be TRUE or FALSE.")
  }

  # Validate strategy argument against allowed values.
  strategy <- match.arg(strategy, c("one_pass", "agentic"))

  # Model override system ----
  # Allow per-function model override via option, similar to transcript correction.
  # This enables different models for different pipeline stages without global changes.
  context_model_label <- getOption("minutemaker_context_gen_llm_model")
  if (!is.null(context_model_label)) {
    # Save current model for restoration on exit (cleanup).
    original_active_model <- getOption("llmr_current_model")
    on.exit(
      {
        # Restore original model or clear if none was set.
        if (is.null(original_active_model)) {
          options(llmr_current_model = NULL)
        } else {
          llmR::set_llmr_model(original_active_model)
        }
      },
      add = TRUE
    )
    # Apply the context-specific model override.
    llmR::set_llmr_model(context_model_label)
    cli::cli_alert_info(
      "Using context generation model: {.val {context_model_label}}"
    )
  }

  # === PATH SETUP ===
  # Define output directory for context files (cached outputs).
  context_dir <- file.path(target_dir, "context")
  # Resolve material directory path (absolute or relative to target_dir).
  material_path <- if (fs::is_absolute_path(material_dir)) {
    material_dir
  } else {
    file.path(target_dir, material_dir)
  }

  # Context field definitions ----
  # Define the 5 context outputs this function can generate.
  # Each field maps to a file, type, and user-facing label for consistent handling.
  # prompt_key: JSON field name in LLM responses
  # file: output filename for caching
  # type: "text" (plain) or "json" (structured, requires parsing)
  # label: human-readable name for messages
  context_fields <- list(
    expected_agenda = list(
      prompt_key = "expected_agenda", # LLM response field name
      file = "expected_agenda.txt", # Output filename
      type = "text", # Plain text output
      label = "expected agenda" # Display name
    ),
    event_description = list(
      prompt_key = "event_description",
      file = "event_description.txt",
      type = "text",
      label = "event description"
    ),
    audience = list(
      prompt_key = "audience",
      file = "audience.txt",
      type = "text",
      label = "audience"
    ),
    vocabulary = list(
      prompt_key = "vocabulary", # LLM response field name
      file = "vocabulary.json", # JSON array of terms
      type = "json", # Requires JSON parsing
      label = "vocabulary"
    ),
    stt_initial_prompt = list(
      prompt_key = "initial_prompt",
      file = "initial_prompt.txt",
      type = "text",
      label = "initial prompt"
    )
  )

  # Field selection ----
  # Build boolean vector of which fields the caller wants generated.
  # Convert to character vector of field names for downstream filtering.
  requested_fields <- c(
    expected_agenda = isTRUE(generate_expected_agenda),
    event_description = isTRUE(generate_event_description),
    audience = isTRUE(generate_audience),
    vocabulary = isTRUE(generate_vocabulary),
    stt_initial_prompt = isTRUE(generate_initial_prompt)
  )
  requested_fields <- names(requested_fields)[requested_fields]

  # Helper functions ----
  # These functions standardize input/output handling across all field types.

  # normalize_text: Convert various input types to a single trimmed string.
  # Handles NULL inputs, lists, and character vectors uniformly.
  normalize_text <- function(value) {
    # Treat missing inputs as absent so we can skip them cleanly.
    if (is.null(value)) {
      return(NULL)
    }

    # Flatten list inputs (e.g., JSON arrays) into a character vector.
    if (is.list(value)) {
      value <- unlist(value, use.names = FALSE)
    }

    # Coerce to character and drop empties introduced by coercion.
    value <- as.character(value)
    value <- value[!is.na(value)]
    if (length(value) == 0) {
      return(NULL)
    }

    # Keep original line breaks but trim trailing whitespace.
    stringr::str_trim(paste(value, collapse = "\n"))
  }

  # normalize_vocab: Convert vocabulary inputs to unique character vector.
  # Vocabulary is stored as JSON arrays, so handle list/vector inputs and deduplication.
  normalize_vocab <- function(value) {
    # Missing vocabulary should stay NULL to avoid creating empty files.
    if (is.null(value)) {
      return(NULL)
    }

    # Flatten list inputs from JSON or user-provided vectors.
    if (is.list(value)) {
      value <- unlist(value, use.names = FALSE)
    }

    # Drop explicit empties so we don't write blank entries.
    value <- as.character(value)
    value <- value[!is.na(value) & value != ""]
    if (length(value) == 0) {
      return(NULL)
    }

    # Keep unique terms in case the model repeats entries.
    unique(value)
  }

  # read_context_file: Load cached context file and normalize to expected type.
  # Handles both JSON (vocabulary) and text files with consistent parsing.
  read_context_file <- function(path, type) {
    # Vocabulary is structured JSON; everything else is stored as text.
    if (type == "json") {
      value <- .gen_cntx_parse_json(
        readr::read_file(path),
        "context vocabulary file"
      )
      return(normalize_vocab(value))
    }

    normalize_text(readr::read_file(path))
  }

  # write_context_file: Persist normalized context value to disk.
  # JSON files are pretty-printed for readability; text files preserve formatting.
  write_context_file <- function(path, value, type) {
    # Preserve vocabulary structure, but keep other files human-readable.
    if (type == "json") {
      jsonlite::write_json(
        value,
        path,
        auto_unbox = TRUE,
        pretty = TRUE
      )
      return(invisible(path))
    }

    readr::write_lines(value, path)
    invisible(path)
  }

  # format_context_materials: Convert context values to human-readable prompt blocks.
  # Used when providing existing context as additional material to LLMs.
  format_context_materials <- function(values, fields) {
    # Build a labelled block for each supplied context field.
    entries <- purrr::imap(values, \(value, name) {
      if (is.null(value)) {
        return(NULL)
      }

      info <- fields[[name]]
      label <- info$label

      # Expand vocabulary entries into bullet-style text for readability.
      if (info$type == "json") {
        value <- paste0("- ", value, collapse = "\n")
      }

      paste0(label, ":\n", value)
    })

    entries <- entries[!purrr::map_lgl(entries, is.null)]
    if (length(entries) == 0) {
      return(NULL)
    }

    paste(entries, collapse = "\n\n")
  }

  # Helper: Build the full LLM prompt while keeping rules at the end.
  build_prompt <- function(
    materials,
    transcript_text,
    provided_context,
    generated_context,
    requested_fields,
    has_external_transcript
  ) {
    # Helper: Format materials into tagged blocks for safer parsing.
    format_materials <- function(materials) {
      # Use tags so the model can distinguish each source file.
      entries <- purrr::imap(materials, \(text, name) {
        paste0("<file name=\"", name, "\">\n", text, "\n</file>")
      })

      paste(entries, collapse = "\n\n")
    }

    # Helper: Assemble field-specific rules with examples.
    build_field_rules <- function(fields, has_external_transcript) {
      # Keep the shared rules concise so they remain stable across outputs.
      rules <- c(
        "Use only the provided materials and transcript.",
        "Do not invent names or agenda items.",
        "You may infer the audience, description, and vocabulary when implied.",
        "Return only the requested field content. Do not add labels or headings (e.g., avoid \"Event description:\").",
        "Do not repeat or paraphrase these instructions. Output only the requested content.",
        "If you return null for a field and user-provided context exists, it will be used as-is. Prefer improving it when possible."
      )

      # Highlight that external transcripts are error-prone to trigger correction.
      if (isTRUE(has_external_transcript)) {
        rules <- c(
          rules,
          paste(
            "The external transcript likely contains transcription errors.",
            "Infer correct names, acronyms, and terms using materials and world knowledge.",
            "Use participant lists or context materials to fix misspellings."
          )
        )
      }

      # Agenda needs to be verbatim if a source agenda exists.
      if ("expected_agenda" %in% fields) {
        rules <- c(
          rules,
          paste(
            "Expected agenda: Only output if an explicit agenda is present in the materials.",
            "If present, copy it verbatim and preserve ordering (including times and session labels).",
            "If not present, return null.",
            "Do not summarize or rewrite agenda items.",
            "Example:\n- Welcome and agenda adoption\n- Work Package 2 update\n- Decisions and next steps",
            "Bad example (do NOT do this):\n\"Meeting agenda includes updates and discussions.\""
          )
        )
      }

      # Description can be inferred but should remain concise.
      if ("event_description" %in% fields) {
        rules <- c(
          rules,
          paste(
            "Event description: 2-8 sentences, concise but complete.",
            "Focus on purpose, scope, and main themes.",
            "Example: Internal update meeting on milestones, risks, and next steps."
          )
        )
      }

      # Audience is often implicit, so allow inference.
      if ("audience" %in% fields) {
        rules <- c(
          rules,
          paste(
            "Audience: infer likely readers if not explicit.",
            "Include roles and knowledge level.",
            "Example: Project managers and technical leads familiar with the topic."
          )
        )
      }

      # Vocabulary should capture people, organisations, and technical terms.
      if ("vocabulary" %in% fields) {
        rules <- c(
          rules,
          paste(
            "Vocabulary: include participant names, organisations, projects, and acronyms.",
            "REQUIREMENTS:",
            "- You MUST include ALL participant names mentioned in materials or transcripts.",
            "- You MUST include ALL acronyms and technical terms used in the meeting.",
            "- Use \"TERM: short explanation\" for uncommon acronyms and just \"TERM\" for common ones.",
            "- Correct misspellings using participant lists, materials, or world knowledge.",
            "- If a participant list is present, include ALL participant names you can find.",
            "- Minimum expectation: at least 15-20 entries for a typical meeting.",
            "- Prioritize completeness over brevity; it is better to include too many terms than too few.",
            "Example: [\"MRSA: methicillin-resistant Staphylococcus aureus\", \"ECDC\", \"Jane Smith\"]"
          )
        )
      }

      # Initial prompt is only needed when STT is requested.
      if ("stt_initial_prompt" %in% fields) {
        rules <- c(
          rules,
          paste(
            "Initial prompt (STT): 1-2 sentences listing key terms and names.",
            "Keep it short to avoid model limits.",
            "Output the prompt itself, not instructions about the prompt.",
            "Example: Ensure accurate transcription for Project Alpha, MRSA, Jane Smith."
          )
        )
      }

      paste(rules, collapse = "\n")
    }

    # Helper: Define output format instructions for single-call JSON generation.
    build_output_rules <- function(fields) {
      field_keys <- vapply(
        fields,
        function(field) {
          context_fields[[field]]$prompt_key
        },
        character(1)
      )

      key_list <- paste(field_keys, collapse = ", ")
      paste(
        "Output format: Return a JSON object with keys:",
        key_list,
        ".",
        "For text fields return a string or null.",
        "For vocabulary return a JSON array.",
        "Wrap the JSON object inside <json_context_output>...</json_context_output> tags.",
        "No extra keys or text outside the tags."
      )
    }

    field_rules <- build_field_rules(requested_fields, has_external_transcript)
    output_rules <- build_output_rules(requested_fields)

    # Build the prompt sequentially so materials remain ahead of instructions.
    prompt_sections <- character(0)

    # Put the transcript first to keep material context at the top of the prompt.
    if (!is.null(transcript_text)) {
      prompt_sections <- c(
        prompt_sections,
        "Transcript:",
        "<transcript>",
        transcript_text,
        "</transcript>"
      )
    }

    # Follow with documentation materials so the LLM can use both sources.
    prompt_sections <- c(
      prompt_sections,
      "Materials:",
      "<materials>",
      format_materials(materials),
      "</materials>"
    )

    # Include any caller-provided context to improve or refine it.
    if (!is.null(provided_context)) {
      prompt_sections <- c(
        prompt_sections,
        "Provided context (may be partial or rough):",
        provided_context
      )
    }

    # Carry forward previously generated fields to guide later outputs.
    if (!is.null(generated_context) && length(generated_context) > 0) {
      generated_block <- format_context_materials(
        generated_context,
        context_fields
      )
      if (!is.null(generated_block)) {
        prompt_sections <- c(
          prompt_sections,
          "Generated context so far:",
          generated_block
        )
      }
    }

    # Keep rules last to preserve them in the model's final context window.
    prompt_sections <- c(
      prompt_sections,
      "Rules:",
      field_rules,
      output_rules
    )

    paste(prompt_sections, collapse = "\n")
  }

  # === INITIALIZE CONTEXT VALUES ===
  # Normalize all user-provided context inputs to standard formats.
  # These may come from function arguments or be NULL if not provided.
  context_values <- list(
    expected_agenda = normalize_text(expected_agenda),
    event_description = normalize_text(event_description),
    audience = normalize_text(audience),
    vocabulary = normalize_vocab(vocabulary),
    stt_initial_prompt = normalize_text(stt_initial_prompt)
  )

  # Nothing to generate when all requested flags are FALSE.
  if (length(requested_fields) == 0) {
    return(context_values)
  }

  # Main execution ----
  cli::cli_h1("Context Generation")

  ## Cache checking ----
  # Respect existing cached files unless overwrite is requested.
  # This allows incremental generation and avoids redundant LLM calls.
  fields_from_files <- character(0)
  for (field in requested_fields) {
    field_info <- context_fields[[field]]
    context_path <- file.path(context_dir, field_info$file)

    if (!isTRUE(overwrite) && fs::file_exists(context_path)) {
      # Load existing context file instead of regenerating.
      cli::cli_alert_info(
        "Using existing context file for {field_info$label}: {.path {context_path}}"
      )

      if (!is.null(context_values[[field]])) {
        # Warn if user provided conflicting input that will be ignored.
        cli::cli_alert_info(
          "Ignoring provided {field_info$label} because a context file exists."
        )
      }

      # Load and normalize the cached file content.
      context_values[[field]] <- read_context_file(
        context_path,
        field_info$type
      )
      fields_from_files <- c(fields_from_files, field)
    }
  }

  # Only generate fields that were not loaded from disk.
  fields_to_generate <- setdiff(requested_fields, fields_from_files)
  if (length(fields_to_generate) == 0) {
    return(context_values)
  }

  ## Material loading ----
  # Read documentation materials and external transcript for LLM processing.
  materials <- .gen_cntx_read_materials(material_path)
  transcript_text <- .gen_cntx_read_external_transcript(external_transcript)

  if (length(materials) == 0 && is.null(transcript_text)) {
    # Cannot generate context without any input materials.
    cli::cli_alert_info(
      "No documentation materials or external transcript found. Skipping LLM context generation."
    )
    return(context_values)
  }

  ## Context formatting ----
  # Format existing context values as additional material for LLM prompts.
  # This helps maintain consistency across generated fields.
  provided_context <- format_context_materials(
    context_values,
    context_fields
  )

  ## LLM calling utilities ----

  # safe_prompt_llm: Robust wrapper for llmR::prompt_llm with retry logic.
  # Handles empty responses and transient failures with linear backoff.
  safe_prompt_llm <- function(messages, max_retries = 3L, ...) {
    for (attempt in seq_len(max_retries)) {
      result <- tryCatch(
        {
          # Call the underlying LLM function with all provided arguments.
          raw <- do.call(
            llmR::prompt_llm,
            c(list(messages = messages), list(...))
          )
          # Check if response is effectively empty (too short to be useful).
          if (rlang::is_string(raw) && nchar(trimws(raw)) < 10L) {
            if (attempt < max_retries) {
              cli::cli_alert_warning(
                "LLM returned very short response (attempt {attempt}/{max_retries}). Retrying..."
              )
              Sys.sleep(2L * attempt) # Linear backoff
              next
            } else {
              cli::cli_alert_warning(
                "LLM returned very short response after {max_retries} attempts. Using empty result."
              )
            }
          }
          return(raw)
        },
        error = function(e) {
          if (attempt < max_retries) {
            cli::cli_alert_warning(
              "LLM call failed (attempt {attempt}/{max_retries}): {.val {conditionMessage(e)}}. Retrying..."
            )
            Sys.sleep(2L * attempt) # Linear backoff
            return(NULL) # Continue retry loop
          } else {
            # All retries exhausted - abort with informative error.
            cli::cli_abort(
              c(
                "LLM call failed after {max_retries} attempts.",
                "x" = "Last error: {conditionMessage(e)}"
              ),
              call = NULL
            )
          }
        }
      )
      if (!is.null(result)) {
        return(result)
      }
    }
    # Fallback: return empty string if all retries exhausted without error.
    return("")
  }

  # Helper: Parse the LLM JSON response with consistent error handling.
  parse_json <- function(
    json_text,
    label,
    expected = c("any", "object", "array")
  ) {
    .gen_cntx_parse_json(json_text, label, expected = expected)
  }

  # Helper: Normalize LLM values based on expected type.
  normalize_llm_value <- function(value, type) {
    # Respect null outputs so we can fall back to provided context.
    if (is.null(value)) {
      return(NULL)
    }

    if (type == "json") {
      return(normalize_vocab(value))
    }

    normalize_text(value)
  }

  # Strategy execution ----
  # Execute the selected generation strategy (one_pass or agentic).

  ## Agentic strategy ----
  # Multi-pass approach for higher quality and robustness.
  # Pass 1: Draft context from documentation materials only.
  # Pass 2: Process transcript in batches, proposing diffs to improve drafts.
  # Pass 3: Consolidate all drafts and diffs into final context files.
  # Benefits: Better handling of long transcripts, intermediate caching, error isolation.
  if (strategy == "agentic") {
    # Prepare LLM arguments for all agentic calls.
    llm_args <- rlang::list2(...)
    if (!"provider" %in% names(llm_args) && !is.null(llm_provider)) {
      llm_args$provider <- llm_provider
    }

    # Create hidden directory for intermediate state and caching.
    state_dir <- file.path(target_dir, ".context_gen", "context_generation")
    fs::dir_create(state_dir, recurse = TRUE)

    ### Agentic helper functions ----

    # prompt_key_to_field: Convert LLM prompt target names to internal field keys.
    # E.g., "initial_prompt" (used in prompts) -> "stt_initial_prompt" (internal key).
    prompt_key_to_field <- function(prompt_key) {
      match <- purrr::keep(
        names(context_fields),
        \(field) identical(context_fields[[field]]$prompt_key, prompt_key)
      )
      if (length(match) == 0) {
        return(NULL)
      }
      match[[1]]
    }

    # field_to_prompt_key: Convert internal field keys to LLM prompt target names.
    # E.g., "stt_initial_prompt" (internal) -> "initial_prompt" (in prompts).
    field_to_prompt_key <- function(field) {
      context_fields[[field]]$prompt_key
    }

    # Helper: Build an "examples/templates" block from user-provided inputs.
    # These may contain placeholders; real data comes from materials/transcript.
    provided_templates <- function(values, fields) {
      entries <- purrr::imap(values, \(value, name) {
        if (is.null(value)) {
          return(NULL)
        }
        info <- fields[[name]]
        label <- info$label
        if (info$type == "json") {
          value <- paste0("- ", value, collapse = "\n")
        }
        paste0(label, " (template/example):\n", value)
      })
      entries <- entries[!purrr::map_lgl(entries, is.null)]
      if (length(entries) == 0) {
        return(NULL)
      }
      paste(entries, collapse = "\n\n")
    }

    # Format user-provided templates as examples for the LLM.
    template_block <- provided_templates(context_values, context_fields)
    # Convert internal field names to prompt target names for LLM communication.
    prompt_targets <- vapply(
      fields_to_generate,
      field_to_prompt_key,
      character(1)
    )

    ### Pass 1: documentation-only draft ----
    # Generate initial context drafts using ONLY documentation materials.
    # Model can request verbatim inclusion of materials via <mm_include> tags.
    # This establishes baseline context before transcript refinement.
    cli::cli_alert("Pass 1: documentation-only draft")
    pass1_raw_path <- file.path(state_dir, "pass1_raw.txt")
    pass1_dir <- file.path(state_dir, "pass1_expanded")
    fs::dir_create(pass1_dir, recurse = TRUE)

    if (isTRUE(overwrite) || !fs::file_exists(pass1_raw_path)) {
      # Format documentation materials as XML blocks for structured LLM input.
      material_entries <- purrr::imap(materials, \(text, source_id) {
        paste0(
          "<file source_id=\"",
          source_id,
          "\">\n",
          text,
          "\n</file>"
        )
      })
      # Wrap all materials in a single <materials> block, or use empty tag if none.
      material_block <- if (length(material_entries) == 0) {
        "<materials />"
      } else {
        paste0(
          "<materials>\n",
          paste(material_entries, collapse = "\n\n"),
          "\n</materials>"
        )
      }

      # Define rules for Pass 1 LLM behavior.
      pass1_rules <- c(
        "You are generating meeting context for a summarisation workflow.",
        "Use ONLY the provided documentation materials below.", # No external knowledge
        "You may write freely outside tags, but ONLY content inside tags will be used.", # Tag discipline
        "For each requested target, output exactly one <mm_context> block.", # Structured output
        "If a material already contains the exact content for a target,", # Efficiency
        "prefer emitting <mm_include source_id=\"...\" ... /> inside <mm_context>", # Verbatim inclusion
        "instead of rewriting it.",
        "The user may provide templates/examples. Treat them as examples only.", # Template guidance
        "Real data comes from materials, not from the templates."
      )

      # Define required output format for Pass 1.
      output_rules <- c(
        paste0(
          "Requested targets: ",
          paste(prompt_targets, collapse = ", "),
          "."
        ),
        "Output format:",
        "- For each requested target, emit:",
        "  <mm_context target=\"TARGET\">...</mm_context>", # Required structure
        "- For vocabulary, the content inside the tag must be a JSON array.", # Type-specific
        "- For other targets, the content is plain text (no headings/labels).", # Plain text
        "- If you do not want to provide content for a target, emit an empty tag." # Optional content
      )

      field_specific_rules <- c(
        "General:",
        "- Do not invent names or agenda items.",
        "- Keep outputs concise and directly usable.",
        "- Do not add labels/headings like \"Event description:\"."
      )

      if ("expected_agenda" %in% prompt_targets) {
        field_specific_rules <- c(
          field_specific_rules,
          "",
          "Expected agenda:",
          "- Only output if an explicit agenda is present in the materials.",
          "- If present, copy it verbatim and preserve ordering (including times).",
          "- If not present, emit an empty <mm_context> tag.",
          "- Do not summarize or rewrite agenda items."
        )
      }

      if ("event_description" %in% prompt_targets) {
        field_specific_rules <- c(
          field_specific_rules,
          "",
          "Event description:",
          "- 2-8 sentences, concise but complete.",
          "- Focus on purpose, scope, and main themes."
        )
      }

      if ("audience" %in% prompt_targets) {
        field_specific_rules <- c(
          field_specific_rules,
          "",
          "Audience:",
          "- Infer likely readers if not explicit.",
          "- Include roles and knowledge level."
        )
      }

      if ("vocabulary" %in% prompt_targets) {
        field_specific_rules <- c(
          field_specific_rules,
          "",
          "Vocabulary:",
          "REQUIREMENTS:",
          "- You MUST include ALL participant names mentioned in materials or transcripts.",
          "- You MUST include ALL acronyms and technical terms used in the meeting.",
          "- Use \"TERM: short explanation\" for uncommon acronyms and just \"TERM\" for common ones.",
          "- Correct misspellings using participant lists, materials, or world knowledge.",
          "- If a participant list is present, include ALL participant names you can find.",
          "- Minimum expectation: at least 15-20 entries for a typical meeting.",
          "- Prioritize completeness over brevity; it is better to include too many terms than too few."
        )
      }

      if ("initial_prompt" %in% prompt_targets) {
        field_specific_rules <- c(
          field_specific_rules,
          "",
          "Initial prompt (STT):",
          "- 1-2 sentences listing key terms and names.",
          "- Keep it short."
        )
      }

      prompt_sections <- list(
        "Pass 1: documentation-only draft.",
        material_block,
        if (!is.null(template_block)) {
          paste("Templates/examples:", template_block)
        } else {
          NULL
        },
        "Rules:",
        paste(pass1_rules, collapse = "\n"),
        output_rules,
        "Field-specific rules:",
        paste(field_specific_rules, collapse = "\n")
      )
      prompt_sections <- prompt_sections[
        !purrr::map_lgl(prompt_sections, is.null)
      ]
      prompt <- paste(unlist(prompt_sections), collapse = "\n\n")

      pass1_raw <- do.call(
        safe_prompt_llm,
        c(
          list(
            messages = c(
              system = "Generate meeting context drafts from documentation.",
              user = prompt
            )
          ),
          llm_args
        )
      )

      readr::write_lines(pass1_raw, pass1_raw_path)
    } else {
      pass1_raw <- readr::read_file(pass1_raw_path)
    }

    pass1_blocks <- .gen_cntx_extract_xml_blocks(
      pass1_raw,
      tag = "mm_context"
    )
    pass1_by_target <- .gen_cntx_blocks_by_attr(
      pass1_blocks,
      attr = "target"
    )

    pass1_expanded <- list()
    for (target in prompt_targets) {
      content <- pass1_by_target[[target]]
      if (is.null(content)) {
        content <- ""
      }

      expanded <- .gen_cntx_expand_mm_include(
        content,
        materials = materials
      )
      expanded <- stringr::str_trim(expanded)
      pass1_expanded[[target]] <- expanded

      readr::write_lines(
        expanded,
        file.path(pass1_dir, paste0(target, ".txt"))
      )
    }

    ### Pass 2: transcript batch refinement ----
    # Process transcript in manageable chunks, proposing diffs to improve Pass 1 drafts.
    # Each chunk proposes context edits based on its specific transcript content.
    # Chunks are persisted to disk for resumability and parallel processing potential.
    chunks_dir <- file.path(state_dir, "transcript_chunks")
    pass2_dir <- file.path(state_dir, "pass2_chunks")
    fs::dir_create(pass2_dir, recurse = TRUE)

    # Split transcript into ~5000-token chunks for LLM processing limits.
    chunk_index <- .gen_cntx_write_transcript_chunks(
      transcript_text = transcript_text,
      chunks_dir = chunks_dir,
      target_tokens = 5000,
      overwrite = isTRUE(overwrite)
    )

    cli::cli_alert("Pass 2: transcript batching ({nrow(chunk_index)} chunks)")

    # Process each transcript chunk to generate context improvement proposals.
    chunk_diffs <- list()
    if (nrow(chunk_index) > 0) {
      for (idx in seq_len(nrow(chunk_index))) {
        chunk_id <- chunk_index$chunk_id[[idx]]
        chunk_path <- chunk_index$chunk_path[[idx]]
        raw_path <- file.path(pass2_dir, paste0(chunk_id, "_raw.txt"))

        if (isTRUE(overwrite) || !fs::file_exists(raw_path)) {
          # Process this chunk - either first time or forced overwrite.
          cli::cli_alert(
            "Pass 2: chunk {idx}/{nrow(chunk_index)} (chunk_id={chunk_id}) - calling LLM"
          )
          chunk_text <- readr::read_file(chunk_path)

          # Format current Pass 1 drafts as XML blocks for the LLM to reference.
          current_blocks <- purrr::imap(prompt_targets, \(target, ...) {
            draft <- pass1_expanded[[target]]
            if (!rlang::is_string(draft) || !nzchar(draft)) {
              draft <- "<empty />" # Handle missing drafts gracefully
            }
            paste0(
              "<current_context target=\"",
              target,
              "\">\n",
              draft,
              "\n</current_context>"
            )
          })

          # Build Pass 2 prompt: propose context edits based on this transcript chunk.
          prompt <- paste(
            paste0(
              "Pass 2: transcript batch ",
              idx,
              "/",
              nrow(chunk_index),
              "."
            ),
            "Current context draft (from documentation/templates):", # Reference baseline
            paste(current_blocks, collapse = "\n\n"),
            "Transcript batch:", # The chunk to analyze
            "<transcript_batch>",
            chunk_text,
            "</transcript_batch>",
            "Task:", # Clear instructions for the LLM
            "- Propose edits based ONLY on this transcript batch.", # Scope limitation
            "- The transcript likely contains transcription errors for names, acronyms, and technical terms, etc... in general the transcript is error prone.", # Error awareness
            "- Use documentation materials (and your general topic knowledge) to infer the correct spellings/terms/names etc...", # Correction guidance
            "- Prefer adding a corrected form to vocabulary / STT prompt over copying a misspelling from transcript.", # Quality preference
            "- Output unified diffs for affected targets, plus motivations.", # Structured output
            "- You may write freely outside tags; ONLY content inside tags is used.", # Tag discipline
            "Output format:", # Required structure
            "- <mm_diff target=\"TARGET\">...unified diff...</mm_diff>", # Diff format
            "- <mm_motivation target=\"TARGET\">...motivation...</mm_motivation>", # Reasoning
            "- If you do not want to edit a target, emit an empty <mm_diff> tag.", # Optional edits
            sep = "\n\n"
          )

          raw <- do.call(
            safe_prompt_llm,
            c(
              list(
                messages = c(
                  system = "Propose context edits from one transcript batch.",
                  user = prompt
                )
              ),
              llm_args
            )
          )

          readr::write_lines(raw, raw_path)
        } else {
          raw <- readr::read_file(raw_path)
        }

        # Extract and organize the LLM's proposed diffs and motivations by target.
        diff_blocks <- .gen_cntx_extract_xml_blocks(raw, tag = "mm_diff")
        mot_blocks <- .gen_cntx_extract_xml_blocks(
          raw,
          tag = "mm_motivation"
        )
        diffs_by_target <- .gen_cntx_blocks_by_attr(
          diff_blocks,
          attr = "target"
        )
        mot_by_target <- .gen_cntx_blocks_by_attr(mot_blocks, attr = "target")

        # Store this chunk's proposals for Pass 3 consolidation.
        chunk_diffs[[chunk_id]] <- list(
          chunk_index = idx,
          diffs = diffs_by_target,
          motivations = mot_by_target
        )
      }
    }

    ### Pass 3: consolidation ----
    # Integrate Pass 1 drafts with all Pass 2 chunk proposals into final context.
    # This is where conflicts are resolved and final outputs are produced.
    cli::cli_alert("Pass 3: consolidation")
    pass3_raw_path <- file.path(state_dir, "pass3_raw.txt")
    if (isTRUE(overwrite) || !fs::file_exists(pass3_raw_path)) {
      # Reformat Pass 1 drafts for the consolidation LLM.
      current_blocks <- purrr::imap(prompt_targets, \(target, ...) {
        draft <- pass1_expanded[[target]]
        if (!rlang::is_string(draft) || !nzchar(draft)) {
          draft <- "<empty />"
        }
        paste0(
          "<current_context target=\"",
          target,
          "\">\n",
          draft,
          "\n</current_context>"
        )
      })

      # Format all Pass 2 chunk proposals as chronological diff sequence.
      diffs_section <- NULL
      if (length(chunk_diffs) > 0) {
        diff_entries <- purrr::imap(chunk_diffs, \(chunk, chunk_id) {
          # For each chunk, format diffs and motivations per target.
          per_target <- purrr::map(prompt_targets, \(target) {
            diff <- chunk$diffs[[target]]
            motivation <- chunk$motivations[[target]]
            if (is.null(diff)) {
              diff <- ""
            }
            if (is.null(motivation)) {
              motivation <- ""
            }
            paste0(
              "<chunk id=\"",
              chunk_id,
              "\" index=\"",
              chunk$chunk_index,
              "\">\n",
              "<mm_diff target=\"",
              target,
              "\">\n",
              diff,
              "\n</mm_diff>\n",
              "<mm_motivation target=\"",
              target,
              "\">\n",
              motivation,
              "\n</mm_motivation>\n",
              "</chunk>"
            )
          })
          paste(per_target, collapse = "\n\n")
        })
        diffs_section <- paste(
          "Transcript diff proposals (chronological):",
          paste(diff_entries, collapse = "\n\n"),
          sep = "\n\n"
        )
      } else {
        diffs_section <- "Transcript diff proposals: none."
      }

      prompt_sections <- list(
        "Pass 3: consolidate drafts + transcript diffs into final context.",
        "Current context draft:",
        paste(current_blocks, collapse = "\n\n"),
        if (!is.null(template_block)) {
          paste(
            "Templates/examples (example only; real data is above):",
            template_block,
            sep = "\n\n"
          )
        } else {
          NULL
        },
        diffs_section,
        "Task:",
        "- Produce the final content for each requested target.",
        "- Integrate documentation draft, templates, and transcript diffs.",
        "- Resolve conflicts; keep content consistent across targets.",
        "- The transcript likely contains transcription errors for names, acronyms, and technical terms.",
        "- Use documentation materials (and general topic knowledge) to correct spellings and normalize terminology.",
        "- You may write freely outside tags; ONLY content inside tags is used.",
        "Output format:",
        "- For each requested target, emit:",
        "  <mm_final target=\"TARGET\">...</mm_final>",
        "- For vocabulary, the content inside the tag must be a JSON array.",
        "- For other targets, the content is plain text (no headings/labels)."
      )
      prompt_sections <- prompt_sections[
        !purrr::map_lgl(prompt_sections, is.null)
      ]
      prompt <- paste(unlist(prompt_sections), collapse = "\n\n")

      pass3_raw <- do.call(
        safe_prompt_llm,
        c(
          list(
            messages = c(
              system = "Consolidate context drafts and diffs into final files.",
              user = prompt
            )
          ),
          llm_args
        )
      )

      readr::write_lines(pass3_raw, pass3_raw_path)
    } else {
      pass3_raw <- readr::read_file(pass3_raw_path)
    }

    # Extract and organize final consolidated outputs from Pass 3.
    final_blocks <- .gen_cntx_extract_xml_blocks(pass3_raw, tag = "mm_final")
    final_by_target <- .gen_cntx_blocks_by_attr(final_blocks, attr = "target")

    ### Final output writing ----
    # Write context files only after all agentic processing is complete.
    # This ensures atomic updates - either all outputs succeed or none are written.
    if (!fs::dir_exists(context_dir)) {
      fs::dir_create(context_dir, recurse = TRUE)
    }

    for (target in prompt_targets) {
      field <- prompt_key_to_field(target)
      if (is.null(field)) {
        next # Skip unknown targets
      }

      field_info <- context_fields[[field]]
      raw_value <- final_by_target[[target]]
      raw_value <- if (is.null(raw_value)) "" else raw_value
      raw_value <- stringr::str_trim(raw_value)

      # Parse and normalize the final output based on expected type.
      value <- NULL
      if (field_info$type == "json") {
        # Vocabulary: parse JSON array and normalize
        if (nzchar(raw_value) && !identical(tolower(raw_value), "null")) {
          parsed <- parse_json(
            raw_value,
            "vocabulary output",
            expected = "array"
          )
          value <- normalize_vocab(parsed)
        }
      } else {
        # Other fields: use raw text if present
        if (nzchar(raw_value) && !identical(tolower(raw_value), "null")) {
          value <- raw_value
        }
      }

      # Fallback to user-provided context if LLM returned nothing.
      if (is.null(value)) {
        value <- context_values[[field]]
        if (!is.null(value)) {
          cli::cli_alert_info(
            "LLM returned empty output for {field_info$label}. Using provided context."
          )
        }
      }

      # Write final context file and update return value.
      if (!is.null(value)) {
        context_values[[field]] <- value
        write_context_file(
          file.path(context_dir, field_info$file),
          value,
          field_info$type
        )
      }
    }

    return(context_values)
  }

  # One-pass strategy execution ----
  # Single-pass approach: generate all fields in one LLM call.

  # Ensure output directory exists before writing files.
  if (!fs::dir_exists(context_dir)) {
    fs::dir_create(context_dir, recurse = TRUE)
  }

  cli::cli_alert(
    "Generating context with a single LLM call ({length(fields_to_generate)} outputs)."
  )

  # Build comprehensive prompt with all materials and context.
  prompt <- build_prompt(
    materials = materials,
    transcript_text = transcript_text,
    provided_context = provided_context,
    generated_context = NULL,
    requested_fields = fields_to_generate,
    has_external_transcript = !is.null(transcript_text)
  )

  # Prepare LLM arguments for single-call execution.
  llm_args <- rlang::list2(...)
  if (!"provider" %in% names(llm_args) && !is.null(llm_provider)) {
    llm_args$provider <- llm_provider
  }
  # Force JSON response for structured single-call output.
  if (!"force_json" %in% names(llm_args)) {
    llm_args$force_json <- TRUE
  }

  result <- do.call(
    safe_prompt_llm,
    c(
      list(
        messages = c(
          system = "Extract meeting context for a summarisation workflow.",
          user = prompt
        )
      ),
      llm_args
    )
  )

  # Parse the JSON response once and distribute fields to outputs.
  result_json <- parse_json(
    result,
    "context JSON output",
    expected = "object"
  )

  for (field in fields_to_generate) {
    field_info <- context_fields[[field]]
    raw_value <- result_json[[field_info$prompt_key]]
    value <- normalize_llm_value(raw_value, field_info$type)

    if (is.null(value)) {
      value <- context_values[[field]]
      if (!is.null(value)) {
        cli::cli_alert_info(
          "LLM returned null for {field_info$label}. Using provided context."
        )
      }
    }

    if (!is.null(value)) {
      context_values[[field]] <- value
      write_context_file(
        file.path(context_dir, field_info$file),
        value,
        field_info$type
      )
    }
  }

  context_values
}

#' Detect whether a file looks like plain text.
#'
#' @param path Path to a file to sample.
#' @param sample_bytes Number of bytes to sample for detection.
#' @param control_threshold Maximum proportion of control bytes tolerated.
#'
#' @return Logical value indicating whether the file is likely plain text.
#'
#' @keywords internal
.gen_cntx_is_plain_text <- function(
  path,
  sample_bytes = 8192,
  control_threshold = 0.3
) {
  raw_sample <- tryCatch(
    readBin(path, "raw", n = sample_bytes),
    error = function(cnd) {
      cli::cli_warn(
        "Failed to sample {.path {path}}: {conditionMessage(cnd)}"
      )
      return(raw(0))
    }
  )

  if (length(raw_sample) == 0) {
    return(FALSE)
  }

  bytes <- as.integer(raw_sample)
  if (length(bytes) >= 2) {
    bom <- bytes[1:2]
    if (
      isTRUE(all(bom == c(0xFF, 0xFE))) ||
        isTRUE(all(bom == c(0xFE, 0xFF)))
    ) {
      return(TRUE)
    }
  }

  if (any(bytes == 0)) {
    return(FALSE)
  }

  control_bytes <- bytes < 32 & !bytes %in% c(9, 10, 13)
  mean(control_bytes) <= control_threshold
}

#' Detect text encoding from a file sample.
#'
#' @param path Path to the file to sample.
#' @param sample_bytes Number of bytes to sample for detection.
#'
#' @return Character scalar encoding label, or `NULL` if unknown.
#'
#' @keywords internal
.gen_cntx_detect_text_encoding <- function(
  path,
  sample_bytes = 8192
) {
  raw_sample <- tryCatch(
    readBin(path, "raw", n = sample_bytes),
    error = function(cnd) {
      cli::cli_warn(
        "Failed to sample {.path {path}}: {conditionMessage(cnd)}"
      )
      return(raw(0))
    }
  )

  if (length(raw_sample) == 0) {
    return(NULL)
  }

  bytes <- as.integer(raw_sample)
  if (length(bytes) >= 2) {
    bom <- bytes[1:2]
    if (isTRUE(all(bom == c(0xFF, 0xFE)))) {
      return("UTF-16LE")
    }
    if (isTRUE(all(bom == c(0xFE, 0xFF)))) {
      return("UTF-16BE")
    }
  }

  if (length(bytes) >= 3) {
    bom_utf8 <- bytes[1:3]
    if (isTRUE(all(bom_utf8 == c(0xEF, 0xBB, 0xBF)))) {
      return("UTF-8")
    }
  }

  "UTF-8"
}

#' Read documentation materials from a folder.
#'
#' @param material_dir Directory holding documentation files.
#'
#' @return Named list of file contents (character scalars).
#'
#' @keywords internal
.gen_cntx_read_materials <- function(material_dir) {
  if (!fs::dir_exists(material_dir)) {
    return(list())
  }

  files <- fs::dir_ls(material_dir, recurse = TRUE, type = "file")
  if (length(files) == 0) {
    return(list())
  }

  # Read each file and capture any file-specific failures.
  contents <- purrr::map(files, .gen_cntx_read_file)
  # Drop empty or failed reads so the prompt stays focused.
  keep <- !purrr::map_lgl(contents, \(text) {
    is.null(text) || is.na(text) || text == ""
  })

  # Align the contents with their relative paths for prompt tags.
  files <- files[keep]
  contents <- contents[keep]
  names(contents) <- fs::path_rel(files, start = material_dir)

  contents
}

#' Read a single documentation file into plain text.
#'
#' @param path Path to the file to read.
#'
#' @return Character scalar with file contents, or `NULL` when unsupported.
#'
#' @keywords internal
.gen_cntx_read_file <- function(path) {
  ext <- tolower(tools::file_ext(path))

  tryCatch(
    {
      # Office documents require `officer` for extraction.
      if (ext %in% c("docx", "pptx")) {
        rlang::check_installed("officer")
        return(.gen_cntx_read_officer(path, ext))
      }

      # Excel files are read sheet-by-sheet to preserve tables.
      if (ext == "xlsx") {
        rlang::check_installed("readxl")

        # Read each sheet as a CSV-like block to preserve tabular structure.
        sheets <- readxl::excel_sheets(path)
        if (length(sheets) == 0) {
          return(NULL)
        }

        sheet_blocks <- purrr::map(sheets, \(sheet) {
          data <- readxl::read_excel(path, sheet = sheet)
          csv <- readr::format_csv(data)
          paste0(
            "Sheet: ",
            sheet,
            "\n",
            paste(csv, collapse = "\n")
          )
        })

        return(paste(sheet_blocks, collapse = "\n\n"))
      }

      # PDF text is extracted per page and then joined.
      if (ext == "pdf") {
        rlang::check_installed("pdftools")

        pages <- pdftools::pdf_text(path)
        pages <- pages[!is.na(pages) & pages != ""]
        if (length(pages) == 0) {
          return(NULL)
        }

        return(paste(pages, collapse = "\n"))
      }

      # RTF content is stripped to plain text.
      if (ext == "rtf") {
        rlang::check_installed("striprtf")

        text <- striprtf::read_rtf(path)
        if (length(text) == 0) {
          return(NULL)
        }

        return(paste(text, collapse = "\n"))
      }

      # Default to plain-text detection for everything else.
      if (.gen_cntx_is_plain_text(path)) {
        encoding <- .gen_cntx_detect_text_encoding(path)
        if (is.null(encoding) || encoding == "UTF-8") {
          return(readr::read_file(path))
        }

        return(
          readr::read_file(path, locale = readr::locale(encoding = encoding))
        )
      }

      cli::cli_warn("Skipping unsupported non-text file: {.path {path}}")
      NULL
    },
    error = function(cnd) {
      cli::cli_warn(
        "Failed to read {.path {path}}: {conditionMessage(cnd)}"
      )
      NULL
    }
  )
}

#' Read DOCX or PPTX content with basic structure preservation.
#'
#' @param path Path to the file.
#' @param ext File extension (docx or pptx).
#'
#' @return Character scalar containing flattened text.
#'
#' @keywords internal
.gen_cntx_read_officer <- function(path, ext) {
  if (ext == "pptx") {
    ppt <- officer::read_pptx(path)
    content <- officer::pptx_summary(ppt)
    text <- content$text
    text <- text[!is.na(text) & text != ""]
    if (length(text) == 0) {
      return(NULL)
    }

    return(paste(text, collapse = "\n"))
  }

  doc <- officer::read_docx(path)
  summary <- officer::docx_summary(doc)
  summary <- summary[!is.na(summary$text) & summary$text != "", ]
  if (nrow(summary) == 0) {
    return(NULL)
  }

  # Keep paragraphs as standalone blocks to preserve narrative structure.
  paragraphs <- summary[summary$content_type == "paragraph", ]
  paragraph_blocks <- NULL
  if (nrow(paragraphs) > 0) {
    paragraph_blocks <- data.frame(
      doc_index = paragraphs$doc_index,
      text = paragraphs$text,
      stringsAsFactors = FALSE
    )
  }

  # Rebuild tables from table cells while retaining row/column alignment.
  table_cells <- summary[summary$content_type == "table cell", ]
  if (nrow(table_cells) > 0) {
    # Coerce index columns to integers for safe arithmetic and ordering.
    table_cells <- table_cells |>
      dplyr::mutate(
        table_index = suppressWarnings(as.integer(.data$table_index)),
        row_id = suppressWarnings(as.integer(.data$row_id)),
        cell_id = suppressWarnings(as.integer(.data$cell_id)),
        col_span = suppressWarnings(as.integer(.data$col_span))
      )
  }
  table_blocks <- NULL
  if (nrow(table_cells) > 0) {
    # Step 1: Sort by original doc index so we keep the in-document order.
    table_cells_sorted <- table_cells |>
      dplyr::arrange(.data$doc_index)

    # Step 2: Collapse multiple text fragments within each cell.
    cell_text <- table_cells_sorted |>
      dplyr::group_by(.data$table_index, .data$row_id, .data$cell_id) |>
      dplyr::summarise(
        text = paste(.data$text, collapse = "\n"),
        col_span = dplyr::first(.data$col_span),
        doc_index = dplyr::first(.data$doc_index),
        .groups = "drop"
      )

    # Build a markdown-like table for each table_index.
    table_blocks <- dplyr::group_split(cell_text, .data$table_index) |>
      purrr::map(\(table_data) {
        table_id <- unique(table_data$table_index)

        # Determine the total column count using spanning information.
        col_span <- table_cells$col_span[table_cells$table_index == table_id]
        col_span <- ifelse(is.na(col_span), 1, col_span)
        max_column <- table_cells$table_index == table_id
        max_column <- table_cells$cell_id[max_column] + col_span - 1
        max_column <- max(max_column, na.rm = TRUE)

        # Create a header row to keep the table machine-readable.
        header <- paste0(
          "| ",
          paste(rep("", max_column), collapse = " | "),
          " |"
        )
        separator <- paste0(
          "| ",
          paste(rep("---", max_column), collapse = " | "),
          " |"
        )

        # Step 1: Sort each row by cell order for consistent columns.
        row_cells <- table_data |>
          dplyr::arrange(.data$row_id, .data$cell_id)

        # Step 2: Turn each row into a markdown-like line.
        rows <- row_cells |>
          dplyr::group_by(.data$row_id) |>
          dplyr::group_map(\(row_data, ...) {
            row_spans <- row_data$col_span
            row_text <- row_data$text

            # If any cell spans multiple columns, collapse the row into one
            # cell.
            if (any(row_spans > 1, na.rm = TRUE)) {
              merged <- stringr::str_squish(paste(row_text, collapse = " "))
              cells <- c(merged, rep("", max_column - 1))
            } else {
              # Otherwise, place each cell in its column position.
              cells <- rep("", max_column)
              cells[row_data$cell_id] <- row_text
            }

            paste0("| ", paste(cells, collapse = " | "), " |")
          })

        data.frame(
          doc_index = min(table_data$doc_index, na.rm = TRUE),
          text = paste(c(header, separator, rows), collapse = "\n"),
          stringsAsFactors = FALSE
        )
      }) |>
      dplyr::bind_rows()
  }

  blocks <- rbind(paragraph_blocks, table_blocks)
  if (is.null(blocks) || nrow(blocks) == 0) {
    return(NULL)
  }

  blocks <- blocks[order(blocks$doc_index), ]
  paste(blocks$text, collapse = "\n\n")
}

#' Read an external transcript file as plain text.
#'
#' @param path Optional path to the transcript file.
#'
#' @return Character scalar transcript contents, or `NULL` if unavailable.
#'
#' @keywords internal
.gen_cntx_read_external_transcript <- function(path) {
  if (!rlang::is_string(path) || !nzchar(path)) {
    return(NULL)
  }

  if (!fs::file_exists(path)) {
    cli::cli_warn(
      "External transcript not found at {.path {path}}."
    )
    return(NULL)
  }

  transcript_text <- .gen_cntx_read_file(path)
  if (!rlang::is_string(transcript_text) || !nzchar(transcript_text)) {
    return(NULL)
  }

  transcript_text
}

#' Parse JSON output from an LLM response.
#'
#' @param json_text JSON string to parse.
#' @param label Label used in error messages.
#' @param expected Expected JSON type: "object", "array", or "any".
#'
#' @return Parsed JSON value.
#'
#' @keywords internal
.gen_cntx_parse_json <- function(
  json_text,
  label,
  expected = c("any", "object", "array")
) {
  expected <- match.arg(expected)

  trimmed_text <- stringr::str_trim(json_text)

  # Prefer content wrapped in explicit XML tags when present.
  xml_match <- stringr::str_match(
    trimmed_text,
    "<json_context_output>\\s*([\\s\\S]*?)\\s*</json_context_output>"
  )[, 2]
  if (!is.na(xml_match)) {
    trimmed_text <- stringr::str_trim(xml_match)
  }

  # Prefer JSON content inside code fences when present.
  fenced_text <- stringr::str_match(
    trimmed_text,
    "```(?:json)?\\s*([\\s\\S]*?)```"
  )[, 2]
  if (!is.na(fenced_text)) {
    trimmed_text <- stringr::str_trim(fenced_text)
  }

  json_candidate <- trimmed_text
  if (expected == "array") {
    candidate <- stringr::str_extract(trimmed_text, "(?s)\\[.*\\]")
    if (!is.na(candidate)) {
      json_candidate <- candidate
    }
  } else if (expected == "object") {
    candidate <- stringr::str_extract(trimmed_text, "(?s)\\{.*\\}")
    if (!is.na(candidate)) {
      json_candidate <- candidate
    }
  } else {
    candidate <- stringr::str_extract(trimmed_text, "(?s)\\[.*\\]")
    if (!is.na(candidate)) {
      json_candidate <- candidate
    } else {
      candidate <- stringr::str_extract(trimmed_text, "(?s)\\{.*\\}")
      if (!is.na(candidate)) {
        json_candidate <- candidate
      }
    }
  }

  tryCatch(
    jsonlite::fromJSON(json_candidate, simplifyVector = FALSE),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Failed to parse {label}.",
          "x" = "{conditionMessage(cnd)}"
        ),
        parent = cnd
      )
    }
  )
}

#' Parse simple XML attributes from a tag header.
#'
#' @param attrs_text Character scalar containing the raw attribute text.
#'
#' @return Named list of parsed attributes.
#'
#' @keywords internal
.gen_cntx_parse_xml_attributes <- function(attrs_text) {
  if (!rlang::is_string(attrs_text) || !nzchar(attrs_text)) {
    return(list())
  }

  matches <- stringr::str_match_all(
    attrs_text,
    "([A-Za-z0-9_:\\-]+)\\s*=\\s*\"([^\"]*)\""
  )[[1]]
  if (nrow(matches) == 0) {
    return(list())
  }

  keys <- matches[, 2]
  values <- matches[, 3]
  stats::setNames(as.list(values), keys)
}

#' Extract XML blocks by tag name.
#'
#' This is a lightweight parser intended for predictable agent outputs. It
#' supports both paired tags (<tag>...</tag>) and self-closing tags (<tag />).
#'
#' @param text Character scalar to scan.
#' @param tag Tag name without brackets.
#'
#' @return List of blocks. Each block is a list with `attrs` and `content`.
#'
#' @keywords internal
.gen_cntx_extract_xml_blocks <- function(text, tag) {
  if (!rlang::is_string(text) || !nzchar(text)) {
    return(list())
  }
  if (!rlang::is_string(tag) || !nzchar(tag)) {
    return(list())
  }

  blocks <- list()

  # Paired tags.
  paired_pattern <- paste0(
    "<",
    tag,
    "([^>]*)>([\\s\\S]*?)</",
    tag,
    ">"
  )
  paired <- stringr::str_match_all(text, paired_pattern)[[1]]
  if (nrow(paired) > 0) {
    for (idx in seq_len(nrow(paired))) {
      attrs_text <- stringr::str_trim(paired[idx, 2])
      content <- paired[idx, 3]
      blocks[[length(blocks) + 1]] <- list(
        attrs = .gen_cntx_parse_xml_attributes(attrs_text),
        content = content
      )
    }
  }

  # Self-closing tags.
  self_pattern <- paste0("<", tag, "([^>]*)\\s*/>")
  self <- stringr::str_match_all(text, self_pattern)[[1]]
  if (nrow(self) > 0) {
    for (idx in seq_len(nrow(self))) {
      attrs_text <- stringr::str_trim(self[idx, 2])
      blocks[[length(blocks) + 1]] <- list(
        attrs = .gen_cntx_parse_xml_attributes(attrs_text),
        content = ""
      )
    }
  }

  blocks
}

#' Index extracted blocks by a chosen attribute.
#'
#' @param blocks List of blocks returned by `.gen_cntx_extract_xml_blocks()`.
#' @param attr Attribute name to index on.
#'
#' @return Named list mapping attribute values to block content.
#'
#' @keywords internal
.gen_cntx_blocks_by_attr <- function(blocks, attr) {
  if (!is.list(blocks) || length(blocks) == 0) {
    return(list())
  }
  if (!rlang::is_string(attr) || !nzchar(attr)) {
    return(list())
  }

  out <- list()
  for (block in blocks) {
    attrs <- block$attrs
    if (!is.list(attrs)) {
      next
    }
    key <- attrs[[attr]]
    if (!rlang::is_string(key) || !nzchar(key)) {
      next
    }
    out[[key]] <- block$content
  }

  out
}

#' Expand <mm_include ... /> tags into the referenced material contents.
#'
#' Inclusion supports optional one-sided or two-sided bounds via `start` and/or
#' `end`. Bounds are line-based and exclude the marker lines themselves. When
#' the computed inclusion is empty, the entire file is included.
#'
#' @param content Character scalar containing include tags.
#' @param materials Named list of processed material file contents.
#'
#' @return Content with include tags replaced by included material slices.
#'
#' @keywords internal
.gen_cntx_expand_mm_include <- function(content, materials) {
  if (!rlang::is_string(content)) {
    return("")
  }
  if (!is.list(materials)) {
    materials <- list()
  }

  # Locate include tags using fixed matching to avoid regex edge cases.
  start_positions <- gregexpr("<mm_include", content, fixed = TRUE)[[1]]
  if (length(start_positions) == 1 && start_positions[[1]] == -1) {
    return(content)
  }

  slice_from_markers <- function(text, start = NULL, end = NULL) {
    if (!rlang::is_string(text) || !nzchar(text)) {
      return("")
    }
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    if (length(lines) == 0) {
      return(text)
    }

    start_hit <- NA_integer_
    if (rlang::is_string(start) && nzchar(start)) {
      hits <- which(grepl(start, lines, fixed = TRUE))
      if (length(hits) > 0) {
        start_hit <- hits[[1]]
      }
    }

    end_hit <- NA_integer_
    if (rlang::is_string(end) && nzchar(end)) {
      search_lines <- lines
      offset <- 0L
      if (!is.na(start_hit)) {
        # Allow end markers to appear on the same line as the start marker.
        # Marker lines are excluded later, so same-line markers yield an empty
        # slice and fall back to including the full file.
        search_lines <- lines[start_hit:length(lines)]
        offset <- start_hit - 1L
      }
      hits <- which(grepl(end, search_lines, fixed = TRUE))
      if (length(hits) > 0) {
        end_hit <- hits[[1]] + offset
      }
    }

    start_idx <- 1L
    if (!is.na(start_hit)) {
      start_idx <- start_hit + 1L
    }
    end_idx <- length(lines)
    if (!is.na(end_hit)) {
      end_idx <- end_hit - 1L
    }

    if (start_idx < 1L || end_idx > length(lines) || start_idx > end_idx) {
      return(text)
    }

    slice <- paste(lines[start_idx:end_idx], collapse = "\n")
    if (!nzchar(stringr::str_trim(slice))) {
      return(text)
    }

    slice
  }

  expanded <- content
  for (pos in rev(start_positions)) {
    tail_text <- substr(expanded, pos, nchar(expanded))
    end_rel <- regexpr("/>", tail_text, fixed = TRUE)[[1]]
    if (end_rel == -1) {
      next
    }

    end_pos <- pos + end_rel + 1L
    full_tag <- substr(expanded, pos, end_pos)
    attrs_text <- full_tag
    attrs_text <- sub("^<mm_include", "", attrs_text)
    attrs_text <- sub("/>$", "", attrs_text)
    attrs <- .gen_cntx_parse_xml_attributes(attrs_text)

    source_id <- attrs$source_id
    if (!rlang::is_string(source_id) || !nzchar(source_id)) {
      prefix <- if (pos > 1) substr(expanded, 1, pos - 1L) else ""
      suffix <- if (end_pos < nchar(expanded)) {
        substr(expanded, end_pos + 1L, nchar(expanded))
      } else {
        ""
      }
      expanded <- paste0(prefix, suffix)
      next
    }

    text <- materials[[source_id]]
    if (!rlang::is_string(text) || !nzchar(text)) {
      cli::cli_warn(
        "Requested include source not found or empty: {.val {source_id}}"
      )
      prefix <- if (pos > 1) substr(expanded, 1, pos - 1L) else ""
      suffix <- if (end_pos < nchar(expanded)) {
        substr(expanded, end_pos + 1L, nchar(expanded))
      } else {
        ""
      }
      expanded <- paste0(prefix, suffix)
      next
    }

    slice <- slice_from_markers(
      text,
      start = attrs$start,
      end = attrs$end
    )

    prefix <- if (pos > 1) substr(expanded, 1, pos - 1L) else ""
    suffix <- if (end_pos < nchar(expanded)) {
      substr(expanded, end_pos + 1L, nchar(expanded))
    } else {
      ""
    }
    expanded <- paste0(prefix, slice, suffix)
  }

  expanded
}

#' Persist transcript chunks to disk for downstream LLM processing.
#'
#' @param transcript_text Transcript content as a character scalar.
#' @param chunks_dir Directory where chunk files and the index are written.
#' @param target_tokens Approximate token target per chunk.
#' @param overwrite Logical indicating whether to overwrite existing chunk files.
#'
#' @return Data frame with chunk metadata (chunk_id, chunk_path, start_line,
#'   end_line).
#'
#' @keywords internal
.gen_cntx_write_transcript_chunks <- function(
  transcript_text,
  chunks_dir,
  target_tokens = 5000,
  overwrite = FALSE
) {
  fs::dir_create(chunks_dir, recurse = TRUE)
  index_path <- file.path(chunks_dir, "chunks_index.csv")

  if (!isTRUE(overwrite) && fs::file_exists(index_path)) {
    return(
      readr::read_csv(
        index_path,
        show_col_types = FALSE,
        progress = FALSE
      )
    )
  }

  if (!rlang::is_string(transcript_text) || !nzchar(transcript_text)) {
    empty_index <- data.frame(
      chunk_id = character(0),
      chunk_path = character(0),
      start_line = integer(0),
      end_line = integer(0),
      stringsAsFactors = FALSE
    )
    readr::write_csv(empty_index, index_path)
    return(empty_index)
  }

  lines <- strsplit(transcript_text, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) {
    empty_index <- data.frame(
      chunk_id = character(0),
      chunk_path = character(0),
      start_line = integer(0),
      end_line = integer(0),
      stringsAsFactors = FALSE
    )
    readr::write_csv(empty_index, index_path)
    return(empty_index)
  }

  estimate_tokens <- function(text) {
    # Crude approximation: ~4 characters per token in English-like text.
    ceiling(nchar(text, type = "chars") / 4)
  }

  chunk_rows <- list()
  current <- character(0)
  current_tokens <- 0L
  start_line <- 1L
  chunk_no <- 1L

  flush_chunk <- function(end_line) {
    if (length(current) == 0) {
      return(invisible(NULL))
    }
    chunk_id <- sprintf("chunk_%04d", chunk_no)
    chunk_path <- file.path(chunks_dir, paste0(chunk_id, ".txt"))
    readr::write_lines(current, chunk_path)
    chunk_rows[[length(chunk_rows) + 1]] <<- data.frame(
      chunk_id = chunk_id,
      chunk_path = chunk_path,
      start_line = start_line,
      end_line = end_line,
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }

  for (idx in seq_along(lines)) {
    line <- lines[[idx]]
    current <- c(current, line)
    current_tokens <- current_tokens + estimate_tokens(line)

    if (current_tokens >= target_tokens) {
      flush_chunk(end_line = idx)
      chunk_no <- chunk_no + 1L
      current <- character(0)
      current_tokens <- 0L
      start_line <- idx + 1L
    }
  }

  if (length(current) > 0) {
    flush_chunk(end_line = length(lines))
  }

  chunk_index <- if (length(chunk_rows) == 0) {
    data.frame(
      chunk_id = character(0),
      chunk_path = character(0),
      start_line = integer(0),
      end_line = integer(0),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, chunk_rows)
  }

  readr::write_csv(chunk_index, index_path)
  chunk_index
}
