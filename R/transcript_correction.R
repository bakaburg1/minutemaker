#' Corrects transcription errors in JSON files using a Large Language Model.
#'
#' This function processes a single transcript JSON file or all JSON files in a
#' specified directory. For each transcript, it uses an LLM to identify and
#' correct errors based on provided terms/names and general context. The
#' corrected transcript is then written back to the original file path. The LLM
#' model used for correction is determined by the R option
#' \code{getOption("minutemaker_correction_llm_model")}.
#'
#' @param input_path Path to a single transcript JSON file or a directory
#'   containing transcript JSON files.
#' @param terms A character vector of important terms, names, or acronyms that
#'   the LLM should pay special attention to. Can be NULL.
#' @param include_reasoning Logical, if \code{TRUE}, the LLM will be asked to
#'   provide its reasoning before the JSON output. This approach improves
#'   quality with non-reasoning models. This reasoning will be messaged to the
#'   console. Defaults to \code{getOption("minutemaker_include_llm_reasoning",
#'   TRUE)}.
#' @param overwrite Logical, if \code{TRUE}, existing files will be overwritten
#'   with the corrected transcript. Defaults to \code{FALSE}.
#' @param max_retries Integer, maximum number of times to retry the LLM call
#'   per file if it fails (e.g., due to temporary API issues or parsing
#'   errors). Defaults to 2.
#' @param ... Additional arguments to be passed as parameters to the LLM call
#'   via \code{llmR::prompt_llm(params = list(...))}, e.g., \code{temperature},
#'   \code{max_tokens}. Note: \code{temperature = 0} is set by default if not
#'   overridden here.
#'
#' @return Invisibly returns a list of paths to the processed (and potentially
#'   modified) files. Errors during processing of individual files are reported
#'   as warnings.
#'
#' @export
apply_llm_correction <- function(
  input_path,
  terms = NULL,
  include_reasoning = getOption("minutemaker_include_llm_reasoning", TRUE),
  overwrite = FALSE,
  max_retries = 2,
  ...
) {
  # Check if the input path is valid.
  if (!file.exists(input_path)) {
    cli::cli_abort(
      c(
        "The provided {.arg input_path} is not a valid file or folder:
          {.path {input_path}}",
        "x" = "Path does not exist."
      )
    )
  }

  # Collect additional LLM parameters
  llm_extra_params <- list(...)

  # === ADAPTIVE BACKOFF CORRECTION SYSTEM ===
  #
  # When LLM calls fail on large transcript chunks, we implement an adaptive
  # backoff strategy that recursively splits the transcript into smaller segments
  # until corrections succeed. This prevents complete failure when large chunks
  # exceed model context limits or cause parsing issues.
  #
  # Key benefits:
  # - Graceful degradation: Large failures become smaller successes
  # - Preserves data integrity: All segments get processed individually
  # - Reduces retry consumption: Backoff attempts don't count as full retries
  #
  # Helper function to apply corrections to text (handles both strings and lists)
  # This function safely applies a corrections map to text, handling different
  # input types (single strings, lists of strings) that may result from
  # segment-based processing.
  apply_corrections_to_text <- function(text, corrections_map) {
    # Early return if no corrections to apply
    if (rlang::is_empty(corrections_map)) {
      return(text)
    }
    # Handle list inputs (e.g., multiple text segments) by applying corrections
    # to each element recursively
    if (is.list(text)) {
      return(purrr::map(
        text,
        ~ apply_single_correction_set(.x, corrections_map)
      ))
    }
    # Apply corrections to single text input
    apply_single_correction_set(text, corrections_map)
  }

  # Core adaptive backoff function for segment-based correction
  #
  # This function implements a recursive binary splitting strategy:
  # 1. Try to correct all segments at once
  # 2. If it fails and we have enough segments (> min_segments), split in half
  # 3. Recursively correct each half independently
  # 4. Merge successful results from both halves
  #
  # The min_segments parameter prevents infinite splitting - if a chunk is too
  # small to split further, we accept the failure rather than continue.
  #
  # Returns a standardized result structure with status and corrections_map
  correct_with_segment_backoff <- function(
    segments,
    min_segments = 5
  ) {
    if (rlang::is_empty(segments)) {
      return(list(status = "no_text_to_correct", corrections_map = NULL))
    }

    # Extract text content from segments, handling missing text fields
    # gracefully
    segment_texts <- purrr::map_chr(segments, "text", .default = "")
    segment_texts <- tidyr::replace_na(segment_texts, "")

    # Recursive function that attempts correction on a subset of segments
    # Uses binary splitting when corrections fail on large chunks
    split_and_correct <- function(indices) {
      result <- correct_transcription_errors(
        text_to_correct = segment_texts[indices],
        terms = terms,
        include_reasoning = include_reasoning,
        llm_extra_params = llm_extra_params
      )

      # === BACKOFF LOGIC ===
      # If correction failed and we have enough segments to split further,
      # recursively process each half of the segment range
      if (
        result$status %in%
          c("llm_call_failed", "parsing_failed") &&
          length(indices) > min_segments
      ) {
        # Calculate midpoint and split segment indices into two halves
        # This ensures balanced processing and prevents one half from being much
        # larger
        mid <- floor(length(indices) / 2)
        left <- split_and_correct(indices[seq_len(mid)])
        right <- split_and_correct(indices[(mid + 1):length(indices)])

        # If either half still failed after backoff, propagate the original failure
        # This prevents partial success from masking underlying issues
        if (
          left$status %in%
            c("llm_call_failed", "parsing_failed") ||
            right$status %in% c("llm_call_failed", "parsing_failed")
        ) {
          return(list(status = result$status, corrections_map = NULL))
        }

        # Successfully processed both halves - merge their correction maps
        # Use c() to combine named lists; duplicate keys will be handled by
        # later processing
        combined_map <- c(left$corrections_map, right$corrections_map)
        combined_status <- if (rlang::is_empty(combined_map)) {
          "no_changes_signal" # Both halves indicated no changes needed
        } else {
          "corrections_applied" # At least one correction was found across halves
        }

        return(list(status = combined_status, corrections_map = combined_map))
      }

      list(status = result$status, corrections_map = result$corrections_map)
    }

    # Seed the recursion with all segment indices.
    split_and_correct(seq_along(segment_texts))
  }

  # Determine which files to process based on input_path
  files_to_process <- character()
  if (dir.exists(input_path)) {
    files_to_process <- list.files(
      input_path,
      pattern = "\\.json$",
      full.names = TRUE
    )
    if (length(files_to_process) == 0) {
      cli::cli_inform("No JSON files found in directory: {.path {input_path}}")
      return(invisible(character())) # Return empty if no files to process.
    }
  } else {
    # input_path is known to be a file due to the initial file.exists check.
    if (tolower(tools::file_ext(input_path)) == "json") {
      files_to_process <- input_path
    } else {
      cli::cli_abort("Input file is not a JSON file: {.path {input_path}}")
    }
  }

  # Validate the terms parameter once before processing files.
  if (!is.null(terms) && !is.character(terms)) {
    cli::cli_abort(
      "Parameter {.arg terms} must be a character vector or NULL."
    )
  }

  # Iterate over files, attempting correction for each.
  # NA_character_ is used to mark failures for later filtering.
  processed_files <- purrr::map_chr(files_to_process, \(file_path) {
    cli::cli_alert_info(
      "Processing transcript file for correction: {.file {basename(file_path)}}"
    )

    transcript_data <- tryCatch(
      {
        jsonlite::read_json(file_path)
      },
      error = function(e) {
        cli::cli_warn(
          c(
            "!" = "Error reading JSON file {.file {basename(file_path)}}:
              {e$message}.",
            "i" = "Skipping this file."
          )
        )
        return(NULL) # Signal failure to read.
      }
    )

    if (is.null(transcript_data)) {
      return(NA_character_)
    }

    # Check if the transcript has already been corrected and if overwrite is
    # FALSE
    if (isTRUE(transcript_data$corrected) && !overwrite) {
      cli::cli_alert_warning(
        "Skipping file, already marked as corrected and overwrite is FALSE:
          {.file {basename(file_path)}}",
        wrap = TRUE
      )
      return(file_path) # Return the path as it was processed (skipped)
    }

    # Start Correction Attempt with Retry Logic ----
    # Initialize variables for the retry loop
    correction_result <- NULL
    retry_count <- 0
    success <- FALSE
    original_transcript_data <- transcript_data # Keep original

    # === MAIN CORRECTION LOOP WITH ADAPTIVE BACKOFF ===
    # Enhanced retry logic that first attempts segment-based backoff before
    # consuming a full retry attempt. This preserves retry budget for genuine
    # failures rather than context-limit issues.
    while (retry_count <= max_retries && !success) {
      # Track whether segment backoff was used in this iteration
      # This prevents backoff success from being treated as a retry failure
      used_segment_backoff <- FALSE
      # Attempt the core correction logic.
      correction_result <- tryCatch(
        {
          # Call the LLM correction function, passing only the text
          correct_transcription_errors(
            text_to_correct = transcript_data$text,
            terms = terms,
            include_reasoning = include_reasoning,
            llm_extra_params = llm_extra_params
          )
        },
        # Catch errors *within* correct_transcription_errors itself (e.g., arg
        # validation)
        error = \(e) {
          cli::cli_warn(
            "Internal error during correction attempt for
                {.file {basename(file_path)}}: {e$message}"
          )
          # Simulate a failure status to handle uniformly below
          # Return structure matching successful call, but with error status
          list(
            corrected_text = transcript_data$text,
            corrections_map = NULL,
            status = "internal_error_pre_llm",
            made_changes = FALSE
          )
        }
      )

      # Check the status returned by correct_transcription_errors
      status <- correction_result$status

      # === SEGMENT BACKOFF INTEGRATION ===
      # Before consuming a retry attempt, try segment-based correction
      # This gives us a chance to salvage the correction using smaller chunks
      if (status %in% c("llm_call_failed", "parsing_failed")) {
        # Only attempt backoff if transcript has segments to work with
        if (!rlang::is_empty(transcript_data$segments)) {
          # Attempt correction using recursive segment splitting
          # This is our "last chance" before consuming a retry
          backoff_result <- correct_with_segment_backoff(
            transcript_data$segments
          )

          # If backoff succeeded, apply those corrections to the main text
          if (
            !backoff_result$status %in% c("llm_call_failed", "parsing_failed")
          ) {
            # Apply corrections from successful segment backoff to main transcript text
            corrected_main_text <- apply_corrections_to_text(
              original_transcript_data$text, # Use original to avoid compounding changes
              backoff_result$corrections_map
            )
            # Check if backoff corrections actually changed the text
            made_changes <- !identical(
              original_transcript_data$text,
              corrected_main_text
            )

            # Update correction result with backoff outcomes
            correction_result <- list(
              corrected_text = corrected_main_text,
              corrections_map = backoff_result$corrections_map,
              status = backoff_result$status,
              made_changes = made_changes
            )
            status <- correction_result$status
            used_segment_backoff <- TRUE
          }
        }

        # If segment backoff succeeded, treat as overall success
        # Skip retry logic and proceed to write results
        if (used_segment_backoff) {
          success <- TRUE
          next
        }

        retry_count <- retry_count + 1
        if (retry_count <= max_retries) {
          cli::cli_alert_warning(
            "Correction attempt {retry_count}/{max_retries} failed
            (status: {status}). Retrying...",
            wrap = TRUE
          )
          # Use the *original* transcript_data for retry, not potentially
          # modified
          # This is important if the function modified the input on failure
          # (though it shouldn't)
          transcript_data <- original_transcript_data
        } else {
          cli::cli_warn(
            "Correction failed after {max_retries} retries
            (final status: {status}) for {.file {basename(file_path)}}.
            Skipping write."
          )
          # Loop will terminate, success remains FALSE
        }
      } else {
        # Any other status means success (corrections applied, none needed,
        # etc.) or non-retryable error.
        success <- TRUE
      }
    }

    # Check final status after loop
    if (!success) {
      # Failed all retries
      return(NA_character_) # Signal failure for this file in map_chr
    }

    # If successful, unpack results from correction_result
    corrected_main_text <- correction_result$corrected_text
    corrections_map <- correction_result$corrections_map
    made_changes <- correction_result$made_changes

    # Status from the last successful attempt
    final_status <- correction_result$status

    # Start building the final data structure from the original comparison point
    final_transcript_data <- original_transcript_data

    # Get the original text for checks
    original_text <- original_transcript_data$text

    # Update the main text field with the corrected vector
    if (!made_changes && is.null(original_text)) {
      # If no changes were made and original was NULL, keep it NULL
      # Explicitly assign a pure NULL, stripping any potential attributes from
      # original_text
      final_transcript_data$text <- NULL
    } else if (is.list(original_text) && length(corrected_main_text) <= 1) {
      # Preserve list structure if original was list and result is single/empty
      final_transcript_data$text <- as.list(corrected_main_text)
    } else {
      # Otherwise, assign the character vector (handles non-list original,
      # multi-element results)
      final_transcript_data$text <- corrected_main_text
    }

    # Apply the same corrections to segments, if a map exists and segments exist
    if (
      !rlang::is_empty(corrections_map) &&
        !rlang::is_empty(final_transcript_data$segments)
    ) {
      cli::cli_alert_info("Applying corrections to transcript segments...")
      final_transcript_data$segments <- purrr::map(
        final_transcript_data$segments,
        \(seg) {
          if (
            !is.null(seg$text) && is.character(seg$text) && nzchar(seg$text)
          ) {
            # Use the same helper function and map
            seg$text <- apply_single_correction_set(seg$text, corrections_map)
          }
          return(seg)
        }
      )
    }

    # If status indicates no changes were needed, ensure made_changes is FALSE,
    # even if some minor non-textual element changed (e.g. attributes)
    if (final_status %in% c("no_changes_signal", "no_text_to_correct")) {
      made_changes <- FALSE
    }

    # If changes were made, store the corrections map in the output JSON.
    # Otherwise, ensure this field is NULL to remove it or prevent its creation
    # if no actual changes were applied (e.g. if overwrite = TRUE but no text
    # changes).
    if (
      made_changes &&
        !is.null(corrections_map) &&
        !rlang::is_empty(corrections_map)
    ) {
      final_transcript_data$corrections_applied <- corrections_map
    } else {
      # Explicitly set to NULL so jsonlite::write_json omits it or clears it.
      final_transcript_data$corrections_applied <- NULL
    }

    # Mark transcript as 'corrected' if `overwrite` is TRUE or if actual changes
    # were made (status = "corrections_applied" and made_changes=TRUE)
    should_write <- FALSE
    write_message_type <- NULL

    # Silence the linter for variables used in string templates only
    write_message_type

    if (overwrite) {
      final_transcript_data$corrected <- TRUE

      # Write even if no text changes occurred but `overwrite` is TRUE
      should_write <- TRUE
      write_message_type <- ifelse(
        made_changes,
        "Applied corrections and saved",
        "Saved (overwrite=TRUE)"
      )
    } else if (made_changes) {
      # Only mark as corrected and write if changes were actually made when
      # `overwrite` is FALSE
      final_transcript_data$corrected <- TRUE
      should_write <- TRUE
      write_message_type <- "Applied corrections and saved"
    }

    if (should_write) {
      tryCatch(
        {
          jsonlite::write_json(
            final_transcript_data, # Use the data from the correction result
            file_path,
            auto_unbox = TRUE,
            pretty = TRUE
          )
          cli::cli_alert_success(
            "{write_message_type}: {.file {file_path}}"
          )
          return(file_path) # Successful processing and write.
        },
        error = \(e) {
          # Use abort here as writing failure is critical
          cli::cli_abort("Error writing {.file {file_path}}: {e$message}")
        }
      )
    } else {
      # No changes were made AND overwrite is FALSE, or status indicated no need
      # for changes.
      cli::cli_alert_info(
        "No effective changes made or required for:
          {.file {basename(file_path)}}. No file written.",
        wrap = TRUE
      )
      return(file_path) # Processed, but no changes to write.
    }
  })

  # Filter out NA values which represent processing failures
  successful_files <- processed_files[!is.na(processed_files)]

  # Return only paths of files that were successfully processed (written or
  # skipped appropriately).
  return(invisible(successful_files))
}


#' Corrects transcription errors using an LLM (Internal Helper).
#'
#' Takes a character vector, sends it collapsed to an LLM for corrections, and
#' applies the corrections element-wise to the original vector.
#'
#' @param text_to_correct Character vector of text to correct.
#' @param terms Character vector of important terms/names. Can be NULL.
#' @param include_reasoning Logical, if TRUE, requests and messages LLM
#'   reasoning.
#' @param llm_extra_params A list of additional parameters for the LLM call,
#'   merged with defaults (e.g., temperature = 0 for standard models, or
#'   reasoning_effort for reasoning models).
#'
#' @return A list containing:
#' - `corrected_text`: Character vector, the input text with corrections
#' applied.
#' - `corrections_map`: Named list/vector of corrections (original ->
#' corrected) or NULL if none/error.
#' - `status`: String indicating outcome (e.g., "corrections_applied",
#' "no_changes_signal", "llm_call_failed", "parsing_failed",
#'  "no_text_to_correct").
#' - `made_changes`: Logical indicating if `corrected_text` differs from
#' `text_to_correct`.
#'
#' @noRd
correct_transcription_errors <- function(
  text_to_correct,
  terms = NULL,
  include_reasoning = TRUE,
  llm_extra_params = list()
) {
  # Ensure input is character
  converted_text <- as.character(unlist(text_to_correct))

  if (!rlang::is_character(converted_text)) {
    utils::str(text_to_correct)
    cli::cli_abort(c(
      "x" = "Input {.arg text_to_correct} must be a character vector or
        list of character vectors.",
      "i" = "Got: {.cls {class(text_to_correct)}}"
    ))
  }

  # Collapse the input vector for the LLM prompt
  full_text_for_llm <- converted_text |>
    paste(collapse = "\n")

  # Validate include_reasoning
  if (!rlang::is_scalar_logical(include_reasoning)) {
    cli::cli_abort(
      "Parameter {.arg include_reasoning} must be a single logical value (TRUE
      or FALSE)."
    )
  }

  # Get the correction model from options from
  # "minutemaker_correction_llm_model", defaulting to "llmr_current_model" if
  # not set.
  correction_model_label <- getOption("minutemaker_correction_llm_model")

  if (is.null(correction_model_label)) {
    cli::cli_inform(
      c(
        "!" = "Model for correction not specified, reverting to general model:
          {.val {getOption('llmr_current_model')}}",
        "i" = "Consider setting {.val minutemaker_correction_llm_model}
          for more consistent results."
      )
    )
    correction_model_label <- getOption("llmr_current_model")
  }

  # Default return structure
  default_return_struct <- list(
    corrected_text = text_to_correct,
    corrections_map = NULL,
    status = NA_character_,
    made_changes = FALSE
  )

  # If no model is set, stop with an error.
  if (is.null(correction_model_label)) {
    cli::cli_abort(
      c(
        "LLM model for correction not set.",
        "x" = "Neither {.val minutemaker_correction_llm_model} nor
          {.val llmr_current_model} are set."
      )
    )
  }

  # If there is no text to correct after collapsing, return original.
  if (!nzchar(stringr::str_trim(full_text_for_llm))) {
    # Return original data with a specific status.
    return_struct <- default_return_struct
    return_struct$status <- "no_text_to_correct"

    return(return_struct)
  }

  # Safely get current llmR model.
  original_active_model <- getOption("llmr_current_model")

  # Restore originally active model on function exit.
  on.exit(
    {
      llmR::set_llmr_model(original_active_model)
    },
    add = TRUE
  )

  # Switch to the correction model.
  llmR::set_llmr_model(correction_model_label)

  # Initialize llm_call_failed flag for this function call
  llm_call_failed <- FALSE
  # Capture this function environment so we can set the flag explicitly without
  # climbing further up the parent chain.
  flag_env <- environment()

  # -- Prepare the LLM prompts --

  terms_instructions <- "Understand the transcript content and context and use
  your domain knowledge to identify the corrections that would make sense to
  propose."

  # Silence the linter for variables used in string templates only
  terms_instructions

  if (!rlang::is_empty(terms)) {
    # shQuote ensures terms with spaces are treated as single entities in the
    # prompt.
    term_list_str <- paste(shQuote(terms, type = "cmd"), collapse = ", ")

    # Silence the linter for variables used in string templates only
    term_list_str

    terms_instructions <- stringr::str_glue(
      "**TERMS TO KEEP IN MIND:**
Here is a list of important general terms, names, and their correct forms to
keep in mind:
{term_list_str}.
- Use this list to guide your corrections.
- IMPORTANT: Some items in this list may contain a colon followed by an
  explanation (e.g., \"TERM: Explanation\"). The part after the colon is for
  your contextual understanding only. If you use such an item for a
  correction, the corrected form in your output should ONLY be the term
  part (e.g., \"TERM\"), not \"TERM: Explanation\".
- Mind that there could be other terms not listed here which you need to correct
  based on your knowledge and understanding of the context. Be suspicious of
  any acronyms, abbreviations, or names even if not in the list.
  "
    )
  }

  reasoning_instructions <- ""

  # XML tags for robust parsing.
  json_wrapper_start <- "<json_corrections_output>"
  json_wrapper_end <- "</json_corrections_output>"

  # Silence the linter for variables used in string templates only
  reasoning_instructions
  json_wrapper_start
  json_wrapper_end

  if (include_reasoning) {
    # Detailed instructions for the LLM if reasoning is requested.
    reasoning_instructions <- stringr::str_glue(
      "**REASON STEP BY STEP:**
before proposing any correction, reason carefully step by step about the
transcript content and the corrections that would make sense to propose. Think
through each potential correction, also considering your general knowledge of
the topics. Identify patterns, analyze ambiguous cases, determine which terms
need correction. This reasoning should be free-form and exploratory, helping
you arrive at the most accurate corrections.
"
    )
  }

  # System prompt constructed using insights from experimental script.
  system_prompt <- stringr::str_glue(
    "You will be passed a segment of a transcribed text from a meeting or
conference. Your task is to correct misspelled terms, acronyms and names
introduced during the transcription.

{terms_instructions}

{reasoning_instructions}

**YOUR TASK:**
Output a JSON structure mapping the *incorrect term found in the input
text* to its *correct form*. Your JSON output should ONLY contain corrections
identified and MUST be enclosed in the following XML tags:
{json_wrapper_start}YOUR_JSON_HERE{json_wrapper_end}

**IMPORTANT SIGNAL FOR NO CORRECTIONS:**
If you determine that NO corrections are needed based on the rules and your
analysis, output this exact string:
{json_wrapper_start}NO_CHANGES_NEEDED{json_wrapper_end}

**OUTPUT JSON STRUCTURE (only if corrections ARE needed):**
If the input text contains errors, list the correction in ad-hoc JSON structure.
For example, if the input text contains 'John Smth spoke about H.A.I. data from
tesy and an arai report mentioned in Uro Surveillance. Piters and John
agreed.', and you determine that the correct forms are 'John Smith', 'Peter S.',
'TESSy', 'ARHAI', 'HAI', and 'Eurosurveillance' (either thanks to a list of
important terms you were provided or based on your general knowledge), your
JSON output should be:
{{
  \"John Smth\": \"John Smith\",
  \"Piters\": \"Peter S.\",
  \"tesy\": \"TESSy\",
  \"arai\": \"ARHAI\",
  \"H.A.I.\": \"HAI\",
  \"Uro Surveillance\": \"Eurosurveillance\"
}}

**CRITICAL RULES:**
Apply these rules to your JSON output if corrections are made:
- **Handle common words with care:** If a term to be corrected is a common
  word (e.g., 'and', 'is', 'it'), do not create a direct mapping (e.g.,
  '{{\"and\": \"AMD\"}}'). This is too aggressive and will lead to incorrect
  replacements. Instead, include surrounding context in the key to make the
  match specific. For example, instead of '{{\"and\": \"AMD\"}}', a better
  mapping would be '{{\"and project\": \"AMD project\"}}' if the context
  was \"and project\". This ensures that only the specific instance
  is corrected.
- **Exact-match only:** Every JSON key must be a verbatim substring of the input
  text. Do not paraphrase, expand, summarize, or add missing words. If you
  cannot point to the exact substring in the input, do not propose that
  correction. This prevents hallucinated corrections that don't match the
  actual transcript content.
- If a first name (e.g., 'John', 'Anna') appears in the input text and is
  ALREADY SPELLED CORRECTLY according to the provided list or general knowledge,
  DO NOT expand it to a full name (e.g., 'Anna' to 'Anna Smith'), even if 'Anna
  Smith' is in the important terms list. For instance, if 'Peter' is in the text
  and 'Peter Novak' is in the terms list, and 'Peter' is correctly spelled, DO
  NOT map 'Peter' to 'Peter Novak'.
- ONLY create a mapping if a name in the input text was genuinely misspelled
  (e.g., 'Jon' -> 'John', 'Anan' -> 'Anna') or an incorrectly transcribed
  initial/abbreviation that needs a full name from the list for clarity and
  cannot be understood otherwise (e.g., 'P. Novak' -> 'Peter Novak' if 'P.
  Novak' is ambiguous, but not if 'Peter' alone is used and correct, or 'J.
  Smith' -> 'John Smith').
- The goal is automated search and replacement of *errors* or *genuinely
  ambiguous abbreviations* in the original transcription. Do not map correctly
  spelled standalone first names to longer forms if the first name alone is
  clear and correct."
  )

  # -- Perform LLM call --

  messages_payload <- c(system = system_prompt, user = full_text_for_llm)
  # Default LLM parameters. Reasoning models do not accept temperature, so use
  # reasoning_effort there and keep temperature only for non-reasoning calls.

  base_llm_params <- if (include_reasoning) {
    list(temperature = 0)
  } else {
    list(reasoning_effort = "medium")
  }

  # Allow user-provided parameters to override defaults
  final_llm_params <- purrr::list_modify(base_llm_params, !!!llm_extra_params)

  # Guard against passing temperature alongside `reasoning_effort` (many
  # reasoning endpoints reject temperature or only accept a constrained value).
  if ("reasoning_effort" %in% names(final_llm_params)) {
    final_llm_params$temperature <- NULL
  }

  llm_response_str <- NULL

  # Force JSON is FALSE since we request an xml tag
  force_json_value <- FALSE

  tryCatch(
    {
      # Run the LLM call.
      llm_response_str <- llmR::prompt_llm(
        messages = messages_payload,
        params = final_llm_params,
        force_json = force_json_value
      )
    },
    error = \(e) {
      cli::cli_warn("LLM call failed: {e$message}")

      # Flip the flag in the current function environment so the caller sees it.
      assign("llm_call_failed", TRUE, envir = flag_env)
    }
  )

  if (
    llm_call_failed ||
      is.null(llm_response_str) ||
      !nzchar(stringr::str_trim(llm_response_str))
  ) {
    # Return original if LLM call issues or empty response.
    # Indicate LLM call failure specifically.
    return_struct <- default_return_struct
    return_struct$status <- "llm_call_failed"

    return(return_struct)
  }

  # Print the raw LLM response for debugging
  cli::cli_h2("LLM correction response")
  cli::cli_verbatim(llm_response_str) # Use verbatim for raw output
  cli::cli_h2("End of LLM correction response")

  # -- Parse and validate the LLM response --

  # Always expect content within XML tags.
  xml_pattern <- stringr::str_glue(
    "{json_wrapper_start}((?:.|\n)*?){json_wrapper_end}"
  )
  match_result <- stringr::str_match(llm_response_str, xml_pattern)
  extracted_content <- match_result[, 2] |>
    stringr::str_trim()

  # Prepare default return structures for different outcomes
  failed_parsing_return_struct <- default_return_struct
  failed_parsing_return_struct$status <- "parsing_failed"

  # Reuse the default structure for no changes signal
  no_changes_return_struct <- default_return_struct
  no_changes_return_struct$status <- "no_changes_signal"

  # Check if the extracted content is empty.
  if (is.na(extracted_content)) {
    cli::cli_warn(
      c(
        "!" = "LLM response did not contain the expected XML wrapper tags.",
        "i" = "Response (raw): {.val {substr(llm_response_str, 1, 200)}}..."
      )
    )
    failed_parsing_return_struct$error_message <-
      "XML wrapper tags not found in LLM response."

    return(failed_parsing_return_struct)
  }

  # Detect NO_CHANGES_NEEDED signal in the trimmed XML content.
  if (stringr::str_detect(extracted_content, "NO_CHANGES_NEEDED")) {
    cli::cli_alert_info(
      "LLM indicated no corrections needed"
    )
    return(no_changes_return_struct)
  }

  # Extract JSON object from the XML content.
  json_candidate_string <- stringr::str_extract(
    extracted_content,
    "(?s)\\{.*\\}"
  )

  # Check if a JSON object was found.
  if (is.na(json_candidate_string)) {
    cli::cli_warn(
      c(
        "!" = "Content within XML tags was not 'NO_CHANGES_NEEDED' signal and
          no JSON object substring was found.",
        "i" = "Content from XML (trimmed):
          {.val {substr(extracted_content, 1, 200)}}"
      )
    )
    failed_parsing_return_struct$error_message <-
      "Content neither 'NO_CHANGES_NEEDED' signal nor a JSON object"

    return(failed_parsing_return_struct)
  }

  # Try to parse the JSON candidate string.
  corrections_map <- try(
    jsonlite::parse_json(json_candidate_string),
    silent = TRUE
  )

  if (inherits(corrections_map, "try-error")) {
    corrections_map <- as.character(corrections_map)
    cli::cli_warn(
      c(
        "!" = "JSON parsing failed: {.str {corrections_map}}",
        "i" = "Candidate string:
          {.val {substr(json_candidate_string, 1, 50)}}..."
      )
    )

    # Return original if LLM call issues or empty response.
    failed_parsing_return_struct$error_message <- corrections_map

    return(failed_parsing_return_struct)
  }

  # Check if the parsed JSON is empty.
  if (rlang::is_empty(corrections_map)) {
    # This should not happen, but we check for it anyway.
    cli::cli_warn(
      c(
        "!" = "LLM returned a JSON object that parsed to an empty R object.
          This will be treated as no changes needed.",
        "i" = "Candidate string:
          {.val {substr(json_candidate_string, 1, 50)}}..."
      )
    )

    return(no_changes_return_struct)
  }

  # Check if the parsed JSON is valid (i.e., named list).
  if (!rlang::is_named(corrections_map) || !rlang::is_list(corrections_map)) {
    cli::cli_warn(
      c(
        "!" = "Parsed JSON (extracted) is not a named list as expected.",
        "i" = "Candidate string:
          {.val {substr(json_candidate_string, 1, 50)}}..."
      )
    )

    failed_parsing_return_struct$error_message <-
      "Parsed JSON (extracted) was not a named list/object."

    return(failed_parsing_return_struct)
  }

  # === CORRECTION VALIDATION ===
  # Validate that all correction keys actually appear in the input text
  # This prevents application of corrections that don't match the source
  # material.
  #
  # LLM models can sometimes hallucinate or misremember text, leading to
  # corrections that reference substrings not present in the input. We warn
  # about these but keep them for transparency (they won't be applied).
  correction_keys <- names(corrections_map)
  correction_keys <- correction_keys[!is.na(correction_keys)]
  if (length(correction_keys) > 0) {
    # Find keys that don't appear as exact substrings in the input text
    unmatched_keys <- correction_keys[
      !stringr::str_detect(
        full_text_for_llm,
        # Exact matching, no regex interpretation
        stringr::fixed(correction_keys)
      )
    ]
    if (length(unmatched_keys) > 0) {
      # Show examples of unmatched keys for debugging
      example_keys <- utils::head(unmatched_keys, 3)
      cli::cli_warn(
        c(
          "!" = paste0(
            length(unmatched_keys),
            " correction key(s) were not found in the input text and will not be applied."
          ),
          "i" = paste0("Examples: ", paste(example_keys, collapse = "; "))
        )
      )
      # TODO: If this issue persists, consider implementing
      # a follow-up LLM call to reconcile mismatched keys with the original text
      # This could help identify typos in the LLM's key proposals.
    }
  }

  # -- Apply corrections --
  corrected_text_vector <- apply_single_correction_set(
    text_to_correct,
    corrections_map
  )
  made_changes <- !identical(text_to_correct, corrected_text_vector)

  if (!made_changes) {
    cli::cli_inform(
      c(
        "!" = "Corrections parsed, but no changes made to main transcript text.
        Treating as no changes."
      )
    )

    return(no_changes_return_struct)
  }

  cli::cli_alert_success("Transcription correction applied.")
  return_struct <- list(
    corrected_text = corrected_text_vector,
    corrections_map = corrections_map,
    status = "corrections_applied",
    made_changes = TRUE
  )
  return(return_struct)
}


#' Apply a set of corrections to a text string
#'
#' This helper function applies a map of corrections to a text string. It sorts
#' the correction keys by length in descending order to prevent shorter
#' incorrect terms from disrupting longer ones during replacement.
#'
#' @param text A character string to which corrections will be applied.
#' @param corr_map A named list where names are incorrect terms and values are
#'   their corrections.
#'
#' @return The corrected text string with all replacements applied.
#'
apply_single_correction_set <- function(text, corr_map) {
  # Return early if nothing to do
  if (
    rlang::is_empty(text) || !any(nzchar(text)) || rlang::is_empty(corr_map)
  ) {
    return(text)
  }

  # Convert list to named character vector for replacement
  corrections <- unlist(corr_map)

  # Order the correction keys by length in descending order so that longer keys
  # are replaced first (avoids partial overlaps)
  correction_order <- order(nchar(names(corrections)), decreasing = TRUE)
  corrections <- corrections[correction_order]

  # Build regex patterns (names) and safely escaped replacement strings (values)
  build_pattern <- function(key) {
    # NOTE on the heavy escaping below
    # * meta_chars is a *character class* listing all the regex metacharacters
    #   we want to escape. In the final regular expression it should read:
    #     [. \ ^ $ | ( ) [ ] { } * + ? \\ ]
    #   (spaces added here only for clarity).  Inside an R string every
    #   backslash itself must be escaped, hence the double `\\` sequences.
    # * Replacement "\\\\\\1" looks crazy, but it boils down to two
    #   chars sent to PCRE: a single backslash followed by the first capture
    #   group (i.e. the matched metacharacter).  It therefore turns e.g. `.`
    #   into `\.` and `*` into `\*`, which is exactly what we need for
    #   literal matching.
    meta_chars <- "([.|()\\^{}+*$?\\[\\]\\\\])"

    escaped_key <- gsub(
      meta_chars, # pattern: any metacharacter
      "\\\\\\1", # replacement: backslash + the char itself
      key,
      perl = TRUE
    )

    # If the key is composed solely of word characters we can safely add word
    # boundaries so we do not replace inside other words (e.g. 'hi' in
    # 'think'). For keys that already contain non-word characters (punctuation,
    # spaces, etc.) word boundaries may prevent a match, so we skip them.
    if (grepl("^[A-Za-z0-9_]+$", key)) {
      escaped_key <- paste0("\\b", escaped_key, "\\b")
    }

    escaped_key
  }

  # Generate the named vector expected by str_replace_all()
  pattern_vec <- vapply(names(corrections), build_pattern, character(1))

  # Escape backslashes in the replacement strings so they are kept literally in
  # the output (otherwise sequences like "\\w" would be interpreted)
  replacement_vec <- stringr::str_replace_all(corrections, "\\\\", "\\\\\\\\")

  names(replacement_vec) <- pattern_vec

  # Perform the replacements in one pass
  stringr::str_replace_all(text, replacement_vec)
}
