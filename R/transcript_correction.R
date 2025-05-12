#' transcription_correction.R

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
  ...
) {
  # Check if the input path is valid.
  if (!file.exists(input_path)) {
    cli::cli_abort(
      c(
        "The provided {.arg input_path} is not a valid file or folder: {.path {input_path}}",
        "x" = "Path does not exist."
      )
    )
  }

  # Collect additional LLM parameters
  llm_extra_params <- list(...)

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
            "!" = "Error reading JSON file {.file {basename(file_path)}}: {e$message}.",
            "i" = "Skipping this file."
          )
        )
        return(NULL) # Signal failure to read.
      }
    )

    if (is.null(transcript_data)) return(NA_character_)

    # Check if the transcript has already been corrected and if overwrite is
    # FALSE
    if (isTRUE(transcript_data$corrected) && !overwrite) {
      cli::cli_alert_warning(
        "Skipping file, already marked as corrected and overwrite is FALSE: {.file {basename(file_path)}}"
      )
      return(file_path) # Return the path as it was processed (skipped)
    }

    # If terms is not a character vector or NULL, ignore it.
    if (!is.null(terms) && !is.character(terms)) {
      cli::cli_abort(
        "Parameter {.arg terms} must be a character vector or NULL."
      )
    }

    # Attempt the core correction logic. If this block errors, original
    # transcript_data is returned to avoid data loss.
    corrected_transcript_data <- tryCatch(
      {
        correct_transcription_errors(
          transcript_data = transcript_data,
          terms = terms,
          include_reasoning = include_reasoning,
          llm_extra_params = llm_extra_params
        )
      },
      error = \(e) {
        cli::cli_warn(
          c(
            "Correction step failed for {.file {basename(file_path)}}: {e$message}.",
            "i" = "Original transcript data kept."
          )
        )
        transcript_data # Return original on failure.
      }
    )

    if (!is.null(corrected_transcript_data)) {
      # Determine if changes occurred before writing to avoid unnecessary I/O.
      made_changes <- !identical(transcript_data, corrected_transcript_data)

      # Mark transcript as 'corrected' by setting the flag to TRUE. This happens
      # in two cases:
      # 1. When overwrite=TRUE is specified (even if no text was actually
      # changed)
      # 2. When actual corrections were made to the transcript text
      if (overwrite || made_changes) {
        corrected_transcript_data$corrected <- TRUE
        tryCatch(
          {
            jsonlite::write_json(
              corrected_transcript_data,
              file_path,
              auto_unbox = TRUE, # Ensure single-element arrays become scalars in JSON.
              pretty = TRUE # Make the JSON human-readable.
            )
            cli::cli_alert_success(
              ifelse(
                made_changes,
                "Applied corrections and saved: {.file {file_path}}",
                "Saved (overwrite=TRUE): {.file {file_path}}"
              )
            )

            return(file_path) # Successful processing and write.
          },
          error = \(e) {
            cli::cli_abort(
              "Error writing {.file {file_path}}: {e$message}"
            )
          }
        )
      } else {
        cli::cli_alert_info(
          "No changes made for: {.file {basename(file_path)}}"
        )

        return(file_path) # Processed, but no changes to write.
      }
    } else {
      # This case implies correct_transcription_errors itself returned NULL
      # (e.g. model not set).
      cli::cli_alert_warning(
        "Skipping write for {.file {basename(file_path)}}:
        null from correction.",
        wrap = TRUE
      )

      return(NA_character_)
    }
  })

  # Return only paths of files that were successfully processed and potentially
  # written.
  return(invisible(processed_files[!is.na(processed_files)]))
}


#' Corrects transcription errors using an LLM (Internal Helper).
#'
#' @param transcript_data List object representing the transcript.
#' @param terms Character vector of important terms/names.
#' @param include_reasoning Logical, if TRUE, requests and messages LLM
#'   reasoning.
#' @param llm_extra_params A list of additional parameters for the LLM call,
#'   merged with defaults (e.g., temperature = 0).
#'
#' @return List, the transcript_data with corrections.
#'
#' @noRd
correct_transcription_errors <- function(
  transcript_data,
  terms = NULL,
  include_reasoning = TRUE,
  llm_extra_params = list()
) {
  # Validate include_reasoning
  if (!rlang::is_scalar_logical(include_reasoning)) {
    cli::cli_abort(
      c(
        "x" = "Parameter {.arg include_reasoning} must be a single logical
        value (TRUE or FALSE)."
      ),
      wrap = TRUE
    )
  }

  # Get the correction model from options, preferring
  # 'minutemaker_correction_llm_model' if set.
  correction_model_label <- getOption(
    "minutemaker_correction_llm_model",
    getOption("llmr_current_model")
  )

  # Warn if minutemaker_correction_llm_model is not set and we're defaulting to current model
  if (
    is.null(getOption("minutemaker_correction_llm_model")) &&
      !is.null(getOption("llmr_current_model"))
  ) {
    cli::cli_alert_warning(
      c(
        "Option {.val minutemaker_correction_llm_model} not set,
        defaulting to current model: {.val {getOption('llmr_current_model')}}",
        "i" = "Consider setting {.val minutemaker_correction_llm_model}
        for more consistent results."
      ),
      wrap = TRUE
    )
  }

  # If no model is set, stop with an error.
  if (is.null(correction_model_label)) {
    cli::cli_abort(
      c(
        "LLM model for correction not set.",
        "x" = "Option {.val minutemaker_correction_llm_model} nor {.val llmr_current_model} is set.",
        "i" = "Please set one of these R options to specify the LLM model for corrections."
      )
    )
  }

  # Primarily targets the full transcript text, but structure allows for
  # segment-wise if needed.
  full_text_to_correct <- transcript_data$text |>
    # $text is a list. I've only seen one element in the list, but this is
    # more robust.
    unlist() |>
    paste(collapse = "\n")

  # If there is no text to correct, return the original transcript data.
  if (
    is.null(full_text_to_correct) ||
      !nzchar(stringr::str_trim(full_text_to_correct))
  ) {
    return(transcript_data) # No text content to correct.
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

  term_list_str <- ""
  if (!rlang::is_empty(terms)) {
    # shQuote ensures terms with spaces are treated as single entities in the
    # prompt.
    term_list_str <- paste(shQuote(terms, type = "cmd"), collapse = ", ")
  }

  reasoning_instructions <- ""

  # XML tags for robust parsing.
  json_wrapper_start <- "<json_corrections_output>"
  json_wrapper_end <- "</json_corrections_output>"

  if (include_reasoning) {
    # Detailed instructions for the LLM if reasoning is requested.
    reasoning_instructions <- stringr::str_glue(
      "REASONING STEP: Before providing the JSON output, please first write a detailed step-by-step thought process for the corrections you're planning to make. Think through each potential correction, considering the provided term lists, and your general knowledge. Identify patterns, analyze ambiguous cases, and determine which terms need correction. This reasoning should be free-form and exploratory, helping you arrive at the most accurate corrections.\n\nOUTPUT FORMATTING WHEN REASONING IS INCLUDED:\nAfter your free-text reasoning, provide the JSON output. This JSON output MUST be enclosed in XML tags like this: {json_wrapper_start}YOUR_JSON_HERE{json_wrapper_end}\nEnsure no other text or explanation follows the closing {json_wrapper_end} tag for the JSON part.\n\n---\n\n"
    )
  }

  # System prompt constructed using insights from experimental script.
  system_prompt <- stringr::str_glue(
    "{reasoning_instructions}You will be passed a transcribed text. Your primary goal is to correct misspelled terms and names.\n\nHere is a list of important general terms, names, and their correct forms: {term_list_str}.\n- Use this list to guide your corrections.\n- IMPORTANT: Some items in this list may contain a colon followed by an explanation (e.g., \"TERM: Explanation\"). The part after the colon is for your contextual understanding only. If you use such an item for a correction, the corrected form in your JSON output should ONLY be the term part (e.g., \"TERM\"), not \"TERM: Explanation\".\n\nMind that there could be other terms not listed here which you need to correct based on your knowledge and understanding of the context. Be suspicious of any acronyms, abbreviations, or names even if not in the list.\n\nYOUR TASK:\nOutput a JSON structure mapping the *incorrect term found in the input text* to its *correct form*.\nYour JSON output should ONLY contain corrections identified.\n\nFor example, if the input text contains errors like 'John Smth spoke about H.A.I. data from tesy and an arai report mentioned in Euro Surveillance. Piters and John agreed.', your output might be:\n{{\n  \"John Smth\": \"John Smith\",\n  \"Piters\": \"Peter S.\",\n  \"tesy\": \"TESSy\",\n  \"arai\": \"ARHAI\",\n  \"H.A.I.\": \"HAI\",\n  \"Euro Surveillance\": \"Eurosurveillance\"\n}}\n\nCRITICAL RULES (apply these to your output):\n- If a first name (e.g., 'John', 'Anna') appears in the input text and is ALREADY SPELLED CORRECTLY according to the provided list or general knowledge, DO NOT expand it to a full name (e.g., 'Anna' to 'Anna Smith'), even if 'Anna Smith' is in the important terms list. For instance, if 'Peter' is in the text and 'Peter Novak' is in the terms list, and 'Peter' is correctly spelled, DO NOT map 'Peter' to 'Peter Novak'.\n- ONLY create a mapping if a name in the input text was genuinely misspelled (e.g., 'Jon' -> 'John', 'Anan' -> 'Anna') or an incorrectly transcribed initial/abbreviation that needs a full name from the list for clarity and cannot be understood otherwise (e.g., 'P. Novak' -> 'Peter Novak' if 'P. Novak' is ambiguous, but not if 'Peter' alone is used and correct, or 'J. Smith' -> 'John Smith').\n- The goal is automated search and replacement of *errors* or *genuinely ambiguous abbreviations* in the original transcription. Do not map correctly spelled standalone first names to longer forms if the first name alone is clear and correct."
  )

  messages_payload <- c(system = system_prompt, user = full_text_to_correct)

  # Default LLM parameters; temperature = 0 for deterministic output.
  base_llm_params <- list(temperature = 0)

  # Allow user-provided parameters to override defaults.
  final_llm_params <- purrr::list_modify(base_llm_params, !!!llm_extra_params)

  llm_response_str <- NULL

  # Determine whether to force JSON based on reasoning inclusion. TRUE if no
  # reasoning, FALSE if reasoning included
  force_json_value <- !include_reasoning

  # Store parent environment to safely modify the flag
  parent_env <- parent.frame()

  tryCatch(
    {
      # Single LLM call with force_json parameter varying based on include_reasoning
      llm_response_str <- llmR::prompt_llm(
        messages = messages_payload,
        params = final_llm_params,
        force_json = force_json_value # Varies based on include_reasoning flag
      )
    },
    error = \(e) {
      stringr::str_glue("LLM call failed: {e$message}") |>
        cli::cli_warn()

      # Editable flag in parent environment.
      parent_env$llm_call_failed <- TRUE
    }
  )

  if (
    llm_call_failed ||
      is.null(llm_response_str) ||
      !nzchar(stringr::str_trim(llm_response_str))
  ) {
    # Return original if LLM call issues or empty response.
    return(transcript_data)
  }

  # Optional: Print the raw LLM response for debugging
  cli::cli_inform(c("i" = "--- LLM correction response ---"))
  cli::cli_verbatim(llm_response_str) # Use verbatim for raw output
  cli::cli_inform(c("i" = "--- End of LLM correction response ---"))

  # Attempt to parse the JSON response
  json_to_parse <- llm_response_str # Start with the full response.

  if (include_reasoning) {
    # Regex to find content within the XML wrapp er tags.
    xml_pattern <- stringr::str_glue(
      "{json_wrapper_start}((?:.|\n)*?){json_wrapper_end}"
    )

    # Extract the content inside the XML wrapper tags (captured group).
    # If pattern not found, str_match returns NA in the capture group.
    match_result <- stringr::str_match(llm_response_str, xml_pattern)
    json_to_parse <- match_result[, 2]

    # cat("\n>>>> DEBUG: Full content of match_result[, 2] directly via cat:\n")
    # cat(match_result[, 2]) # Print the whole thing
    # cat("\n<<<< END DEBUG: Full content of match_result[, 2]\n\n")

    if (is.na(json_to_parse)) {
      cli::cli_warn(
        c(
          "!" = "Reasoning enabled, but XML wrapper not found.",
          "i" = "Attempting to parse entire response as JSON."
        )
      )
      # If wrapper not found, proceed assuming the whole response might be
      # parsable JSON or LLM forgot tags.
      json_to_parse <- llm_response_str
    }
  }

  # Clean potential markdown ```json wrapper and trim whitespace
  json_to_parse <- json_to_parse |>
    stringr::str_remove("^\\s*```(?:json)?\\s*") |>
    stringr::str_remove("\\s*```\\s*$") |>
    # Trim any remaining whitespace
    stringr::str_trim()

  corrections_map <- NULL
  # Check if the string looks like a JSON object before attempting to parse.

  if (startsWith(json_to_parse, "{") && endsWith(json_to_parse, "}")) {
    tryCatch(
      {
        corrections_map <- jsonlite::fromJSON(json_to_parse)
        if (rlang::is_empty(corrections_map)) {
          cli::cli_warn(
            c(
              "!" = "LLM returned an empty or null JSON object for
              corrections_map, or parsing failed to produce a map."
            ),
            wrap = TRUE
          )
        }
      },
      error = \(e) {
        cli::cli_warn(
          c(
            "!" = "JSON parsing failed: {e$message}.",
            "i" = "JSON content (first 200 chars): {.val {substr(json_to_parse, 1, 200)}}..."
          ),
          wrap = TRUE
        )
        # Parsing failed, corrections_map remains NULL.
      }
    )
  } else {
    cli::cli_warn(
      c(
        "!" = "Final string for parsing doesn't look like a JSON object.",
        "i" = "Content (first 200 chars): {.val {substr(json_to_parse, 1, 200)}}..."
      )
    )
    # String doesn't appear to be JSON, corrections_map remains NULL.
  }

  if (rlang::is_empty(corrections_map)) {
    cli::cli_alert_warning(
      "No corrections parsed or LLM returned no changes."
    )
    return(transcript_data) # No valid corrections obtained.
  }

  original_full_text <- transcript_data$text # Keep original for comparison.
  transcript_data$text <- apply_single_correction_set(
    transcript_data$text,
    corrections_map
  )

  # Apply corrections to individual segments as well, if they exist.
  if (
    !is.null(transcript_data$segments) && length(transcript_data$segments) > 0
  ) {
    transcript_data$segments <- purrr::map(
      transcript_data$segments,
      \(seg) {
        if (is.character(seg$text)) {
          seg$text <- apply_single_correction_set(seg$text, corrections_map)
        }
        return(seg)
      }
    )
  }

  if (identical(original_full_text, transcript_data$text)) {
    cli::cli_alert_warning(
      "Corrections parsed, but no changes made to main transcript."
    )
  } else {
    cli::cli_alert_success("Transcription correction applied.")
  }
  return(transcript_data)
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
  if (rlang::is_empty(text) || !nzchar(text) || rlang::is_empty(corr_map)) {
    return(text)
  }
  # Convert list to named character vector for str_replace_all
  corrections <- unlist(corr_map)

  # Order the correction keys by length in descending order
  correction_order <- order(nchar(names(corrections)), decreasing = TRUE)
  sorted_corrections <- corrections[correction_order]

  # Use fixed string replacement to handle special characters
  return(stringr::str_replace_all(text, stringr::fixed(sorted_corrections)))
}
