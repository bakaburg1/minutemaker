# Produce the package general documentation
#' @title minutemaker
#'
#' @name minutemaker
#' @docType package
#' @description Package which uses LLM models to generate minutes from audio recordings or transcriptions of meetings.
#'
#' @import dplyr
"_PACKAGE"

# Initialize prompt options when the package loads
.default_opts <- function() {
  list(
    transcription_output_dir = "transcription_output_data",
    stt_audio_dir = "audio_to_transcribe",
    transcript_file = "transcript.csv",
    agenda_file = "agenda.R",
    multipart_summary_str_file = "outputs/multipart_summary_structure.R",
    formatted_output_file = "outputs/event_summary.md",
    summary_tree_output_file = "event_summary.R",
    context_material_dir = "documentation",
    context_dir = "context",
    context_gen_strategy = "agentic",
    context_gen_llm_model = NULL,
    context_generation_max_retries = 3,
    overwrite_context = FALSE,
    stt_model = "whisper_local",
    whisper_package = "openai-whisper",
    mlx_whisper_package = "mlx_whisper",
    openai_api_key = NULL,
    azure_resource_whisper = NULL,
    azure_deployment_whisper = NULL,
    azure_api_key_whisper = NULL,
    azure_api_version = NULL,
    split_audio_parallel = FALSE,
    split_audio_output_dir = "recording_parts",
    split_audio_parallel_timeout = 120,
    correction_llm_model = NULL,
    include_llm_reasoning = TRUE,
    event_start_time = NULL,
    temp_agenda = NULL,
    temp_agenda_last_bp = NULL,
    temp_agenda_hash = NULL,
    data_merge_hash = NULL,
    data_merge_model = NULL
  )
}

.onLoad <- function(libname, pkgname) {
  set_prompts()

  # Build fully qualified option names from defaults.
  default_options <- .default_opts()
  names(default_options) <- paste0("minutemaker.", names(default_options))

  # Set only missing options so user values are preserved.
  current_options <- options()
  missing_options <- setdiff(names(default_options), names(current_options))

  if (length(missing_options) > 0) {
    options(default_options[missing_options])
  }
}
