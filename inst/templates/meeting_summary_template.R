# Meeting summary generation template
#
# Instructions:
# 1) Copy this file to your project folder and edit the placeholders.
# 2) Place meeting materials next to the script (audio, transcript, chat).
# 3) Run the script in a fresh R session.
# 4) Outputs are written to an "outputs" folder under the script directory.
#
# Optional: Use this to locate the template from the package.
# template_path <- system.file(
#   "templates",
#   "meeting_summary_template.R",
#   package = "minutemaker"
# )

rlang::check_installed(c("minutemaker", "funr", "llmR", "fs", "cli"))

minutemaker::set_prompts()

# Session-level options
# These are only needed when performing speech-to-text (STT).
# If you are using an external transcript, you can omit STT options.
options(
  minutemaker_stt_model = "your_stt_provider",
  minutemaker_correction_llm_model = "your_correction_model",
  minutemaker_split_audio_parallel = TRUE,

  # Context generation options:
  # Folder containing meeting materials
  minutemaker_context_material_dir = "documentation",
  # Generation strategy
  minutemaker_context_gen_strategy = "agentic",
  # Model for context generation
  minutemaker_context_gen_llm_model = "your_context_model",
  # Don't overwrite existing context
  minutemaker_overwrite_context = FALSE
)

# Select the LLM used for summarization
llmR::set_llmr_model("your_summary_model")

# Resolve the working directory based on the script location
work_dir <- funr::get_script_path()
if (is.null(work_dir)) {
  work_dir <- getwd()
}

cli::cli_alert_info("Working directory: {work_dir}")

chat_log <- NULL

output_dir <- file.path(work_dir, "outputs")
fs::dir_create(output_dir)

# Context inputs -----------------------------------------------------------

# Initial prompt to bias transcription toward key terms or names.
# Only used when running STT. Keep this short to avoid model limits.
initial_prompt <- paste(
  "Ensure accurate transcription for these terms and names:",
  "Project Alpha, Working Group 1, Acronym XYZ, Person One, Person Two."
)

# Description of the event based on the agenda scope and purpose
event_description <- paste(
  "Internal project meeting covering progress updates,",
  "key decisions, and next steps."
)

# Expected agenda structure based on the meeting content
expected_agenda <- c(
  "Welcome and agenda adoption",
  "Project updates and milestones",
  "Risks, issues, and mitigation",
  "Decisions and action items",
  "Next steps and closing"
) |>
  paste(collapse = "\n")

# Define the audience for the summary
event_audience <- paste(
  "Team leads, project managers, and stakeholders",
  "who need a concise summary of decisions and actions."
)

# Vocabulary list including names, acronyms, and domain terms
vocabulary <- c(
  "Project Alpha",
  "Working Group 1",
  "Acronym XYZ",
  "Milestone A",
  "Deliverable B",
  "Person One",
  "Person Two"
) |>
  unique()

# Diarization instructions (speaker identification)
diarization_instructions <- paste(
  "Virtual meeting with multiple speakers.",
  "Use consistent initials after the first mention."
)

# Extra formatting rules for the summary output
extra_output_instructions <- c(
  "Use consistent bullet points (-) for main points",
  "Use (o) for sub-points or Q&A items",
  "Define abbreviations on first use",
  "Keep action items concise and explicit"
)

use_agenda <- "yes" # Use "ask" to interactively choose

# Outputs -----------------------------------------------------------------

multipart_r_path <- file.path(output_dir, "event_summary.multipart.R")
multipart_md_path <- file.path(output_dir, "Summary by agenda items.md")
if (!file.exists(multipart_r_path) || !file.exists(multipart_md_path)) {
  cli::cli_alert("Running multipart summary with agenda")
  minutemaker::speech_to_summary_workflow(
    target_dir = work_dir,
    # STT-only inputs (omit if you use an external transcript)
    split_audio_duration = 10,
    stt_initial_prompt = initial_prompt,
    # External transcript inputs (set to a file path to skip STT)
    external_transcript = NULL,
    use_agenda = use_agenda,
    expected_agenda = expected_agenda,
    audience = event_audience,
    event_description = event_description,
    enable_llm_correction = TRUE,
    overwrite_transcription_files = FALSE,
    vocabulary = vocabulary,
    extra_diarization_instructions = diarization_instructions,
    extra_output_instructions = extra_output_instructions,
    chat_file = chat_log,
    generate_context = TRUE, # Enable automatic context generation
    summarization_output_file = multipart_r_path,
    formatted_output_file = multipart_md_path
  )
} else {
  cli::cli_alert_info("Skipping multipart summary: outputs already exist")
}

single_md_path <- file.path(output_dir, "Short summary.md")
if (!file.exists(single_md_path)) {
  cli::cli_alert("Running short summary with agenda")
  minutemaker::speech_to_summary_workflow(
    target_dir = work_dir,
    use_agenda = use_agenda,
    expected_agenda = expected_agenda,
    multipart_summary = FALSE,
    # STT-only inputs (omit if you use an external transcript)
    stt_initial_prompt = initial_prompt,
    # External transcript inputs (set to a file path to skip STT)
    external_transcript = NULL,
    audience = event_audience,
    event_description = event_description,
    summarization_output_length = 3,
    enable_llm_correction = TRUE,
    overwrite_transcription_files = FALSE,
    chat_file = chat_log,
    vocabulary = vocabulary,
    extra_diarization_instructions = diarization_instructions,
    extra_output_instructions = extra_output_instructions,
    generate_context = TRUE, # Enable automatic context generation
    formatted_output_file = single_md_path
  )
} else {
  cli::cli_alert_info("Skipping short summary: output already exists")
}

rolling_md_path <- file.path(output_dir, "Long summary.md")
if (!file.exists(rolling_md_path)) {
  cli::cli_alert("Running rolling-window summary with agenda")
  minutemaker::speech_to_summary_workflow(
    target_dir = work_dir,
    use_agenda = use_agenda,
    expected_agenda = expected_agenda,
    multipart_summary = FALSE,
    # STT-only inputs (omit if you use an external transcript)
    stt_initial_prompt = initial_prompt,
    # External transcript inputs (set to a file path to skip STT)
    external_transcript = NULL,
    audience = event_audience,
    event_description = event_description,
    enable_llm_correction = TRUE,
    overwrite_transcription_files = FALSE,
    vocabulary = vocabulary,
    extra_diarization_instructions = diarization_instructions,
    extra_output_instructions = extra_output_instructions,
    chat_file = chat_log,
    summarization_method = "rolling",
    generate_context = TRUE, # Enable automatic context generation
    formatted_output_file = rolling_md_path
  )
} else {
  cli::cli_alert_info("Skipping rolling summary: output already exists")
}
