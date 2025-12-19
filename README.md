
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minutemaker

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/minutemaker)](https://CRAN.R-project.org/package=minutemaker)
<!-- badges: end -->

This R package provides tools for transcribing and summarizing meeting
and conference recordings.

## Installation

You can install the development version of minutemaker from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("bakaburg1/minutemaker")
```

## Usage

This package allows transcribing audio recordings using different
speech-to-text APIs and then summarizing the transcripts using remote or
local Large Language Models. You can also start directly with existing
transcripts from the major meeting platforms (WebEx, Zoom, MS Teams,
etc.) to bypass speech-to-text entirely.

The package supports the Whisper API from OpenAI or Azure, or local
models via <https://github.com/Softcatala/whisper-ctranslate2> (install
separately) for speech transcription.

For the transcript summarization, the package can use the chat endpoint
from either OpenAI, Azure or a local model with an OpenAI compatible
REST API (for example as served by LMStudio or Ollama).

We suggest to use GPT-4 grade models for long meetings/talks (~ 1h)
since they provide a longer context window and better summarization
quality. Otherwise, the “rolling window” summarization method can be
used to produce summaries of consistent quality on long transcripts also
with smaller models.

In general, we suggest to use an LLM with a \>32K long context window,
to avoid loss of information.

Here is an example workflow.

### Setting up the package

You need to set up the infrastructure for the speech-to-text and text
summarization APIs. The LLM-powered summarization requires the
`bakaburg1/llmR` package which is installed (from GitHub) together with
`minutemaker`.

You can do this by setting the following options:

``` r
# Load the necessary packages
library(minutemaker)

# Set up LLM model of choice using llmR functions

# Example: Setting up OpenAI GPT-4 model
llmR::record_llmr_model(
  label = "openai",
  provider = "openai",
  model = "gpt-4",
  api_key = "your_openai_api_key"
)

# Example: Setting up Azure GPT model
llmR::record_llmr_model(
  label = "azure_gpt",
  provider = "azure",
  model = "your_azure_deployment_id",
  endpoint = "https://your-resource-name.openai.azure.com",
  api_key = "your_azure_api_key",
  api_version = "2024-06-01"
)

# Set the preferred LLM globally using one of the labels defined above
llmR::set_llmr_model("openai")

# Set up the speech-to-text (STT) options

options(
  # Choose the STT model among online models: "azure_whisper" or "openai_whisper"
  # or local models:  "whisper_local", "mlx_whisper_local" (python based),
  # (use "mlx_whisper_local" for Apple Silicon)
  # "whisper_ctranslate2" (cli based, install ctranslate2 separately)
  minutemaker_stt_model = "whisper_local",
  
  # OpenAI Whisper API Key (for remote OpenAI whisper model)
  minutemaker_openai_api_key = "your_openai_api_key",
  
  # Azure Whisper API credentials (for remote Azure whisper model)
  minutemaker_azure_resource_whisper = "your_azure_resource_name",
  minutemaker_azure_deployment_whisper = "your_azure_deployment_id",
  minutemaker_azure_api_key_whisper = "your_azure_api_key",
  minutemaker_azure_api_version = "2024-06-01",
  
  # LLM for transcript correction
  minutemaker_correction_llm_model = "your_preferred_correction_llm_label",
  
  # Whether to add explicit chain-of-thought reasoning for non-reasoning LLMs;
  # set this TRUE to improve corrections on those models, and FALSE for models
  # that already reason internally (e.g., OpenAI o3 or Google Gemini 2.5)
  minutemaker_include_llm_reasoning = TRUE 
)
```

These settings can also be passed manually to the various functions, but
the option system is more convenient. You only need to set the options
for the APIs you want to use (e.g., you don’t need the speech-to-text
API options if you already have a transcript).

### Transcript-First Workflow

If you already have a transcript from your meeting platform (such as
WebEx VTT files, SRT files, or MS Teams DOCX exports), you can skip the
speech-to-text transcription entirely and start directly with the
transcript. The package supports:

- **WebEx transcripts** (.vtt files)
- **Generic SRT subtitles** (.srt files)
- **MS Teams transcripts** (.docx files exported from Teams meetings)

This approach saves time and API costs while maintaining all downstream
functionality like LLM-based correction, agenda inference, and
summarization.

``` r
# Use an external transcript directly
speech_to_summary_workflow(
  target_dir = "path/to/your/project",

  # Skip audio processing entirely by providing external transcript
  external_transcript = "path/to/transcript.vtt", # or .srt, or .docx
  source_audio = NULL, # No audio file needed

  # Enable speaker diarization import if available in the transcript
  import_diarization = TRUE,

  # All other arguments work the same as with audio input
  use_agenda = "yes",
  llm_provider = "openai",
  formatted_output_file = "meeting_summary.txt"
)
```

The workflow will automatically detect and standardize your external
transcript into the package’s internal format, then proceed with LLM
correction and summarization as usual.

Alternatively, simply place your transcript file (.vtt, .srt, or .docx)
in your working directory - the workflow will auto-detect it and use it
as the primary source.

### Transcribing the audio

If you don’t have an existing transcript, the first step is transcribing
the audio recording of a meeting.

Since the Whisper API can process files only up to 25MB (~ 50-60 minutes
in mp3), it could be useful to split the audio file into smaller chunks.
This can be done using the `split_audio()` function.

``` r

# Construct the path to the source audio/video file

audio_file <- "path-to-audio-file.mp3"
stt_audio_dir <- "path-to-audio-files-folder"

# Split the audio files into 40 minutes segments

split_audio(
  audio_file = audio_file,
  segment_duration = 40,
  output_folder = stt_audio_dir
)
```

The function will ask you to install the `av` package to split the audio
file `split_audio()` should works only with video files via the `ffmpeg`
command line tool (a separated installation is required). Only the “mp4”
format was tested.

The second step is transcribing the audio files. This can be done using
the `transcribe_audio()` function.

``` r

# Construct the path to the recording parts directory

stt_audio_dir <- "path-to-audio-files-folder"

transcript <- perform_speech_to_text(
  # Can also be the path to a single audio file
  audio_path = stt_audio_dir,
  
  # A folder where the transcript for each audio file will be saved in JSON
  # format. Defaults to "transcription_output_data" in the same directory as
  # `audio_path`.
  output_dir = "transcription_output_data",
  
  # Already transcribed files will be skipped if this is FALSE
  overwrite = FALSE,
  
  # Optional; useful to recognize uncommon words, acronyms, and names
  initial_prompt = "Keep these terms in mind: term1, name2, etc..."
  
  # Takes more arguments to pass to the API functions. See the documentation for
  # details.
)

# In alternative, the transcript data frame can be generated at will using the
# parse_transcript_json() function and the folder where the trascription outputs
# are stored.
transcript <- parse_transcript_json("transcription_output_data")

# This allows you also to pass in the starting time of the event in
# the HH:MM(:SS)( AM/PM) format which allows to compute the timing of the speech
# segment in real clock time.
# The event start time can be set globally using the
# minutemaker_event_start_time option
transcript_with_clocktime <- parse_transcript_json(
  "transcription_output_data",
  event_start_time = "10:00" # or 10:30:12, or 15:30, or 03:30 PM, etc...
)

# Then, you can save the transcript to a file
readr::write_csv(transcript, file.path(work_dir, day, "transcript.csv"))
```

NOTE: this process sometimes may fails for issues on the API side. Some
times the process stops and with the creation with a JSON file with an
error inside; just remove this file and restart the process. For other
API related error, restarting the process is usually enough. Otherwise,
feel free to open an issue.

### Manual Transcript Import and Enhancement

For advanced users who want to work with transcripts manually, the
package provides functions to import and enhance transcript data.

The `import_transcript_from_file()` function can read transcripts in
various formats:

``` r
# Import a transcript file
transcript <- import_transcript_from_file(
  transcript_file = "path/to/transcript.vtt", # Supports .vtt, .srt, .docx
  import_diarization = TRUE # Include speaker information if available
)
```

For transcripts that include chat messages (such as WebEx exports), you
can integrate them using `add_chat_transcript()`:

``` r
# Add chat messages to an existing transcript
enhanced_transcript <- add_chat_transcript(
  transcript_data = transcript,
  chat_transcript = "path/to/chat_export.txt",
  start_time = "12:00 PM" # To align chat timestamps
)
```

**Note**: For most users, the transcript-first workflow (see above)
handles all of this automatically. Manual import functions are mainly
useful for custom processing or when working with unusual transcript
formats.

### LLM-based Transcript Correction

After obtaining a transcript, you may want to use a Large Language Model
to correct potential errors in spelling, grammar, or term usage. The
function `apply_llm_correction()` is designed for this purpose. It
processes transcript JSON files (either a single file or all JSON files
in a directory), applies corrections using an LLM, and saves the
corrected version.

Key features:

- **Selective Processing**: It can skip files already marked as
  corrected if `overwrite = FALSE`.
- **Term Guidance**: You can provide a list of important `terms` (names,
  acronyms, etc.) to guide the LLM.
- **Model Choice**: The LLM used for correction is determined by the R
  option `minutemaker_correction_llm_model`.
- **Reasoning Output**: If `minutemaker_include_llm_reasoning` is `TRUE`
  (or passed directly as an argument), the LLM’s reasoning for
  corrections can be included in the output, which is useful for
  debugging.

``` r
# Example: Correcting transcripts in a directory
corrected_files <- apply_llm_correction(
  input_path = "transcription_output_data", # Directory with transcript JSONs
  terms = c(
    "Specific Term",
    "Participant name",
    "ACRONYM: explanation"), # Explanations give context to the LLM but are not used for the corrections
  overwrite = TRUE # Set to FALSE to skip already corrected files
)

# Example: Correcting a single transcript file
corrected_file <- apply_llm_correction(
  input_path = "transcription_output_data/segment_1.json",
  terms = vocabulary, # Use a pre-defined vocabulary
  overwrite = FALSE
)
```

This step is also integrated into the main
`speech_to_summary_workflow()` function, as shown later.

### Creation of the “Agenda” object

The next step is the creation of the “Agenda” object. This is a list of
lists, where each element represents a talk in the meeting.

Here’s an example of Agenda object with 2 talks:

``` r

agenda <- list(
  list(
    session = "Session 1",
    title = "Opening",
    
    # Information to provide context to the summarization process
    type = "Conference introduction",
    description = "A description of the topic",
    
    # The start and end time of a talk in seconds from the beginning of the
    # event
    from = 0,
    to = 334,
    
    # Speakers and moderators as vectors of names
    speakers = c("Speaker 1", "Speaker 2"),
    moderators = "Moderator name",
  ),
  list(
    session = "Session 1",
    title = "Presentation 1",
    type = "conference presentation",
    description = "This presentation is about this and that.",
    from = 334,
    to = 600,
    speakers = c("Speaker 3"),
    moderators = "Moderator name",
  )
)

# Alternatively, actual clock times can be used, according to the HH:MM(:SS)(
# AM/PM) format
list(
  list(
    session = "Session 1",
    title = "Presentation 1",
    type = "conference presentation",
    description = "This presentation is about this and that.",
    from = "10:10",
    to = "10:50",
    speakers = c("Speaker 3"),
    moderators = "Moderator name",
  )
)
```

This object can be saved in an R file with the `dput()` function and
then loaded with the `dget()` function.

The most important components of the agenda are the start and end time
of each talk. These needs to be manually extracted by reading the
transcript. The timings are not mandatory if the meeting consists of
only one talk. The agenda object itself is not strictly necessary, but
can be helpful for long meetings with multiple talks.

An alternative approach is to generate the agenda automatically via the
`infer_agenda_from_transcript()` function. This function uses the
transcript to infer the different sessions of the talk, their start and
end times and also generate a description and a title. The function can
use contextual information to improve the quality of the generated
agenda, such as the event description, the audience, a vocabulary, and
the expected agenda.

It’s important to review and correct the inferred agenda, since the
function might not be able to infer the correct structure of the
meeting.

### Summarizing a single meeting transcript

The final step is summarizing the transcript. This can be done using the
`summarise_transcript()` function. The first choice is to decide which
summarization method to use. The package currently supports two
summarization methods: “simple” and “rolling”. The “simple” method
ingest the whole transcript at once and produces a summary of the whole
transcript; it’s a faster method but the quality may drop with very long
transcripts. The “rolling” method ingest the transcript in chunks of a
given size and produces a summary for each chunk; these summaries are
then aggregated into a final summary of the whole transcript; it’s a
slower method but the quality is more consistent across different
transcript lengths. If the “rolling” method is used, the user can also
specify the size of the chunks to use (`window_size` argument, default
is 15 minutes in transcript time).

The user can also define the expected output length in number of pages
(`output_length`, default is 3 pages). The summarization process will
try to produce a summary of the given length, but the actual length may
vary depending on the transcript content (and the LLM idiosyncrasies).

The following code shows how to set all the arguments for the
summarization process; but it’s possible to use the function with only
the transcript data to get a summary. The other arguments only provide
additional contextual information to improve the summary.

For example an agenda generated via `infer_agenda_from_transcript()` can
be formatted into text using `format_agenda()` and then added to
`summary_structure` argument (see example below).

``` r

# Construct the path to the transcript file
transcript_data <- readr::read_csv(
  "path-to-transcript.csv",
  show_col_types = FALSE)

# Extract the text from the transcript file at a given time interval
transcript_text <- extract_text_from_transcript(
  transcript_data, start_time = 0, end_time = 334
)

# Define the summarization method
method <- "rolling" # or "simple"

# Alternatively, you can extract the text from the transcript file using the
# agenda object which has the start and end time of each talk
agenda <- dget("path-to-agenda.R")

transcript_text <- extract_text_from_transcript(
  transcript_data, agenda = agenda[[1]] # First talk in the agenda
)

# You may want to define some other contextual information about the meeting
# to improve the summary.

## A description of the meeting/conference with some background
event_description <- "***Title of the Conference***
  
  Description of the conference."

## A vocabulary of terms to keep in mind when summarizing the transcript AND
## for LLM-based transcript correction. Can be useful also to make the 
## summarization robust to transcription errors.
vocabulary <- c("Term1", "ACRONYM: description", "Jack Black", "etc")

## Specific talk details, like title and speakers. Can be built using
## `generate_recording_details()` from an agenda element:
recording_details <- generate_recording_details(
  agenda[[1]],
)

## or manually:
recording_details <- generate_recording_details(
  session = "Session 1",
  title = "Opening",
  type = "Conference introduction",
  speakers = c("Speaker 1", "Speaker 2"),
)

## The audience of the meeting/conference, which helps the summarisation to
## focus on specific topics and helps setting the tone of the summary.
audience <- "An audience with some expertise in the topic
    of the conference and with a particular interest in this and that."

# Not mandatory but may help, for example with hybrid events where the room
# speakers are not reported in the transcript. Your instructions will be
# appended to the default ones. See get_prompts("diarization_template") for
# the defaults.
diarization_instructions <- "The event is hybrid, with both online and
in-presence participants and speakers. Extrapolate the speakers from the
context if not reported"

# The user can modify the expected structure of the summary (check it using
# get_prompts("summary_structure")). For example, one can add an extra
# summarisation section with:
summary_structure <- paste0(
  get_prompts("summary_structure"),
  "
- My extra summarisation instruction"
)

# The use can also use the summarisation instruction to add and agenda to drive
# the summarisation focus:
agenda_text <- format_agenda(agenda) # Renamed from agenda to avoid conflict
summary_structure_with_agenda <- stringr::str_glue("
  {get_prompts('summary_structure')}
  Here is an agenda of the event to keep into account while summarizing:
  {agenda_text}
  Strictly follow the agenda to understand which information is worth summarizing.
")


# Finally, the user can add extra output instructions to the default ones (check
# them using get_prompts("output_summarisation") for the summarisation and
# get_prompts("output_rolling_aggregation") for the rolling aggregation). For
# example, one can add an extra output instruction with:
output_instructions <- "
- Focus on and report quantitative details when they
are discussed."

# Get the summary

talk_summary <- summarise_transcript(
  transcript_data = transcript_text,
  method = method,
  event_description = event_description,
  recording_details = recording_details,
  audience = audience,
  vocabulary = vocabulary,
  
  summary_structure = summary_structure_with_agenda, # Or don't pass it to use the default
  extra_diarization_instructions = diarization_instructions,
  extra_output_instructions = output_instructions,
  
  provider = "my-LLM-provider-of-choice",
)
```

Use `cat(talk_summary)` to print the summary to the console.

### Summarizing multiple meeting transcripts

If your transcript covers multiple talks, you can use the
`summarise_full_meeting()` function to generate a summary for each talk
in the transcript. The various contextual information (e.g. event
description and audience) and custom prompt instructions will be applied
to the whole set of talks.

In this context, the use of an agenda object is what brings the most
value, since it clearly defines the timing of each talk and provide
extra context as a title, the recording type (e.g., conference talk,
meeting, lesson, etc), description of the topic, the list of speakers,
etc…

The output of the function is a “summary tree”, i.e. a list with a
different summary for each talk in the agenda.

``` r

conference_summaries <- summarise_full_meeting(
  transcript_data = transcript_data,
  method = method,
  agenda = agenda,
  output_file = "path-to-output-file.R",

  event_description = event_description,
  audience = audience,
  vocabulary = vocabulary,

  summary_structure = summary_structure_with_agenda, # Using the one with agenda
  extra_diarization_instructions = diarization_instructions,
  extra_output_instructions = output_instructions,

  provider = "my-LLM-provider-of-choice",
)
```

The “summary tree” will be saved at the location indicated by
`output_file`. Unless the `overwrite` argument is set to `TRUE`, the
function will stop if the `output_file` already exists.

### Formatting the summary

To generate a more human-readable document with all the summaries, you
can use the `format_summary_tree()` function. This function will
generate a formatted text file with the summaries of all the talks in
the agenda. The `agenda` provide information about each talk.

``` r

formatted_summary <- format_summary_tree(
  summary_tree = conference_summaries, # Use the output from summarise_full_meeting
  agenda = agenda,
  output_file = "path-to-output-file.txt" # Optional file to save the summary
)

cat(formatted_summary)
```

The output will be saved at the location indicated by `output_file` if
not `NULL`.

### Workflow shortcut function

The package is built around a flexible workflow that can start from
either audio files or existing transcripts, ending with a formatted
summary file. The workflow includes all the steps described above:

- **Audio-first path**: Splitting audio files → Speech-to-text
  transcription → LLM correction → Summarization
- **Transcript-first path**: External transcript import →
  Standardization → LLM correction → Summarization

Both paths support chat message integration, agenda inference, and
formatted output generation.

The `speech_to_summary_workflow()` function implements this workflow and
can be used to perform all the steps in one go.

``` r

# initial_prompt, event_description, audience,
# vocabulary, diarization_instructions, summary_structure_with_agenda
# are defined in the previous examples

# Set a workspace directory (replace with your own project path if needed)
work_dir <- "path/to/your/project"

# Perform the whole audio files to formatted summary workflow. Most arguments
# are omitted since the defaults are fine but some (e.g. the output paths and
# the overwrite arguments) are shown for clarity.
speech_to_summary_workflow(
  target_dir = work_dir,

  # Arguments for `split_audio`
  source_audio = file.path(work_dir, "recording.mp3"),
  split_audio = TRUE,
  split_audio_duration = 40,
  stt_audio_dir = file.path(work_dir, "audio_to_transcribe"),
  overwrite_stt_audio = FALSE,

  # Arguments for `perform_speech_to_text`
  stt_output_dir = file.path(work_dir, "transcription_output_data"),
  stt_model = getOption("minutemaker_stt_model"),
  stt_initial_prompt = initial_prompt,
  stt_language = "en",
  stt_overwrite_output_files = FALSE,

  # Arguments for LLM-based transcript correction (runs after STT)
  enable_llm_correction = TRUE, # Defaults to TRUE
  # `vocabulary` is also used for `terms` in `apply_llm_correction`

  # Arguments for `parse_transcript_json`
  transcript_file = file.path(work_dir, "transcript.csv"),
  event_start_time = "09:00 AM",
  overwrite_transcript = FALSE,

  # Arguments for external transcript input (transcript-first workflow)
  external_transcript = list.files(
    work_dir,
    pattern = "\\.(vtt|srt|docx)$",
    full.names = TRUE
  )[1], # Auto-detects external transcripts in working directory
  import_diarization = TRUE, # Include speaker info from transcript

  # Arguments for `add_chat_transcript`
  chat_file = list.files(work_dir, pattern = "(?i)Chat.*\\.txt$", full.names = TRUE)[1],
  chat_format = "webex",

  # Arguments for `summarise_full_meeting` and `infer_agenda_from_transcript`
  use_agenda = "yes", # or "ask"/"no"
  agenda_file = file.path(work_dir, "agenda.R"),
  expected_agenda = NULL,
  agenda_generation_window_size = 3600,
  extra_agenda_generation_args = NULL,

  # Arguments for the summarization
  multipart_summary = TRUE,
  event_description = event_description,
  audience = audience,
  vocabulary = vocabulary, # Used for summarization AND LLM correction
  consider_diarization = TRUE,
  summary_structure = summary_structure_with_agenda,
  extra_diarization_instructions = diarization_instructions,
  extra_output_instructions = output_instructions,
  llm_provider = getOption("llmr_llm_provider"),
  summarization_window_size = 15,
  summarization_output_length = 1,
  summarization_method = "simple",

  summarization_output_file = file.path(work_dir, "event_summary.R"),
  overwrite_summary_tree = FALSE,

  # Arguments for `format_summary_tree`
  formatted_output_file = file.path(work_dir, "event_summary.txt"),
  overwrite_formatted_output = FALSE
)
```

### Changing the default prompts

The summarisation process is built upon a number of “technical prompt”
under the hood.

You can use the default prompts or define your own through a number of
options.

You can see the current prompts by running:

``` r

prompts <- get_prompts() # get all the prompts in a named vector
prompt <- get_prompts("output_instructions") # get a specific prompt

# Print one of the prompts, cat() improves the formatting
cat(prompts[[1]])
```

Or you can use `set_prompts()` function to edit the defaults.

``` r

set_prompts(audience = "My custom audience description")
```

The package uses “structural prompt” (a “base_task” prompt and all the
promp ending with “\_template”) which organize the prompt general
implementation and then uses the “instruction prompt”
(e.g. “diarization_instructions” or “summarisation_sections”) to
specifically instruct the LLM.

The user is mostly expected to change the second type, while changing
the first is an expert option since it can break the prompt generation.

Use `set_promts(force = TRUE)` to restore all prompts to their default
values.
