
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
local Large Language Models.

The package at the moment uses the Whisper API from either OpenAI or
Azure or a local model on top of
<https://github.com/Softcatala/whisper-ctranslate2> (to be installed
separetely) for the speech transcription.

For the transcript summarization, the package can use the chat endpoint
from either OpenAI, Azure or a local model with an OpenAI compatible
REST API (for example as served by LMStudio or Ollama). We suggest to
use GPT-4 grade models for long meetings/talks (~ 1h) since they provide
a longer context window and better summarization quality. In the future
we plan to add support for other summarization techiques which should
allow better performance with smaller models or very long transcripts.

Here is an example workflow.

### Setting up the package

You need to set up the API keys for the speech-to-text and text
summarization APIs. You can do this by setting the following options:

``` r

# Load the package
library(minutemaker)

# Set the API information for the speech-to-text API of choice

# OpenAI example

options(
  
  # OpenAI API Key (for both text-to-speech and text summary)
  minutemaker_openai_api_key = "***"
  
  minutemaker_openai_model_gpt = "gpt-4",
)

# Azure example

options(
  
  # Azure Whisper API (for text-to-speech)
  minutemaker_azure_resource_whisper = "***",
  minutemaker_azure_deployment_whisper = "***",
  minutemaker_azure_api_key_whisper = "***",
  
  # Azure GPT API (for text summary)
  minutemaker_azure_resource_gpt = "***",
  minutemaker_azure_deployment_gpt = "***",
  minutemaker_azure_api_key_gpt = "***",
  
  # Azure common parameters (common)
  minutemaker_azure_api_version = "***"
)

# Local GPT model example

options(
  # Local LLM model (for text summary)
  minutemaker_local_endpoint_gpt = "local-host-path-to-model"
)
```

These setting can be also passed manually to the various functions, but
the option system is more convenient. Of course you just need to set the
options for the APIs you want to use (e.g., you don’t need the
speech-to-text API options if you already have a transcript).

### Transcribing the audio

The first step is usually transcribing the audio recording of a meeting.

Since the Whisper API can process files only up to 25MB (~ 20 minutes in
mp3), it could be useful to split the audio file into smaller chunks.
This can be done using the `split_audio()` function.

``` r

# Construct the path to the source audio/video file

audio_file <- "path-to-audio-file.mp3"
recording_parts_folder <- "path-to-audio_segments_folder"

# Split the audio files into 20 minutes segments

split_audio(
  audio_file = audio_file,
  output_folder = recording_parts_folder,
  segment_duration = 20
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

recording_parts_folder <- "path-to-audio_segments_folder"

transcript <- perform_speech_to_text(
  # Can also be the path to a single audio file
  audio_path = recording_parts_folder,
  
  # A folder where the transcript for each audio file will be saved in JSON
  # format. Defaults to "transcripts" in the "audio_path" parent directory.
  output_dir = "transcript_folder",
  
  # Already transcribed files will be skipped if this is FALSE
  overwrite = FALSE,
  
  # Optional; useful to recognize uncommon words, acronyms, and names
  initial_prompt = "Keep these terms in mind: ..."
  
  # Takes more arguments to pass to the API functions. See the documentation for
  # details.
)

# Optionally, you can save the transcript to a file
readr::write_csv(transcript, file.path(work_dir, day, "transcript.csv"))
```

### Importation of existing transcript data

Since the transcription process is never perfect (for example the
Whisper model may lose some segments), it may be useful to fill the gaps
using an existing transcript from another source. For example WebEx
transcritps are more complete than the Whisper ones, but they are not as
accurate. Furthermore, these external transcript may contain additional
information, such as the speaker names or chat messages.

The package provides the function `extract_text_from_transcript()` to
read a vtt/srt transctipt and `add_chat_transcript()` to import the chat
messages into a transcript. These functions at the moment only work with
WebEx outputs.

Then, the `merge_transcripts()` function can be used to merge the
transcript from different sources, also importing the speaker name if
present. The merging follows a number of heuristics since two
transcripts may not be perfectly aligned. Specifically, an imported
segment is put in place of the closest empty segment in the source
transcritpt; for speaker importation, a similarity score between
close-by segments is computed through the GloVe word embedding model to
associate the speaker name to the most likely segment.

``` r

imported_transcript <- import_transcript_from_file(
  transcript_file = "path-to-transcript-file.vtt",
)

merged_transcript <- merge_transcripts(
  transcript_data = original_transctript,
  imported_transcript = imported_transcript,
)

final_transcript <- add_chat_transcript(
  transcript_data = merged_transcript,
  chat_file = "path-to-chat-file.txt",
  # Necessary to align the chat messages with the transcript
  # can use both HH::MM AM/PM or HH:MM:SS
  chat_start_time = "12:00 PM"
)

# Optionally, you can save the transcript to a file
readr::write_csv(final_transcript, file.path(work_dir, day, "transcript.csv"))
```

``` r

### Creation of the "Agenda" object

The next step is creating the "Agenda" object. This is a list of lists, where each element represents a talk in the meeting.

Here's an example of Agenda object with 2 talks:
```

``` r

agenda <- list(
  list(
    session = "Session 1",
    title = "Opening",
    type = "Conference introduction",
    
    # The start and end time of a talk in seconds
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
    from = 334,
    to = 600,
    speakers = c("Speaker 3"),
    moderators = "Moderator name",
  )
)
```

This object can be saved in an R file with the `dput()` function and
then loaded with the `dget()` function.

The most important parts of the agenda are the start and end time of
each talk. These needs to be manually extracted by reading the
transcript. The timings are not mandatory if the meeting consists of
only one talk. The agenda object itself is not strictly necessary, but
can be helpful for long meetings with multiple talks.

### Summarizing a single meeting transcript

The final step is summarizing the transcript. This can be done using the
`summarize_transcript()` function:

``` r

# Construct the path to the transcript file

transcript_data <- readr::read_csv(
  "path-to-transcript.csv",
  show_col_types = FALSE)

# Extract the text from the transcript file at a given time interval
transcript_text <- extract_text_from_transcript(
  transcript_data, start_time = 0, end_time = 334
)

# Alternatively, you can extract the text from the transcript file using the
# agenda object which has the start and end time of each talk
agenda <- dget("path-to-agenda.R")

transcript_text <- extract_text_from_transcript(
  transcript_data, agenda = agenda[[1]]
)

# You may want to define some other contextual information about the meeting
# to improve the summary.

## A description of the meeting/conference with some background
event_description <- "***Title of the Conference***
  
  Description of the conference."

## A vocabulary of terms to keep in mind when summarizing the transcript. Can
## be useful also to make the summarization robust to transcription errors.
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
event_audience <- "An audience with some expertise in the topic
    of the conference and with a particular interest in this and that."

# Not mandatory but may help, for example with hybrid events where the room
# speakers are not reported in the transcript. Check
# get_prompts("diarization_instructions") for the defaults.
diarization_instructions <- "The event is hybrid, with both online and
in-presence participants and speakers. Extrapolate the speakers from the
context if not reported"

# The user can also modify the summary_sections and output_instructions (check
# them using get_prompts() with the relative option). For example, one can add
# an extra output instructions with:
output_instructions <- paste0(
  get_prompts("output_instructions"),
  "\n- Focus on and report quantitative details when they are discussed."
)

# Get the summary

talk_summary <- summarise_transcript(
  transcript = transcript_text,
  
  event_description = event_description,
  recording_details = recording_details,
  vocabulary = vocabulary,
  audience = event_audience,
  
  diarization_instructions = diarization_instructions,
  output_instructions = output_instructions
  
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

``` r

conference_summaries <- summarise_full_meeting(
  transcript_data = transcript_data,
  agenda = agenda,
  output_file = "path-to-output-file.R",
  
  event_description = event_description,
  event_audience = event_audience,
  vocabulary = vocabulary,
  
  diarization_instructions = diarization_instructions,
  output_instructions = output_instructions
  
  provider = "my-LLM-provider-of-choice",
)
```

A “summary tree” (a list with each talk summary) will be created at the
location indicated by `output_file`.

### Formatting the summary

To generate a more human-readable document with all the summaries, you
can use the `format_summary_tree()` function:

``` r

formatted_summary <- format_summary_tree(
  summary_tree = talk_summary,
  agenda = agenda[[1]],
  output_file = "path-to-output-file.txt" # Optional file to save the summary
)

cat(formatted_summary)
```

### Workflow shortcut function

The package is built around a typical workflow that goes from the
transcription of an audio file, to the summarization of the transcript,
to the production of a formatted output. The
`speech_to_summary_workflow()` function implements this workflow and can
be used to perform all the steps in one go.

``` r

# initial_prompt, event_description, event_audience,
# vocabulary, diarization_instructions are defined in the previous examples

# Perform the whole audio files to formatted summary workflow. Most arguments are
# omitted since the defaults are fine but some (e.g. the output paths and the
# overwrite arguments) are shown for clarity.
speech_to_summary_workflow(
  # Arguments for `perform_speech_to_text`
  audio_path = "recording_parts",
  
  stt_intermediate_dir = "transcripts",
  stt_overwrite_intermediate_files = FALSE,
  stt_model = getOption("minutemaker_stt_model"),
  
  stt_initial_prompt = initial_prompt,
  
  # Arguments for `merge_transcripts`
  # Assumes an existing .vtt transcript file in the working directory
  transcript_to_merge = list.files(pattern = "\\.vtt")[[1]],
  
  # Arguments for `add_chat_transcript`
  # Assumes an existing Chat.txt file in the name in the working
  # directory
  chat_file = list.files(pattern = "Chat")[[1]],
  chat_start_time = "09:00 AM", # or e.g. 14:00:00
  
  transcript_output_file = "transcript.csv",
  overwrite_transcript = FALSE,
  
  # Arguments for `summarise_full_meeting`
  # Assumes an existing agenda.R file in the working directory
  agenda = "agenda.R",
  summarization_output_file = "event_summary.R",
  
  event_description = event_description,
  event_audience = event_audience,
  vocabulary = vocabulary,
  
  # you can pass summary_sections, diarization_instructions, or
  # output_instructions to override the default prompts
  diarization_instructions = diarization_instructions,
  
  llm_provider = "my-LLM-provider-of-choice",
  
  overwrite_summary_tree = FALSE,
  
  # Arguments for `format_summary_tree`
  formatted_output = "event_summary.txt",
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
prompt ending with “\_template”) which organize the prompt general
implementation and then uses the “instruction prompt”
(e.g. “diarization_instructions” or “summarisation_sections”) to
specifically instruct the LLM.

The user is mostly expected to change the second type, while changing
the first is an expert option since it can break the prompt generation.

Use `set_promts(force = TRUE)` to restore all prompts to their default
values.
