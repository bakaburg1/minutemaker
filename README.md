
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
REST API (for example as served by LMStudio or Ollama).

We suggest to use GPT-4 grade models for long meetings/talks (~ 1h)
since they provide a longer context window and better summarization
quality. Otherwise, the “rolling window” summarization method can be
used to produce summaries of consistent quality on long transcripts also
with smaller models.

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

# Optionally, you can save the transcript to a file
readr::write_csv(transcript, file.path(work_dir, day, "transcript.csv"))
```

### Importation of existing transcript data

Due to imperfections in transcription processes, such as those from the
Whisper model, gaps in the transcript might occur. To address this, one
can use transcripts from other sources like WebEx, which, while more
complete, may lack the accuracy of Whisper transcripts. Additionally,
WebEx transcripts offer extra details like speaker names and chat
messages.

The provided package includes two key functions:
`extract_text_from_transcript()`, which is designed to read vtt/srt
format transcripts, and `add_chat_transcript()`, which integrates chat
messages into the existing transcript data. Currently, these functions
are compatible exclusively with WebEx outputs.

For combining transcripts from various sources, the package offers the
`merge_transcripts()` function. This function is also capable of
integrating speaker names if they are included in the transcripts. The
merging process is guided by several heuristics due to potential
misalignments between different transcripts. For instance, a segment
from the imported transcript is placed into the nearest corresponding
empty segment in the original transcript. Regarding speaker names, the
GloVe word embedding model is used to calculate a similarity score
between adjacent segments, helping to accurately assign speaker names to
the most appropriate segment.

``` r

imported_transcript <- import_transcript_from_file(
  transcript_file = "path-to-transcript-file.vtt",
)

merged_transcript <- merge_transcripts(
  transcript_x = original_transctript,
  transcript_y = imported_transcript,
)

final_transcript <- add_chat_transcript(
  transcript_data = merged_transcript,
  chat_transcript = "path-to-chat-file.txt",
  # Necessary to align the chat messages with the transcript
  # can use both HH::MM AM/PM or HH:MM:SS
  chat_start_time = "12:00 PM"
)

# Optionally, you can save the transcript to a file
readr::write_csv(final_transcript, file.path(work_dir, day, "transcript.csv"))
```

### Creation of the “Agenda” object

The next step is the creation of the “Agenda” object. This is a list of
lists, where each element represents a talk in the meeting.

Here’s an example of Agenda object with 2 talks:

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

The most important components of the agenda are the start and end time
of each talk. These needs to be manually extracted by reading the
transcript. The timings are not mandatory if the meeting consists of
only one talk. The agenda object itself is not strictly necessary, but
can be helpful for long meetings with multiple talks.

### Summarizing a single meeting transcript

The final step is summarizing the transcript. This can be done using the
`summarize_transcript()` function. The first choice is to decide which
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
  "\n- My Extra section"
)

# Finally, the user can add extra output instructions to the default ones (check
# them using get_prompts("output_summarisation") for the summarisation and
# get_prompts("output_rolling_aggregation") for the rolling aggregation). For
# example, one can add an extra output instruction with:
output_instructions <- "\n- Focus on and report quantitative details when they
are discussed."

# Get the summary

talk_summary <- summarise_transcript(
  transcript_data = transcript_text,
  method = method,
  event_description = event_description,
  recording_details = recording_details,
  audience = event_audience,
  vocabulary = vocabulary,
  
  summary_structure = summary_structure, # Or don't pass it to use the default
  extra_diarization_instructions = diarization_instructions,
  extra_output_instructions = output_instructions
  
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
  event_audience = event_audience,
  vocabulary = vocabulary,
  
  summary_structure = summary_structure,
  extra_diarization_instructions = diarization_instructions,
  extra_output_instructions = output_instructions
  
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
  summary_tree = talk_summary,
  agenda = agenda,
  output_file = "path-to-output-file.txt" # Optional file to save the summary
)

cat(formatted_summary)
```

The output will be saved at the location indicated by `output_file` if
not `NULL`.

### Workflow shortcut function

The package is built around a typical workflow that starts from a source
audio file and ends with a formatted summary file. The workflow includes
all the steps described above, from the splitting of the audio file (if
requested, necessary for the Whisper API), to the transcription of the
audio segments, to the merging of externally generated transcripts
and/or chat files, to the summarization of the transcript, to the
formatting of the summary.

The `speech_to_summary_workflow()` function implements this workflow and
can be used to perform all the steps in one go.

``` r

# initial_prompt, event_description, event_audience,
# vocabulary, diarization_instructions are defined in the previous examples

# Perform the whole audio files to formatted summary workflow. Most arguments
# are omitted since the defaults are fine but some (e.g. the output paths and
# the overwrite arguments) are shown for clarity.
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
promp ending with “\_template”) which organize the prompt general
implementation and then uses the “instruction prompt”
(e.g. “diarization_instructions” or “summarisation_sections”) to
specifically instruct the LLM.

The user is mostly expected to change the second type, while changing
the first is an expert option since it can break the prompt generation.

Use `set_promts(force = TRUE)` to restore all prompts to their default
values.
