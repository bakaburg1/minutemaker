
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
  minutemaker_openai_api_key = "***",
  
  minutemaker_openai_model_gpt = "gpt-4"
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

# Save the transcript to a file
readr::write_csv(transcript, file.path(work_dir, day, "transcript.csv"))
```

### Creation of the “Agenda” object

The next step is creating the “Agenda” object. This is a list of lists,
where each element represents a talk in the meeting.

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
  meeting_description <- "***Title of the Conference***
    
    Description of the conference."
  
  ## A vocabulary of terms to keep in mind when summarizing the transcript. Can
  ## be useful also to make the summarization robust to transcription errors.
  vocabulary <- c("Term1", "ACRONYM: description", "Jack Black", "etc")
  
  ## Specific talk details. Can be built using `generate_recording_details()`
  ## from an agenda element:
  
  meeting_details <- generate_recording_details(
    agenda[[1]],
  )
  
  ## or manually:
  
  meeting_details <- generate_recording_details(
    session = "Session 1",
    title = "Opening",
    type = "Conference introduction",
    speakers = c("Speaker 1", "Speaker 2"),
  )
  
  ## The audience of the meeting/conference, which helps the summarisation to
  ## focus on specific topics and helps setting the tone of the summary.
  
  meeting_audience <- "An audience with some expertise in the topic
    of the conference and with a particular interest in this and that."

# Get the summary

talk_summary <- summarise_transcript(
  transcript = transcript_text,
  event_description = meeting_description,
  event_details = meeting_details,
  vocabulary = vocabulary,
  audience = meeting_audience,
  
  provider = "my-LLM-provider-of-choice",
)
```

Use `cat(talk_summary)` to print the summary to the console.

### Summarizing multiple meeting transcripts

If your transcript covers multiple talks, you can use the
`summarise_full_meeting()` function to generate a summary for each talk
in the transcript.

``` r

conference_summaries <- summarise_full_meeting(
  transcript_data = transcript_data,
  agenda = agenda,
  meeting_description = meeting_description,
  meeting_audience = meeting_audience,
  vocabulary = vocabulary,
  output_file = "path-to-output-file.R",
  overwrite = TRUE,
  
  provider = "my-LLM-provider-of-choice",
)
```

The output of the `summarise_full_meeting()` function is a
`summary_tree`, i.e., a list containing the summary and some other
information about each talk.

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

### Changing the default prompts

The summarisation process is built upon a number of “technical prompt”
under the hood.

You can use the default prompts or define your own through a number of
options.

You can see the current prompts by running:

``` r

prompts <- get_prompts()

# Print one of the prompts, cat() improves the formatting
cat(prompts[[1]])
```

The package exposes the function `set_prompts()` function to edit the
prompts.

Be careful, changing the prompt will change the output drastically. The
prompts use the `glue` package syntax to automatically insert some
relevant variables in the prompts, which are needed for many of the
package feature to work. `set_promts(force = TRUE)` can be used to
restore all prompts to their default values.
