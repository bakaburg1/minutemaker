## General instructions

For any requested change or task which is not trivial, you should always assess the situation, make tests and then present a detailed plan of action before ANY change to the code.
You'll enact your plan of action after the plan has been approved by the user.

**Exception:** The planning and wait for approval process is not needed when asked to add documentation.

**⚠️ IMPORTANT: When In Doubt, Ask First**

Do not blindly interpret and enact changes if:

- The requirements or desiderata are not clear.
- There are two or more drastically different approaches to solve an issue.
- Some evidence you found would lead you to take bold or significant choices.
- You noticed something else that seems to be wrong or not working as expected while trying to perform a requested task.

**Always ask to confirm the course of action before proceeding.** When in doubt, ask.

### R guidelines

Read and follow these guidelines:
https://raw.githubusercontent.com/bakaburg1/my-ai-skills/main/R/R-rules.md
https://raw.githubusercontent.com/bakaburg1/my-ai-skills/main/R/unit-testing.md

### Track learning points

Update progessively this document when you learn something about how to better perform your tasks related to this project. This could be coding best practices, implementation details, overall design decisions, corrections and remarks from the user,etc.

Update the list below with the new learning points:

```yaml
- name: cli alert level conventions
  description: Use cli_alert for action logs, cli_alert_info for supplemental details, cli_warn for runtime logical issues that would have used warning(), and cli_alert_warning for non-code cautions when results need careful interpretation (e.g., low-quality input, incomplete data).
  scope: logging/messages
- name: agenda inference start times validation
  description: Warn and skip when LLM agenda start_times are empty; abort when non-numeric or coercion introduces NA values.
  scope: agenda inference
- name: external transcript overwrite control
  description: use_transcript_input skips writing when existing JSON segments are present unless overwrite is TRUE, and workflow passes overwrite_transcription_files through.
  scope: transcript import/workflow
- name: cli alert capture
  description: cli_alert* emits messages that expect_message() can capture in tests; expect_warning() will not.
  scope: testing
- name: agenda cleaning for empty slices
  description: clean_agenda() drops or aborts agenda items with empty transcript slices to keep agenda and summary tree aligned.
  scope: agenda/workflow
- name: roxygen generation only
  description: Never write .Rd or NAMESPACE manually; always run devtools::document() to update documentation and exports.
  scope: documentation
- name: git write confirmation
  description: Always ask for explicit user confirmation before performing any write operation to the git repository, such as commit, push, or other actions that modify git history.
  scope: git operations
- name: dependency additions via usethis
  description: Always add packages with usethis::use_package using min_version = TRUE when updating dependencies.
  scope: dependencies
```

### Dependency Management (`renv`)

`renv` is turned off by default in this repository. The repo dependency status
(i.e. the `renv.lock` file) is brought up to date only when stable versions are
reached.

## Project Description

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

We suggest to use >GPT-4 grade models for long meetings/talks (~ 1h)
since they provide a longer context window and better summarization
quality. Otherwise, the "rolling window" summarization method can be
used to produce summaries of consistent quality on long transcripts also
with smaller models.
