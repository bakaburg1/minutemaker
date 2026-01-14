## General instructions

For any requested change or task which is not trivial, you should always assess the situation, test your assumptions in the console (e.g., small repros or focused checks), make tests, and then present a detailed plan of action before ANY change to the code. Run relevant unit tests after the edit (not before), unless explicitly requested otherwise.
You'll enact your plan of action after the plan has been approved by the user.

**Exception:** The planning and wait for approval process is not needed when asked to add documentation.

**⚠️ IMPORTANT: When In Doubt, Ask First**

Do not blindly interpret and enact changes if:

- The requirements or desiderata are not clear.
- There are two or more drastically different approaches to solve an issue.
- Some evidence you found would lead you to take bold or significant choices.
- You noticed something else that seems to be wrong or not working as expected while trying to perform a requested task.

**Always ask to confirm the course of action before proceeding.** When in doubt, ask.

### Structured communication and referencing

When presenting headings, plans, questions, or lists, always use ordered numbering or lettering (e.g., 1, 2, 3; a, b, c; I, II, III) and nest them hierarchically when necessary. This allows easy reference when the user replies or comments on specific points.

Examples:

- Use numbered lists for main points: 1. First point, 2. Second point, 3. Third point
- Use lettered sub-points when needed: a. Sub-point, b. Another sub-point
- Use nested numbering for hierarchical structures: 1. Main item, 1.1. Sub-item, 1.2. Another sub-item
- Apply this to all structured content: plans of action, questions, lists of options, step-by-step instructions, etc.

### Agent skills and guidelines

Read and follow these guidelines when appropriate:
- Writing R code: <https://raw.githubusercontent.com/bakaburg1/my-ai-skills/main/R/R-rules.md>
- Writing R unit tests: <https://raw.githubusercontent.com/bakaburg1/my-ai-skills/main/R/unit-testing.md>
- Writing and performing git commits: <https://raw.githubusercontent.com/bakaburg1/llm-commands/refs/heads/main/commit.md>
- Assess and apply code review recommendations: <https://raw.githubusercontent.com/bakaburg1/llm-commands/refs/heads/main/verify.md>
- Write git Pull Request descriptions: <https://raw.githubusercontent.com/bakaburg1/llm-commands/refs/heads/main/pr.md>

Do not read these guidelines if not relevant to the task at hand.

### Track learning points

Update progressively this document when you learn something about how to better perform your tasks related to this project. This could be coding best practices, implementation details, overall design decisions, etc. and especially, corrections and remarks from the user.

Any learning point that refers to functions, datasets, or concepts of this package is **project-specific** by definition and must be recorded under `current_project`, not `general`.

Update the list below with the new learning points, filing each memory into the correct group:

```yaml
current_project:
  - name: agenda inference start times validation
    description: Warn and skip when LLM agenda start_times are empty; abort when non-numeric or coercion introduces NA values.
    scope: agenda inference
  - name: external transcript overwrite control
    description: use_transcript_input skips writing when existing JSON segments are present unless overwrite is TRUE, and workflow passes overwrite_transcription_files through.
    scope: transcript import/workflow
  - name: agenda cleaning for empty slices
    description: clean_agenda() drops or aborts agenda items with empty transcript slices to keep agenda and summary tree aligned.
    scope: agenda/workflow
  - name: meeting summary template location
    description: Store the meeting summary generation template under `inst/templates/meeting_summary_template.R` so users can copy it via `system.file()`.
    scope: documentation
  - name: readme generation workflow
    description: Update only `README.Rmd` and regenerate `README.md` via `devtools::build_readme()`.
    scope: documentation
  - name: context generation cache
    description: generate_context caches per-field context in `context/`, uses `minutemaker_context_material_dir` and `minutemaker_overwrite_context` options, and existing files override manual inputs with info alerts.
    scope: context generation
  - name: context generation strategy control
    description: generate_context uses only `strategy` parameter ("one_pass" or "agentic") to control generation method. The legacy `mode` parameter (single/per_field) was removed; "one_pass" now always uses single-call JSON generation.
    scope: context generation
  - name: helper usage preference
    description: Avoid trivial global helpers; keep globals only for logic that needs tests or reuse, and add preamble comments to in-body helpers.
    scope: code structure
general:
  - name: cli alert level conventions
    description: Use cli_alert for action logs, cli_alert_info for supplemental details, cli_warn for runtime logical issues that would have used warning(), and cli_alert_warning for non-code cautions when results need careful interpretation (e.g., low-quality input, incomplete data).
    scope: logging/messages
  - name: cli alert capture
    description: cli_alert* emits messages that expect_message() can capture in tests; expect_warning() will not.
    scope: testing
  - name: roxygen generation only
    description: Never write .Rd or NAMESPACE manually; always run devtools::document() to update documentation and exports.
    scope: documentation
  - name: git write confirmation
    description: Always ask for explicit user confirmation before performing any write operation to the git repository, such as commit, push, or other actions that modify git history.
    scope: git operations
  - name: dependency additions via usethis
    description: Always add packages with usethis::use_package using min_version = TRUE when updating dependencies.
    scope: dependencies
  - name: cli alert bullet handling
    description: cli_alert* functions do not support named bullet vectors; emit one cli_alert* call per bullet message instead of passing a named vector.
    scope: logging/messages
  - name: devtools test filter usage
    description: Run tests with Rscript -e 'devtools::test(filter = \"...\")' (no testthat::test_file); use devtools::load_all() only for small console repros, not test runs.
    scope: testing
  - name: base r null coalesce operator
    description: `%||%` is available in base R in this project environment; prefer base `%||%` where appropriate instead of importing from rlang.
    scope: language/base
  - name: missing argument detection with defaults
    description: `rlang::is_missing()` returns FALSE for arguments that were not supplied but have defaults; use base `missing()` when you need to detect that case.
    scope: language/base
  - name: renv autoloading in script runs
    description: Set `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` when running `Rscript` to avoid renv autoload/lock issues during local checks.
    scope: tooling/renv
  - name: rscript dollar expansion
    description: When running `Rscript -e` in the shell, escape `$` or use single quotes otherwise shell expansion will transform e.g. `Rscript -e "a <- list(); a$b <- 3"` into `a <- list(); a <- 3`.
    scope: shell
```

### Tested assumptions

Record every time an assumption was tested and found to be wrong, to avoid
repeating the same errors or beliefs. If you are unsure which group applies,
ask the user before recording the memory.

Any tested assumption that refers to functions, datasets, or concepts of this package is **project-specific** by definition and must be recorded under `current_project`, not `general`.

```yaml
current_project: []
general: []
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
