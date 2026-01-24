## General instructions

As the first step in a fresh thread, read `README.md` to understand the project.

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

When asking questions, **always state the best approach you recommend and why**.

### Structured communication and referencing

When presenting headings, plans, questions, or lists, always use ordered numbering or lettering (e.g., 1, 2, 3; a, b, c; I, II, III) and nest them hierarchically when necessary. This allows easy reference when the user replies or comments on specific points.

Examples:

- Use numbered lists for main points: 1. First point, 2. Second point, 3. Third point
- Use lettered sub-points when needed: a. Sub-point, b. Another sub-point
- Use nested numbering for hierarchical structures: 1. Main item, 1.1. Sub-item, 1.2. Another sub-item
- Apply this to all structured content: plans of action, questions, lists of options, step-by-step instructions, etc.

### ACC memory usage

1. Use the `acc-memory-control` skill in READ mode at the beginning of a discussion when you judge the task as medium-high complexity and likely to benefit from checking ACC memory (e.g. multi-step refactors, workflow changes, behavior changes, or multi-step debugging).
2. Use the ACC skill in UPDATE/WRITE mode when important changes or milestones are reached (e.g. refactoring a function, adding/removing a function, or when the user corrects the model).
3. Avoid calling the ACC skill for small refactors, style-only changes, or small documentation updates (especially inline edits).

### Package discovery

At the beginning, and only if you think you'll need to write code in the package, follow the following steps: (1) list files in `R/`, (2) list utility function signatures using the `list-r-functions` skill and call `list_r_functions()`. Inspect other R files on demand using `list_r_functions("regex")`.

To inspect a function body in this repo, load the package with `devtools::load_all()` and run `print(function_name)`; the function code will print to the console so reading the file is not required.

Avoid following these instructions if you think you'll only be just writing documentation, do small edits, or support with other chore tasks unrelated to writing code in the package.

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
    description: generate_context caches per-field context in `context/`, uses `minutemaker.context_material_dir` and `minutemaker.overwrite_context` options, and existing files override manual inputs with info alerts.
    scope: context generation
  - name: context generation strategy control
    description: generate_context uses only `strategy` parameter ("one_pass" or "agentic") to control generation method. The legacy `mode` parameter (single/per_field) was removed; "one_pass" now always uses single-call JSON generation.
    scope: context generation
  - name: helper usage preference
    description: Avoid trivial global helpers; keep globals only for logic that needs tests or reuse, and add preamble comments to in-body helpers.
    scope: code structure
  - name: path validation with check_path
    description: Use check_path() for consistent validation of paths intended for creating/writing files or directories. It trims whitespace and returns the trimmed path on success; rejects NULL/NA/empty strings; validates against reserved names and dangerous characters; and (for relative paths) tests whether the directory structure can be created in a temp location. Use stop_on_error = TRUE to abort on failure and stop_on_error = FALSE to warn and return FALSE.
    scope: validation
  - name: existence checks with path_exists
    description: Use path_exists() for existence checks only (wraps check_path() then fs::file_exists()/dir_exists()). It returns the trimmed path when it exists with the requested type, otherwise returns FALSE (or aborts if stop_on_error = TRUE). Default type is "file"; pass type = "dir" or type = "any" only when needed.
    scope: validation
  - name: fail_msg contract for path validators
    description: check_path() and path_exists() accept fail_msg to centralize messaging. If missing, default in-function messages are used. If a string or cli-style bullet vector, it is used for cli_abort/cli_warn and can interpolate `{`_path`}` (full path) and `{`_basename`}` (basename). If fail_msg is FALSE or NA, messaging is suppressed and the function returns FALSE when stop_on_error = FALSE; when stop_on_error = TRUE, fail_msg = FALSE/NA is treated as missing (defaults are used). Do not use a separate silent argument.
    scope: validation
  - name: avoid is_scalar_character for path checks
    description: Avoid rlang::is_scalar_character() for path validation because it can allow NA values; prefer check_path()/path_exists() to handle NULL/NA/whitespace consistently.
    scope: validation
  - name: transcript intermediate files directory option
    description: Use the `minutemaker.transcription_output_dir` option as the default location for transcript intermediate files across workflow, STT, and external transcript import; allow overrides via arguments and validate/create with create_dir().
    scope: transcript/workflow
  - name: file path utilities centralize validation
    description: Avoid ad hoc string checks for file paths; rely on `check_path()`, `path_exists()`, and `create_dir()` which centralize validation and messaging. Consult their documentation for how to raise file errors via `fail_msg`, `stop_on_error`, and related rules.
    scope: validation
  - name: working directory path semantics
    description: Remove `target_dir` from workflows; relative paths are resolved from the working directory, and users should set it with `setwd()` or `withr::with_dir()`.
    scope: workflow/paths
general:
  - name: cli alert level conventions
    description: Use cli_alert for action logs, cli_alert_info for supplemental details, cli_warn for runtime logical issues that would have used warning(), and cli_alert_warning for non-code cautions when results need careful interpretation (e.g., low-quality input, incomplete data).
    scope: logging/messages
  - name: test comment dash style
    description: Section comment headers in code and test files should use four dashes (e.g., `# Tests for foo() ----`), not three. Otherwise IDEs will not recognize them as sections.
    scope: code/documentation
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
    description: Rerun with `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` when running `Rscript` if you get renv autoload/lock issues during local checks.
    scope: tooling/renv
  - name: rscript dollar expansion
    description: When running `Rscript -e` in the shell, escape `$` or use single quotes otherwise shell expansion will transform e.g. `Rscript -e "a <- list(); a$b <- 3"` into `a <- list(); a <- 3`.
    scope: shell
  - name: roxygen backticks for inline code
    description: Use backticks (`code`) instead of \code{code} for inline code formatting in Roxygen documentation to follow modern R documentation conventions.
    scope: documentation
```

### Tested assumptions

Record every time an assumption was tested and found to be wrong, to avoid
repeating the same errors or beliefs. If you are unsure which group applies,
ask the user before recording the memory.

Any tested assumption that refers to functions, datasets, or concepts of this package is **project-specific** by definition and must be recorded under `current_project`, not `general`.

```yaml
current_project: []
general:
  - name: rlang is_scalar export
    description: `rlang::is_scalar()` is not exported; use a length check or another exported helper instead.
    scope: language/base
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
