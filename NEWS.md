# minutemaker 0.15.0

**Transcript-first workflow and external transcript support**

## Enhancements
- External transcript-first workflow with DOCX and VTT transcripts support and transcript standardization via `use_transcript_input`, plus automatic STT bypass and term correction pipeline processing for external transcripts. (Commits: 113cbdf, 92c13d4)
- External transcript segmentation with adjustable chunk sizes and overwrite controls to keep LLM prompts safe. (Commits: b52c8af, dd2ad7a, 782ac73)
- Agenda quality improvements with start-time validation and `clean_agenda()` integration to drop empty slices before summarization. (Commits: bc4efd6, a177de3, 50013e0, 83f8248)
- Audio processing hardening with stronger input validation and structured failure handling for extraction/splitting. (Commits: 30e8dbd, 9bf0ea3)
- Standardized CLI alert levels across the package for clearer user messaging. (Commit: d727e19)

## Fixes
- Corrected reasoning parameter selection for transcript correction to avoid invalid API combinations. (Commit: 9491670)
- Improved transcript import robustness and timing normalization for Webex/Teams segments. (Commits: f6aa022, 8390a5e)
- Tightened agenda and summarization validation paths to avoid empty/invalid segment handling failures. (Commits: 7c13a4f, cd44f94)

## Documentation
- README and workflow docs updated for transcript-first usage, platform support, and chat file patterns. (Commits: 1eecd7f, 31272a, 0e7b023)
- New and updated Rd docs for external transcript helpers and agenda-time warnings. (Commits: 113cbdf, b7037aa)

## Summary
This pull request expands minutemaker with transcript-first workflows (including DOCX support), safer external transcript chunking, and stronger agenda/audio validation, while refining correction logic and documentation in preparation for v0.15.0. (Commit: 72f5a3f)

Docs marched in, the agenda trimmed,
Chunks behaved, prompts all primed.
DOCX danced, the logs now chirped,
Errors fled and warnings burped.
Minutes brewed with extra grace,
The bot now wins the meeting race.

# minutemaker 0.14.0

## Enhancements

- **LLM-Based Transcript Correction**: Added `apply_llm_correction()` with retry logic, term guidance, and correction mapping, and wired it into `speech_to_summary_workflow` so transcripts can be auto-corrected post-STT. (Commit: [8799708](https://github.com/bakaburg1/minutemaker/commit/8799708))
- **Audio Processing & STT Backends**: Extracted audio splitting utilities with corruption checks and optional parallel processing, plus added Parakeet MLX CLI as a new speech-to-text backend alongside Whisper options. (Commits: [d7cf3d8](https://github.com/bakaburg1/minutemaker/commit/d7cf3d8), [bcdff38](https://github.com/bakaburg1/minutemaker/commit/bcdff38))
- **Workflow Messaging & Refactor**: Improved `speech_to_summary_workflow` logging and refactored helper code for clarity. (Commit: [936cfba](https://github.com/bakaburg1/minutemaker/commit/936cfba))
- **Summarization Prompt Refresh**: Renamed the `summary_template` argument to `summarisation_template`, passed transcripts/vocabulary into template assembly, and defaulted reasoning settings to keep prompt rendering consistent. (Commits: [9c8d5eb](https://github.com/bakaburg1/minutemaker/commit/9c8d5eb), [0eb4b99](https://github.com/bakaburg1/minutemaker/commit/0eb4b99), [c4ec9f5](https://github.com/bakaburg1/minutemaker/commit/c4ec9f5), [4f7f775](https://github.com/bakaburg1/minutemaker/commit/4f7f775))
- **Extensive Test Coverage**: Added comprehensive tests across audio processing, prompts, STT, summarization, transcript import/manipulation, validation, and workflow paths. (Commits: [5c30d92](https://github.com/bakaburg1/minutemaker/commit/5c30d92), [137ff50](https://github.com/bakaburg1/minutemaker/commit/137ff50))

## Fixes

- **Transcript Import & Diarization Robustness**: Improved speaker extraction for VTT/MS Teams files, merging logic, diarization handling, and now skip malformed cues while preserving valid ones, safely interleave chat logs, and retain speaker types when merging transcripts. (Commits: [44bac1f](https://github.com/bakaburg1/minutemaker/commit/44bac1f), [65519a3](https://github.com/bakaburg1/minutemaker/commit/65519a3), [54b4e21](https://github.com/bakaburg1/minutemaker/commit/54b4e21), [ad5ec01](https://github.com/bakaburg1/minutemaker/commit/ad5ec01), [980303f](https://github.com/bakaburg1/minutemaker/commit/980303f))
- **Agenda & Time Validation Hardening**: Strengthened agenda/time validation and NA handling, centralized file loading with `load_serialized()` for clearer CLI errors, rejected empty or malformed agendas/summary trees, honored time subclasses, and fixed agenda time conversions. (Commits: [42a87b8](https://github.com/bakaburg1/minutemaker/commit/42a87b8), [50ba892](https://github.com/bakaburg1/minutemaker/commit/50ba892), [10ec38f](https://github.com/bakaburg1/minutemaker/commit/10ec38f), [1ea9ae8](https://github.com/bakaburg1/minutemaker/commit/1ea9ae8), [127c6d0](https://github.com/bakaburg1/minutemaker/commit/127c6d0), [c6ace28](https://github.com/bakaburg1/minutemaker/commit/c6ace28), [6a921b4](https://github.com/bakaburg1/minutemaker/commit/6a921b4))
- **Summarization Stability**: Resolved prompt defaults and workflow ordering issues that could trigger errors when summarizing without prompts set, ensured transcript text is available for template glue, and clarified zero-result errors. (Commits: [81236e1](https://github.com/bakaburg1/minutemaker/commit/81236e1), [4f7f775](https://github.com/bakaburg1/minutemaker/commit/4f7f775), [a800c8f](https://github.com/bakaburg1/minutemaker/commit/a800c8f))
- **Correction Resilience**: Added retry/error guards around LLM correction calls to reduce transient failures and now flag failed correction attempts consistently. (Commits: [972a1c7](https://github.com/bakaburg1/minutemaker/commit/972a1c7), [412ac40](https://github.com/bakaburg1/minutemaker/commit/412ac40))
- **Workflow Defaults & Error Surfacing**: Initialize prompts on package load, surface agenda load errors with CLI context, and tighten workflow input validation and boolean handling. (Commits: [fe2b8c3](https://github.com/bakaburg1/minutemaker/commit/fe2b8c3), [f503da6](https://github.com/bakaburg1/minutemaker/commit/f503da6), [9daba38](https://github.com/bakaburg1/minutemaker/commit/9daba38))
- **Audio & STT Reliability**: Hardened `split_audio` retries/parallel handling while respecting external `mirai` daemons, normalized `m4a` output naming and CLI argument quoting, allowed recursive STT output directory creation, and added retries for OpenAI Whisper errors. (Commits: [7a6275c](https://github.com/bakaburg1/minutemaker/commit/7a6275c), [da1d13c](https://github.com/bakaburg1/minutemaker/commit/da1d13c), [0c94f7b](https://github.com/bakaburg1/minutemaker/commit/0c94f7b), [0629c2d](https://github.com/bakaburg1/minutemaker/commit/0629c2d), [2b4a76f](https://github.com/bakaburg1/minutemaker/commit/2b4a76f), [329ad39](https://github.com/bakaburg1/minutemaker/commit/329ad39), [c397463](https://github.com/bakaburg1/minutemaker/commit/c397463))

## Documentation

- **Transcript Correction & STT Docs**: Updated README and man pages to cover transcript correction flow, agenda-aware summarization examples, and Parakeet MLX usage. (Commits: [187596d](https://github.com/bakaburg1/minutemaker/commit/187596d), [a75e5f4](https://github.com/bakaburg1/minutemaker/commit/a75e5f4))
- **Testing Guidelines**: Added unit-testing guidance to standardize warning/error handling. (Commit: [c944a0f](https://github.com/bakaburg1/minutemaker/commit/c944a0f))

## Dependencies

- **Dependency Refresh**: Bumped `llmR` to 1.4.1, added `cli` import, updated `reticulate`/`testthat` versions, and refreshed `renv.lock`. (Commits: [ed96a5a](https://github.com/bakaburg1/minutemaker/commit/ed96a5a), [1fdfba6](https://github.com/bakaburg1/minutemaker/commit/1fdfba6))

## Internal

Non-user facing: introduced shared `load_serialized()` usage, expanded audio/prompt/agenda test scaffolding and style guidelines, refreshed `renv` autoloader, and tidied gitignore/build metadata. (Commits: [2bb3a13](https://github.com/bakaburg1/minutemaker/commit/2bb3a13), [c5937c9](https://github.com/bakaburg1/minutemaker/commit/c5937c9), [5400342](https://github.com/bakaburg1/minutemaker/commit/5400342), [8e3d4de](https://github.com/bakaburg1/minutemaker/commit/8e3d4de))

## Summary

This release delivers a full LLM-based transcript correction pipeline, refreshed summarization prompts/templates, safer agenda/summary loading, sturdier audio/STT flows, and broader validation, alongside substantial test and tooling upkeep.

# minutemaker 0.13.0

## Enhancements

- **Support for Local Whisper Models**: Added functions `use_whisper_local_stt` and `use_mlx_whisper_local_stt` to support local Whisper models via Python with reticulate, with the second being optimized for Mac OS with Apple Silicon (Commit: [69e4f5e](https://github.com/bakaburg1/minutemaker/commit/69e4f5e59518da51d7f757a5076511d4224c6d65)).
- **Integration with llmR**: Refactored the code to rely on the `llmR` package for LLM interactions, removing redundant LLM-related functions (Commit: [2331b46](https://github.com/bakaburg1/minutemaker/commit/2331b463e0606cd4ee49ecb353b89b163da06d9e)).
- **Enhanced Speech-to-Text Workflow**: Updated `perform_speech_to_text` to use `whisper_local` as the default model and enhanced `speech_to_summary_workflow` to display the selected speech-to-text model (Commit: [69e4f5e](https://github.com/bakaburg1/minutemaker/commit/69e4f5e59518da51d7f757a5076511d4224c6d65)).
- **Duplicate Agenda Item Title Resolution**: Added functionality to `infer_agenda_from_transcript` to detect and resolve duplicate agenda item titles using LLM-generated suggestions (Commit: [22ed720](https://github.com/bakaburg1/minutemaker/commit/22ed720bb3e8ad3a7e257b55411ed7b6c45db67d)).

## Fixes

- **Dependency Management**: Replaced custom dependency check function with `rlang::check_installed` for better package management (Commit: [3227b0d](https://github.com/bakaburg1/minutemaker/commit/3227b0d7dba8785949c1d66c83d232bb38438c08)).
- **Entity Extraction and Prompt Generation**: Enhanced `entity_extractor` function prompt construction and modified text concatenation for more precise entity extraction tasks (Commit: [5517a28](https://github.com/bakaburg1/minutemaker/commit/5517a28118d0ad497092b8f3e4938946917408f2)).
- **Audio File Pattern Matching**: Improved audio file pattern matching to support more file extensions and removed extra whitespace in file name cleaning (Commit: [e03a0dc](https://github.com/bakaburg1/minutemaker/commit/e03a0dccd8a79ba1435ac9ae8a591786df08ae4c)).

## Documentation

- **Updated README**: Revised README to describe the use of `llmR` for summarization and the addition of new local models for speech-to-text (Commit: [8bff883](https://github.com/bakaburg1/minutemaker/commit/8bff88380c5dc977a52c2207f1ef380904784bf4)).
- **Version Bump**: Updated version to 0.12.0 and added relevant changes to NEWS.md (Commit: [e6a10e6](https://github.com/bakaburg1/minutemaker/commit/e6a10e6b2203cc4a86c3cd43c85f91f50658fb2b)).

## Summary

This pull request introduces significant enhancements to the `minutemaker` package by adding support for local Whisper models, integrating the `llmR` package for LLM interactions, and improving the speech-to-text workflow. Additionally, it fixes dependency management issues and updates the documentation to reflect these changes.

# minutemaker 0.12.0

## Enhancements

- **Support for Local Whisper Models**: Added functions `use_whisper_local_stt` and `use_mlx_whisper_local_stt` to support local Whisper models via Python with reticulate, with the second being optimized for Mac OS with Apple Silicon (Commit: [69e4f5e](https://github.com/bakaburg1/minutemaker/commit/69e4f5e59518da51d7f757a5076511d4224c6d65)).
- **Integration with llmR**: Refactored the code to rely on the `llmR` package for LLM interactions, removing redundant LLM-related functions (Commit: [2331b46](https://github.com/bakaburg1/minutemaker/commit/2331b463e0606cd4ee49ecb353b89b163da06d9e)).
- **Enhanced Speech-to-Text Workflow**: Updated `perform_speech_to_text` to use `whisper_local` as the default model and enhanced `speech_to_summary_workflow` to display the selected speech-to-text model (Commit: [69e4f5e](https://github.com/bakaburg1/minutemaker/commit/69e4f5e59518da51d7f757a5076511d4224c6d65)).

## Fixes

- **Dependency Management**: Replaced custom dependency check function with `rlang::check_installed` for better package management (Commit: [3227b0d](https://github.com/bakaburg1/minutemaker/commit/3227b0d7dba8785949c1d66c83d232bb38438c08)).

## Documentation

- **Updated README**: Revised README to describe the use of `llmR` for summarization and the addition of new local models for speech-to-text (Commit: [8bff883](https://github.com/bakaburg1/minutemaker/commit/8bff88380c5dc977a52c2207f1ef380904784bf4)).

## Summary

This pull request introduces significant enhancements to the `minutemaker` package by adding support for local Whisper models, integrating the `llmR` package for LLM interactions, and improving the speech-to-text workflow. Additionally, it fixes dependency management issues and updates the documentation to reflect these changes.

# minutemaker 0.10.0

## Enhancements

- **Change interrogate_llm to prompt_llm**: Renamed the `interrogate_llm` function to `prompt_llm` to better reflect its purpose of generating prompts for language models (Commit: [2f9eeddd]).
- **Model Logging for LLM Requests**: Added a `log_request` parameter to `use_openai_llm`, `use_azure_llm`, and `use_custom_llm` functions to log the specific endpoint and model being used (Commit: [7e85f2f]).
- **Handling Long LLM Responses**: Improved the handling of LLM responses that exceed the output token limit. Users are now prompted to decide how to proceed, and incomplete answers are saved to a file for reference (Commit: [18cfada]).
- **Model Parameter for Custom LLM APIs**: Added a `model` parameter to LLM calls to allow specifying a model for custom APIs with multiple models (Commit: [cd4227b]).

## Fixes

- **Restore Correct Speaker Extraction for Webex VTT**: Fixed the parsing of Webex VTT files which was broken by the implementation of MS Teams VTT parsing (Commit: [d189980]).
- **Remove Newlines from JSON Output**: Fixed an issue where some custom LLMs produced invalid JSON with newlines, causing errors during parsing (Commit: [e9e578a]).
- **Support JSON Mode for Custom LLMs**: Ensured that most custom LLMs now support JSON mode by keeping the JSON mode option in the call (Commit: [f4df24c]).

## Documentation

- **Improve Code Comments and Error Handling**: Enhanced code comments and error handling for better clarity and maintenance (Commit: [4b689ff]).

## Summary

This pull request introduces several enhancements to the LLM handling, including logging the model being used, better management of long responses, and support for specifying models in custom APIs. It also includes fixes for speaker extraction in Webex VTT files and handling of JSON outputs from custom LLMs. Additionally, code comments and error handling have been improved for better clarity.

# minutemaker 0.9.0

### Improve agenda review and add custom LLM support

## Enhancements

- Improve user interaction for agenda review: the `speech_to_summary_workflow` function now displays the generated agenda in the console, facilitating review and reducing the need to locate the agenda file separately. (Commit: 3bed1cc).
- Add support for custom language model endpoints: replace `use_local_llm()` with `use_custom_llm()` to send requests to custom (local or remote) language model endpoints compatible with the OpenAI API specification, allowing to use also API Keys. (Commit: 0fdfe57).
- Dynamic output_length parameter as default: dynamically set `summarization_output_length` in the workflow function based on whether a multipart summary is generated (shorter outputs) or not (longer outputs), optimizing the readability and relevance of summaries. (Commit: 2511287).

## Fixes

- Fix output_length usage: `output_length` was not being considered in the summarization process. (Commit: 08e95d1).
- Fix agenda file validation: update the validation logic for the 'agenda' argument in the `speech_to_summary_workflow` function to handle character type inputs correctly and provide clearer error messages. (Commit: d200a55).
- Fix agenda validation: add checks for empty agenda elements and missing required items, improve error messages for invalid time formats, and update tests. (Commit: 6d870ee).

## Documentation

- Fix messages typos: correct typos in messages. (Commit: 0fdfe57).

## Summary

This pull request focuses on improving the user experience and adding support for custom language model endpoints. It enhances the agenda review process, ensures dynamic output length adjustment, fixes agenda validation, and adds documentation typo corrections.

# minutemaker 0.8.0

### Enhanced Agenda Management and Utilization

## Enhancements:

- Added a new `multipart_summary` argument in `speech_to_summary_workflow()` to allow users to choose between summarizing each agenda item separately (the previous approach, now the default) or as a single summary just using the agenda to focus the model, offering greater flexibility in the summarization process (Commit: 99168d4168e6394e1789d7ae0dadb1e3b37b006d).
- Introduced `format_agenda()` function to convert machine-readable agendas into human-readable text, improving the usability of agenda-driven summarization (Commit: 0d27980f21ea3ab1df2ed289df421b7034b2fd22).
- Added `validate_agenda()` function to ensure the validity of agenda structures before summarization, enhancing the reliability of the summarization process (Commit: 5e943af522529edea6166ab62da41cbfa7a08ebb).
- Added the ability for users to proceed with the summarization workflow after agenda generation without re-running the entire workflow function, streamlining the user experience (Commit: 8056ed6763368347efa4c1f79ae9497ca7ff1597).
- Changed the summarization workflow logic to not ask whether the user wants to overwrite the summarization output if `overwrite_formatted_output` is `FALSE` (Commit: 99168d4168e6394e1789d7ae0dadb1e3b37b006d).
- Implemented global configuration for the language model (LLM) provider via `getOption("minutemaker_llm_provider")`, allowing for more flexible and centralized LLM provider management (Commit: 159335d2d413462c93f5dd3f60c1919e1b2f8918).
- Updated `interrogate_llm()` to retrieve the LLM provider setting from global options, providing a more dynamic and user-friendly approach to specifying the LLM provider (Commit: 15723d6f8c03c648deb00b1705837eadef04b609).

## Fixes:

- Addressed an issue where the summarization process could fail due to invalid agendas by implementing the `validate_agenda()` function (Commit: 6bdabadbb3bbee66e20907f5f7dec4330842ed39).

# minutemaker 0.7.0

### Manage events without agendas in the summarisation workflow

This pull request includes a series of enhancements and fixes that improve the transcript summarization workflow, add new functionality for entity extraction, and ensure better support for various transcript formats. The changes also address code quality and documentation to provide a more robust and user-friendly experience.

## Breaking:

- Replaced `event_audience` with `audience` as argument name for consistency across the framework. Before, some functions used the first and some the second term (Commit: 644fb2982f8d83420736382c75a89ee231464eef).

## Enhancements:

- **Workflow Enhancement**: Added support for summarizing meetings without an agenda in the workflow. Before, the full workflow function was designed to only work with long meetings organized in sub-talks described by an agenda. (Commit: 644fb2982f8d83420736382c75a89ee231464eef).
- **Entity Extraction Functionality**: Introduced the `entity_extractor` function to identify and extract entities such as people, acronyms, organizations, and concepts from a text, which can be particularly useful for building vocabularies for LLMs from event descriptions or transcripts (Commit: ae4fc3cebf025f0331ffd6eeb82e3be47d4cf3af).
- **Agenda Management**: Added the ability to manage deviations from the expected agenda, allowing the LLM to add talks not described in the expected agenda, enhancing the flexibility of the summarization process (Commit: 40f7620a43684ace41b0aa44e20c8ed1dc8eab00).
- **Support for MS Teams VTT Files**: Implemented support for importing transcripts from MS Teams VTT files, which do not follow the standard VTT specification (Commit: cfa96733d86879ca4977c65a8d8b58eace108af2).
- **Output Quality Improvements**: Utilized the `styler` package to enhance the readability of generated agendas and unformatted summary outputs, contributing to better readability and user experience (Commit: 194b8c8c45bf09e1f8f3cabec6c6d362ee950f0f).

## Fixes:

- **Agenda Generation Bug**: Resolved an issue where the agenda generation was creating infinite unnamed speaker lists, exhausting the context window (Commit: bfc5597bd453518960b6208268725ee0a3157dba).

## Dependencies:

- **Styler Package Addition**: Added the `styler` package and its dependencies to the project, which is used to improve the formatting of the generated outputs (Commit: e88a6bdd76ff0fa51902894d820ff9461addedb9).

# minutemaker 0.6.0

## Major Enhancements:

- Introduced the `infer_agenda_from_transcript` function to automate the generation of an event agenda by analyzing a given transcript, aiming to identify and extract key segments and construct a structured overview of the event's proceedings. This enhancement significantly streamlines the workflow for summarizing meetings and conferences. (Commit: c458b0d9f9ebe7b20ad1775c44beb69712cfa933)

## Minor Improvements and Fixes:

- Enhanced error handling for transcription processes, including managing empty transcription JSON files and transcription files with no speaker information. (Commits: 3c4e877f5d953abd93c54f8e6ae04d94f51843ba, 41b823add34d18f0feeaed54d45a00a601a9b8e0)
- Improved the summarization process by adding checks to handle cases where a transcript subset for a talk is empty and ensuring the final result tree is not empty. (Commit: b66b912cd0c7e802c4b9b33e2d9f279e64db172d)
- Addressed various minor issues, including dependency installation, handling of integers as agenda times, and managing fatal Whisper API errors. (Commits: b1daf88cb68969c1bfb2c14cbfed2736306150ec, 4a2d159575f5a4e1ebb504820f92387e863af8a9, b66b912cd0c7e802c4b9b33e2d9f279e64db172d)

## Development and Maintenance:

- Cleaned up unused code and improved the robustness of the LLM prompt function. (Commits: e9afb2d44b4f9de1c3ab3dc30905a9295954edc5, 2e7abbc8249c85d1e3055ac65ad7a679fbfe5628)
- Started using `renv` for dev reproducibility. (Commit: 3b18519190ae185022ee7023ac02aae9b77c7148)

# minutemaker 0.5.0

### Time management improvements

This release provides many extra features in time management.

The agenda times now can be inserted in a more natural format, e.g 10:30, 10:30:25, 17:00, 09:00 PM, etc
Also the transcript will have clock times in addition to seconds and time from the event start.
Finally, the formatted output will show the start and end times of each talk in a format chosen by the user (defaults to HH:MM).

The user will need to provide the starting time of the event to harness such functionalities.

# minutemaker 0.4.0

The addition of the "rolling window" summarization method, which, instead of performing summarization of a long transcript in one go, splits it into chunks and each is summarised. Then, those summaries are aggregated into one comprehensive summary.
Inclusion of the audio splitting feature in the workflow function which now really works as a one-stop solution from a source audio file to the formatted summary. Also added a folder argument to the function, so that users can point it to a folder with the right input files and have the function do the rest, with very few lines of code.
Improved prompt templates

## New Features

- Added the new "rolling window" summarization method.

## Improvements

- Implemented the audio splitting into speech_to_summary_workflow.
- speech_to_summary_workflow now takes a target_dir argument which allows to encapsulate and perform the whole workflow in one folder. Very few arguments are necessary to run the whole workflow now.
- Updated the interrogate_llm function with a log_request parameter to hide/show LLM technical messages.
- Enhanced the default prompt, the prompt fine-tuning and the prompt generation system.
-Updated the outputs0 nomenclature.
- Updated default segment_duration for the split_audio function to 40 minutes. It improves transcriptions.

# minutemaker 0.3.0

## New Features

- Allows to import transcripts and chat files generated from WebEx.
- Transcript merging uses the Glove text embedding model from the `text2vec` package to match segments between transcripts.
- Implemented a full speech-to-summary workflow, handling audio files to generate human-readable summaries.

## Enhancements

- Improved speech-to-text functionality to handle folder inputs and initial prompts.
- Updated summarization functions to manage different speakers.
- Expanded import functionality to support subtitle file formats with speaker information.
- Provided utility functions to handle silent segments in transcripts in a coherent way.
- Implemented a series of heuristics to remove Whisper hallucinations from transcripts.

Plus various bug fixes.

# minutemaker 0.2.0

First stable version
