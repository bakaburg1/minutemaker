#' Converts audio files to text transcripts using Whisper.
#'
#' Loops through audio files, runs a speech-to-text model on each, parses the
#' generated .srt files into a transcript data frame.
#'
#' Users can provide their own models by writing a function with the following
#' name pattern: `use_<model_name>_stt`. See the existing functions using the
#' ::: operator for examples.
#'
#' @param audio_path Path to one or more audio files or to a folder of audio
#'   files. Tested with .mp3 and .wav files.
#' @param output_dir Path to output directory. If the directory doesn't exist,
#'   it will be created in the same directory as the audio file.
#' @param model Name of the model to use. The models available in the package
#'   are: "azure_whisper" (online API), "openai_whisper" (online API),
#'   "whisper_ctranslate2" (offline local model). Users can provide their own
#'   models.
#' @param initial_prompt Text to prepend to the beginning of each transcript.
#'   This is useful for adding hard to parse words like acronyms.
#' @param overwrite If TRUE, will overwrite existing files. If FALSE, will skip
#'   the transcription of audio files whose output already exist in the output
#'   directory.
#' @param language Language in which the audio is spoken. If null, the model
#'   will try to detect the language automatically.
#' @param ... Additional arguments to pass to the model function.
#'
#' @return A data frame with the text and time of each segment.
#'
#' @export
#'
#' @importFrom utils tail
#'
perform_speech_to_text <- function(
    audio_path,
    output_dir = file.path(dirname(audio_path), "transcription_output_data"),
    model = getOption("minutemaker_stt_model", "whisper_local"),
    initial_prompt = NULL, overwrite = FALSE,
    language = "en",
    ...
) {

  audio_files <- character()
  if (is.null(initial_prompt)) initial_prompt <- ""

  # Check if file_path is a folder
  if (dir.exists(audio_path)) {
    # Get all audio files in the folder
    audio_files <- list.files(audio_path, pattern = "\\.(mp3|wav)$")

    # Reorder the files by the number in the file name, otherwise they will be
    # processed in alphabetical order putting e.g. segment 10 before segment 2
    file_order <- stringr::str_order(audio_files, numeric = TRUE)

    audio_files <- file.path(audio_path, audio_files[file_order])
  } else if (file.exists(audio_path)){
    # User the provided file path
    audio_files <- audio_path
  } else {
    stop("The provided audio path is not a file or a folder.")
  }

  if (length(audio_files) == 0) {
    stop("No audio files found in the folder.")
  }

  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Initialize the prompt to link two transcripts
  last_text <- ""

  # Get the model function
  model_fun <- get(paste0("use_", model, "_stt"))

  # Loop through each audio file
  for (i in seq_along(audio_files)) {
    # Get the current file path
    current_file <- audio_files[i]

    cat("Processing file ", i, " of ", length(audio_files), ": ",
        basename(current_file), "\n")

    # Generate the output file name by replacing the extension with .json
    output_file_name <- basename(current_file) |>
      stringr::str_remove("\\.(mp\\d|wav)$") |>
      paste0(".json")
    output_file_name <- file.path(output_dir, output_file_name)

    # Skip the file if it already exists and overwrite is FALSE
    if (!overwrite && file.exists(output_file_name)) {
      message("Skipping ", basename(current_file), " because it already exists.")

      transcript_json <- jsonlite::read_json(output_file_name)
    } else {
      # Remove the output file if it already exists
      if (file.exists(output_file_name)) file.remove(output_file_name)

      # Append the last text from the previous file to the initial prompt
      prompt <- paste0(initial_prompt, ". ", last_text) |>
        stringr::str_replace_all("\\.+", ".") |>
        stringr::str_remove("^\\. $")

      if (requireNamespace("tictoc", quietly = TRUE))  tictoc::tic()

      # Run the model
      transcript_json <- model_fun(
        current_file,
        initial_prompt = initial_prompt,
        language = language,
        ...
      )

      if (requireNamespace("tictoc", quietly = TRUE))  tictoc::toc()

      # Store the transcript on disk
      jsonlite::write_json(transcript_json, output_file_name)
    }

    last_text <- transcript_json$text |> trimws() |> stringr::word(-10, -1)
  }

  # Parse the transcript JSON files into a data frame
  invisible(parse_transcript_json(output_dir))
}

#' Run a command in a terminal window
#'
#' This function is useful since it gives more control than just running
#' commands from R directly. For example it allows to check the output of the
#' command in real time.
#'
#' @param cmd The command to run.
#' @param args The arguments to pass to the command as a character vector.
#'
#' @return The terminal/command line window ID.
#'
#' @export
#'
#' @examples
#'
#' # Run a command in a terminal window
#' run_in_terminal("ls", args = c("-l", "-a"))
run_in_terminal <- function(cmd, args) {
  # Get the current working directory in R
  working_dir <- getwd()

  # Function to quote arguments only if they contain spaces
  quote_if_needed <- function(arg) {
    if (grepl(" ", arg)) {
      return(shQuote(arg, type = "cmd"))
    } else {
      return(arg)
    }
  }

  # Apply the function to each argument
  processed_args <- sapply(args, quote_if_needed)

  # Combine command and arguments
  full_cmd <- paste(cmd, paste(processed_args, collapse = " "))

  # Write the command to a temporary script file
  tmp_script <- tempfile()
  writeLines(paste("cd", shQuote(working_dir), "\n", full_cmd), tmp_script)

  message("Running command:\n", full_cmd, "\nwith file ID: ", tmp_script)

  # Detect the operating system
  os_type <- .Platform$OS.type

  if (os_type == "windows") {
    # Windows command
    system2("cmd", args = c("/c", "start", "cmd", "/k", "call", shQuote(tmp_script)))
  } else if (os_type == "unix") {
    # Check if it's macOS or Linux
    if (Sys.info()["sysname"] == "Darwin") {
      # macOS command
      # Execute the script in Terminal
      applescript_cmd <- sprintf("osascript -e 'tell app \"Terminal\" to do script \"bash %s\"'", shQuote(tmp_script))
      system(applescript_cmd)
    } else {
      # Linux command (using gnome-terminal as an example)
      system(sprintf("gnome-terminal -- bash -c 'bash %s; exec bash'", shQuote(tmp_script)))
    }
  } else {
    stop("Unsupported operating system.")
  }
}

#' Split an audio file into segments of a specified duration
#'
#' Some speech-to-text models have a limit on the size of the audio file. For
#' example, the "Whisper" based models have a limit of 25 MB. This function
#' splits an audio file into segments of a specified duration and saves them as
#' mp3 files.
#'
#' @param audio_file The path to the audio file to split.
#' @param segment_duration The duration of each splitted audio file in minutes.
#'   20 minutes equate to more or less 7-8 MB.
#' @param output_folder The path to the folder where to save the segments in.
#'
#' @return Nothing, but saves the segments to files.
#'
#' @export
#'
split_audio <- function(
    audio_file,
    segment_duration = 40,
    output_folder = file.path(dirname(audio_file), "recording_parts")
) {

  # Check if the av package is installed and ask to install it if not
  check_and_install_dependencies("av")

  # Calculate segment length in seconds
  segment_length_sec <- segment_duration * 60

  # Get the total duration of the file in seconds
  # Using av::av_media_info for getting duration
  media_info <- av::av_media_info(audio_file)
  total_duration_sec <- media_info$duration

  # Calculate the number of segments
  num_segments <- ceiling(total_duration_sec / segment_length_sec)

  # Ensure the output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  # Loop over the segments to write them to files
  purrr::walk(1:num_segments, \(i) {
    # Extract the segment start time
    # Start 30 secs before the segment start time, to ensure continuity
    # Also prevent negative start times
    start_time <- (i - 1) * segment_length_sec

    # Generate the output file path
    output_file_path <- file.path(output_folder, paste0("segment_", i, ".mp3"))

    # Print message for each file being processed
    cat("Outputting:", basename(output_file_path), "\n")

    # Use av::av_audio_extract to extract the segment
    # Convert and output each segment
    av::av_audio_convert(
      audio_file, output_file_path,
      start_time = start_time,
      total_time = segment_length_sec,
    )
  })
}

#' Use a local Whisper CTranslate2 model to transcribe audio
#'
#' The function opens a terminal window and runs the command to transcribe the
#' audio file using the specified model. Opening a terminal window is necessary
#' to visualize the progress of the transcription. See
#' https://github.com/Softcatala/whisper-ctranslate2.
#'
#' @param audio_file Path to the audio file to transcribe.
#' @param initial_prompt Initial prompt to use for the transcription.
#' @param model_version Version of the model to use. Can be one of "tiny",
#'   "medium", "medium.en" "large-v1", "large-v2", "large-v3",
#' @param language Language of the recording. E.g. "en" for English, "es" for
#'   Spanish, etc.
#' @param n_threads Number of CPU threads to use for the transcription. Defaults
#'   to the number of cores in the system, detected with
#'   `parallel::detectCores()`.
#'
#' @return A list with the full transcript and the transcription by segments.
use_whisper_ctranslate2_stt <- function(
    audio_file, initial_prompt,
    model_version = "large-v3",
    language = NULL,
    n_threads = NULL
) {

  check_and_install_dependencies("parallel")

  if (is.null(n_threads)) {
    n_threads <- parallel::detectCores()
  }

  output_dir <- tempdir()
  output_file <- basename(audio_file) |>
    stringr::str_remove("\\..*$")

  args <- c(
    audio_file,
    "--model", model_version,
    "--threads", n_threads,
    "--compute_type", "auto",
    "--output_format", "json",
    "--language", language,
    "--vad_filter", "False",
    "--output_dir", output_dir,
    "--vad_filter", "True",
    "--repetition_penalty", "1",
    "--patience", "10",
    if (initial_prompt != "") {
      c("--initial_prompt", initial_prompt)
    }
  )

  output_file_path <- paste0(output_dir, "/output.json")

  run_in_terminal("whisper-ctranslate2", args)

  while (!file.exists(output_file_path)) {
    Sys.sleep(1)
  }

  jsonlite::read_json(output_file_path)

  # In console, synchronous version, no logging
  # p <- processx::process$run(
  #   "whisper-ctranslate2",
  #   args = args,
  #   echo_cmd = T, stdout = "|")
}

#' Use Azure Whisper Model for Speech-to-Text
#'
#' This function sends an audio file to the Azure Whisper API for transcription.
#' See https://learn.microsoft.com/en-us/azure/ai-services/openai/reference.
#'
#' @param audio_file The path to the audio file to transcribe.
#' @param language The language of the input audio in mp3/wav format. Should be
#'   less than 25 MB.
#' @param initial_prompt Text to guide the model's style or continue a previous
#'   segment.
#' @param temperature number (optional) The sampling temperature between 0 and
#'   1. Defaults to 0.
#' @param resource_name The name of the Azure resource to use.
#' @param deployment_id The deployment ID of the Azure resource to use.
#' @param api_key The API key to use for the Azure resource.
#' @param api_version The API version to use for the Azure resource.
#'
#' @return A list with the full transcript and the transcription by segments.
#'
use_azure_whisper_stt <- function(
    audio_file,
    language = "en",
    initial_prompt = "",
    temperature = 0,
    resource_name = getOption("minutemaker_azure_resource_whisper"),
    deployment_id = getOption("minutemaker_azure_deployment_whisper"),
    api_key = getOption("minutemaker_azure_api_key_whisper"),
    api_version = getOption("minutemaker_azure_api_version")
) {

  if (is.null(resource_name) || is.null(deployment_id) ||
      is.null(api_key) || is.null(api_version)) {
    stop("Resource name, deployment name,",
         ", API key, or API version are not set. ",
         "Use the following options to set them:\n",
         "minutemaker_azure_resource_whisper, ",
         "minutemaker_azure_deployment_whisper, ",
         "minutemaker_azure_api_key_whisper, ",
         "minutemaker_azure_api_version."
    )
  }

  # Prepare the URL
  url <- paste0(
    "https://", resource_name,
    ".openai.azure.com/openai/deployments/", deployment_id,
    "/audio/transcriptions?api-version=", api_version)

  # Prepare headers and body
  headers <- httr::add_headers(
    `Content-Type` = "multipart/form-data",
    `api-key` = api_key)

  body <- list(
    file = httr::upload_file(audio_file),
    language = language,
    prompt = initial_prompt,
    temperature = temperature,
    response_format = "verbose_json")

  # Make the HTTP request
  response <- httr::POST(url, headers, body = body)

  if (response$status_code == 424) {
    stop("Fatal error: ", httr::content(response, "text"))
  }

  # Check response status
  if (response$status_code != 200) {

    warning("Error ", response$status_code, " in Azure Whisper API request: ",
            httr::content(response, "text"), call. = FALSE, immediate. = TRUE)

    wait_for <- stringr::str_extract(
      httr::content(response, "text", encoding = "UTF-8"),
      "\\d+(?= seconds)") |> as.numeric()

    if (is.na(wait_for) && !interactive()) stop()

    if (is.na(wait_for)) wait_for <- 30

    message("Retrying in ", wait_for, " seconds...")

    Sys.sleep(wait_for)

    res <- use_azure_whisper_stt(
      audio_file = audio_file,
      language = language,
      initial_prompt = initial_prompt,
      temperature = temperature)

    return(res)
  }

  # Return the response
  res <- httr::content(response)
}

#' Use OpenAI Whisper Model for Speech-to-Text
#'
#' This function sends an audio file to the Azure Whisper API for transcription.
#' See https://platform.openai.com/docs/api-reference/audio/createTranscription
#'
#' @param audio_file The path to the audio file to transcribe.
#' @param language The language of the input audio in mp3/wav format. Should be
#'   less than 25 MB.
#' @param initial_prompt Text to guide the model's style or continue a previous
#'   segment.
#' @param temperature number (optional) The sampling temperature between 0 and
#'   1. Defaults to 0.
#' @param api_key The OpenAI API key. If not provided, it will be retrieved from
#'   the global options.
#'
#' @return A list with the full transcript and the transcription by segments.
#'
use_openai_whisper_stt <- function(
    audio_file,
    language = "en",
    initial_prompt = "",
    temperature = 0,
    api_key = getOption("minutemaker_openai_api_key")
) {

  if (is.null(api_key)) {
    stop("OpenAI API key is not set.")
  }

  # Prepare the URL
  url <- "https://api.openai.com/v1/audio/transcriptions"

  # Prepare headers and body
  headers <- httr::add_headers(
    "Content-Type" = "multipart/form-data",
    "Authorization" = paste("Bearer", api_key))

  body <- list(
    file = httr::upload_file(audio_file),
    language = language,
    model = "whisper-1",
    prompt = initial_prompt,
    temperature = temperature,
    response_format = "verbose_json")

  # Make the HTTP request
  response <- httr::POST(url, headers, body = body)

  # Check response status
  if (response$status_code != 200) {
    warning("Error in OpenAI Whisper API request: ",
            httr::content(response, "text"))
    if (response$status_code == 429 ||
        grepl("temporarily unable to process", httr::content(response, "text"))
    ) {
      message("Retrying in 30 seconds...")
      Sys.sleep(30)
      use_openai_whisper_stt(
        audio_file = audio_file,
        language = language,
        initial_prompt = initial_prompt,
        temperature = temperature
      )
    } else stop()
  }

  # Return the response
  res <- httr::content(response)
}

#' Use Local Whisper Model for Speech-to-Text
#'
#' This function uses a local Whisper model via Python with reticulate to
#' transcribe audio. It can use the official OpenAI Whisper package or any
#' compatible Python package.
#'
#' @param audio_file The path to the audio file to transcribe.
#' @param language The language of the input audio. Default is "en" for English.
#'   If NULL, Whisper will attempt to detect the language.
#' @param initial_prompt Text to guide the model's style or continue a previous
#'   segment.
#' @param model The Whisper model to use. Default is "turbo". Check
#'   https://github.com/openai/whisper for other available models.
#' @param whisper_package The Python package to use for Whisper (default:
#'   "openai-whisper").
#'
#' @return A list with the full transcript and the transcription by segments.
#'
#' @export
use_whisper_local_stt <- function(
    audio_file,
    language = "en",
    initial_prompt = "",
    model = "turbo",
    whisper_package = getOption(
      "minutemaker_whisper_package", "openai-whisper")
) {
  # Check if reticulate is installed
  if (!rlang::is_installed("reticulate")) {
    stop("Package 'reticulate' is required. ",
         "Please install it using install.packages('reticulate')")
  }

  # Check if Miniconda is installed
  if (length(list.files(reticulate::miniconda_path())) == 0) {
    message("Miniconda not found. Installing it now...")
    reticulate::install_miniconda()
  }

  conda_env <- "minutemaker_env"

  # Check if the conda environment exists
  if (!reticulate::condaenv_exists(conda_env)) {
    message(
      "Conda environment '", conda_env, "' does not exist. Creating it now...")

    reticulate::conda_create(conda_env, python_version = "3.9")
  }

  # Use the conda environment
  reticulate::use_miniconda(conda_env, required = TRUE)

  # Check if Whisper is already installed
  if (!reticulate::py_module_available("whisper")) {
    message("Whisper not found. Installing dependencies...")

    # Install the required packages
    reticulate::conda_install(
      conda_env,
      c("numpy==1.23.5", "numba==0.56.4", "llvmlite==0.39.1", whisper_package),
      pip = TRUE)
  }

  # Import the Whisper module
  whisper <- reticulate::import("whisper")

  # Load the Whisper model
  model <- whisper$load_model(model)

  # Prepare transcription options
  options <- list(
    language = language,
    initial_prompt = initial_prompt,
    fp16 = FALSE
  )

  # Remove NULL values from options
  options <- options[!sapply(options, is.null)]

  # Perform transcription
  result <- do.call(model$transcribe, c(list(audio_file), options))

  # Extract segments
  segments <- lapply(result$segments, function(seg) {
    list(
      id = seg$id,
      start = seg$start,
      end = seg$end,
      text = seg$text
    )
  })

  # Return results in the expected format
  list(
    text = result$text,
    segments = segments
  )
}

#' Use MLX Whisper Local Model for Speech-to-Text (Mac OS only)
#'
#' This function uses a local MLX Whisper model via Python with reticulate to
#' transcribe audio. It is specifically designed to work with the MLX Whisper
#' package. MLX allows faster inference on Mac OS with Apple Silicon.
#'
#' @param audio_file The path to the audio file to transcribe.
#' @param language The language of the input audio. Default is "en" for English.
#'   If NULL, Whisper will attempt to detect the language.
#' @param initial_prompt Text to guide the model's style or continue a previous
#'   segment.
#' @param model The MLX Whisper model to use. Default is
#'   "mlx-community/whisper-large-v3-turbo".
#' @param whisper_package The Python package to use for MLX Whisper (default:
#'   "mlx_whisper").
#'
#' @return A list with the full transcript and the transcription by segments.
#'
#' @export
use_mlx_whisper_local_stt <- function(
    audio_file,
    language = "en",
    initial_prompt = "",
    model = "mlx-community/distil-whisper-large-v3",
    whisper_package = getOption("minutemaker_whisper_package", "mlx_whisper")
) {
  # Check if reticulate is installed
  if (!rlang::is_installed("reticulate")) {
    stop("Package 'reticulate' is required. ",
         "Please install it using install.packages('reticulate')")
  }

  # Check if Miniconda is installed
  if (length(list.files(reticulate::miniconda_path())) == 0) {
    message("Miniconda not found. Installing it now...")
    reticulate::install_miniconda()
  }

  conda_env <- "minutemaker_env"

  # Check if the conda environment exists
  if (!reticulate::condaenv_exists(conda_env)) {
    message(
      "Conda environment '", conda_env, "' does not exist. Creating it now...")

    reticulate::conda_create(conda_env, python_version = "3.9")
  }

  # Use the conda environment
  reticulate::use_condaenv(conda_env, required = TRUE)

  # Check if Whisper is already installed
  if (!reticulate::py_module_available(whisper_package)) {
    message("Whisper not found. Installing dependencies...")

    # Install the required packages reticulate::conda_install(conda_env,
    # c("numpy==1.23.5", "numba==0.56.4", "llvmlite==0.39.1", whisper_package),
    # pip = TRUE)
    reticulate::conda_install(conda_env, whisper_package, pip = TRUE)
  }

  # Import the Whisper module
  mlx_whisper <- reticulate::import(whisper_package)

  # Prepare transcription options
  decode_options <- list(
    language = language,
    initial_prompt = initial_prompt
  )

  # Remove NULL values from options
  decode_options <- decode_options[!sapply(decode_options, is.null)]

  # Perform transcription
  result <- mlx_whisper$transcribe(
    audio_file,
    path_or_hf_repo = model,
    fp16 = FALSE,
    word_timestamps = TRUE,
    !!!decode_options
  )

  # Extract segments
  segments <- lapply(result$segments, function(seg) {
    list(
      id = seg$id,
      start = seg$start,
      end = seg$end,
      text = seg$text
    )
  })

  # Return results in the expected format
  list(
    text = result$text,
    segments = segments
  )
}
