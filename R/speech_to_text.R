#' Converts audio files to text transcripts using Whisper.
#'
#' Loops through audio files, runs a speech-to-text model on each, parses the
#' generated .srt files into a transcript data frame.
#'
#' @param audio_path Path to one or more audio files or to a folder of audio
#'   files. Tested with .mp3 and .wav files.
#' @param output_dir Path to output directory. If the directory doesn't exist,
#'   it will be created in the same directory as the audio file and will be
#'   named "transcripts".
#' @param initial_prompt Text to prepend to the beginning of each transcript.
#'   This is useful for adding hard to parse words like acronyms.
#' @param overwrite If TRUE, will overwrite existing files. If FALSE, will skip
#'   the transcription of audio files whose output already exist in the output
#'   directory.
#' @param language Language in which the audio is spoken. If null, the model will
#'  try to detect the language automatically.
#' @param model Name of the model to use. Can be one "azure_whisper" (online API) or
#'   "whisper_ctranslate2" (offline local model).
#'
#' @return A data frame with the text and time of each segment.
#'
perform_speech_to_text <- function(
    audio_path,
    output_dir = file.path(dirname(audio_path), "transcripts"),
    initial_prompt = "", overwrite = FALSE,
    language = "en",
    model = "azure_whisper",
    ...
) {

  audio_files <- character()

  # Check if file_path is a folder
  if (dir.exists(audio_path)) {
    # Get all audio files in the folder
    audio_files <- list.files(audio_path, pattern = "\\.(mp3|wav)$")

    # Reorder the files by the number in the file name, otherwise they will be
    # processed in alphabetical order putting e.g. segment 10 before segment 2
    file_order <- stringr::str_order(audio_files, numeric = TRUE)

    audio_files <- file.path(audio_path, audio_files[file_order])
  } else {
    # Use the provided file path
    audio_files <- audio_path[file.exists(audio_files)]
  }

  if (length(audio_files) == 0) {
    stop("No audio files found in the folder.")
  }

  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Initialize the transcript data frame
  full_transcript <- data.frame()
  last_text <- ""
  last_time <- 0

  # Get the model function
  model_fun <- get(paste0("use_", model, "_model"))

  # Loop through each audio file
  for (i in seq_along(audio_files)) {
    # Get the current file path
    current_file <- audio_files[i]

    cat("Processing file ", i, " of ", length(audio_files), ": ", basename(current_file), "\n")

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

    # Parse the transcript into a data frame
    transcript_data <- transcript_json$segments |> purrr::map(\(df) {
      data.frame(
        doc = i,
        file = basename(current_file),
        start = unlist(df$start) + last_time,
        end = unlist(df$end) + last_time,
        text = unlist(df$text)
      )
    }) |> dplyr::bind_rows()

    full_transcript <- rbind(full_transcript, transcript_data)

    # Get the last segment info
    last_time <- transcript_data$end |> tail(1)
    last_text <- transcript_data$text |> tail(2) |> paste(collapse = " ") |>
      trimws() |> stringr::word(-10, -1)
  }

  # Return the transcript data frame
  return(full_transcript)
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

# split_audio <- function(
    #     audio_file,
#     segment_duration = 60,
#     output_folder = file.path(dirname(audio_file_path), "recorded_segments")
# ) {
#
#   check_and_install_dependencies(c("tuneR"))
#
#   # Extract the file extension
#   file_ext <- stringr::str_extract(audio_file, "(?<=\\.)[^.]+$")
#
#   if (file_ext == "mp4") {
#     check_and_install_dependencies("av")
#     # Extract audio from MP4
#     temp_audio <- tempfile(fileext = ".wav")
#     av::av_audio_convert(audio = audio_file, temp_audio)
#     audio <- tuneR::readWave(temp_audio)
#   } else if (file_ext == "mp3") {
#     # Read MP3 file
#     audio <- tuneR::readMP3(audio_file)
#   } else {
#     stop("Unsupported file format")
#   }
# browser()
#   # Handle output folder
#   if (!dir.exists(output_folder)) {
#     dir.create(output_folder)
#   }
#
#   # Calculate segment length in samples
#   segment_length_samples <- segment_duration * 60 * audio@samp.rate
#
#   # Split the audio and 4. Write segments to files
#   num_segments <- ceiling(length(audio@left) / segment_length_samples)
#
#   # Loop over the segments to write them to files
#   purrr::walk(1:num_segments, \(i) {
#     # Extract the segment time range
#     start_sample <- (i - 1) * segment_length_samples + 1
#     end_sample <- min(i * segment_length_samples, length(audio@left))
#
#     # Extract the segment
#     segment <- audio[start_sample:end_sample, ]
#
#     # Generate the output file path
#     output_file_path <- file.path(output_folder, paste0("segment_", i, ".wav"))
#
#     # Write the segment to a file using tuneR::writeWave
#     tuneR::writeWave(segment, output_file_path)
#   }, .progress = TRUE)
# }

#' Split an audio file into segments of a specified duration
#'
#' @param audio_file The path to the audio file to split.
#' @param segment_duration The duration of each segment in minutes.
#' @param output_folder The folder to save the segments in.
#'
#' @return Nothing, but saves the segments to files.
#'
split_audio <- function(
    audio_file,
    segment_duration = 60,
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
#' @param model_size Size of the model to use. Can be one of "tiny", "medium",
#'   "medium.en" "large-v1", "large-v2", "large-v3",
#' @param n_threads Number of CPU threads to use for the transcription.
#'
#' @return A list with the full transcript and the transcription by segments.
use_whisper_ctranslate2_model <- function(
    audio_file, initial_prompt,
    model_size = "large-v3",
    language = language,
    n_threads = parallel::detectCores()
) {

  output_dir <- tempdir()
  output_file <- basename(audio_file) |>
    stringr::str_remove("\\..*$")

  args <- c(
    audio_file,
    "--model", model,
    "--threads", n_threads,
    "--compute_type", "auto",
    "--output_format", "json",
    "--language", language,
    "--vad_filter", "False",
    "--output_dir", output_dir,
    "--vad_filter", "True",
    "--repetition_penalty", "1",
    "--patience", "10",
    if (prompt != "") {
      c("--initial_prompt", prompt)
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
#' @param prompt Text to guide the model's style or continue a previous segment.
#' @param temperature number (optional) The sampling temperature between 0 and
#'   1. Defaults to 0.
#' @param resource_name The name of the Azure resource to use.
#' @param deployment_id The deployment ID of the Azure resource to use.
#' @param api_key The API key to use for the Azure resource.
#' @param api_version The API version to use for the Azure resource.
#'
#' @return A list with the full transcript and the transcription by segments.
#'
use_azure_whisper_model <- function(
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

  # Check response status
  if (response$status_code != 200) {
    warning("Error in Azure Whisper API request: ",
            httr::content(response, "text"))
    if (response$status_code == 429 ||
        grepl("temporarily unable to process", httr::content(response, "text"))
    ) {
      Sys.sleep(30)
      use_azure_whisper_model(
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

#' Use OpenAI Whisper Model for Speech-to-Text
#'
#' This function sends an audio file to the Azure Whisper API for transcription.
#' See https://platform.openai.com/docs/api-reference/audio/createTranscription
#'
#' @param audio_file The path to the audio file to transcribe.
#' @param language The language of the input audio in mp3/wav format. Should be
#'   less than 25 MB.
#' @param prompt Text to guide the model's style or continue a previous segment.
#' @param temperature number (optional) The sampling temperature between 0 and
#'   1. Defaults to 0.
#' @param api_key The OpenAI API key. If not provided, it will be retrieved
#'  from the global options.
#'
#' @return A list with the full transcript and the transcription by segments.
#'
use_openai_whisper_model <- function(
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
      Sys.sleep(30)
      use_openai_whisper_model(
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
