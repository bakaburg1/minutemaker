#' Process chat message into standard format
#'
#' This function takes one or more (a list of) chat messages and processes them
#' into a standard list format with role and content for each message to be fed
#' to a large language model.
#'
#' The standard format is a list of chat messages with the following structure:
#' message: `c(role = "system", content = "Welcome to the chat!")`
#' list of messages: \code{list(
#'    c(role = "system", content = "You are an useful AI assistant."),
#'    c(role = "user", content = "Hi there!")
#'  )}
#'  list format: \code{list(
#'    list(role = "system", content = "You are an useful AI assistant."),
#'    list(role = "user", content = "Hi there!")
#'  )}
#'  list of lists format: \code{list(
#'    list(
#'      list(role = "system", content = "You are an useful AI assistant."),
#'      list(role = "user", content = "Hi there!")
#'    ),
#'    list(
#'      list(role = "system", content = "You are an useful AI assistant."),
#'      list(role = "user", content = "Hi there!")
#'    )
#'  )}
#'
#' @param messages A character vector or list of chat messages. In can be a
#'   vector, a specifically structured list or a list of both if the goal is the
#'   have the API process multiple messages at once.
#'
#' @return A list of chat messages in standard format.
#'
#' @importFrom utils hasName
#'
process_messages <- function(messages) {

  if (missing(messages) || is.null(messages)) {
    stop("User messages are required.")
  }

  # Convert vector to list format
  vector_to_list <- function(msg_vec) {

    # Check if vector is in named format
    check <- all(
      names(msg_vec) %in%
        c("system", "user", "assistant", "function")
      , na.rm = TRUE)

    check <- check && !is.null(names(msg_vec))

    if (check) {

      # Convert from vector to list format
      msg_vec <- purrr::imap(msg_vec, function(msg, nm) {
        list(role = nm, content = msg)
      }) |> setNames(NULL)

    } else {
      stop("Invalid format for 'messages' vector.")
    }
  }

  # Validate list format
  validate_list_format <- function(msg_list) {

    # Check if the message is in correct list format
    check <- !purrr::every(msg_list, function(msg) {
      vctrs::vec_is_list(msg) &&
        hasName(msg, "role") &&
        hasName(msg, "content") &&
        msg$role %in% c("system", "user", "assistant", "function")
    })

    return(!check)
  }

  # Check if message is in a valid vector format
  if (is.character(messages)) {
    return(vector_to_list(messages))
  }


  if (vctrs::vec_is_list(messages)) {

    # Check if valid list format
    if (validate_list_format(messages)) {
      return(messages)
    }

    # It turned out the API doesn't really support batch calls of
    # multiple prompts

    # # Check if list of vectors
    # if (purrr::every(messages, is.character)) {
    #
    #   # Convert each to list
    #   return(purrr::map(messages, vector_to_list))
    #
    # }
    #
    # # Check if list of lists
    # if (purrr::every(messages, validate_list_format)) {
    #
    #   return(messages)
    #
    # }

  }

  stop("Message is neither a valid vector nor a valid list.")

}

#' Interrogate Language Model
#'
#' This function sends requests to a specified language model provider (OpenAI,
#' Azure, or a locally running LLM server) and returns the response. It handles
#' rate limiting and retries the request if necessary, and also processes errors
#' in the response.
#'
#' Users can provide their own models by writing a function with the following
#' name pattern: `use_<model_name>_llm`. See the existing functions using the
#' ::: operator for examples.
#'
#' @param messages Messages to be sent to the language model.
#' @param provider The provider of the language model. Defaults to "openai".
#'   Other options are "azure" and "local".
#' @param params Additional parameters for the language model request. Defaults
#'   to a list with `temperature = 0`.
#' @param force_json A boolean to force the response in JSON format. Default is
#'   FALSE. Works only for OpenAI and Azure endpoints.
#' @param ... Additional arguments passed to the language model provider
#'   functions.
#'
#' @return Returns the content of the message from the language model response.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' response <- interrogate_llm(
#'  messages = c(user = "Hello there!"),
#'  provider = "openai")
#'  }
#'
interrogate_llm <- function(
    messages = NULL,
    provider = c("openai", "azure", "local"),
    params = list(
      temperature = 0
    ),
    force_json = FALSE,
    ...) {

  messages <- process_messages(messages)
  provider <- match.arg(provider)

  # Prepare the body of the request and merge with default
  body <- purrr::list_modify(list(
    temperature = 0
  ), !!!params)

  body$messages <- messages

  # Force the LLM to answer in JSON format (only openai and azure)
  if (force_json) {
    body$response_format <- list("type" = "json_object")
  }

  # Map provider to specific function
  llm_fun <- paste0("use_", provider, "_llm")

  if (!exists(llm_fun, mode = "function")) {
    stop("Unsupported LLM provider.")
  }

  llm_fun <- get(llm_fun)

  # Try to send the request
  retry <- FALSE

  while(!exists("response") || retry) {

    #message("Sending message to Azure GPT API.")
    retry <- FALSE

    tictoc::tic()
    response <- llm_fun(body, ...)
    tictoc::toc()

    if (httr::status_code(response) == 429) {
      warning("Rate limit exceeded. Waiting before retrying.", immediate. = TRUE)

      to_wait <- as.numeric(httr::headers(response)$`retry-after`)
      message("Waiting for ", to_wait, " seconds.")
      Sys.sleep(to_wait)
      retry <- TRUE
    }
  }


  # Check for errors in response
  if (httr::http_error(response)) {
    err_obj <- httr::content(response)$error

    err_message <- if (is.character(err_obj)) {
      err_obj
    } else {
      err_obj$message
    }

    stop("Error in LLM request: ", err_message)
  }

  # Return the response
  parsed <- httr::content(response, as = "parsed", encoding = "UTF-8")

  with(parsed$usage,
       paste(
         "Prompt tokens:", prompt_tokens,
         "\nResponse tokens:", completion_tokens,
         "\nTotal tokens:", total_tokens
       )
  ) |> message()

  answer <- parsed$choices[[1]]

  if (answer$finish_reason == "length") {
    stop("Answer exhausted the context window!")
  }

  answer$message$content
}

#' Use OpenAI Language Model
#'
#' Sends a request to the OpenAI API  using the parameters in the `body`
#' argument. It requires an API key and model identifier set in the R options.
#'
#' @param body The body of the request.
#' @param model Model identifier for the OpenAI API. Obtained from R options.
#' @param api_key API key for the OpenAI service. Obtained from R options.
#'
#' @return The function returns the response from the OpenAI API.
#'
use_openai_llm <- function(
    body,
    model = getOption("minutemaker_openai_model_gpt"),
    api_key = getOption("minutemaker_open_api_key")
    ) {

  if (is.null(api_key)) {
    stop("OpenAI GPT model and API key are not set. ",
    "Use the following options to set them:\n",
    "minutemaker_openai_model_gpt, ",
    "minutemaker_open_api_key options.")
  }

  body$model = model

  # Prepare the request
  httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      `Content-Type` = "application/json",
      `Authorization` = paste0("Bearer ", api_key)
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )

}

#' Use Azure Language Model
#'
#' Sends a request to the Azure API for language model completions using the
#' parameters in the `body` argument. This function requires specific Azure
#' configurations (deployment ID, resource name, API key, and API version) set
#' in the R options.
#'
#' @param body The body of the request.
#' @param deployment_id Azure deployment ID for the language model. Obtained
#'   from R options.
#' @param resource_name Azure resource name. Obtained from R options.
#' @param api_key API key for the Azure language model service. Obtained from R
#'   options.
#' @param api_version API version for the Azure language model service. Obtained
#'   from R options.
#'
#' @return The function returns the response from the Azure API.
use_azure_llm <- function(
    body,
    deployment_id = getOption("minutemaker_azure_deployment_gpt"),
    resource_name = getOption("minutemaker_azure_resource_gpt"),
    api_key = getOption("minutemaker_azure_api_key_gpt"),
    api_version = getOption("minutemaker_azure_api_version")
    ) {

  if (is.null(resource_name) || is.null(deployment_id) ||
      is.null(api_key) || is.null(api_version)) {
    stop("Azure GPT resource name, deployment name,",
         ", API key, or API version are not set. ",
         "Use the following options to set them:\n",
         "minutemaker_azure_deployment_gpt, ",
         "minutemaker_azure_resource_gpt, ",
         "minutemaker_azure_api_key_gpt, ",
         "minutemaker_azure_api_version."
    )
  }

  # Prepare the request
  httr::POST(
    url = paste0(
      "https://",
      resource_name,
      ".openai.azure.com/openai/deployments/",
      deployment_id,
      "/chat/completions?api-version=",
      api_version),
    httr::add_headers(`Content-Type` = "application/json", `api-key` = api_key),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

}

#' Use Local Language Model
#'
#' Sends a request to a local language model endpoint using the parameters in
#' the `body` argument. The endpoint URL should be set in the R options, with a
#' default provided.
#'
#' @param body The body of the request.
#' @param endpoint The local endpoint for the language model service. Can be
#'   obtained from R options.
#' @return The function returns the response from the local language model
#'   endpoint.
use_local_llm <- function(
    body,
    endpoint = getOption("minutemaker_local_endpoint_gpt",
                         "http://localhost:1234/v1/chat/completions")
    ) {

  if (is.null(endpoint)) {
    stop("Local endpoint is not set. ",
         "Use the following options to set it:\n",
         "minutemaker_local_endpoint_gpt."
    )
  }

  body$response_format <- NULL

  # Prepare the request
  httr::POST(
    url = endpoint,
    httr::add_headers(`Content-Type` = "application/json"),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

}

