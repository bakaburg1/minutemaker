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

  if (missing(messages) || is.null(messages) || length(messages) == 0) {
    stop("User messages are required.")
  }

  # Assume that a single message is from the user
  if (length(messages) == 1 &&
      is.character(messages) &&
      is.null(names(messages))) {
    messages <- c(user = messages)
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
      vctrs::obj_is_list(msg) &&
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


  if (vctrs::obj_is_list(messages)) {

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

#' Interrogate a Language Model
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
#' @param log_request A boolean to log the request time. Can be set up globally
#'   using the `minutemaker_log_requests` option, which defaults to TRUE.
#' @param ... Additional arguments passed to the language model provider
#'   functions.
#'
#' @return Returns the content of the message from the language model response.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' response <- prompt_llm(
#'  messages = c(user = "Hello there!"),
#'  provider = "openai")
#'  }
#'
prompt_llm <- function(
    messages = NULL,
    provider = getOption("minutemaker_llm_provider"),
    params = list(
      temperature = 0
    ),
    force_json = FALSE,
    log_request = getOption("minutemaker_log_requests", TRUE),
    ...) {

  messages <- process_messages(messages)

  if (is.null(provider)) {
    stop("Language model provider is not set. ",
         "You can use the following option to set it globally:\n",
         "minutemaker_llm_provider.")
  }

  if (log_request) {
    check_and_install_dependencies("tictoc")
  }

  # Prepare the body of the request and merge with default
  body <- purrr::list_modify(list(
    temperature = 0
  ), !!!params)

  body$messages <- messages

  # Force the LLM to answer in JSON format (not all models support this)
  if (force_json) {
    body$response_format <- list("type" = "json_object")
  }

  # Map provider to specific function
  llm_fun <- paste0("use_", provider, "_llm")

  if (!exists(llm_fun, mode = "function")) {
    stop("Unsupported LLM provider.
         You can set it project-wide using the minutemaker_llm_provider option.")
  }

  llm_fun <- get(llm_fun)

  # Try to send the request
  retry <- FALSE

  while(!exists("response", inherits = FALSE) || retry) {

    #message("Sending message to Azure GPT API.")
    retry <- FALSE

    if (log_request) tictoc::tic()
    response <- llm_fun(body, ...)
    if (log_request) elapsed <- tictoc::toc()

    if (httr::status_code(response) == 429) {
      warning("Rate limit exceeded. Waiting before retrying.",
              immediate. = TRUE, call. = FALSE)

      to_wait <- as.numeric(httr::headers(response)$`retry-after`)
      message("Waiting for ", to_wait, " seconds.\n...")
      Sys.sleep(to_wait)
      message("Retrying...")
      retry <- TRUE
    }
  }


  # Check for errors in response
  if (httr::http_error(response)) {
    err_obj <- httr::content(response)$error

    err_message <- if (is.character(err_obj)) {
      err_obj
    } else if (is.character(err_obj$message)) {
      err_obj$message
    } else {
      httr::content(response)
    }

    stop("Error in LLM request: ", err_message)
  }

  # Return the response
  parsed <- httr::content(response, as = "parsed", encoding = "UTF-8")

  if (log_request) {
    with(parsed$usage,
         paste(
           "Prompt tokens:", prompt_tokens,
           "\nResponse tokens:", completion_tokens,
           "\nGeneration speed:", paste(
             signif(completion_tokens/(elapsed$toc - elapsed$tic), 3), "t/s"),
           "\nTotal tokens:", total_tokens
         )
    ) |> message()
  }

  # Return the response
  purrr::imap_chr(parsed$choices, \(ans, i) {
    ans_content <- ans$message$content

    # Manage the case when the answer is cut off due to exceeding the
    # output token limit
    if (ans$finish_reason == "length") {
      i <- if (length(parsed$choices) > 1) paste0(" ", i, " ") else " "

      warning("Answer", i, "exhausted the context window!")

      file_name <- paste0("output_", Sys.time(), ".txt")

      warning(
        "Answer", i, "exhausted the context window!\n",
        "The answer has been saved to a file: ", file_name
      )

      readr::write_lines(ans_content, file_name)

      choice <- utils::menu(
        c(
          "Try to complete the answer",
          "Keep the incomplete answer",
          "Stop the process"),
        title = "How do you want to proceed?"
      )

      if (choice == 1) {
        # Ask the model to continue the answer
        messages_new <- c(
          messages,
          list(list(
            role = "assistant",
            content = ans_content
          )),
          list(list(
            role = "user",
            content = "continue"
          ))
        )

        ans_new <- prompt_llm(
          messages_new, provider = provider, params = params,
          force_json = force_json,
          log_request = log_request, ...
        )

        return(paste0(ans_content, ans_new))
      } else if (choice == 2) {
        return(ans_content)
      } else {
        stop("The process has been stopped.")
      }
    } else ans_content
  })
}

#' Use OpenAI Language Model
#'
#' Sends a request to the OpenAI API using the parameters in the `body`
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
    api_key = getOption("minutemaker_openai_api_key")
) {

  if (is.null(api_key) || is.null(model)) {
    stop("OpenAI GPT model or API key are not set. ",
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

#' Use Custom Language Model
#'
#' Sends a request to a custom (local or remote) language model endpoint
#' compatible with the OpenAi API specification, using the parameters in the
#' `body` argument. The user can provide an API key if required.
#'
#' @param body The body of the request.
#' @param endpoint The local endpoint for the language model service. Can be
#'   obtained from R options.
#' @param model Model identifier for the custom API, if needed (some API have
#'   one model per endpoint, some multiple ones). Obtained from R options.
#' @param api_key Optional API key for the custom language model services that
#'   require it. Obtained from R options.
#'
#' @return The function returns the response from the local language model
#'   endpoint.
use_custom_llm <- function(
    body,
    endpoint = getOption("minutemaker_custom_endpoint_gpt"),
    model = getOption("minutemaker_custom_model_gpt", NULL),
    api_key = getOption("minutemaker_custom_api_key")
) {

  if (is.null(endpoint)) {
    stop("Local endpoint is not set. ",
         "Use the following options to set it:\n",
         "minutemaker_custom_endpoint_gpt"
    )
  }

  if (!is.null(model)) {
    body$model = model
  }

  # Prepare the request
  httr::POST(
    url = endpoint,
    httr::add_headers(
      `Content-Type` = "application/json",
      if (!is.null(api_key)) {
        .headers = c(Authorization = paste0("Bearer ", api_key))
      }),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

}
