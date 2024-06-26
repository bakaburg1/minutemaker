% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LLM_calls.R
\name{use_custom_llm}
\alias{use_custom_llm}
\title{Use Custom Language Model}
\usage{
use_custom_llm(
  body,
  endpoint = getOption("minutemaker_custom_endpoint_gpt"),
  model = getOption("minutemaker_custom_model_gpt", NULL),
  api_key = getOption("minutemaker_custom_api_key"),
  log_request = getOption("minutemaker_log_requests", TRUE)
)
}
\arguments{
\item{body}{The body of the request.}

\item{endpoint}{The local endpoint for the language model service. Can be
obtained from R options.}

\item{model}{Model identifier for the custom API, if needed (some API have
one model per endpoint, some multiple ones). Obtained from R options.}

\item{api_key}{Optional API key for the custom language model services that
require it. Obtained from R options.}

\item{log_request}{A boolean to log the request time. Can be set up globally
using the \code{minutemaker_log_requests} option, which defaults to TRUE.}
}
\value{
The function returns the response from the local language model
endpoint.
}
\description{
Sends a request to a custom (local or remote) language model endpoint
compatible with the OpenAi API specification, using the parameters in the
\code{body} argument. The user can provide an API key if required.
}
