# Produce the package general documentation
#' @title minutemaker
#'
#' @name minutemaker
#' @docType package
#' @description Package which uses LLM models to generate minutes from audio recordings or transcriptions of meetings.
#'
#' @import dplyr
"_PACKAGE"

# Initialize prompt options when the package loads
.onLoad <- function(libname, pkgname) {
  set_prompts()
}
