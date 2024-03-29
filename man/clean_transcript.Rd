% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_management.R
\name{clean_transcript}
\alias{clean_transcript}
\title{Clean transcript}
\usage{
clean_transcript(transcript_data, remove_silence = FALSE)
}
\arguments{
\item{transcript_data}{A data frame containing the transcript data.}

\item{remove_silence}{A boolean indicating whether silence segments should be
removed.}
}
\value{
A data frame containing the cleaned transcript data.
}
\description{
Cleans a transcript by removing isolated text, consecutive segments with the
same text, and, optionally, silence segments "\link{...}".
}
