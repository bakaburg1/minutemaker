% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_management.R
\name{import_transcript_from_file}
\alias{import_transcript_from_file}
\title{Import transcript from subtitle file}
\usage{
import_transcript_from_file(transcript_file, import_diarization = TRUE)
}
\arguments{
\item{transcript_file}{A string with the path to the transcript file.
Supported formats are SRT and VTT.}

\item{import_diarization}{A boolean indicating whether the speaker should be
recorded, if present.}
}
\value{
A data frame containing the transcript data with a start and end time
in seconds for each text segment.
}
\description{
Import a transcript from typical subtitle file formats like SRT and VTT.
}
