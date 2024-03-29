% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compatibility_checks.R
\name{check_input_list}
\alias{check_input_list}
\title{Check input for agreement with provided lists}
\usage{
check_input_list(
  input,
  allowlist,
  allowlength,
  allowsingle,
  allowfile = FALSE,
  checkindex = NA
)
}
\arguments{
\item{input}{vector}

\item{allowlist}{vector}

\item{allowlength}{numeric}

\item{allowsingle}{logical}

\item{allowfile}{logical}

\item{checkindex}{numeric, the index to verify if the list length is greater than one. Default is NA meaning all indices are verified.}
}
\value{
boolean true if successful check
}
\description{
It checks that filepaths are valid, but does not check whether the file contents are as required. I assume BayesACT does this?
}
\keyword{internal}
