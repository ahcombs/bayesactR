% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_input_file.R
\name{agent}
\alias{agent}
\title{Write agent into input}
\usage{
agent(
  name,
  bayesact_dir,
  dict = "usfullsurveyor2015",
  dict_stat = "mean",
  dict_group = "all",
  eqns = "us2010",
  eqns_group = c("av", "female"),
  opt_args = ""
)
}
\arguments{
\item{name}{string agent name}

\item{bayesact_dir}{top level directory at which the bayesact code lives}

\item{dict}{string or length 4 vector, key(s) of an available dictionary OR filepaths OR data frames. If there are any file paths or data frames, it must be a length 4 vector.}

\item{dict_stat}{string or length 4 vector (\code{"mean"}, \code{"sd"}, or \code{"cov"})}

\item{dict_group}{string or length 4 vector (\code{"all"}, \code{"male"}, or \code{"female"})}

\item{eqns}{string or length 2 vector, key(s) of an available equation set OR filepath(s). First is group for impression ABO equation, second is group for emotion equation.}

\item{eqns_group}{string or length 2 vector (\code{"all"}, \code{"f"}, or \code{"m"}). First is group for impression ABO equation, second is group for emotion equation.}

\item{opt_args}{more agent-level parameters that may or may not be included}
}
\value{
file with inserted lines
}
\description{
Compile all necessary information for an individual agent and add it to a template object that can be written to a text file
}
\keyword{internal}
