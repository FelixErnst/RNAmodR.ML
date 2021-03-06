% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModifierMLkeras-class.R
\docType{class}
\name{ModifierMLkeras-class}
\alias{ModifierMLkeras-class}
\alias{ModifierMLkeras}
\alias{useModel,ModifierMLkeras,ModifierML-method}
\title{ModifierMLkeras class}
\usage{
\S4method{useModel}{ModifierMLkeras,ModifierML}(x, y)
}
\arguments{
\item{x}{a \code{ModifierMLkeras} object}

\item{y}{a \code{ModifierML} object}
}
\value{
a \code{ModifierMLkeras} object
}
\description{
The \code{ModifierMLkeras} class extends the virtual class
\code{ModifierMLModel} and unifies the access to \code{Keras} machine
learning models used in the detection of post-transcriptional modifications
in RNA sequencing data. The \code{ModifierMLkeras} class is virtual itself
and must be extended from for each individual machine learning model.

The \code{ModifierMLkeras} class extends the virtual class
\code{ModifierMLModel} and unifies the access to \code{Keras} machine
learning models used in the detection of post-transcriptional modifications
in RNA sequencing data. The \code{ModifierMLkeras} class is virtual itself
and must be extended from for each individual machine learning model.

Since a stored model needs to be loaded from file, the additional slot
\code{modelFile} is used and can be accessed through a function of the same
name. Upon creation of a \code{ModifierMLkeras} object, the model is loaded
from file, if \code{modelFile} is not an empty character value and a valid
file name.
}
\section{Slots}{

\describe{
\item{\code{modelFile}}{a \code{character} vector of length == 1L, which describes
a model to load via \code{\link[keras:save_model_hdf5]{load_model_hdf5}}.
The model is then stored in the \code{model} slot.}
}}

\seealso{
\code{\link[=ModifierMLModel]{ModifierMLModel}}
}
