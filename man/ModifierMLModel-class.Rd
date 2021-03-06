% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/ModifierMLModel-class.R
\docType{class}
\name{useModel}
\alias{useModel}
\alias{ModifierMLModel-class}
\alias{ModifierMLModel}
\alias{useModel,ModifierMLModel,ANY-method}
\title{ModifierMLModel virtual class}
\usage{
useModel(x, y)

\S4method{useModel}{ModifierMLModel,ANY}(x, y)
}
\arguments{
\item{x}{a \code{ModifierMLModel} object}

\item{y}{See \code{\link[=ModifierMLranger-class]{ModifierMLranger}} for an
example}
}
\value{
a \code{ModifierMLModel} object
}
\description{
The \code{ModifierMLModel} is a virtual class and is used for representing
different types of machine learning models used in the detection of
post transcriptional modifiations in RNA sequencing data.

The next class inheriting from here should only implement a certain type
of model and also be virtual. The grand child of the \code{ModifierMLModel}
class should than implement a specific model for detecting certain types
of modifications.
}
\section{Slots}{

\describe{
\item{\code{model}}{a machine learning object of any type}
}}

\examples{
# an example implementation of a ModifierMLModel object using the
# ModifierMLModel derived class ModifierMLranger
setClass("ModifierMLexample",
         contains = c("ModifierMLranger"))
ModifierMLexample <- function(...){
  new("ModifierMLexample")
}
}
\seealso{
\code{\link[=ModifierMLranger-class]{ModifierMLranger}}
\code{\link[=ModifierMLkeras-class]{ModifierMLkeras}}

\code{\link{ModifierMLranger}} for an example implementation
}
