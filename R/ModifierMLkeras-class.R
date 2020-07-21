#' @include ModifierMLModel-class.R
NULL

#' @name ModifierMLkeras-class
#' @aliases ModifierMLkeras
#'
#' @title ModifierMLkeras class
#'
#' @description
#' The \code{ModifierMLkeras} class extends the virtual class
#' \code{ModifierMLModel} and unifies the access to \code{Keras} machine
#' learning models used in the detection of post-transcriptional modifications
#' in RNA sequencing data. The \code{ModifierMLkeras} class is virtual itself
#' and must be extended from for each individual machine learning model.
#'
#' The \code{ModifierMLkeras} class extends the virtual class
#' \code{ModifierMLModel} and unifies the access to \code{Keras} machine
#' learning models used in the detection of post-transcriptional modifications
#' in RNA sequencing data. The \code{ModifierMLkeras} class is virtual itself
#' and must be extended from for each individual machine learning model.
#'
#' Since a stored model needs to be loaded from file, the additional slot
#' \code{modelFile} is used and can be accessed through a function of the same
#' name. Upon creation of a \code{ModifierMLkeras} object, the model is loaded
#' from file, if \code{modelFile} is not an empty character value and a valid
#' file name.
#'
#' @slot modelFile a \code{character} vector of length == 1L, which describes
#' a model to load via \code{\link[keras:save_model_hdf5]{load_model_hdf5}}.
#' The model is then stored in the \code{model} slot.
#'
#' @param x a \code{ModifierMLkeras} object
#' @param y a \code{ModifierML} object
#'
#' @seealso \code{\link[=ModifierMLModel]{ModifierMLModel}}
#'
#' @return a \code{ModifierMLkeras} object
NULL

#' @rdname ModifierMLkeras-class
#' @export
setClass("ModifierMLkeras",
         contains = c("ModifierMLModel"),
         slots = c(modelFile = "character"))

.load_keras_model <- function(modelFile){
  keras::load_model_hdf5(modelFile)
}

.is_keras_model <- function(model){
  is(model,"keras.engine.training.Model")
}

setMethod("initialize",
          signature = "ModifierMLkeras",
          function(.Object, ...){
            .Object <- callNextMethod()
            if(is.null(.Object@model) && !.is_empty(.Object@modelFile)){
              if(!file.exists(.Object@modelFile)){
                stop("File for existing model was not found at ",
                     .Object@modelFile, call. = FALSE)
              }
              .Object@model <- .load_keras_model(.Object@modelFile)
            }
            if(!.is_keras_model(.Object@model)){
              stop("Something went wrong. 'model' should be predefined by the ",
                   "actual ModifierMLkeras class through the `modelFile` slot ",
                   "or given as a Keras model during object creation.")
            }
            .Object
          }
)

# accessors --------------------------------------------------------------------

setGeneric("modelFile",
           signature = "x",
           function(x) standardGeneric("modelFile"))
setMethod("modelFile",
          signature = c("x" = "ModifierMLkeras"),
          function(x){x@modelFile})

# functions --------------------------------------------------------------------

#' @rdname ModifierMLkeras-class
#' @export
setMethod(f = "useModel",
          signature = signature(x = "ModifierMLkeras", y = "ModifierML"),
          definition =
            function(x, y){
              stop("This functions needs to be implemented by '",class(x),
                   "'.",call. = FALSE)
            }
)
