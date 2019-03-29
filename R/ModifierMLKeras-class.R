#' @include ModifierMLModel-class.R
NULL

#' @name ModifierMLKeras-class
#' @aliases ModifierMLKeras
#'
#' @title ModifierMLKeras class
#'
#' @description
#' The \code{ModifierMLKeras} class extends the virtual class
#' \code{ModifierMLModel} and unifies the access to \code{Keras} machine
#' learning models used in the detection of post-transcriptional modifications
#' in RNA sequencing data. The \code{ModifierMLKeras} class is virtual itself
#' and must be extended from for each individual machine learning model.
#'
#' @seealso \code{\link[=ModifierMLModel]{ModifierMLModel}}
NULL

#' @rdname ModifierMLModel-class
#' @export
setClass("ModifierMLKeras",
         contains = c("ModifierMLModel"))

#' @importFrom keras load_model_hdf5
.load_keras_model <- function(modelFile){
  keras::load_model_hdf5(modelFile)
}

.is_keras_model <- function(model){
  return(TRUE)
}

#' @importFrom keras model_to_yaml
setMethod("initialize",
          signature = "ModifierMLKeras",
          function(.Object, model){
            if(is.null(model)){
              if(!file.exists(object@modelFile)){
                stop("File for existing model was not found at ",
                     object@modelFile, call. = FALSE)
              }
              model <- .load_keras_model(object@modelFile)
              abstraction <- keras::model_to_yaml(model)
            } else if(.is_keras_model(model)) {
              abstraction <- keras::model_to_yaml(model)
            } else {
              stop("Something went wrong. 'model' should be predefined by the ",
                   "actual ModifierMLKeras class or given as Keras model.")
            }
            .Object@abstraction <- abstraction
            .Object@model <- model
            .Object
          }
)