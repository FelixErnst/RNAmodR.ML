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
#' @slot modelFile a \code{character} vector of length == 1L, which describes
#' a model to load via \code{\link[keras:save_model_hdf5]{load_model_hdf5}}.
#' The model is then stored in the \code{model} slot.
#'
#' @seealso \code{\link[=ModifierMLModel]{ModifierMLModel}}
NULL

#' @rdname ModifierMLkeras-class
#' @export
setClass("ModifierMLkeras",
         contains = c("ModifierMLModel"),
         slots = c(modelFile = "character",
                   abstraction = "character"))

#' @importFrom keras load_model_hdf5
.load_keras_model <- function(modelFile){
  keras::load_model_hdf5(modelFile)
}

.is_keras_model <- function(model){
  return(TRUE)
}

#' @importFrom keras model_to_yaml
setMethod("initialize",
          signature = "ModifierMLkeras",
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
                   "actual ModifierMLkeras class or given as Keras model.")
            }
            .Object@abstraction <- abstraction
            .Object@model <- model
            .Object
          }
)

# functions --------------------------------------------------------------------

#' @rdname ModifierMLkeras-class
#' @export
setMethod(f = "useModel",
          signature = signature(x = "ModifierMLkeras", y = "ModifierML"),
          definition =
            function(x, y){
              data <- getAggregateData(y)
              model <- x@model
              if(!is(model,"keras.engine.training.Model")){
                stop("model is not a keras model")
              }
              unlisted_data <- unlist(data, use.names = FALSE)

              browser()

              unlisted_data[,mainScore(y)] <- 0
              ans <- relist(unlisted_data,data)
              ans
            }
)
