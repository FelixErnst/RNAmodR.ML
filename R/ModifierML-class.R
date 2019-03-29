#' @include RNAmodR.ML.R
#' @include AllGenerics.R
#' @include ModifierMLModel-class.R
NULL

#' @name ModifierML-class
#' @aliases ModifierML
#'
#' @title The ModifierML class
#'
#' @description
#' The \code{ModifierML} class is a virtual class, which provides the central
#' functionality for searching with a machine learning models for patterns of
#' post-transcriptional RNA modifications in high throughput sequencing data.
#'
#' It extends the virtual \code{Modifier} class form the \code{RNAmodR} package
#' and add the \code{useMLModel} function. If not called  directly for a
#' \code{ModifierML} class, the \code{useMLModel} will be called from the
#' \code{aggregate} function.
#'
#' @slot mlModel a \code{character} describing a class name for creating a
#' \code{ModifierMLModel} object or a \code{ModifierMLModel} object itself.
#' If \code{mlModel} is a \code{character}, the class will tried to be create
#' by calling a function of the same name.
#'
#' @return a \code{ModifierML} object of type 'className'
#'
#' @seealso \code{\link[RNAmodR:Modifier-class]{Modifier}}
NULL

setClassUnion("character_OR_ModifierMLModel",
              members = c("character","ModifierMLModel"))

#' @rdname ModifierML-class
#' @export
setClass("ModifierML",
         contains = c("Modifier","VIRTUAL"),
         slots = c(mlModel = "character_OR_ModifierMLModel"))

#' @importFrom keras model_to_yaml
setMethod("initialize",
          signature = "ModifierML",
          function(.Object, ...){
            if(is.character(.Object@mlModel) &
               !assertive::is_an_empty_string(.Object@mlModel)){
              .Object@mlModel <- .load_ModifierMLModel(.Object@mlModel, ...)
            }
            .Object
          }
)

.load_ModifierMLModel <- function(modelName, ...){
  FUN <- match.fun(modelName)
  FUN(...)
}


# functions --------------------------------------------------------------------

#' @rdname ModifierML-class
#' @export
setMethod("setMLModel",
          signature = c(x = "ModifierML", model = "ModifierMLModel"),
          function(x, model){
            x@mlModel <- model
            x
          })

#' @rdname ModifierML-class
#' @export
setMethod("getMLModel",
          signature = c(x = "ModifierML"),
          function(x){
            if(!is(x@mlModel,"ModifierMLModel")){
              mlModel <- .load_ModifierMLModel(x@mlModel)
            } else {
              mlModel <- x@mlModel
            }
            mlModel
          })

#' @rdname ModifierML-class
#' @export
setMethod("hasMLModel",
          signature = c(x = "ModifierML"),
          function(x){
            if(is(getMLModel(x),"ModifierMLModel")){
              return(TRUE)
            }
            return(FALSE)
          })

# modifier aggregate function --------------------------------------------------

#' @rdname ModifierML-class
#' @export
setMethod(f = "aggregate",
          signature = signature(x = "ModifierML"),
          definition =
            function(x, force = FALSE){
              if(missing(force)){
                force <- FALSE
              }
              assertive::assert_is_a_bool(force)
              if(!hasAggregateData(x) || force){
                x@aggregate <- aggregateData(x)
                if(hasMLModel(x)){
                  x@aggregate <- .check_aggregate_modifier(useMLModel(x), x)
                  x@aggregateValidForCurrentArguments <- TRUE
                } else {
                  settings(x) <- list(findMod = FALSE)
                }
              }
              x
            }
)

#' @rdname ModifierML-class
#' @export
setMethod(f = "useMLModel",
          signature = signature(x = "ModifierML"),
          definition =
            function(x){
              stop("This functions needs to be implemented by '",class(x),
                   "'.",call. = FALSE)
            }
)
