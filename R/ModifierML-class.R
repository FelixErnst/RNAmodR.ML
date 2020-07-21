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
#' and add the \code{useMLModel} function. If not called directly for a
#' \code{ModifierML} class, the \code{useMLModel} will be called from the
#' \code{aggregate} function.
#'
#' The slot \code{mlModel} is added and serves a dual purpose. If \code{mlModel}
#' is a character, a class of the type \code{ModifierMLModel} is created upon
#' creation of a \code{ModifierML} object. However, for developing purposes
#' the slot can also remain empty and a \code{ModifierMLModel} object can be set
#' using the \code{setMLModel} function and retrieved using the
#' \code{getMLModel}. If the \code{mlModel} slot is empty, the \code{findMod}
#' setting will be set to \code{FALSE} and the \code{ModifierML} object will
#' be returned just with the aggregate data. Such an object can then be used to
#' train a machine learning model. The data can be accessed using
#' \code{\link{trainingData}}.
#'
#' @slot mlModel a \code{character} describing a class name for creating a
#' \code{ModifierMLModel} object or a \code{ModifierMLModel} object itself.
#' If \code{mlModel} is a \code{character}, the class will tried to be create
#' by calling a function of the same name.
#'
#' @param x a \code{ModifierML} object.
#' @param value a \code{ModifierMLModel} object
#' @param force whether to recreate the aggregated data, if it is already stored
#' inside the \code{Modifier} object.
#'
#' @return a \code{ModifierML} object
#'
#' @seealso \code{\link[RNAmodR:Modifier-class]{Modifier}}
#'
#' @examples
#' # an example implementation of a ModifierML object
#' setClass("ModMLExample",
#'          contains = c("RNAModifierML"),
#'          prototype = list(mod = c("D"),
#'                           score = "score",
#'                           dataType = c("PileupSequenceData",
#'                                        "CoverageSequenceData"),
#'                           mlModel = character(0)))
#' # constructor function for ModMLExample
#' ModMLExample <- function(x, annotation = NA, sequences = NA, seqinfo = NA,
#'                          ...){
#'   RNAmodR:::Modifier("ModMLExample", x = x, annotation = annotation,
#'                      sequences = sequences, seqinfo = seqinfo, ...)
#' }
NULL

setClassUnion("character_OR_ModifierMLModel",
              members = c("character","ModifierMLModel"))

#' @rdname ModifierML-class
#' @export
setClass("ModifierML",
         contains = c("Modifier","VIRTUAL"),
         slots = c(mlModel = "character_OR_ModifierMLModel"))

setMethod("initialize",
          signature = "ModifierML",
          function(.Object, ...){
            .Object <- callNextMethod()
            if(is.character(.Object@mlModel)){
              .Object@mlModel <-
                .load_ModifierMLModel(modelName = .Object@mlModel, ...)
            }
            .Object
          }
)

.load_ModifierMLModel <- function(modelName, ...){
  if(.is_empty(modelName)){
    return(modelName)
  }
  FUN <- match.fun(modelName)
  FUN(...)
}

# show -------------------------------------------------------------------------

setMethod(
  f = "show",
  signature = signature(object = "ModifierML"),
  definition = function(object) {
    cat("A", class(object), "object containing",dataType(object),
        "with",length(object@data),"elements.\n")
    model <- class(object@mlModel)
    if(.is_empty(model)){
      cat("| ML model: not set\n")
    } else {
      cat("| ML model: ",model,"\n")
    }
    files <- BiocGenerics::path(object@bamfiles)
    cat("| Input files:\n",paste0("  - ",names(files),": ",files,"\n"))
    cat("| Modification type(s): ",paste0(object@mod, collapse = " / "),"\n")
    cat("| Modifications found:",ifelse(length(object@modifications) != 0L,
                                        paste0("yes (",
                                               length(object@modifications),
                                               ")"),
                                        "no"),"\n")
    cat("| Settings:\n")
    settings <- settings(object)
    settings <- lapply(settings,
                       function(s){
                         if(length(s) > 1L){
                           ans <- S4Vectors::List(s)
                           return(ans)
                         }
                         s
                       })
    settings <- S4Vectors::DataFrame(settings)
    RNAmodR:::.show_settings(settings)
    valid <- c(validAggregate(object), validModification(object))
    if(!all(valid) && hasMLModel(object)){
      warning("Settings were changed after data aggregation or modification ",
              "search. Rerun with modify(x,force = TRUE) to update with ",
              "current settings.", call. = FALSE)
    }
  }
)


# functions --------------------------------------------------------------------

#' @rdname ModifierML-class
#' @export
setReplaceMethod("setMLModel",
          signature = c(x = "ModifierML"),
          function(x, value){
            if(!is(value,"ModifierMLModel")){
              stop("'value' must be object of class 'ModifierMLModel'")
            }
            x@mlModel <- value
            x
          })

#' @rdname ModifierML-class
#' @export
setMethod("getMLModel",
          signature = c(x = "ModifierML"),
          function(x){
            if(.is_empty(x@mlModel)){
              return(NULL)
            }
            class <- class(x@mlModel)
            tryRes <- try(getClass(class))
            if(is(tryRes,"try-error")){
              stop("Class '",class,"' not defined.", call. = FALSE)
            }
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
              if(!.is_a_bool(force)){
                stop("'force' must be TRUE or FALSE.", call. = FALSE)
              }
              if(!hasAggregateData(x) || force){
                x@aggregate <- aggregateData(x)
                if(hasMLModel(x)){
                  x <- useMLModel(x)
                  x@aggregateValidForCurrentArguments <- TRUE
                } else {
                  settings(x) <- list(findMod = FALSE)
                }
                x@aggregate <-
                  RNAmodR:::.check_aggregate_modifier(x@aggregate, x)
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
              stop("The 'useMLModel' functions needs to be implemented by '",
                   class(x), "'.",call. = FALSE)
            }
)

#' @rdname ModifierML-class
#' @export
setMethod(f = "modify",
          signature = signature(x = "ModifierML"),
          definition =
            function(x, force = FALSE){
              if(hasMLModel(x)){
                x <- callNextMethod()
              } else {
                warning("ML model not set. Skipped search for ",
                        "modifications ...",
                        call. = FALSE)
              }
              x
            }
)

# RNAModifierML and DNAModifierML ----------------------------------------------

#' @rdname ModifierML-class
#' @export
setClass("RNAModifierML",
         contains = c("VIRTUAL","ModifierML"),
         prototype = list(seqtype = seqtype(Biostrings::RNAString())))

#' @rdname ModifierML-class
#' @export
setClass("DNAModifierML",
         contains = c("VIRTUAL","ModifierML"),
         prototype = list(seqtype = seqtype(Biostrings::DNAString())))
