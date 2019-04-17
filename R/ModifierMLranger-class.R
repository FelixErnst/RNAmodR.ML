#' @include ModifierMLModel-class.R
NULL

#' @name ModifierMLranger-class
#' @aliases ModifierMLranger
#'
#' @title ModifierMLranger class
#'
#' @description
#' The \code{ModifierMLranger} class extends the virtual class
#' \code{ModifierMLModel} and unifies the access to
#' \code{\link[ranger:ranger]{ranger}} machine learning models used in the
#' detection of post-transcriptional modifications in RNA sequencing data. The
#' \code{ModifierMLranger} class is virtual itself and must be extended from for
#' each individual machine learning model.
#'
#' @seealso \code{\link[=ModifierMLModel]{ModifierMLModel}}
NULL

#' @rdname ModifierMLranger-class
#' @import ranger
#' @export
setClass("ModifierMLranger",
         contains = c("ModifierMLModel"))

# functions --------------------------------------------------------------------

#' @rdname ModifierMLranger-class
#' @export
setMethod(f = "useModel",
          signature = signature(x = "ModifierMLranger", y = "ModifierML"),
          definition =
            function(x, y){
              data <- getAggregateData(y)
              model <- x@model
              if(!is(model,"ranger")){
                stop("model is not a ranger model")
              }
              unlisted_data <- unlist(data, use.names = FALSE)
              p <- predict(x@model, data.frame(unlisted_data))
              relist(p$predictions,data)
            }
)
