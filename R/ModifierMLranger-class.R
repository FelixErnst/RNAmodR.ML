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
#' @param x a \code{ModifierMLranger} object
#' @param y a \code{ModifierML} object
#'
#' @seealso \code{\link[=ModifierMLModel]{ModifierMLModel}}
#'
#' @return a \code{ModifierMLranger} object
#'
#' @examples
#' # example class derived from the virtual ModifierMLranger class
#' setClass("ModifierMLexample",
#'          contains = c("ModifierMLranger"))
#' ModifierMLexample <- function(...){
#'   new("ModifierMLexample")
#' }
#' mlmodel <- ModifierMLexample()
NULL

#' @rdname ModifierMLranger-class
#' @importFrom stats predict
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
