#' @include RNAmodR.ML.R
#' @include AllGenerics.R
#' @include ModifierML-class.R
NULL

#' @name assembleTrainingData
#'
#' @title Assemble training data from aggregate sequence data
#'
#' @description
#' title
#'
#'
#' @return
#' @export
#'
#' @examples
#' ModifierMLKeras()
NULL

#' @rdname assembleTrainingData
#' @export
setMethod("assembleTrainingData",
          signature = c("ModifierML", "GRanges"),
          function(x, coord){
            browser()

            data <- NULL
            data
          }
)
#' @rdname assembleTrainingData
#' @export
setMethod("assembleTrainingData",
          signature = c("ModifierML", "GRangesList"),
          function(x, coord){
            browser()

            data <- NULL
            data
          }
)
