#' @include RNAmodR.ML.R
#' @include AllGenerics.R
#' @include ModifierML-class.R
NULL

#' @name trainingData
#'
#' @title Assemble training data from aggregate sequence data
#'
#' @description
#' \code{trainingData} subsets the aggregate data of a \code{ModifierML} object
#' to positions as defined by \code{coord}. Positions with an entry in the
#' \code{mod} column are labeled \code{TRUE}.
#'
#' @seealso For more details have a look at
#' \code{\link[RNAmodR:subsetByCoord]{subsetByCoord}}.
#'
#' @param x a \code{ModifierML} object
#' @param coord a \code{GRanges} or a \code{GRangesList} object
#' @param ... See \code{\link[RNAmodR:subsetByCoord]{subsetByCoord}}
#'
#' @return a \code{CompressedSplitDataFrameList} with aggregate data and an
#' addition label column.
#'
#' @export
#'
#' @examples
#' data("dmod",package = "RNAmodR.ML")
#' data("me",package = "RNAmodR.ML")
#' nextUPos <- function(gr){
#'   grl <- lapply(seq.int(1,3),
#'                 function(i){
#'                   pos <- start(dmod) + 2L +
#'                     vapply(strsplit(as.character(subseq(sequences(me)[dmod$Parent],
#'                                                         start(dmod)+3L)),""),
#'                            function(y){which(y == "U")[i]},integer(1))
#'                   nextU <- dmod
#'                   ranges(nextU) <- IRanges(start = pos, width = 1L)
#'                   nextU
#'                 })
#'   grl <- GRangesList(grl)
#'   gr <- unlist(grl)
#'   gr$mod <- NULL
#'   unique(gr)
#' }
#' nondmod <- nextUPos(dmod)
#' coord <- c(dmod,nondmod)
#' trainingData(me,coord)
NULL

.get_training_data <- function(x, coord, ...){
  labeledData <- labelByCoord(x, coord)
  unlisted_ld <- unlist(labeledData, use.names = FALSE)
  unlisted_ld <-
    unlisted_ld[,!(colnames(unlisted_ld) %in% mainScore(x))]
  labeledData <- relist(unlisted_ld, labeledData)
  subsetByCoord(labeledData, coord, type = NA)
}

#' @rdname trainingData
#' @export
setMethod("trainingData",
          signature = c("ModifierML", "GRanges"),
          function(x, coord, ...){
            .get_training_data(x, coord, ...)
          }
)
#' @rdname trainingData
#' @export
setMethod("trainingData",
          signature = c("ModifierML", "GRangesList"),
          function(x, coord, ...){
            .get_training_data(x, coord, ...)
          }
)
