#' @title RNAmodR.ML
#'
#' @author Felix G M Ernst [aut]
#'
#' @description
#' To extend the functionality of the \code{RNAmodR} package and classical
#' detection strategies towards detection through machine learning models,
#' \code{RNAmodR.ML} provides classes and an example workflow.
#'
#' @seealso \code{\link[RNAmodR:RNAmodR]{RNAmodR}} package.
#'
#' @docType package
#' @name RNAmodR.ML
NULL

#' @import methods
#' @import RNAmodR
#' @import BiocGenerics
#' @import S4Vectors
#' @import IRanges
#' @import GenomicRanges
NULL

#' @name RNAmodR.ML-datasets
#' @title Example data in the RNAmodR.ML package
#' @description
#' This contains an example ModifierSet object
#' @docType data
#' 
#' @format a \code{GRanges} object containg D positions
#' @keywords datasets
#' @usage data(dmod)
"dmod"
#' @name RNAmodR.ML-datasets
#' @format a \code{GRanges} object containg m7G positions
#' @usage data(mod7)
"mod7"
#' @name RNAmodR.ML-datasets
#' @format a \code{ModMLExample} object for examples
#' @usage data(me)
"me"
#' @name RNAmodR.ML-datasets
#' @format a \code{ranger} object for examples
#' @usage data(model)
"model"
