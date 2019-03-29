#' @include RNAmodR.ML.R
#' @include AllGenerics.R
NULL

#' @name ModifierMLModel-class
#' @aliases ModifierMLModel
#'
#' @title ModifierMLModel virtual class
#'
#' @description
#' Teh \code{ModifierMLModel} is a virtual class and is used for representing
#' different types of machine learning models used in the detection of
#' post transcriptional modifiations in RNA sequencing data.
#'
#' The next class inheriting from here should only implement a certain type
#' of model and also be virtual. The grand child of the \code{ModifierMLModel}
#' class should than implement a specific model for detecting certain types
#' of modifications.
#'
#' @seealso \code{\link[=ModifierMLKeras-class]{ModifierMLKeras}}
#'
#' @slot model a machine learning object of any type
#' @slot modelFile a file representation of the model
NULL

#' @rdname ModifierMLModel-class
#' @export
setClass("ModifierMLModel",
         contains = c("VIRTUAL"),
         slots = c("model" = "ANY",
                   "modelFile" = "character"))


