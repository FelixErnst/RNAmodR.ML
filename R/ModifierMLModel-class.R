#' @include RNAmodR.ML.R
#' @include AllGenerics.R
NULL

#' @name ModifierMLModel-class
#' @aliases ModifierMLModel
#'
#' @title ModifierMLModel virtual class
#'
#' @description
#' The \code{ModifierMLModel} is a virtual class and is used for representing
#' different types of machine learning models used in the detection of
#' post transcriptional modifiations in RNA sequencing data.
#'
#' The next class inheriting from here should only implement a certain type
#' of model and also be virtual. The grand child of the \code{ModifierMLModel}
#' class should than implement a specific model for detecting certain types
#' of modifications.
#'
#' @seealso \code{\link[=ModifierMLranger-class]{ModifierMLranger}}
#' \code{\link[=ModifierMLkeras-class]{ModifierMLkeras}}
#'
#' @slot model a machine learning object of any type
#'
#' @param x a \code{ModifierMLModel} object
#' @param y See \code{\link[=ModifierMLranger-class]{ModifierMLranger}} for an
#' example
#'
#' @return a \code{ModifierMLModel} object
#'
#' @seealso \code{\link{ModifierMLranger}} for an example implementation
#'
#' @examples
#' # an example implementation of a ModifierMLModel object using the
#' # ModifierMLModel derived class ModifierMLranger
#' setClass("ModifierMLexample",
#'          contains = c("ModifierMLranger"))
#' ModifierMLexample <- function(...){
#'   new("ModifierMLexample")
#' }
NULL

#' @rdname ModifierMLModel-class
#' @export
setClass("ModifierMLModel",
         contains = c("VIRTUAL"),
         slots = c(model = "ANY"))

# function ---------------------------------------------------------------------

#' @rdname ModifierMLModel-class
#' @export
setMethod(f = "useModel",
          signature = signature(x = "ModifierMLModel", y = "ANY"),
          definition =
            function(x, y){
              stop("This functions needs to be implemented by '",class(x),
                   "'.",call. = FALSE)
            }
)
