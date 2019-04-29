#' @include RNAmodR.ML.R
NULL

# accessors --------------------------------------------------------------------

#' @rdname ModifierML-class
#' @export
setGeneric(
  name = "setMLModel<-",
  signature = c("x"),
  def = function(x, value)
    standardGeneric("setMLModel<-")
)
#' @rdname ModifierML-class
#' @export
setGeneric(
  name = "getMLModel",
  signature = c("x"),
  def = function(x)
    standardGeneric("getMLModel")
)
#' @rdname ModifierML-class
#' @export
setGeneric(
  name = "hasMLModel",
  signature = c("x"),
  def = function(x)
    standardGeneric("hasMLModel")
)

# data handling functions ------------------------------------------------------

#' @rdname ModifierMLModel-class
#' @export
setGeneric(
  name = "useModel",
  signature = c("x","y"),
  def = function(x, y)
    standardGeneric("useModel")
)
#' @rdname ModifierML-class
#' @export
setGeneric(
  name = "useMLModel",
  signature = c("x"),
  def = function(x)
    standardGeneric("useMLModel")
)

#' @rdname trainingData
#' @export
setGeneric(
  name = "trainingData",
  signature = c("x", "coord"),
  def = function(x, coord, ...)
    standardGeneric("trainingData")
)

