#' @include RNAmodR.ML.R
NULL

# accessors --------------------------------------------------------------------

#' @rdname ModifierML-class
#' @export
setGeneric(
  name = "setMLModel",
  signature = c("x", "model"),
  def = function(x, model)
    standardGeneric("setMLModel")
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

#' @rdname ModifierML-class
#' @export
setGeneric(
  name = "useMLModel",
  signature = c("x"),
  def = function(x)
    standardGeneric("useMLModel")
)

#' @rdname assembleTrainingData
#' @export
setGeneric(
  name = "assembleTrainingData",
  signature = c("x", "coord"),
  def = function(x, coord)
    standardGeneric("assembleTrainingData")
)

