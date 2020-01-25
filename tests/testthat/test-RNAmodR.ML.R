
context("ModifierML")
test_that("ModifierML:",{
  library(RNAmodR.Data)
  library(rtracklayer)
  setClass("ModMLExample",
           contains = c("RNAModifierML"),
           prototype = list(mod = c("D"),
                            score = "score",
                            dataType = c("PileupSequenceData",
                                         "CoverageSequenceData"),
                            mlModel = character(0)))
  # constructor function for ModMLExample
  ModMLExample <- function(x, annotation = NA, sequences = NA, seqinfo = NA,
                           ...){
    RNAmodR:::Modifier("ModMLExample", x = x, annotation = annotation,
                       sequences = sequences, seqinfo = seqinfo, ...)
  }
  setMethod(
    f = "aggregateData",
    signature = signature(x = "ModMLExample"),
    definition =
      function(x){
        aggregate_example(x)
      }
  )
  annotation <- GFF3File(RNAmodR.Data.example.gff3())
  sequences <- RNAmodR.Data.example.fasta()
  files <- c(treated = RNAmodR.Data.example.bam.1(),
             treated = RNAmodR.Data.example.bam.2(),
             treated = RNAmodR.Data.example.bam.3())
  expect_warning(me <-  ModMLExample(files, annotation, sequences),
                 "ML model not set. Skipped ")
  expect_s4_class(me,"ModMLExample")
  expect_false(hasMLModel(me))
  # show
  expect_output(show(me))
  #
  data(model, package = "RNAmodR.ML")
  expect_error(setMLModel(me) <- model,
               "'value' must be object of class 'ModifierMLModel'")
  #
  expect_equal(getMLModel(me),NULL)
  setClass("ModifierMLexample",
           contains = c("ModifierMLranger"),
           prototype = list(model = model))
  #
  mlmodel <- new("ModifierMLexample")
  expect_s4_class(setMLModel(me) <- mlmodel,"ModifierMLexample")
  expect_s4_class(me,"ModMLExample")
  expect_true(hasMLModel(me))
  expect_s4_class(getMLModel(me),"ModifierMLexample")
  #
  data(me, package = "RNAmodR.ML")
  actual <- useModel(getMLModel(me),me)
  expect_s4_class(actual,"NumericList")
  expect_named(actual)
  expect_error(useMLModel(me),
               "The 'useMLModel' functions needs to be implemented")
  setMethod(f = "useMLModel",
            signature = signature(x = "ModMLExample"),
            definition =
              function(x){
                predictions <- useModel(getMLModel(x), x)
                data <- getAggregateData(x)
                unlisted_data <- unlist(data, use.names = FALSE)
                unlisted_data$score <- unlist(predictions)
                x@aggregate <- relist(unlisted_data,data)
                x
              })
  actual2 <- useMLModel(me)
  expect_s4_class(actual2,"ModMLExample")
  # training Data
  data(dmod, package = "RNAmodR.ML")
  actual <- trainingData(me,dmod)
  expect_s4_class(actual,"CompressedSplitDFrameList")
  expect_named(actual)
  expect_equal(colnames(actual[[1]]),
               c("d2.base","d2.arrest","d1.base","d1.arrest","base","coverage",
                 "G","A","T","C","arrest","mismatch","u1.base","u1.arrest",
                 "u2.base","u2.arrest","labels"))
  # findMod
  expect_error(modify(me),
               "The 'findMod' functions needs to be implemented by")
  setMethod(
    f = "findMod",
    signature = signature(x = "ModMLExample"),
    definition =
      function(x){
        find_mod_example(x, 25L)
      }
  )
  actual <- modify(me)
  expect_s4_class(actual,"ModMLExample")
  expect_s4_class(modifications(actual),"GRanges")
})
context("ModifierMLModel")
test_that("ModifierMLModel:",{
  setClass("ModifierMLexample",
           contains = c("ModifierMLranger"))
  ModifierMLexample <- function(...){
    new("ModifierMLexample")
  }
  mlmodel <- ModifierMLexample()
  expect_s4_class(mlmodel,"ModifierMLexample")
})
