
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
  me <-  ModMLExample(files, annotation, sequences)
  expect_s4_class(me,"ModMLExample")
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
