#' @include RNAmodR.ML.R
NULL

#' @name RNAmodR.ML-example
#' @aliases RNAmodR.ML.example aggregate_example calculate_correct_base_score
#'
#' @title RNAmodR.ML functions for example
#'
#' @description
#' The exported functions here are used in the vignette as examples. If you want
#' to reuse them, please implement them yourself. This will allow for fine
#' tuning on your side and does not create a depency to example functions, which
#' could change
#'
#' @param x a \code{ModifierML} object
#' @param minCoverage the minimum coverage for finding modifications
NULL

calculate_arrest_rate <- function(data){
  unlisted_data <- unlist(data, use.names = FALSE)
  rownames(unlisted_data) <- NULL
  numerator <- unlisted_data[,1,drop=FALSE]
  offsetAdd <- S4Vectors::DataFrame(as.list(rep(0,ncol(unlisted_data))))
  colnames(offsetAdd) <- colnames(unlisted_data)
  divisor <- IRanges::SplitDataFrameList(lapply(data,
                                                function(m){
                                                  m <- m[seq.int(2L,nrow(m)),,drop=FALSE]
                                                  rbind(m,offsetAdd)
                                                }))
  divisor <- unlist(divisor, use.names = FALSE)
  divisor <- divisor[,1,drop=FALSE]
  arrest <- (divisor[,1] - numerator[,1]) / divisor[,1]
  arrest[arrest < 0] <- -1
  arrest[is.infinite(arrest)] <- -1
  arrest[is.na(arrest)] <- -1
  arrest
}

calculate_mismatch_rate <- function(pileup, letters){
  pos <- seq_along(letters)
  letters[letters == "U"] <- "T"
  categorial <- table(seq_along(letters),letters)
  categorial <- as.data.frame(matrix(as.logical(categorial),
                                     ncol =  ncol(categorial),
                                     dimnames = list(NULL,
                                                     colnames(categorial))))
  categorial <- categorial[,colnames(pileup)]
  pileup <- as(pileup,"NumericList")
  pileup <- IRanges::NumericList(lapply(pileup,
                                        function(p){
                                          names(p) <- pos
                                          p
                                        }))
  scores <- unlist(unname(pileup[IRanges::LogicalList(categorial)]))
  # add N nucleotides values
  Npos <- pos[!(pos %in% names(scores))]
  N <- rep(0,length(Npos))
  names(N) <- Npos
  scores <- c(scores,N)
  #
  scores <- scores[order(as.integer(names(scores)))]
  scores[is.infinite(scores) | is.na(scores)] <- 0
  1 - unname(scores)
}

#' @rdname RNAmodR.ML-example
#' @export
aggregate_example <- function(x){
  # get the means. the sds arecurrently disregarded for this analysis
  mod <- aggregate(sequenceData(x), condition = "Treated")
  letters <- IRanges::CharacterList(strsplit(as.character(sequences(x)),""))
  letters <- unname(unlist(letters))
  coverage <- mod[["CoverageSequenceData"]][,"means.treated",drop=FALSE]
  colnames(coverage) <- "coverage"
  pileup <- unlist(mod[["PileupSequenceData"]])[,c("means.treated.G",
                                                     "means.treated.A",
                                                     "means.treated.T",
                                                     "means.treated.C")]
  pileup <- as.data.frame(pileup)
  pileup <- S4Vectors::DataFrame(pileup / rowSums(pileup))
  colnames(pileup) <- c("G","A","T","C")
  rownames(pileup) <- NULL
  pileup[is.na(pileup$G),] <- 0
  arrest <- calculate_arrest_rate(coverage)
  mismatch <- calculate_mismatch_rate(pileup,letters)
  scores <- S4Vectors::DataFrame(arrest = arrest, mismatch = mismatch)
  baseInfo <- S4Vectors::DataFrame("base" = letters)
  ans <- cbind(baseInfo,unlist(coverage,use.names = FALSE),pileup,scores)
  # resample to two positions in front and two positions behind
  offset <- ans[1,]
  offset$base <- "N"
  offset[,seq.int(2,ncol(offset))] <- 0
  offsetCol <- c("base","arrest")
  offset <- offset[,offsetCol]
  ans <- S4Vectors::DataFrame(
    "d2" = rbind(offset,offset,ans[seq_len(nrow(ans) - 2L),offsetCol]),
    "d1" = rbind(offset,ans[seq_len(nrow(ans) - 1L),offsetCol]),
    ans,
    "u1" = rbind(ans[seq.int(from = 2L, to = nrow(ans)),offsetCol],offset),
    "u2" = rbind(ans[seq.int(from = 3L, to = nrow(ans)),offsetCol],offset,offset))
  # covert to numeric values
  baseLevels <- c("G","A","U","C","N")
  ans$u2.base <- as.numeric(factor(ans$u2.base, levels = baseLevels))
  ans$u1.base <- as.numeric(factor(ans$u1.base, levels = baseLevels))
  ans$base <- as.numeric(factor(ans$base, levels = baseLevels))
  ans$d1.base <- as.numeric(factor(ans$d1.base, levels = baseLevels))
  ans$d2.base <- as.numeric(factor(ans$d2.base, levels = baseLevels))
  # set default score
  ans$score <- 0
  # relist and return
  ans <- relist(ans, mod[[1]])
  rownames(ans) <- rownames(mod[[1]])
  ans
}

get_D_score <- function(data){
  list(score = data$score)
}

#' @rdname RNAmodR.ML-example
#' @export
find_mod_example <- function(x, minCoverage){
  baseLevels <- c("G","A","U","C","N")
  data <- getAggregateData(x)
  unlisted_data <- unlist(data)
  unlisted_correctBase <- unlisted_data$base == which(baseLevels == "U")
  unlisted_validCoverage <- unlisted_data$coverage >= minCoverage
  unlisted_validScore <- unlisted_data$score >= 0.8
  correctBase <- relist(unlisted_correctBase,data)
  validCoverage <- relist(unlisted_validCoverage,data)
  validScore <- relist(unlisted_validScore,data)
  valid <- correctBase & validCoverage & validScore
  grl <- ranges(x)
  modifications <- mapply(
    function(d,v,r){
      d <- d[v,,drop=FALSE] # coverage check
      if(nrow(d) == 0L) return(NULL)
      ans <- RNAmodR:::constructModRanges(range = r, data = d, modType = "D",
                                          scoreFun = RNAmodR.ML:::get_D_score,
                                          source = "RNAmodR.ML",
                                          type = "RNAMOD")
      ans
    },
    data,
    valid,
    grl,
    SIMPLIFY = FALSE)
  f <- !vapply(modifications, is.null, logical(1))
  modifications <- mapply(
    function(m,name){
      m$Parent <- name
      m
    },
    modifications[f],
    names(grl)[f],
    SIMPLIFY = FALSE)
  modifications <- GenomicRanges::GRangesList(modifications)
  unname(unlist(modifications))
}