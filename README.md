# RNAmodR.ML

Post-transcriptional modifications can be found abundantly in rRNA and tRNA and
can be detected classically via several strategies. However, difficulties arise
if the identity and the position of the modified nucleotides is to be determined
at the same time. Classically, a primer extension, a form of reverse
transcription (RT), would allow certain modifications to be accessed by blocks
during the RT changes or changes in the cDNA sequences. Other modification would
need to be selectively treated by chemical reactions to influence the outcome of
the reverse transcription.

With the increased availability of high throughput sequencing, these classical
methods were adapted to high throughput methods allowing more RNA molecules to
be accessed at the same time. However, patterns of some modification cannot be
detected by accessing small number of parameters. For these cases machine 
learning models can be trained on data from positions known to be modified
in order to detect additional modified positions.

``RNAmodR.ML` implements additional classes from the base package `RNAmodR`
to train and use machine learning models. The package contains an example 
workflow for random forest models with the `ranger` package 
([Wright & Ziegler 2017](#Literature)). In addition classes for using
deep learning models generated with the `keras` package are also implemented
([Allaire & Chollet 2018](#Literature)). Classes for other machine learning
models can also be easily implemented.

# Installation

The current version of the RNAmodR.Data package is available from GitHub.

```
remotes::install_github("FelixErnst/RNAmodR.Data")
remotes::install_github("FelixErnst/RNAmodR")
remotes::install_github("FelixErnst/RNAmodR.ML")
#
library(RNAmodR.ML)
```

A submission to Bioconductor is planned.

# Introduction

The `ModifierML` class extends the `Modifier` class from the `RNAmodR` package
and adds one slot, `mlModel`, a getter/setter `getMLModel`/`setMLModel`, an 
additional `useMLModel` function. 

For different types of models `ModifierMLModel` derived classes are available, 
which currently are:

* `ModifierMLranger` for models generated with the `ranger` package
([Wright & Ziegler 2017](#Literature))
* `ModifierMLkeras` for models generated with the `keras` package 
([Allaire & Chollet 2018](#Literature))

An trained model can be used to create a `ModifierMLModel` object. The generated
`ModifierMLModel` object can then be set for the `ModifierML` object using the 
`setMLModel` function.

For more details, please have a look at the vignette.

# Literature

- Marvin N. Wright and Andreas Ziegler (2017): "ranger: A Fast Implementation of
Random Forests for High Dimensional Data in C++ and R". Journal of Statistical 
Software 77 (1): 1-17. https://doi.org/10.18637/jss.v077.i01

- JJ Allaire and François Chollet (2018): "keras: R Interface to 'Keras'". 
R package version 2.2.4 (https://CRAN.R-project.org/package=keras)