emptyDropsData <- function(x, y) {

  require("SingleCellExperiment")

  colData(x)$EmptyDropsLogProb = y$LogProb

  colData(x)$EmptyDropsPValue = y$PValue

  colData(x)$EmptyDropsLimited = y$Limited

  colData(x)$EmptyDropsFDR = y$FDR

  x

}

