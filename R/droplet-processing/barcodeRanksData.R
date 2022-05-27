barcodeRanksData <- function(x, y) {

  require("SingleCellExperiment")

  colData(x)$BarcodeRank = y$rank

  colData(x)$BarcodeTotal = y$total

  colData(x)$BarcodeFitted = y$fitted

  x

}

