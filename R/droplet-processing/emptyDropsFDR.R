emptyDropsFDR <- function(x, FDR = 0.001) {

  stopifnot("EmptyDropsFDR" %in% colnames(colData(x)))

  x[, which(x$EmptyDropsFDR < FDR)]

}
