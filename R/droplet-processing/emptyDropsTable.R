emptyDropsTable <- function(x, FDR = 0.001) {

  samples <- names(x)

  cells <- sapply(x, function(x) sum(x$FDR < FDR, na.rm = TRUE))

  empty <- sapply(x, function(x) sum(is.na(x$FDR)) + sum(x$FDR >= FDR, na.rm = TRUE))

  total <- sapply(x, function(x) length(x$FDR))

  data.frame(
    Sample = samples,
    Cell = cells,
    Empty = empty,
    Total = total
  )

}
