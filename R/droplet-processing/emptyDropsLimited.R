emptyDropsLimited <- function(x, FDR = 0.001) {

  samples <- names(x)

  limited <- lapply(x, function(x) table(x$FDR < FDR, x$Limited))

  limited <- sapply(limited, function(x) x["FALSE", "TRUE"])

  data.frame(
    Sample = samples,
    Limited = limited
  )

}
