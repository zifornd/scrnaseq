cycloneTable <- function(x, FDR = 0.001) {

  data.frame(
    Sample = names(x),
    G1     = sapply(x, function(x) sum(x$Phase == "G1")),
    S      = sapply(x, function(x) sum(x$Phase == "S")),
    G2M    = sapply(x, function(x) sum(x$Phase == "G2M")),
    Total  = sapply(x, ncol),
    row.names = NULL
  )

}
