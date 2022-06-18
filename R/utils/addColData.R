addColData <- function(x, value, label = "") {
  colData(x)[[label]] <- value
  x
}
