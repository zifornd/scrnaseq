addRowData <- function(x, value, label = "") {
  rowData(x)[[label]] <- value
  x
}
