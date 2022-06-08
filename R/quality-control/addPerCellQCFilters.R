addPerCellQCFilters <- function(x, ...) {
  colData(x) <- cbind(colData(x), perCellQCFilters(x, ...))
  x
}
