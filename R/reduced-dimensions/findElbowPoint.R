findElbowPoint <- function(x) {

  require(PCAtools)

  dim <- reducedDim(x, "PCA")

  var <- attr(dim, "percentVar")

  num <- PCAtools::findElbowPoint(var)

}
