wrapPlots <- function(x, f, ...) {

  p <- vector(mode = "list", length = length(x))

  names(p) <- names(x)

  for (n in names(x)) {

    p[[n]] <- f(x[[n]], ...) + ggtitle(n)

  }

  p

}
