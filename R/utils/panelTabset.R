panelTabset <- function(x, f, ..., level = "#") {

  for (n in names(x)) {

    cat(level, n, "\n")

    print(f(x[[n]], ...))

    cat("\n\n")

  }

}
