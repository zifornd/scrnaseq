plotElbowPoint <- function(x) {

  require(scales)

  num <- findElbowPoint(x)

  dim <- reducedDim(x, "PCA")

  var <- attr(dim, "percentVar")

  dat <- data.frame(x = seq_along(var), y = var)

  ggplot(dat, aes(x, y)) +
    geom_point() +
    geom_vline(xintercept = num, colour = "#E41A1C") +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    labs(x = "PC", y = "Variance") +
    theme_minimal()

}
