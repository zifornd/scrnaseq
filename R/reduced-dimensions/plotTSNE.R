plotTSNE <- function(x, name) {

  require(ggforce)

  require(scater)

  d <- makePerCellDF(x)

  ggplot(d, aes(TSNE.1, TSNE.2)) +
    geom_point() +
    labs(x = "TSNE 1", y = "TSNE 2", title = name) +
    theme_no_axes() +
    theme(aspect.ratio = 1)

}
