plotTSNE <- function(x, name) {

  require(scater)

  d <- makePerCellDF(x)

  ggplot(d, aes(TSNE.1, TSNE.2)) +
    geom_point() +
    labs(x = "TSNE 1", y = "TSNE 2", title = name) +
    theme_minimal() +
    theme(aspect.ratio = 1)

}
