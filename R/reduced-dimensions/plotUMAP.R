plotUMAP <- function(x, name) {

  require(ggforce)

  require(scater)

  d <- makePerCellDF(x)

  ggplot(d, aes(UMAP.1, UMAP.2)) +
    geom_point() +
    labs(x = "UMAP 1", y = "UMAP 2", title = name) +
    theme_no_axes() +
    theme(aspect.ratio = 1)

}
