plotUMAP <- function(x, name) {

  require(scater)

  d <- makePerCellDF(x)

  ggplot(d, aes(UMAP.1, UMAP.2)) +
    geom_point() +
    labs(x = "UMAP 1", y = "UMAP 2", title = name) +
    theme_minimal() +
    theme(aspect.ratio = 1)

}
