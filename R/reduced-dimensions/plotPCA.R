plotPCA <- function(x, name) {

  require(scater)

  d <- makePerCellDF(x)

  ggplot(d, aes(PCA.1, PCA.2)) +
    geom_point() +
    labs(x = "PC1", y = "PC2", title = name) +
    theme_minimal() +
    coord_fixed()

}
