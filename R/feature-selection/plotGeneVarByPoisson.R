plotGeneVarByPoisson <- function(x, n = 10) {

  require(ggplot2)

  require(ggrepel)

  require(scales)

  require(scran)

  dec <- rowData(x)[["modelGeneVarByPoisson"]]

  dec$hvg <- rowData(x)[["HVG"]]

  lab <- list(
    "TRUE" = sprintf("Variable (%s)", comma(sum(dec$hvg))),
    "FALSE" = sprintf("Non-variable (%s)", comma(sum(!dec$hvg)))
  )

  col <- list(
    "TRUE" = "#E15759",
    "FALSE" = "#BAB0AC"
  )

  dec$name <- ""

  ind <- which(dec$bio >= sort(dec$bio, decreasing = TRUE)[n], arr.ind = TRUE)

  dec$name[ind] <- rownames(dec)[ind]

  ggplot(as.data.frame(dec)) +
    geom_point(aes(x = mean, y = total, colour = hvg)) +
    geom_line(aes(x = mean, y = tech), colour = "#000000") +
    scale_colour_manual(name = "Features", values = col, labels = lab) +
    geom_text_repel(aes(x = mean, y = total, label = name), colour = "#000000", size = 2, segment.size = 0.2) +
    labs(x = "Mean of log-expression", y = "Variance of log-expression") +
    theme_minimal() +
    theme(legend.position = "top")

}
