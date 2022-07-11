plotUMAP <- function(x, name) {

  require(ggplot2)

  require(scuttle)

  dat <- makePerCellDF(x)

  dat <- as.data.frame(dat)

  lab <- c("G1" = "G1", "S" = "S", "G2M" = "G2/M")

  col <- c("G1" = "#E03531", "S" = "#F0BD27", "G2M" = "#51B364")

  dat$Phase <- factor(dat$Phase, levels = c("G1", "S", "G2M"))

  ggplot(dat, aes(UMAP.1, UMAP.2, colour = Phase)) +
    geom_point(data = dat[, c("UMAP.1", "UMAP.2")], aes(UMAP.1, UMAP.2), colour = "#BAB0AC") +
    geom_point(show.legend = FALSE) +
    scale_colour_manual(labels = lab, values = col) +
    labs(x = "UMAP 1", y = "UMAP 2", title = name) +
    facet_wrap( ~ Phase) +
    theme_bw() +
    theme(aspect.ratio = 1, strip.background = element_blank())

}
