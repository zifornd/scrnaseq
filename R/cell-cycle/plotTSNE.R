plotTSNE <- function(x, name) {

  require(ggplot2)

  require(scuttle)

  dat <- makePerCellDF(x)

  dat <- as.data.frame(dat)

  lab <- c("G1" = "G1", "S" = "S", "G2M" = "G2/M")

  col <- c("G1" = "#E03531", "S" = "#F0BD27", "G2M" = "#51B364")

  dat$Phase <- factor(dat$Phase, levels = c("G1", "S", "G2M"))

  ggplot(dat, aes(TSNE.1, TSNE.2, colour = Phase)) +
    geom_point(data = dat[, c("TSNE.1", "TSNE.2")], aes(TSNE.1, TSNE.2), colour = "#BAB0AC") +
    geom_point(show.legend = FALSE) +
    scale_colour_manual(labels = lab, values = col) +
    labs(x = "TSNE 1", y = "TSNE 2", title = name) +
    facet_wrap( ~ Phase) +
    theme_bw() +
    theme(aspect.ratio = 1, strip.background = element_blank())

}
