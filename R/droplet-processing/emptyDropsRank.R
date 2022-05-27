emptyDropsRank <- function(x, FDR = 0.001) {

  require("ggplot2")

  require("scales")

  data <- as.data.frame(x)

  cell <- which(data$FDR < FDR)

  data$Droplet <- "Empty"

  data$Droplet[cell] <- "Cell"

  counts <- table(data$Droplet)

  labels <- list(
    "Cell" = sprintf("Cell (%s)", comma(counts["Cell"])),
    "Empty" = sprintf("Empty (%s)", comma(counts["Empty"]))
  )

  colours <- list(
    "Cell" = "#FF4466",
    "Empty" = "#828E84"
  )

  data$Rank <- rank(-data$Total)

  data <- subset(data, !duplicated(Rank))

  ggplot(data, aes(Rank, Total, colour = Droplet)) +

    geom_point(
      shape = 1,
      show.legend = TRUE
    ) +

    scale_colour_manual(
      name = "Droplet",
      labels = labels,
      values = colours
    ) +

    scale_x_log10(
      name = "Barcode Rank",
      labels = label_number()
    ) +

    scale_y_log10(
      name = "Total Count",
      labels = label_number()
    )

}
