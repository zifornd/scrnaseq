emptyDropsLogProb <- function(x, FDR = 0.0001) {

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
    "Cell" = "#E15759",
    "Empty" = "#BAB0AC"
  )

  ggplot(data, aes(Total, -LogProb, colour = Droplet)) +

    geom_point() +

    scale_colour_manual(
      name = "Droplet",
      values = colours,
      labels = labels
    ) +

    scale_x_continuous(
      name = "Total Count",
      breaks = breaks_extended(),
      labels = label_number()
    ) +

    scale_y_continuous(
      name = "-log(Probability)",
      breaks = breaks_extended(),
      labels = label_number()
    )

}
