barcodeRanksPlot <- function(x, lower = 100) {

  require("ggplot2")

  require("scales")

  data <- as.data.frame(x)

  metadata <- metadata(x)

  labels <- list(
    knee = sprintf("Knee = %s", comma(metadata$knee)),
    inflection = sprintf("Inflection = %s", comma(metadata$inflection)),
    lower = sprintf("Lower = %s", comma(lower))
  )

  colours <- list(
    knee = "#309143",
    inflection = "#E39802",
    lower = "#B60A1C"
  )

  data <- subset(data, !duplicated(rank))

  ggplot(data, aes(rank, total)) +

    geom_point(
      shape = 1,
      colour = "#000000"
    ) +

    geom_hline(
      yintercept = metadata$knee,
      colour = colours$knee,
      linetype = "dashed"
    ) +

    geom_hline(
      yintercept = metadata$inflection,
      colour = colours$inflection,
      linetype = "dashed"
    ) +

    geom_hline(
      yintercept = lower,
      colour = colours$lower,
      linetype = "dashed"
    ) +

    annotate(
      geom = "text",
      x = 1,
      y = metadata$knee,
      label = labels$knee,
      colour = colours$knee,
      hjust = 0,
      vjust = -1
    ) +

    annotate(
      geom = "text",
      x = 1,
      y = metadata$inflection,
      label = labels$inflection,
      colour = colours$inflection,
      hjust = 0,
      vjust = -1
    ) +

    annotate(
      geom = "text",
      x = 1,
      y = lower,
      label = labels$lower,
      colour = colours$lower,
      hjust = 0,
      vjust = -1
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
