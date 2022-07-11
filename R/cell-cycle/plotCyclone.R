plotCyclone <- function(x, name) {

  require(ggplot2)

  data <- data.frame(
    G1 = colData(x)[["G1.Score"]],
    G2M = colData(x)[["G2M.Score"]],
    Phase = colData(x)[["Phase"]]
  )

  colours <- c("G1" = "#E03531", "S" = "#F0BD27", "G2M" = "#51B364")

  phases <- c("G1", "S", "G2M")

  labels <- sapply(phases, function(phase) {

        number <- sum(data$Phase == phase)

        total <- length(data$Phase)

        percent <- (number / total) * 100

        label <- sprintf("%s (%s%%)", phase, round(percent, digits = 2))

        label <- sub("G2M", "G2/M", label)

    })

  ggplot(data, aes(G1, G2M, colour = Phase)) +
    geom_point() +
    scale_colour_manual(values = colours, labels = labels) +
    labs(x = "G1 score", y = "G2/M score", colour = "Phase", title = name) +
    theme_minimal() +
    theme(aspect.ratio = 1, legend.position = "top")

}
