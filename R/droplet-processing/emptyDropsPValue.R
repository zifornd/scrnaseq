emptyDropsPValue <- function(x, lower = 100) {

    require("ggplot2")

    require("scales")

    data <- as.data.frame(x)

    data <- subset(data, Total <= lower & Total > 0)

    ggplot(data, aes(PValue)) +

      geom_histogram(
        binwidth = 0.05,
        colour = "#000000",
        fill = "#EBEBEB"
      ) +

      scale_x_continuous(
        name = "P value",
        breaks = breaks_extended(),
        labels = label_number()
      ) +

      scale_y_continuous(
        name = "Frequency",
        breaks = breaks_extended(),
        labels = label_number_si()
      )

}
