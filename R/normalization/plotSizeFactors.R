plotSizeFactors <- function(x) {

  require(ggplot2)

  require(scales)

  dat <- data.frame(value = sizeFactors(x))

  ggplot(dat, aes(value)) +
    geom_histogram(bins = 50, colour = "#000000", fill = "#e5c494") +
    scale_x_continuous(name = "Size factor", breaks = breaks_extended()) +
    scale_y_continuous(name = "Number of cells", breaks = breaks_extended(), label = label_number(big.mark = ",")) +
    theme_minimal()

}
