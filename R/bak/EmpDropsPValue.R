#' Plot 
#'
#' Visualise the distribution of a  by dividing the x axis into bins and counting the number of observations in each bin.
#'
#' @param x A DataFrame containing the output of barcodeRanks with test.ambient = TRUE.
#' @param lower A numeric scalar specifying the lower bound on the total UMI count, at or below which all barcodes are assumed to correspond to empty droplets.
#'
#' @return A ggplot object.

EmpDropsPValue <- function(x, lower = 100) {

  x <- subset(x, Total <= lower & Total > 0)
  
  x <- as.data.frame(x)
  
  ggplot(x, aes(PValue)) + 
    geom_histogram(binwidth = 0.05, colour = "#000000", fill = "#EBEBEB") + 
    scale_x_continuous(name = "P value", breaks = breaks_extended(), labels = label_number()) + 
    scale_y_continuous(name = "Frequency", breaks = breaks_extended(), labels = label_number_si())
  
}