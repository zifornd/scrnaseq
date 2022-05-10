#' Plot barcode ranks
#' 
#' Plot barcode rank statistics
#' 
#' @param a DataFrame containing the output of barcodeRanks.
#'
#' @return a ggplot object.

BarcodeRank <- function(x, lower = 100) {
  
  require("ggplot2")
  
  require("scales")
  
  metadata(x)$lower <- lower

  x <- subset(x, !duplicated(rank))
  
  lab <- list(
    knee = sprintf("Knee = %s", comma(metadata(x)$knee)),
    inflection = sprintf("Inflection = %s", comma(metadata(x)$inflection)),
    lower = sprintf("Lower = %s", comma(metadata(x)$lower))
  )
  
  col <- list(
    knee = "#309143",
    inflection = "#E39802",
    lower = "#B60A1C"
  )
  
  breaks_log10 <- function() {
    
    # Return breaks for log10 scale
    
    function(x) 10^seq(ceiling(log10(min(x))), ceiling(log10(max(x))))
    
  }

  ggplot(as.data.frame(x), aes(rank, total)) + 
    geom_point(shape = 1, colour = "#000000") + 
    geom_hline(yintercept = metadata(x)$knee, colour = col$knee, linetype = "dashed") + 
    geom_hline(yintercept = metadata(x)$inflection, colour = col$inflection, linetype = "dashed") + 
    geom_hline(yintercept = metadata(x)$lower, colour = col$lower, linetype = "dashed") + 
    annotate("text", x = 1, y = metadata(x)$knee, label = lab$knee, colour = col$knee, hjust = 0, vjust = -1) +
    annotate("text", x = 1, y = metadata(x)$inflection, label = lab$inflection, colour = col$inflection, hjust = 0, vjust = -1) + 
    annotate("text", x = 1, y = metadata(x)$lower, label = lab$lower, colour = col$lower, hjust = 0, vjust = -1) + 
    scale_x_log10(name = "Barcode Rank", breaks = breaks_log10(), labels = label_number_si()) + 
    scale_y_log10(name = "Total Count", breaks = breaks_log10(), labels = label_number_si())

}