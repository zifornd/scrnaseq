#' Plot emptyDrops LogProb
#'
#' @param 
#' @param
#'
#' @return A ggplot object.

EmpDropsLogProb <- function(x, FDR = 0.001) {
  
  data <- as.data.frame(x)
  
  signif <- which(data$FDR < FDR)
  
  data$Droplet <- "Empty"
  
  data$Droplet[signif] <- "Cell"
  
  counts <- table(data$Droplet)
  
  labels <- list(
    "Cell" = sprintf("Cell (%s)", scales::comma(counts["Cell"])),
    "Empty" = sprintf("Empty (%s)", scales::comma(counts["Empty"]))
  )
  
  values <- list(
    "Cell" = "#E15759", 
    "Empty" = "#BAB0AC"
  )

  ggplot(data, aes(Total, -LogProb, colour = Droplet)) + 
    geom_point(shape = 1) + 
    scale_colour_manual(name = "Droplet", values = values, labels = labels) + 
    scale_x_continuous(name = "Total Count", breaks = scales::breaks_extended(), labels = label_number_si()) + 
    scale_y_continuous(name = "-log(Probability)", breaks = scales::breaks_extended(), labels = label_number_si()) 

}
