#'
#'
#'
#'
#'
#'

EmpDropsLimited <- function(x, FDR = 0.001) {
  
  data <- subset(x, !is.na(FDR))
  
  data <- data.frame(Limited = data$Limited, Significant = data$FDR < FDR)
  
  values <- c("TRUE" = "#59A14F", "FALSE" = "#E15759")
  
  labels <- c("TRUE" = "True", "FALSE" = "False")
  
  plt <- ggplot(data, aes(Limited, fill = Significant)) + 
    geom_bar() + 
    scale_fill_manual(values = values, labels = labels) + 
    scale_x_discrete(name = "Limited", labels = labels) + 
    scale_y_continuous(name = "Barcodes", labels = label_number_si())

}
