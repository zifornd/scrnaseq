#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'

plotCyclone <- function(x) {
  
  data <- data.frame(G1 = colData(x)$G1.Score, G2M = colData(x)$G2M.Score, Phase = colData(x)$Phase)
  
  colours <- c("G1" = "#E03531", "S" = "#F0BD27", "G2/M" = "#51B364")
  
  phases <- c("G1", "S", "G2/M")
  
  labels <- sapply(phases, function(phase) {
        
        num <- sum(dat$phase == phase)
        
        pct <- (num / length(dat$phase)) * 100
        
        lab <- sprintf("%s (%s%%)", x, round(pct, digits = 2))
        
        lab <- sub("G2M", "G2/M", lab)

    })
    
  ggplot(dat, aes(G1, G2M, colour = phase)) + 
    geom_point() + 
    scale_colour_manual(values = values, breaks = breaks, labels = labels) + 
    labs(x = "G1 score", y = "G2/M score", colour = "Phase")

}