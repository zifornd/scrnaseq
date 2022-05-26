#'
#'
#'
#'
#'
#'
#'
#'

cyclone <- function(x, organism) {
  
  organism <- match.arg(organism)
  
  filename <- switch(
    organism, 
    "human" = system.file("exdata", "human_cycle_markers.rds", package = "scran"),
    "mouse" = system.file("exdata", "mouse_cycle_markers.rds", package = "scran")
  )
  
  markers <- readRDS(filename)

  cycles <- cyclone(x, pairs = markers, gene.names = rowData(x)$ID)
  
  colData(x)$G1.Score <- cycles$scores$G1
  
  colData(x)$S.Score <- cycles$scores$S
  
  colData(x)$G2M.Score <- cycles$scores$G2M
  
  colData(x)$Phase <- cycles$phases

  x

}