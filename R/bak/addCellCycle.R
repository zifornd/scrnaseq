#' Add cell cycle
#'
#'
#'
#'
#'
#'
#'
#'
#'

addCellCycle <- function(x, organism = c("human", "mouse")) {

  organism <- match.arg(organism)
  
  filename <- paste0(params$organism, "_cycle_markers.rds")
  
  fullname <- system.file("exdata", filename, package = "scran")
  
  pairs <- readRDS(fullname)
  
  cycles <- scran::cyclone(x, pairs, gene.names = SingleCellExperiment::rowData(x)$ID)
  
  SingleCellExperiment::colData(x)$G1Score <- cycles$scores$G1
  
  SingleCellExperiment::colData(x)$SScore <- cycles$scores$G1
  
  SingleCellExperiment::colData(x)$G2MScore <- cycles$scores$G2M
  
  SingleCellExperiment::colData(x)$CellCycle <- cycles$phases
  
  x

}