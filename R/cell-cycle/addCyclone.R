addCyclone <- function(object, organism) {

  require(scran)

  organism <- match.arg(organism, choices = c("human", "mouse"))

  filename <- switch(
    organism,
    "human" = system.file("exdata", "human_cycle_markers.rds", package = "scran"),
    "mouse" = system.file("exdata", "mouse_cycle_markers.rds", package = "scran")
  )

  markers <- readRDS(filename)

  genes <- rowData(object)$ID

  cycles <- cyclone(object, pairs = markers, gene.names = genes)

  colData(object)$G1.Score <- cycles$scores$G1

  colData(object)$S.Score <- cycles$scores$S

  colData(object)$G2M.Score <- cycles$scores$G2M

  colData(object)$Phase <- cycles$phases

  object

}
