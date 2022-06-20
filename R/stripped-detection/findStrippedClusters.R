findStrippedClusters <- function(x, clusters, threshold = 0.05) {

  pct <- split(x$subsets_mt_percent / 100, clusters)

  med <- vapply(pct, median, numeric(1))

  DataFrame(cluster = names(pct), median = med, discard = med < threshold)

}
