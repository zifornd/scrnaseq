plotPerCellQCMetrics <- function(x) {

  require(patchwork)

  require(scuttle)

  df <- makePerCellDF(x)

  p1 <- plotPerCellQCMetrics.sum(df)

  p2 <- plotPerCellQCMetrics.detected(df)

  p3 <- plotPerCellQCMetrics.subsets_mt_percent(df)

  p4 <- plotPerCellQCMetrics.subsets_rp_percent(df)

  p5 <- plotPerCellQCMetrics.sum.detected(df)

  p6 <- plotPerCellQCMetrics.sum.subsets_mt_percent(df)

  wrap_plots(p1, p2, p3, p4, p5, p6, ncol = 1)

}

plotPerCellQCMetrics.sum <- function(x) {

  require(ggplot2)

  require(scales)

  dat <- data.frame(value = x$sum, variable = x$low_lib_size)

  ann <- list(
    threshold = attr(dat$variable, "thresholds")["lower"],
    discarded = sum(dat$variable)
  )

  lab <- list(
    threshold = sprintf("Threshold (%s counts) ", comma(round(ann$threshold))),
    discarded = sprintf("%s cells ", comma(ann$discarded))
  )

  ggplot(dat, aes(value, fill = discard)) +
    geom_histogram(bins = 50, colour = "#000000", fill = "#66C2A5") +
    geom_vline(xintercept = ann$threshold, linetype = "dashed", colour = "#000000") +
    annotate("text", x = ann$threshold, y = Inf, label = lab$threshold, angle = 90, vjust = -1, hjust = 1, colour = "#000000") +
    annotate("text", x = ann$threshold, y = Inf, label = lab$discarded, angle = 90, vjust = 2, hjust = 1, colour = "#000000") +
    scale_x_log10(name = "Total counts", breaks = breaks_log(), label = label_number(big.mark = ",")) +
    scale_y_continuous(name = "Number of cells", breaks = breaks_extended(), label = label_number(big.mark = ",")) +
    ggtitle("UMI counts per cell") +
    theme_minimal()

}

plotPerCellQCMetrics.detected <- function(x) {

  require(ggplot2)

  require(scales)

  dat <- data.frame(value = x$detected, variable = x$low_n_features)

  ann <- list(
    threshold = attr(dat$variable, "thresholds")["lower"],
    discarded = sum(dat$variable)
  )

  lab <- list(
    threshold = sprintf("Threshold (%s genes) ", comma(round(ann$threshold))),
    discarded = sprintf("%s cells ", comma(ann$discarded))
  )

  ggplot(dat, aes(value)) +
    geom_histogram(bins = 50, colour = "#000000", fill = "#FC8D62") +
    geom_vline(xintercept = ann$threshold, linetype = "dashed", colour = "#000000") +
    annotate("text", x = ann$threshold, y = Inf, label = lab$threshold, angle = 90, vjust = -1, hjust = 1, colour = "#000000") +
    annotate("text", x = ann$threshold, y = Inf, label = lab$discarded, angle = 90, vjust = 2, hjust = 1, colour = "#000000") +
    scale_x_log10(name = "Total features", breaks = breaks_log(), label = label_number(big.mark = ",")) +
    scale_y_continuous(name = "Number of cells", breaks = breaks_extended(), label = label_number(big.mark = ",")) +
    ggtitle("Genes detected per cell") +
    theme_minimal()

}

plotPerCellQCMetrics.subsets_mt_percent <- function(x) {

  require(ggplot2)

  require(scales)

  dat <- data.frame(value = x$subsets_mt_percent, variable = x$high_subsets_mt_percent)

  ann <- list(
    threshold = attr(dat$variable, "thresholds")["higher"],
    discarded = sum(dat$variable)
  )

  lab <- list(
    threshold = sprintf("Threshold (%s %%) ", comma(round(ann$threshold))),
    discarded = sprintf("%s cells ", comma(ann$discarded))
  )

  ggplot(dat, aes(value)) +
    geom_histogram(bins = 50, colour = "#000000", fill = "#8DA0CB") +
    geom_vline(xintercept = ann$threshold, linetype = "dashed", colour = "#000000") +
    annotate("text", x = ann$threshold, y = Inf, label = lab$threshold, angle = 90, vjust = -1, hjust = 1, colour = "#000000") +
    annotate("text", x = ann$threshold, y = Inf, label = lab$discarded, angle = 90, vjust = 2, hjust = 1, colour = "#000000") +
    scale_x_continuous(name = "MT proportion", breaks = breaks_extended(), label = label_percent(scale = 1)) +
    scale_y_continuous(name = "Number of cells", breaks = breaks_extended(), label = label_number(big.mark = ",")) +
    ggtitle("Mitochondrial proportion per cell") +
    theme_minimal()

}

plotPerCellQCMetrics.subsets_rp_percent <- function(x) {

  require(ggplot2)

  require(scales)

  dat <- data.frame(value = x$subsets_rp_percent)

  ggplot(dat, aes(value)) +
    geom_histogram(bins = 50, colour = "#000000", fill = "#E78AC3") +
    scale_x_continuous(name = "RP proportion", breaks = breaks_extended(), label = label_percent(scale = 1)) +
    scale_y_continuous(name = "Number of cells", breaks = breaks_extended(), label = label_number(big.mark = ",")) +
    ggtitle("Ribosomal proportion per cell") +
    theme_minimal()

}

plotPerCellQCMetrics.sum.detected <- function(x) {

  require(ggplot2)

  require(scales)

  val <- list(x = "sum", y = "detected")

  var <- list(x = "low_lib_size", y = "low_n_features")

  use <- list(x = "lower", y = "lower")

  ann <- list(
    threshold = list(
      x = attr(x[, var$x], "thresholds")[use$x],
      y = attr(x[, var$y], "thresholds")[use$y]
    ),
    discarded = list(
      x = x[, var$x],
      y = x[, var$y]
    ),
    ncells = list(
      x = sum(x[, var$x]),
      y = sum(x[, var$y])
    )
  )

  col <- c("TRUE" = "#CA0020", "FALSE" = "#0571B0")

  lab <- c("TRUE" = "Low", "FALSE" = "High")

  x$status <- ann$discarded$x | ann$discarded$y

  ggplot(x, aes_string(x = val$x, y = val$y, colour = "discard")) +
    geom_point() +
    scale_colour_manual(name = "Quality", values = col, labels = lab) +
    geom_vline(xintercept = ann$threshold$x, linetype = "dashed", colour = "#000000") +
    geom_hline(yintercept = ann$threshold$y, linetype = "dashed", colour = "#000000") +
    annotate("text", x = ann$threshold$x, y = Inf, label = sprintf("Threshold (%s counts) ", comma(round(ann$threshold$x))), angle = 90, vjust = -1, hjust = 1, colour = "#000000") +
    annotate("text", x = ann$threshold$x, y = Inf, label = sprintf("%s cells ", comma(ann$ncells$x)), angle = 90, vjust = 2, hjust = 1, colour = "#000000") +
    annotate("text", x = Inf, y = ann$threshold$y, label = sprintf("Threshold (%s genes) ", comma(round(ann$threshold$y))), angle = 0, vjust = -1, hjust = 1, colour = "#000000") +
    annotate("text", x = Inf, y = ann$threshold$y, label = sprintf("%s cells ", comma(ann$ncells$y)), angle = 0, vjust = 2, hjust = 1, colour = "#000000") +
    scale_x_log10(name = "Total counts", breaks = breaks_log(), labels = label_number(big.mark = ",")) +
    scale_y_log10(name = "Total features", breaks = breaks_log(), labels = label_number(big.mark = ",")) +
    theme_minimal()

}

plotPerCellQCMetrics.sum.subsets_mt_percent <- function(x) {

  require(ggplot2)

  require(scales)

  val <- list(x = "sum", y = "subsets_mt_percent")

  var <- list(x = "low_lib_size", y = "high_subsets_mt_percent")

  use <- list(x = "lower", y = "higher")

  ann <- list(
    threshold = list(
      x = attr(x[, var$x], "thresholds")[use$x],
      y = attr(x[, var$y], "thresholds")[use$y]
    ),
    discarded = list(
      x = x[, var$x],
      y = x[, var$y]
    ),
    ncells = list(
      x = sum(x[, var$x]),
      y = sum(x[, var$y])
    )
  )

  col <- c("TRUE" = "#CA0020", "FALSE" = "#0571B0")

  lab <- c("TRUE" = "Low", "FALSE" = "High")

  x$status <- ann$discarded$x | ann$discarded$y

  ggplot(x, aes_string(x = val$x, y = val$y, colour = "discard")) +
    geom_point() +
    scale_colour_manual(name = "Quality", values = col, labels = lab) +
    geom_vline(xintercept = ann$threshold$x, linetype = "dashed", colour = "#000000") +
    geom_hline(yintercept = ann$threshold$y, linetype = "dashed", colour = "#000000") +
    annotate("text", x = ann$threshold$x, y = Inf, label = sprintf("Threshold (%s counts) ", comma(round(ann$threshold$x))), angle = 90, vjust = -1, hjust = 1, colour = "#000000") +
    annotate("text", x = ann$threshold$x, y = Inf, label = sprintf("%s cells ", comma(ann$ncells$x)), angle = 90, vjust = 2, hjust = 1, colour = "#000000") +
    annotate("text", x = Inf, y = ann$threshold$y, label = sprintf("Threshold (%.02f %%) ", round(ann$threshold$y, digits = 2)), angle = 0, vjust = -1, hjust = 1, colour = "#000000") +
    annotate("text", x = Inf, y = ann$threshold$y, label = sprintf("%s cells ", comma(ann$ncells$y)), angle = 0, vjust = 2, hjust = 1, colour = "#000000") +
    scale_x_log10(name = "Total counts", breaks = breaks_log(), labels = label_number(big.mark = ",")) +
    scale_y_continuous(name = "MT proportion", labels = label_percent(scale = 1)) +
    theme_minimal()

}
