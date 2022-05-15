all: data-loading.html \
     droplet-processing.html \
     quality-control.html \
     normalization.html

# Basic

data-loading.html: data-loading.qmd
	quarto render $<

droplet-processing.html: droplet-processing.qmd data-loading.html
	quarto render $<

quality-control.html: quality-control.qmd droplet-processing.html
	quarto render $<

normalization.html: normalization.qmd quality-control.html
	quarto render $<

feature-selection.html: feature-selection.qmd normalization.html
	quarto render $<

reduced-dimensions.html: reduced-dimensions.qmd feature-selection.html
	quarto render $<

clustering.html: clustering.qmd reduced-dimensions.html
	quarto render $<

marker-detection.html: marker-detection.qmd clustering.html
	quarto render $<

cell-annotation.html: cell-annotation.qmd marker-detection.html
	quarto render $<

# Advanced

doublet-detection.html: doublet-detection.qmd
	quarto render $<

cell-cycle.html: cell-cycle.qmd
	quarto render $<

trajectory.html: trajectory.qmd
	quarto render $<

# Multi-sample

batch-correction.html: batch-correction.qmd
	quarto render $<

correction-diagnostics.html: correction-diagnostics.qmd
	quarto render $<

pseudobulk-dge.html: pseudobulk-dge.qmd
	quarto render $<

cluster-abundance.html: cluster-abundance.qmd
	quarto render $<
