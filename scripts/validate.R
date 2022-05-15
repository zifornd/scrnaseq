library(validate)

samples <- read.delim("samples.tsv")



rules <- validator(
  is.character("Lane"),
  is.character("Sample"),
  is.character("Index")
  
)

out <- confront(samples, rules)