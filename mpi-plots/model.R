
x <- read.table('data.tsv', header=T)
x$size.per.cpu <- x$size/x$cpus

library(dplyr)
library(tidyr)

x |>
  group_by(algorithm) |>
  do(mod = lm(time ~ size.per.cpu + size + cpus, data=., weights=log1p(.$size)),) |>
  mutate(
    r.squared = summary(mod)$r.squared,
    coefs=list(summary(mod)$coefficients[,'Estimate'])
  ) |>
  unnest_wider(col=coefs) |>
  select(-mod) -> result
