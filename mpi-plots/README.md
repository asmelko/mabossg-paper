# simple performance plot templates

How-to:
- install R>4, ggplot and cowplot
- put your data into `data.tsv`
- `R -f plots.R`
- optionally tune plot sizes and `facet_wrap` columns, depending on how much
  data you have

**DEMO DATA:** The demo `data.tsv` shows a relatively large parallelization
overhead; mostly because the data is really tiny. Optimally, the efficiency
plots should be flat and well-balanced among facets. The plot previews are in
`plots/`.

**BASE OPERATIONS:** It is common to measure the efficiency against the
expected problem solution overhead instead of the base problem size. For
example, if multiplying matrices, you'd use problem size of `n^3` (or `n^2.81`
if benchmarking Strassen implementations) instead of just `n`. This ensures
that the regression between the problem size and CPU time stays as linear as
possible, avoiding artifacts.
