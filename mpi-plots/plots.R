
if(F) {
  # use this to install deps
  install.packages(c('ggplot2', 'cowplot', 'sitools'))
}

library(ggplot2)
library(cowplot)

# x <- read.table('data.tsv', header=T)

# #
# # helpers
# #

# sival <- function(...) Vectorize(function(x)sitools::f2si(x, ...))

# labeldf <- function(df) {
#   for(x in names(df))
#     df[[x]] <- paste(x, '=', df[[x]])
#   df
# }

# #
# # Scaling: how much resources do we need to solve a problem of given expected
# # size? (is there an unexpected scalability problem?)
# #

# ggsave("plots/scalability.png", units="in", width=7, height=7,
# ggplot(x, aes(size, time*cpus/size, color=algorithm)) +
#   geom_point() +
#   geom_smooth(method='loess', aes(fill=algorithm), alpha=.1) +
#   scale_x_log10("Base problem size", labels=sival('op')) +
#   scale_y_log10("Time per operation", labels=sival('Cs/op')) +
#   ggtitle("Overhead of processing different problem sizes") +
#   scale_color_brewer("Software", palette='Dark2') +
#   scale_fill_brewer("Software", palette='Dark2') +
#   theme_cowplot(font_size=9) +
#   theme(
#     panel.grid.major=element_line(size=.2, color='#cccccc'),
#   )
# )

# #
# # Speed: How much does throwing extra CPUs help to solve the problem faster?
# #

# ggsave("plots/speed.png", units="in", width=7, height=7,
# ggplot(x, aes(cpus/size, time/size, color=algorithm)) +
#   geom_point() +
#   geom_smooth(method='loess', aes(fill=algorithm), alpha=.1) +
#   scale_x_log10("Base operations assigned to one CPU", labels=function(x)(sival('op/C')(1/x))) +
#   scale_y_log10("Time per base operation", labels=sival('s/op')) +
#   ggtitle("Speedup given for invested resources") +
#   scale_color_brewer("Software", palette='Dark2') +
#   scale_fill_brewer("Software", palette='Dark2') +
#   theme_cowplot(font_size=9) +
#   theme(
#     panel.grid.major=element_line(size=.2, color='#cccccc'),
#   )
# )

# #
# # Other detailed plots
# #

# ggsave("plots/cputime.pdf", units="in", width=7, height=7,
# ggplot(x, aes(cpus, time, color=algorithm, shape=algorithm, group=algorithm)) +
#   geom_point() +
#   stat_smooth(geom='line', method='lm') +
#   scale_x_log10("CPUs", labels=sival()) +
#   scale_y_log10("Wall-clock time (lower is better)", labels=sival('s')) +
#   scale_color_brewer("Software", palette='Dark2') +
#   scale_shape("Software") +
#   ggtitle("Parallelization performance") +
#   facet_wrap(size+dataset~., scales='free', ncol=3, labeller=labeldf) +
#   theme_cowplot(font_size=9) +
#   theme(
#     panel.grid.major=element_line(size=.2, color='#cccccc'),
#   )
# )

# ggsave("plots/cpueffi.pdf", units="in", width=7, height=5,
# ggplot(x, aes(cpus, time*cpus/size, color=algorithm, shape=algorithm, group=algorithm)) +
#   geom_point() +
#   stat_smooth(geom='line', method='lm') +
#   scale_x_log10("CPUs", labels=sival()) +
#   scale_y_log10("Total CPU time per base operation (lower is better)", labels=sival('s/op')) +
#   scale_color_brewer("Software", palette='Dark2') +
#   scale_shape("Software") +
#   ggtitle("Parallelization efficiency") +
#   facet_wrap(size+dataset~., ncol=4, labeller=labeldf) +
#   theme_cowplot(font_size=9) +
#   theme(
#     panel.grid.major=element_line(size=.2, color='#cccccc'),
#   )
# )

# ggsave("plots/cpuspeed.pdf", units="in", width=7, height=7,
# ggplot(x, aes(cpus, size/(cpus*time), color=algorithm, shape=algorithm, group=algorithm)) +
#   geom_point() +
#   stat_smooth(geom='line', method='lm') +
#   scale_x_log10("CPUs", labels=sival()) +
#   scale_y_log10("Base operations per total CPU time (higher is better)", labels=sival('/Cs')) +
#   scale_color_brewer("Software", palette='Dark2') +
#   scale_shape("Software") +
#   ggtitle("Speed by parallelization") +
#   facet_wrap(size+dataset~., scales='free', ncol=3, labeller=labeldf) +
#   theme_cowplot(font_size=9) +
#   theme(
#     panel.grid.major=element_line(size=.2, color='#cccccc'),
#   )
# )

# ggsave("plots/sizetime.pdf", units="in", width=7, height=7,
# ggplot(x, aes(size, time, color=algorithm, shape=algorithm, group=algorithm)) +
#   geom_point() +
#   stat_smooth(geom='line', method='lm') +
#   scale_x_log10("Problem size in basic operations", labels=sival('op')) +
#   scale_y_log10("Wall-clock time (lower is better)", labels=sival('s')) +
#   scale_color_brewer("Software", palette='Dark2') +
#   scale_shape("Software") +
#   ggtitle("Scaling performance") +
#   facet_grid(seed+dataset~cpus, scales='free', labeller=labeldf) +
#   theme_cowplot(font_size=9) +
#   theme(
#     panel.grid.major=element_line(size=.2, color='#cccccc'),
#   )
# )

# ggsave("plots/sizeeffi.pdf", units="in", width=7, height=5,
# ggplot(x, aes(size, time*cpus/size, color=algorithm, shape=algorithm, group=algorithm)) +
#   geom_point() +
#   stat_smooth(geom='line', method='lm') +
#   scale_x_log10("Problem size in basic operations", labels=sival('op')) +
#   scale_y_log10("Total CPU time per base operation (lower is better)", labels=sival('Cs/op')) +
#   scale_color_brewer("Software", palette='Dark2') +
#   scale_shape("Software") +
#   ggtitle("Scaling efficiency") +
#   facet_grid(seed+dataset~cpus, labeller=labeldf) +
#   theme_cowplot(font_size=9) +
#   theme(
#     panel.grid.major=element_line(size=.2, color='#cccccc'),
#   )
# )

# ggsave("plots/sizespeed.pdf", units="in", width=7, height=7,
# ggplot(x, aes(size, size/cpus/time, color=algorithm, shape=algorithm, group=algorithm)) +
#   geom_point() +
#   stat_smooth(geom='line', method='lm') +
#   scale_x_log10("Problem size in basic operations", labels=sival('op')) +
#   scale_y_log10("Tasks per total CPU time (higher is better)", labels=sival('op/Cs')) +
#   scale_color_brewer("Software", palette='Dark2') +
#   scale_shape("Software") +
#   ggtitle("Speed by scaling") +
#   facet_grid(seed+dataset~cpus, scales='free', labeller=labeldf) +
#   theme_cowplot(font_size=9) +
#   theme(
#     panel.grid.major=element_line(size=.2, color='#cccccc'),
#   )
# )

data <- read.table('data_sizek.tsv', header=T)
data <- subset(data, algorithm %in% c('mpi', 'new'))

data$algorithm <- factor(data$algorithm, levels=c('mpi', 'new'), labels=c('MPI', 'CPU'))

ggsave("plots/sizek_mpi.pdf", units="in", width=7, height=5,
ggplot(data, aes(cpus, time, color=algorithm, shape=algorithm, group=algorithm)) +
  geom_point() +
  stat_smooth(geom='line', method='lm') +
  scale_x_log10("Cores (log-scale)") +
  scale_y_log10("Wall time (log-scale)", labels=c(expression('10'^1*' s'),expression('10'^2*' s'),expression('10'^3*' s'),expression('10'^4*' s')), breaks=c(10,100,1000,10000)) +
  scale_color_brewer("Software", palette='Dark2') +
  scale_shape("Software") +
  theme_cowplot(font_size=9) +
  theme(
    panel.grid.major=element_line(size=.2, color='#cccccc'),
  )
)

data <- read.table('data_large.tsv', header=T)

data$algorithm <- factor(data$algorithm, levels=c('mpi'), labels=c('MPI'))

ggsave("plots/synth_mpi.pdf", units="in", width=7, height=5,
ggplot(data, aes(cpus, time, color=algorithm, shape=algorithm, group=algorithm)) +
  geom_point() +
  stat_smooth(geom='line', method='lm') +
  scale_x_log10("Cores (log-scale)") +
  scale_y_log10("Wall time (log-scale)", labels=c(expression('1 x 10'^3*' s'),expression('3 x 10'^3*' s'),expression('1 x 10'^4*' s'),expression('3 x 10'^4*' s')), breaks=c(1000,3000,10000,30000)) +
  scale_color_brewer("Software", palette='Dark2') +
  scale_shape("Software") +
  theme_cowplot(font_size=9) +
  theme(
    panel.grid.major=element_line(size=.2, color='#cccccc'),
  )
)

base = 69895

ggsave("plots/synth_mpi_speedup.pdf", units="in", width=7, height=5,
ggplot(data, aes(cpus, 32 *(base / time), color=algorithm, shape=algorithm, group=algorithm)) +
  geom_point() +
  stat_smooth(geom='line', method='lm') +
  scale_x_log10("Cores (log-scale)") +
  scale_y_log10("Speedup (log-scale)", labels = function (x) paste0(x,"x")) +
  scale_color_brewer("Software", palette='Dark2') +
  scale_shape("Software") +
  theme_cowplot(font_size=9) +
  theme(
    panel.grid.major=element_line(size=.2, color='#cccccc'),
  )
)