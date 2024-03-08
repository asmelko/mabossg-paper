library(ggplot2)
library(cowplot)
library(sitools)
library(dplyr)


W=4.804
H=2
S=1
point_size=0.8
line_size=0.5
linecolors=scale_color_brewer(palette="Set2")
theme = theme_cowplot(font_size=7)

sisec=Vectorize(function(t)if(is.na(t))NA else sitools::f2si(t / 10^6, 's'))

average <- function(data)
{
    data["total"] = data["compilation"] + data["visualization"] + data["simulation"]
    data <- subset(data, select = -X)

    data_s <- split(data, paste0(data$max_time, data$nodes, data$formula_size, data$sample_count))
    data <- NULL
    for (i in 1:length(data_s)){
        tmp = subset(data_s[[i]], !(total %in% boxplot(data_s[[i]]$total, plot = FALSE)$out))
        data <- rbind(data, tmp)
    }

    data = data.frame(data %>% group_by_at(names(data)[-grep("(total)|(compilation)|(visualization)|(simulation)", names(data))]) %>% summarise(total = mean(total), compilation = mean(compilation), visualization = mean(visualization), simulation = mean(simulation)))
}

data_gpu = read.csv('data/gpu_out.csv', header=T, sep=';')
data_gpu = average(data_gpu)

data_v100 = read.csv('data/gpu_out_v100.csv', header=T, sep=';')
data_v100 = average(data_v100)

data_a100 = read.csv('data/gpu_out_a100.csv', header=T, sep=';')
data_a100 = average(data_a100)

data_cpu = read.csv('data/cpu_out.csv', header=T, sep=';')
names(data_cpu)[names(data_cpu) == "parsing"] <- "compilation"
data_cpu = subset(data_cpu, select = -threads)
data_cpu = average(data_cpu)

data_cpu["type"] = "Intel Xeon Gold 6130 (MaBoSS.CPU)"
data_gpu["type"] = "NVIDIA RTX 3070 Laptop (MaBoSS.GPU)"
data_v100["type"] = "NVIDIA Tesla V100 (MaBoSS.GPU)"
data_a100["type"] = "NVIDIA Tesla A100 (MaBoSS.GPU)"
data = rbind(data_cpu, data_gpu, data_a100)


data_gpu_real = read.csv('data/gpu_out_real.csv', header=T, sep=';')
data_gpu_real = average(data_gpu_real)

data_a100_real = read.csv('data/gpu_out_real_a100.csv', header=T, sep=';')
data_a100_real = average(data_a100_real)

data_cpu_real = read.csv('data/cpu_out_real.csv', header=T, sep=';')
names(data_cpu_real)[names(data_cpu_real) == "parsing"] <- "compilation"
data_cpu_real = subset(data_cpu_real, threads == 64)
data_cpu_real = subset(data_cpu_real, select = -threads)
data_cpu_real = average(data_cpu_real)

data_cpu_real["type"] = "Intel Xeon Gold 6130 (MaBoSS.CPU)"
data_gpu_real["type"] = "NVIDIA RTX 3070 Laptop (MaBoSS.GPU)"
data_a100_real["type"] = "NVIDIA Tesla A100 (MaBoSS.GPU)"
data_real = rbind(data_cpu_real, data_gpu_real, data_a100_real)

{
    data_c = data

    data_c = subset(data_c, max_time == 100)
    # data_c = subset(data_c, nodes <= 100)
    data_c = subset(data_c, formula_size == 4)
    data_c = subset(data_c, sample_count == 1000000)

    data_x = data_c
    data_y = data_c
    data_x["total"] = data_x["simulation"]
    data_x["desc"] = "Simulation"
    data_y["total"] = data_y["compilation"] + data_y["simulation"]
    data_y["desc"] = "Simulation + Runtime Compilation"

    data_c = rbind(data_x, data_y)

    ggsave("plots/nodes.pdf", device='pdf', units="in", scale=S, width=W, height=H,
        ggplot(data_c, aes(x=nodes,y=total, color=factor(type), shape=factor(type))) +
        geom_point(size=point_size) +
        geom_line(linewidth=line_size) +
        xlab("Nodes count (log-scale)")+
        ylab("Wall time (log-scale)")+
        labs(color="Machine", shape="Machine") +
        linecolors +
        scale_y_log10(labels = sisec) +
        scale_x_log10(breaks=c(0, 10, 20, 40, 100, 200, 400, 1000)) +
        facet_wrap(~desc) +
        theme + background_grid() + theme(legend.position="bottom") +
        guides(color=guide_legend(nrow=2, byrow=TRUE)) 
    )
}

for (m in c("NVIDIA RTX 3070 Laptop (MaBoSS.GPU)", "NVIDIA Tesla A100 (MaBoSS.GPU)"))
{
    data_c = data

    data_c = subset(data_c, max_time == 100)
    data_c = subset(data_c, type == m)
    data_c = subset(data_c, nodes %in% c(200, 400, 600, 800, 1000))
    data_c = subset(data_c, formula_size %in% c(4, 9, 24, 49))
    data_c = subset(data_c, sample_count %in% c(4*10^6, 6*10^6, 8*10^6, 10*10^6))

    data_c["total"] = data_c["compilation"] + data_c["simulation"]
    data_c["ratio"] = data_c["compilation"] / data_c["total"]

    data_c["formula_size"] =  2 * (data_c["formula_size"] + 1)

    data_c$sample_count = factor(c("4M", "6M", "8M", "10M"), levels=c("4M", "6M", "8M", "10M"))

    ggsave(paste0("plots/nodes-compilation-big-", m, ".pdf"), device='pdf', units="in", scale=S, width=W, height=H,
        ggplot(data_c, aes(x=nodes,y=ratio, color=sample_count, shape=sample_count)) +
        geom_point(size=point_size) +
        geom_line(linewidth=line_size) +
        xlab("Nodes count (log-scale)")+
        ylab("% of total time compiling (log-scale)")+
        labs(color="Simulated Trajectories", shape="Simulated Trajectories") +
        #scale_color_manual(values=RColorBrewer::brewer.pal(9,'YlGnBu')[2:9]) +
        linecolors +
        scale_y_log10(labels = scales::percent) +
        scale_x_continuous(breaks=c(200, 400, 600, 800, 1000)) +
        facet_wrap(~formula_size, ncol=4, labeller=labeller(formula_size=Vectorize(function(x) paste0("Formula size: ", x)))) +
        theme + background_grid() + theme(legend.position="bottom", axis.text.x = element_text(angle = -25, vjust=0.05))
    )
}

{
    data_c = data_real

    data_c = subset(data_c, name != "metastasis")

    norms = data_c[c("total", "type", "name")]

    data_c["nn"] = 0
    data_c["diff"] = 0
    data_c["speedup"] = 0

    for (n in c("cellcycle", "Montagud", "sizek"))
    {
        norm = as.numeric(norms[norms$type == "Intel Xeon Gold 6130 (MaBoSS.CPU)" & norms$name == n, ]["total"])
        data_c <- transform(data_c, nn = ifelse(norms$name == n, norm, nn))
        data_c <- transform(data_c, diff = ifelse(norms$name == n, total - norm, diff))
        data_c <- transform(data_c, speedup = ifelse(norms$name == n, norm / total, speedup))
    }

    ggsave("plots/real.pdf", device='pdf', units="in", scale=S, width=W, height=H,
        ggplot(data_c, aes(x=type, y=total, fill=type)) +
        geom_bar(stat="identity", position=position_dodge()) +
        geom_errorbar(aes(ymin=total, ymax=(total - diff)), width=.1) + 
        geom_text(aes(label=ifelse(speedup == 1, sitools::f2si(round(total / 10^6, digits=2), 's'), paste0('-',round(speedup, digits=0),'x')), y=total), size=2, vjust=0.5*3) + 
        geom_hline(aes(yintercept=nn), linewidth=.3) +
        xlab("")+
        ylab("Wall time (sqrt-scale)")+
        labs(color="Machine", shape="Machine", fill="Machine") +
        scale_fill_brewer(palette="Set2") +
        scale_y_continuous(trans=scales::trans_new('my_sqrt', function (x) x ^ (1/4), function (x) x^4), labels = sisec, breaks=c(100 * 10^6, 10 * 10^6, 100 * 10^3, 500 * 10^3)) +
        facet_wrap(~name, ncol=4, scales="free_y") +
        theme + background_grid() + theme(legend.position="bottom", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
        guides(fill=guide_legend(nrow=2, byrow=TRUE)) 
    )
}

data <- read.table('data/mpi_real.tsv', header=T)
data <- subset(data, algorithm %in% c('mpi', 'new'))

data$algorithm <- factor(data$algorithm, levels=c('mpi', 'new'), labels=c('MPI', 'CPU'))

ggsave("plots/sizek_mpi.pdf", units="in", width=3.5, height=2,
ggplot(data, aes(cpus, time, color=algorithm, shape=algorithm, group=algorithm)) +
  geom_point() +
  stat_smooth(geom='line', method='loess') +
  scale_x_log10("Cores (log-scale)") +
  scale_y_log10("Wall time (log-scale)", labels=c(expression('10'^1*' s'),expression('10'^2*' s'),expression('10'^3*' s'),expression('10'^4*' s')), breaks=c(10,100,1000,10000)) +
  scale_color_brewer("Software", palette='Dark2') +
  scale_shape("Software") +
  theme_cowplot(font_size=5) +
  theme(
    panel.grid.major=element_line(size=.2, color='#cccccc'),
  )
)

data <- read.table('data/mpi_synth.tsv', header=T)

data$algorithm <- factor(data$algorithm, levels=c('mpi'), labels=c('MPI'))

ggsave("plots/synth_mpi.pdf", units="in", width=3.25, height=2,
ggplot(data, aes(cpus, time*cpus, color=algorithm, shape=algorithm, group=algorithm)) +
  geom_point() +
  scale_x_log10("Cores (log-scale)") +
  scale_y_log10("Total wall time (all CPUs, log-scale)", labels=c(expression('1 x 10'^3*' s'),expression('3 x 10'^3*' s'),expression('1 x 10'^4*' s'),expression('3 x 10'^4*' s')), breaks=c(1000,3000,10000,30000)) +
  scale_color_brewer("Software", palette='Dark2') +
  scale_shape("Software") +
  theme_cowplot(font_size=5) +
  theme(
    panel.grid.major=element_line(size=.2, color='#cccccc'),
  )
)

base = 69895

ggsave("plots/synth_mpi_speedup.pdf", units="in", width=3.25, height=2,
ggplot(data, aes(cpus, 32*base/time/cpus, color=algorithm, shape=algorithm, group=algorithm)) +
  geom_point() +
  scale_x_log10("Cores (log-scale)") +
  scale_y_log10("Speedup per core (log-scale)", labels = function (x) paste0(x,"x")) +
  scale_color_brewer("Software", palette='Dark2') +
  scale_shape("Software") +
  theme_cowplot(font_size=5) +
  theme(
    panel.grid.major=element_line(size=.2, color='#cccccc'),
  )
)