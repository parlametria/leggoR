args <- commandArgs(trailingOnly = TRUE)

num_args <- 3

if (length(args) != num_args) {
  stop(paste("Wrong number of arguments!", help, sep = "\n"))
}

path_para_jus_all_dist <- args[1]
path_para_distancias <- args[2]
path_para_emendas <- args[3]

## Install local repository R package version
devtools::install()

agoradigital::format_table_distances_to_emendas(path_para_jus_all_dist, path_para_distancias)
agoradigital::add_distances_to_emendas(readr::read_csv(path_para_emendas), path_para_distancias)

#Rscript scripts/write_emendas_dist.R ../leggo-content/util/data/jus_all_dist/ data/distancias/ ../leggo-backend/data/emendas.csv