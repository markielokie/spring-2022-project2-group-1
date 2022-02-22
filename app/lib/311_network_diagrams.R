### This R file plots the network diagrams for the biwords from the
### 6 different phases of the pandemic. The thicker the lines in the
### network diagrams, the greater the occurrence of the term/topic.

### NOTE ###
# Phase 0: Pre-pandemic (Oct 1, 2019 - Feb 28, 2020)
# Phase 1: Initial outbreak (Mar 1, 2020 - May 31, 2020)
# Phase 2: Cases go down  (Jun 1, 2020 - Oct 31, 2020)
# Phase 3: Cases go up + Delta variant (Nov 1, 2020 - May 31, 2021)
# Phase 4: Cases go down (Jun 1, 2021 - Oct 31, 2021)
# Phase 5: Cases go up + Omicron variant (Nov 1, 2021 - present)

# Add source functions from lib folder
source("lib/func_plot_network_diag.R")

# Load libraries
library(dplyr)
library(igraph)
library(ggraph)

# Load data
biwords_counts_p0 = read.csv("output/biwords_phase0.csv")
biwords_counts_p1 = read.csv("output/biwords_phase1.csv")
biwords_counts_p2 = read.csv("output/biwords_phase2.csv")
biwords_counts_p3 = read.csv("output/biwords_phase3.csv")
biwords_counts_p4 = read.csv("output/biwords_phase4.csv")
biwords_counts_p5 = read.csv("output/biwords_phase5.csv")

# Bind all data into a single dataframe
biwords_counts = rbind(mutate(biwords_counts_p0, phase = "phase0"),
                       mutate(biwords_counts_p1, phase = "phase1"),
                       mutate(biwords_counts_p2, phase = "phase2"),
                       mutate(biwords_counts_p3, phase = "phase3"),
                       mutate(biwords_counts_p4, phase = "phase4"),
                       mutate(biwords_counts_p5, phase = "phase5"))

# Plot network diagrams for each phase of the pandemic
plot_network_diagram(biword_df = biwords_counts, phase_str = "phase0")
plot_network_diagram(biword_df = biwords_counts, phase_str = "phase1")
plot_network_diagram(biword_df = biwords_counts, phase_str = "phase2")
plot_network_diagram(biword_df = biwords_counts, phase_str = "phase3")
plot_network_diagram(biword_df = biwords_counts, phase_str = "phase4")
plot_network_diagram(biword_df = biwords_counts, phase_str = "phase5")
