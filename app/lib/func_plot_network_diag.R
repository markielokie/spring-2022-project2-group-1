### This R script contains the function to plot network diagrams\

# Load libraries
library(dplyr)
library(igraph)
library(ggraph)

# Function to plot network diagram
# Inputs: biword_df is the dataframe that contains biword counts and
# phase_str is the phase number of the pandemic in character format
plot_network_diagram = function(biword_df, phase_str){
  phases = c("phase0", "phase1", "phase2", "phase3", "phase4", "phase5")
  if (is.data.frame(biwords_counts) & (phase_str %in% phases)){
    set.seed(617)
    biword_df %>%
      filter(phase == phase_str) %>%
      slice(1:65) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#FF9F33") +
      geom_node_point(size = 3) +
      geom_node_text(aes(label = name), repel = T,
                     point.padding = unit(0.2, "lines")) +
      theme_graph() +
      theme(legend.position = "none")
  }
  else {
    print("Error: Please ensure inputs to the function are correct.")
  }
}