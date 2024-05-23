# Load necessary packages
library(igraph)
library(tidyverse)
library(knitr)
library(kableExtra)

# Load preprocessed data
setwd("/Users/rohitkandala/Desktop/UChicago/Academic Quarters/2023-24/Spring 2024/MACSS 40101/galaskiewicz_sna_project-main")
load("Data/allnetworks.RData")

# Categorize nodes:
num_nodes <- vcount(education_net)
ceo_nodes <- 1:16
educ_nodes <- 17:num_nodes

# Define vertex attributes to include
attributes_to_include <- c(
  "company", "age", "gender", "mba", "marketcap", "industry", "region", "top20")

# Convert igraph object to a data frame and select specific vertex attributes:
df_all <- igraph::as_data_frame(
  education_net, what = "vertices")[, attributes_to_include, drop = FALSE]

# Calculate centrality measures
calculate_centrality_measures <- function(network, dataframe) {
  
  # Calculate centrality measures:
  degree_centrality <- igraph::degree(network, normalized = TRUE)
  eigenvector_centrality <- igraph::eigen_centrality(network, scale = TRUE)$vector
  betweenness_centrality <- igraph::betweenness(network, normalized = TRUE)
  closeness_centrality <- igraph::closeness(network, normalized = TRUE)
  coreness_measure <- igraph::coreness(network)
  
  # Add centrality measures to the dataframe
  dataframe <- dataframe %>%
    mutate(
      Degree = round(degree_centrality, 3),
      Eigenvector = round(eigenvector_centrality, 3),
      Betweenness = round(betweenness_centrality, 3),
      Closeness = round(closeness_centrality, 3),
      Coreness = round(coreness_measure, 3)
    )
  
  return(dataframe)
}

# Add centrality measures to dataframe:
df_all <- calculate_centrality_measures(education_net, df_all)

# Create node-specific dataframes:
df_ceo <- df_all[ceo_nodes, ]
df_educ <- df_all[educ_nodes, ]

# Create one-mode networks (CEO-by-CEO & Educ-by-Educ) from igraph object:
adj_matrix <- as.matrix(as_adjacency_matrix(education_net))
adj_matrix_t <- t(adj_matrix)
ceo_by_ceo <- adj_matrix[1:16, ] %*% adj_matrix_t[, 1:16]
diag(ceo_by_ceo) <- 0
ceo_by_ceo_igraph <- graph_from_adjacency_matrix(ceo_by_ceo)
ceo_by_ceo_df <- as.data.frame(ceo_by_ceo)
educ_by_educ <- adj_matrix[17:num_nodes, ] %*% adj_matrix_t[, 17:num_nodes]
diag(educ_by_educ) <- 0
educ_by_educ_igraph <- graph_from_adjacency_matrix(educ_by_educ)
educ_by_educ_df <- as.data.frame(educ_by_educ)

# Generalized plot function to inspect network:
plot_network <- function(network, num_nodes, ceo_nodes) {
  
  # Create a vector for node colors
  node_colors <- rep("darkgreen", num_nodes)  
  node_colors[ceo_nodes] <- "red"
  
  # Calculate degree for node size scaling
  degree_values <- degree(network)
  
  # Plot the network
  plot(network,
       layout = layout_with_kk(network),
       vertex.size = sqrt(degree_values) * 10, # Scale node sizes
       vertex.color = node_colors, 
       vertex.label = ifelse(degree_values > 3, V(network)$name, NA),
       vertex.label.cex = 0.8,
       vertex.label.color = "black",
       vertex.label.dist = 0.5,
       edge.width = 1, # Thicker edges
       edge.color = "grey",
       main = "Education Network")
}

# Let's check this plot function!
plot_network(education_net, num_nodes, ceo_nodes)

# Since there are a lot of vertex attributes, we'll first inspect some 
# homophily measures. Let's write a function calculating the normalized
# blau index. (Source: Loizos's comments on Assignment #5!): 
calculate_blau_index <- function(network, attribute_name) {
  blau <- rep(0, vcount(network))
  
  for (node in 1:vcount(network)) {
    n <- neighbors(network, node)
    num_neighbors <- length(n)
    
    if (num_neighbors == 0) {
      blau[node] <- NA
    } else {
      attribute_neighbors <- igraph::vertex_attr(network, attribute_name)[n]
      proportions <- prop.table(table(attribute_neighbors))
      blau[node] <- 1 - sum(proportions^2)
    }
  }
  
  # Normalize the Blau index values if there are valid (non-NA) values
  if (sum(!is.na(blau)) > 1) {
    min_val <- min(blau, na.rm = TRUE)
    max_val <- max(blau, na.rm = TRUE)
    normalized_blau <- (blau - min_val) / (max_val - min_val)
  } else {
    normalized_blau <- blau
  }
  
  return(normalized_blau)
}


# Vertex attributes we want to run through Blau index
attributes_to_process <- setdiff(vertex_attr_names(education_net), c("type", 
                                                                     "name"))

# Create an empty data frame to store the results:
blau_indices <- data.frame(node = V(education_net)$name)

# Calculate normalized Blau index for each attribute and store in data frame:
for (attr in attributes_to_process) {
  blau_indices[[attr]] <- calculate_blau_index(education_net, attr)
}

plot_network_with_blau <- function(network, blau_index) {
  # Normalize the Blau index
  normalized_blau <- blau_index
  
  # Defining vertex sizes:
  vertex_size <- rep(2, vcount(network))
  vertex_size[ceo_nodes] <- 10  # Larger size for CEO nodes
  vertex_size[educ_nodes] <- 5  # Smaller size for Education nodes
  
  # Define two colors for low and high Blau Index scores
  low_color <- "pink"
  high_color <- "yellow"
  
  # Map the normalized Blau index to the two colors
  node_colors <- ifelse(normalized_blau < 0.5, low_color, high_color)
  
  # Handle NA values by setting them to a default color (e.g., gray)
  node_colors[is.na(node_colors)] <- "gray"
  
  # Plot the network
  plot(network,
       layout = layout_nicely(network),
       vertex.size = vertex_size,
       vertex.color = node_colors, 
       vertex.label = NA,
       edge.color = "black",
       asp = 0.5,
       edge.arrow.size = 0.5)
}

# Convert string to proper case
toproper <- function(str) {
  return(tools::toTitleCase(tolower(str)))
}

# Create a list to store plots
blau_plots <- list()

# Generate and store plots in the list using a for loop
for (attr in attributes_to_process) {
  plot_network_with_blau(education_net, blau_indices[[attr]])
  title(main = paste("Network with Blau Index for", toproper(attr)))
  blau_plots[[attr]] <- recordPlot()
  print(blau_plots[attr])
}

# Function to create LaTex tables
create_latex_table <- function(data_frame, caption) {
  table <- kable(data_frame, align = "c", 
                 caption = caption, 
                 format = "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = c("hold_position"))
  return(table)
}

# Calculating Density:
# Two-Mode:
edge_density(education_net)

# One-Mode:
edge_density(ceo_by_ceo_igraph)
edge_density(educ_by_educ_igraph)

# LaTex Tables:
# CEO 2-mode Table:
table_ceo <- create_latex_table(df_ceo[, c(
  "Degree", "Eigenvector", "Betweenness", "Closeness", "Coreness")], 
  "Two-Mode Centrality Measures: CEOs & Universities")

# Education 2-mode Table:
table_educ <- create_latex_table(df_educ[, c(
  "Degree", "Eigenvector", "Betweenness", "Closeness", "Coreness")],
  "Two-Mode Centrality Measures: Universities & CEOs")

# CEO one-mode:
ceo_by_ceo_df <- calculate_centrality_measures(ceo_by_ceo_igraph, ceo_by_ceo_df)
table_ceo_by_ceo <- create_latex_table(ceo_by_ceo_df[, c(
  "Degree", "Eigenvector", "Betweenness", "Closeness", "Coreness")], 
  "One-Mode Centrality Measures: CEOs in University Network")

# Education one-mode:
educ_by_educ_df <- calculate_centrality_measures(educ_by_educ_igraph, educ_by_educ_df)
table_educ_by_educ <- create_latex_table(educ_by_educ_df[, c(
  "Degree", "Eigenvector", "Betweenness", "Closeness", "Coreness")], 
  "One-Mode Centrality Measures: Universities in Universities Network")