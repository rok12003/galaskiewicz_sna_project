# Load necessary packages
library(igraph)
library(tidyverse)
library(knitr)
library(kableExtra)
library(statnet)

# Load preprocessed data
setwd("/Users/rohitkandala/Desktop/UChicago/Academic Quarters/2023-24/Spring 2024/MACSS 40101/galaskiewicz_sna_project-main")
load("Data/allnetworks.RData")

# Categorize nodes:
num_nodes <- vcount(org_net)
ceo_nodes <- 1:16
org_nodes <- 17:num_nodes

# Define vertex attributes to include
attributes_to_include <- c(
  "company", "age", "gender", "mba", "marketcap", "industry", "sector", "level")

# Convert igraph object to a data frame and select specific vertex attributes:
df_all <- igraph::as_data_frame(
  org_net, what = "vertices")[, attributes_to_include, drop = FALSE]

# Create node-specific dataframes:
df_ceo <- df_all[ceo_nodes, ]
df_org <- df_all[org_nodes, ]

# Create one-mode networks (CEO-by-CEO & Org-by-Org) from igraph object:
adj_matrix <- as.matrix(as_adjacency_matrix(org_net))
adj_matrix_t <- t(adj_matrix)
ceo_by_ceo <- adj_matrix[1:16, ] %*% adj_matrix_t[, 1:16]
org_by_org <- adj_matrix[17:num_nodes, ] %*% adj_matrix_t[, 17:num_nodes]

# Generalized plot function to inspect network:
plot_network <- function(network) {
  
  # Create a vector for node colors
  node_colors <- rep("blue", num_nodes)  
  node_colors[ceo_nodes] <- "red"       
  
  # Plot the network
  plot(network,
       layout = layout_nicely(network),
       vertex.size = 5,
       vertex.color = node_colors, 
       vertex.label = NA,
       edge.color = "black",
       asp = 0.5,
       edge.arrow.size = 0.5)
}

# Let's check this plot function!
plot_network(org_net)

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
      categories <- unique(attribute_neighbors)
      proportions <- numeric(length(categories))
      for (i in seq_along(categories)) {
        proportions[i] <- sum(
          attribute_neighbors == categories[i]) / num_neighbors
      }
      blau[node] <- 1 - sum(proportions^2)
    }
  }
  
  # Normalize the Blau index values
  min_val <- min(blau, na.rm = TRUE)
  max_val <- max(blau, na.rm = TRUE)
  normalized_blau <- (blau - min_val) / (max_val - min_val)
  
  return(normalized_blau)
}

# Vertex attributes we want to run through Blau index
attributes_to_process <- setdiff(vertex_attr_names(org_net), c("type", "name"))

# Create an empty data frame to store the results:
blau_indices <- data.frame(node = V(org_net)$name)

# Calculate normalized Blau index for each attribute and store in data frame:
for (attr in attributes_to_process) {
  blau_indices[[attr]] <- calculate_blau_index(org_net, attr)
}

# Plot function to inspect blau index:
plot_network_with_blau <- function(network, blau_indices) {
  
  # Normalize the Blau index
  normalized_blau <- blau_indices
  
  # Defining vertex sizes:
  vertex_size <- rep(2, vcount(network))
  vertex_size[ceo_nodes] <- 10  # Larger size for CEO nodes
  vertex_size[org_nodes] <- 5   # Smaller size for org nodes
  
  # Define a color gradient
  colors <- colorRampPalette(c("blue", "green", "yellow", "red"))(100)
  
  # Map the normalized Blau index to the color gradient
  node_colors <- colors[ceiling(normalized_blau * 99) + 1]
  
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
  plot_network_with_blau(org_net, blau_indices[[attr]])
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

# LaTex Tables:
# CEO 2-mode Table:
table_ceo <- create_latex_table(df_ceo[, c(
  "Degree", "Eigenvector", "Betweenness", "Closeness")], 
  "Two-Mode Centrality Measures: CEOs")

# Org 2-mode Table:
table_org <- create_latex_table(df_org[, c(
  "Degree", "Eigenvector", "Betweenness", "Closeness")],
  "Two-Mode Centrality Measures: Orgs")

# CEO one-mode:


# Org one-mode

# 

# Graphs:
# CEO One-Mode Graph:
gplot(ceo_by_ceo, 
      edge.lwd = 0.01*ceo_by_ceo,
      label = rownames(ceo_by_ceo),
      vertex.col="light blue",
      mode = "fruchtermanreingold",
      vertex.cex=1.5,
      usearrows=FALSE,
      edge.col = "black",
      vertex.border = "black",
      label.col = "red",
      label.pos = 5,
      label.cex = 0.6,
      main = "CEO-by-CEO Network")

# Org One-Mode Graph:
gplot(org_by_org, 
      edge.lwd = 0.5*org_by_org,
      label = rownames(org_by_org),
      vertex.col="light blue",
      mode = "fruchtermanreingold",
      vertex.cex=1.5,
      usearrows=FALSE,
      edge.col = "black",
      vertex.border = "black",
      label.col = "blue",
      label.pos = 5,
      label.cex = 0.6,
      main = "Club-by-Club Network (Galaskiewicz)")


