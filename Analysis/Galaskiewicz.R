# Set up WD and load packages
setwd("~/")
setwd("Documents/galaskiewicz_sna_project-main")
library("statnet")
library("igraph")
library("skimr")
library("janitor")
library("intergraph")
library("knitr")

# Read in Gal. data
load("Data/ceos.RData")
adj_matrix <- as.matrix(CEOs)
g <- graph_from_incidence_matrix(adj_matrix, directed = FALSE)

# Create one mode matrices: CEO-by-CEO and club-by-club
adj_matrix_t <- t(adj_matrix)
ceo_by_ceo <- adj_matrix %*% adj_matrix_t
print(ceo_by_ceo)
club_by_club <- adj_matrix_t %*% adj_matrix
print(club_by_club)

# Base plots of two mode and one mode networks
sna::gplot(adj_matrix, 
           gmode = "twomode", 
           label = c(rownames(adj_matrix), colnames(adj_matrix)), 
           mode = "fruchtermanreingold",
           vertex.cex = 1.5,
           usearrows = FALSE,
           edge.col = "black",
           vertex.border = "black",
           label.pos = 5,
           label.cex = 0.8,
           main = "CEOs and Social Clubs (Galaskiewicz)")
gplot(ceo_by_ceo, 
      edge.lwd = 0.5*ceo_by_ceo,
      label = rownames(ceo_by_ceo),
      vertex.col="light blue",
      mode = "fruchtermanreingold",
      vertex.cex=1.5,
      usearrows=FALSE,
      edge.col = "black",
      vertex.border = "black",
      label.col = "blue",
      label.pos = 5,
      label.cex = 0.6,
      main = "CEO-by-CEO Network (Galaskiewicz)")
gplot(club_by_club, 
      edge.lwd = 0.5*club_by_club,
      label = rownames(club_by_club),
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

# Latex tables with two-mode centrality measures for CEOs and Clubs
degree_centrality <- degree(g, normalized = TRUE)
eigenvector_centrality <- eigen_centrality(g)$vector
betweenness_centrality <- betweenness(g, normalized = TRUE)
closeness_centrality <- closeness(g, normalized = TRUE)
df <- data.frame(Degree = round(degree_centrality, 3),
                 Eigenvector= round(eigenvector_centrality, 3),
                 Betweenness = round(betweenness_centrality, 3),
                 Closeness = round(closeness_centrality, 3))
df_ceo <- df[1:26, ]
df_club <- df[27:nrow(df), ]
table_ceo <- kable(df_ceo, align = "c", caption = "Two-Mode Centrality Measures: Galaskiewicz CEOs", format = "latex", booktabs = TRUE)
cat("Table 1 (CEO Nodes):\n")
print(table_ceo)
table_club <- kable(df_club, align = "c", caption = "Two-Mode Centrality Measures: Galaskiewicz Clubs", format = "latex", booktabs = TRUE)
cat("Table 2 (Club Nodes):\n")
print(table_club)

# Latex table with one-mode centrality measures for CEOs (non-binarized)
diag(ceo_by_ceo) <- 0
ceo_by_ceo_matrix <- graph_from_adjacency_matrix(ceo_by_ceo, mode = "undirected")
degree_ceo_nonbin <- degree(ceo_by_ceo_matrix)
eigen_ceo_nonbin <- eigen_centrality(ceo_by_ceo_matrix)$vector
between_ceo_nonbin <- betweenness(ceo_by_ceo_matrix)
close_ceo_nonbin <- closeness(ceo_by_ceo_matrix)
df_ceo_nonbin <- data.frame(Degree = round(degree_ceo_nonbin, 3),
                 Eigenvector= round(eigen_ceo_nonbin, 3),
                 Betweenness = round(between_ceo_nonbin, 3),
                 Closeness = round(close_ceo_nonbin, 3))
table_ceo_nonbin <- kable(df_ceo_nonbin, align = "c", caption = "One-Mode Centrality Measures: Galaskiewicz CEOs (Non-Binarized)", format = "latex", booktabs = TRUE)
cat("Table 3 (CEO Nodes):\n")
print(table_ceo_nonbin)

# Latex table with one-mode centrality measures for Clubs (non-binarized)
diag(club_by_club) <- 0
club_by_club_matrix <- graph_from_adjacency_matrix(club_by_club, mode = "undirected")
degree_club_nonbin <- degree(club_by_club_matrix)
eigen_club_nonbin <- eigen_centrality(club_by_club_matrix)$vector
between_club_nonbin <- betweenness(club_by_club_matrix)
close_club_nonbin <- closeness(club_by_club_matrix)
df_club_nonbin <- data.frame(Degree = round(degree_club_nonbin, 3),
                            Eigenvector= round(eigen_club_nonbin, 3),
                            Betweenness = round(between_club_nonbin, 3),
                            Closeness = round(close_club_nonbin, 3))
table_club_nonbin <- kable(df_club_nonbin, align = "c", caption = "One-Mode Centrality Measures: Galaskiewicz Clubs (Non-Binarized)", format = "latex", booktabs = TRUE)
cat("Table 4 (Club Nodes):\n")
print(table_club_nonbin)

# Binarizing the one-mode matrices
binary_ceo <- ifelse(ceo_by_ceo > 0, 1, 0)
binary_ceo_by_ceo_matrix <- graph_from_adjacency_matrix(binary_ceo, mode = "undirected")
binary_club <- ifelse(club_by_club > 0, 1, 0)
binary_club_by_club_matrix <- graph_from_adjacency_matrix(binary_club, mode = "undirected")

# Latex table with one-mode centrality measures for CEOs (binarized)
degree_ceo_bin <- degree(binary_ceo_by_ceo_matrix)
eigen_ceo_bin <- eigen_centrality(binary_ceo_by_ceo_matrix)$vector
between_ceo_bin <- betweenness(binary_ceo_by_ceo_matrix)
close_ceo_bin <- closeness(binary_ceo_by_ceo_matrix)
df_ceo_bin <- data.frame(Degree = round(degree_ceo_bin, 3),
                            Eigenvector= round(eigen_ceo_bin, 3),
                            Betweenness = round(between_ceo_bin, 3),
                            Closeness = round(close_ceo_bin, 3))
table_ceo_bin <- kable(df_ceo_bin, align = "c", caption = "One-Mode Centrality Measures: Galaskiewicz CEOs (Binarized)", format = "latex", booktabs = TRUE)
cat("Table 5 (CEO Nodes):\n")
print(table_ceo_bin)

# Latex table with one-mode centrality measures for CEOs (binarized)
degree_club_bin <- degree(binary_club_by_club_matrix)
eigen_club_bin <- eigen_centrality(binary_club_by_club_matrix)$vector
between_club_bin <- betweenness(binary_club_by_club_matrix)
close_club_bin <- closeness(binary_club_by_club_matrix)
df_club_bin <- data.frame(Degree = round(degree_club_bin, 3),
                         Eigenvector= round(eigen_club_bin, 3),
                         Betweenness = round(between_club_bin, 3),
                         Closeness = round(close_club_bin, 3))
table_club_bin <- kable(df_club_bin, align = "c", caption = "One-Mode Centrality Measures: Galaskiewicz Clubs (Binarized)", format = "latex", booktabs = TRUE)
cat("Table 6 (lub Nodes):\n")
print(table_club_bin)

# Clustering coefficient
global_clust_coef_club <- transitivity(binary_club_by_club_matrix, type="undirected")
local_clust_coefs_club <- local_transitivity(binary_club_by_club_matrix, mode="undirected")
df_clust_club <- data.frame(Coefficient = round(local_clust_coefs_club, 3))
table_clust_club <- kable(df_clust_club, align = "c", caption = "Local Clustering Coefficients: Galaskiewicz Clubs (Binarized)", format = "latex", booktabs = TRUE)
cat("Table 7 (lub Nodes):\n")
print(table_clust_club)

# Network density


# Average path length


# Clique detection




