# Set up WD and load packages

setwd("~/")
setwd("Documents/SNA")
library("statnet")
library("igraph")
library("skimr")
library("janitor")
library("intergraph")

# Read in Gal. data
load("ceos.RData")
adj_matrix <- as.matrix(CEOs)
g <- graph_from_incidence_matrix(adj_matrix, directed = FALSE)

