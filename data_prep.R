# Importing packages
library(tidyverse)
library(igraph)

# Preprocessing function that applies to columns and contents:
preprocess_df <- function(df) {
  
  # Remove punctuation from row names and column names:
  rownames(df) <- gsub("[[:punct:] ]", "", tolower(rownames(df)))
  colnames(df) <- gsub("[[:punct:] ]", "", tolower(colnames(df)))
  
  # Lowercase and remove punctuation from data frame contents:
  df <- df %>%
    mutate(across(everything(), ~ gsub("[[:punct:] ]", "", 
                                       tolower(as.character(.)))))
  
  return(df)
}

# Reading in the CSV files:
load("ceos.RData")
org_df <- read.csv("Affiliation Matrix - Orgs.csv")
education_df <- read.csv("Affiliation Matrix - Education.csv")
ceo_attr_df <- read.csv("Affiliation Matrix - CEO_Attr.csv")
org_attr_df <- read.csv("Affiliation Matrix - Org_Attr.csv")

# Applying the preprocessing function:
org_df <- preprocess_df(org_df)
education_df <- preprocess_df(education_df)
ceo_attr_df <- preprocess_df(ceo_attr_df)
org_attr_df <- preprocess_df(org_attr_df)

# Set the first column as row names & remove it from the data frame:
# For orgs:
rownames(org_df) <- org_df[[1]]
org_df <- org_df[,-1]

# For education:
rownames(education_df) <- education_df[[1]]
education_df <- education_df[,-1]

# Converting dataframes to matrices:
org_matrix <- as.matrix(org_df)
education_matrix <- as.matrix(education_df)

# Creating the bipartite networks:
org_net <- graph_from_incidence_matrix(org_matrix, directed = FALSE)
education_net <- graph_from_incidence_matrix(education_matrix, directed = FALSE)

# Function to add CEO attributes:
add_ceo_attributes <- function(graph, attr_df) {
  for (col in colnames(attr_df)[-1]) {  # Exclude 'ceo' column
    for (i in 1:nrow(attr_df)) {
      ceo_name <- attr_df$ceo[i]
      if (ceo_name %in% V(graph)$name) {
        graph <- set_vertex_attr(graph, name = col, index = V(
          graph)[name == ceo_name], value = attr_df[[col]][i])
      }
    }
  }
  return(graph)
}

# Function to add organization attributes:
add_org_attributes <- function(graph, attr_df) {
  for (col in colnames(attr_df)[-1]) {  # Exclude 'org' column
    for (i in 1:nrow(attr_df)) {
      org_name <- attr_df$org[i]
      if (org_name %in% V(graph)$name) {
        graph <- set_vertex_attr(graph, name = col, index = V(
          graph)[name == org_name], value = attr_df[[col]][i])
      }
    }
  }
  return(graph)
}

# Adding CEO attributes to both networks:
org_net <- add_ceo_attributes(org_net, ceo_attr_df)
education_net <- add_ceo_attributes(education_net, ceo_attr_df)

# Adding org attributes to org_net only:
org_net <- add_org_attributes(org_net, org_attr_df)