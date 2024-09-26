# Load the required libraries
library(psychonetrics)
library(bootnet)
library(qgraph)
library(psych)  # For loading the bfi dataset

# Load the Big Five personality dataset
data(bfi, package = "psych")

# Remove rows with missing values
bfi_clean <- na.omit(bfi)

# Create composite scores for each personality trait by averaging their respective items
bfi_composite <- data.frame(
  Extraversion = rowMeans(bfi_clean[, c("E1", "E2", "E3", "E4", "E5")]),
  Agreeableness = rowMeans(bfi_clean[, c("A1", "A2", "A3", "A4", "A5")]),
  Conscientiousness = rowMeans(bfi_clean[, c("C1", "C2", "C3", "C4", "C5")]),
  Neuroticism = rowMeans(bfi_clean[, c("N1", "N2", "N3", "N4", "N5")]),
  Openness = rowMeans(bfi_clean[, c("O1", "O2", "O3", "O4", "O5")])
)

# Display the first few rows of the composite scores
print(head(bfi_composite))

# Estimate the Gaussian graphical model using the composite scores
bfi_composite_network <- estimateNetwork(bfi_composite, default = "EBICglasso")

# Visualize the network
plot(bfi_composite_network)

# Extract the pairwise edge weights from the network
edge_weights <- getWmat(bfi_composite_network)
edge_weights_df <- as.data.frame(edge_weights)

# Print the edge weights table
print(edge_weights_df)

# Perform a nonparametric bootstrap to assess the stability of the edges
bfi_bootstrap <- bootnet(bfi_composite_network, nBoots = 1000)

# Plot the results of the bootstrapped edge strength
plot(bfi_bootstrap)

# Plot centrality measures (strength, closeness, betweenness) for the composite network
centralityPlot(bfi_composite_network, include = c("Strength", "Closeness", "Betweenness"))

# Extract and print the betweenness centrality values
centrality_values <- centrality(bfi_composite_network)$Betweenness
print(centrality_values)

