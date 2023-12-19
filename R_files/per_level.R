setwd("Documents/corpus_annotation_tp/R_files/")
load(".RData")
source("term_paper_functions.R")
library(ggplot2)
library(gridExtra)
library("dplyr")
library("stringr")
library(tidyr)
load("ef2_short_lvl.RData")


# Dataframe with word counts
per_level_word_counts <- data.frame(matrix(ncol = 1, nrow = 6))
per_level_word_counts[] <- 0

rownames(per_level_word_counts) <- c("a1", "a2", "b1", "b2", "c1", "c2")
colnames(per_level_word_counts) <- "n_words"

for (level in rownames(per_level_word_counts)){
  per_level_word_counts[level, "n_words"] <- count_words_in_directory(paste0("../data/output_50k/", level, "/text"))

}

count_feats_per_level_short <- get_feat_count_per_level(all_features, "../data/output_50k/", FALSE)
count_feats_per_level_long <- make_long_feats_df(count_feats_per_level_short, "level", "total")

# Remove features that are never found
feats_level_only <- count_feats_per_level_short[,2:ncol(count_feats_per_level_short)]
zero_feats_new <- colnames(feats_level_only[,colSums(feats_level_only) == 0])

count_feats_per_level_long <- subset(count_feats_per_level_long, !(count_feats_per_level_long$feature %in% zero_feats_new))
count_feats_per_level_short <- count_feats_per_level_short[,!colnames(count_feats_per_level_short) %in% zero_feats_new]

# Normalize per number of words in level and multiply by 1000 to get frequency per 1000 words. 
normalized_counts <- cbind("level" = c("a1", "a2", "b1", "b2", "c1", "c2"), 
                           (count_feats_per_level_short[,2:ncol(count_feats_per_level_short)] / per_level_word_counts$n_words) * 1000 )
normalized_counts_long <- make_long_feats_df(normalized_counts, "level", "total")

a1_long <- get_level_feats("A1", normalized_counts_long)
plot_percentages_level(a1_long, "Frequecy of A1 features per CEFR level (per thousand words)") 

# We see that 71 and 72 appear too often to let us see the behaviour of the remaining features
plot_percentages_level(a1_long[a1_long$feature != 71 & a1_long$feature != 72,], "Frequecy of A1 features per CEFR level (per thousand words)")

a2_long <- get_level_feats("A2", normalized_counts_long)
plot_percentages_level(a2_long, "Frequecy of A2 features per CEFR level (per thousand words)") 
# feature 275 stands out

plot_percentages_level(a2_long[a2_long$feature != 275,], "Frequecy of A2 features per CEFR level (per thousand words)")

b1_long <- get_level_feats("B1", normalized_counts_long)
plot_percentages_level(b1_long, "Frequecy of B1 features per CEFR level (per thousand words)")


b2_long <- get_level_feats("B2", normalized_counts_long)
# The feature sticking out at A1 is 785: 
# "FORM: OF + NOUN PHRASE ''S' Can use ''s' after a noun phrase with of to indicate possession."
plot_percentages_level(b2_long, "Frequecy of B2 features per CEFR level (per thousand words)") 
# remove 785 from analysis
b2_long <- b2_long[b2_long$feature != 785,]
plot_percentages_level(b2_long, "Frequecy of B2 features per CEFR level (per thousand words)") 


c1_long <- get_level_feats("C1", normalized_counts_long)
c1_long <- c1_long[c1_long$total < 5,]
plot_percentages_level(c1_long, "Frequecy of C1 features per CEFR level (per thousand words)")

c2_long <- get_level_feats("C2", normalized_counts_long)
plot_percentages_level(c2_long, "Frequecy of C2 features per CEFR level (per thousand words)")

# ------------------------------------------------------------------------
# For clustering:
library(factoextra)

# _______________________________________________________
# A1 clustering 
# _______________________________________________________
a1 <- normalized_counts[, names(normalized_counts) %in% as.character(1:109)]
a1 <- t(a1)
fviz_nbclust(a1, kmeans, method="wss") # I'll choose 5
kmeans_out_a1 <- kmeans(a1, centers = 5, nstart = 100)
# Visualize cluster
fviz_cluster(list(data=a1, cluster= kmeans_out_a1$cluster), main = "A1 features cluster means (unscaled)")
# Get the means of the clusters
a1_cluster_means <- aggregate(a1, by=list(cluster=kmeans_out_a1$cluster), mean)
colnames(a1_cluster_means) <- c("cluster", "a1", "a2", "b1", "b2", "c1", "c2")

# Visualize them 
a1_cluster_means_long <- pivot_longer(a1_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
a1_cluster_means_long$cluster <- as.factor(a1_cluster_means_long$cluster)
plot_cluster_means(a1_cluster_means_long, "Clustering of A1 features (no scaling)")



# The rest of levels

# A2
a2 <- normalized_counts[, names(normalized_counts) %in% as.character(110:397)]
a2 <- delete_column(a2, "275")
a2 <- t(a2)
colnames(a2) <- c("a1", "a2", "b1", "b2", "c1", "c2")
fviz_nbclust(a2, kmeans, method="wss") # either 4 or 5 centers
kmeans_out_a2 <- kmeans(a2, centers = 5, nstart = 100)
fviz_cluster(list(data=a2, cluster= kmeans_out_a2$cluster), main = "A2 features cluster means (unscaled)") # Feat 275 is a clear outlier
a2_cluster_means <- aggregate(a2, by=list(cluster=kmeans_out_a2$cluster), mean)
a2_cluster_means_long <- pivot_longer(a2_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
a2_cluster_means_long$cluster <- as.factor(a2_cluster_means_long$cluster)
plot_cluster_means(a2_cluster_means_long, "Clustering of A2 features")

# B1
b1 <- normalized_counts[, names(normalized_counts) %in% as.character(401:734)]
b1 <- delete_column(b1, "477")
b1 <- delete_column(b1, "574")
b1 <- t(b1)
colnames(b1) <- c("a1", "a2", "b1", "b2", "c1", "c2")
fviz_nbclust(b1, kmeans, method="wss") # Choosing 3 
kmeans_out_b1 <- kmeans(b1, centers = 5, nstart = 100)
fviz_cluster(list(data=b1, cluster= kmeans_out_b1$cluster), main = "B1 features cluster means (unscaled)")
b1_cluster_means <- aggregate(b1, by=list(cluster=kmeans_out_b1$cluster), mean)
b1_cluster_means_long <- pivot_longer(b1_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
b1_cluster_means_long$cluster <- as.factor(b1_cluster_means_long$cluster)
plot_cluster_means(b1_cluster_means_long, "Clustering of B1 features")

# B2
b2 <- normalized_counts[, names(normalized_counts) %in% as.character(739:977)]
b2 <- delete_column(b2, "785")
b2 <- t(b2)
colnames(b2) <- c("a1", "a2", "b1", "b2", "c1", "c2")
fviz_nbclust(b2, kmeans, method="wss") # Choosing 4
kmeans_out_b2 <- kmeans(b2, centers = 4, nstart = 100)
fviz_cluster(list(data=b2, cluster= kmeans_out_b2$cluster), main = "B2 features cluster means (unscaled)")
b2_cluster_means <- aggregate(b2, by=list(cluster=kmeans_out_b2$cluster), mean)
b2_cluster_means_long <- pivot_longer(b2_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
b2_cluster_means_long$cluster <- as.factor(b2_cluster_means_long$cluster)
plot_cluster_means(b2_cluster_means_long, "Clustering of B2 features")


# C1
c1 <- normalized_counts[, names(normalized_counts) %in% as.character(982:1105)]
# remove misbehaving feature
c1 <- delete_column(c1, "1057")
c1 <- t(c1)
colnames(c1) <- c("a1", "a2", "b1", "b2", "c1", "c2")
fviz_nbclust(c1, kmeans, method="wss") # Choosing 4
kmeans_out_c1 <- kmeans(c1, centers = 4, nstart = 100)
fviz_cluster(list(data=c1, cluster= kmeans_out_c1$cluster), main = "C1 features cluster means (unscaled)")
c1_cluster_means <- aggregate(c1, by=list(cluster=kmeans_out_c1$cluster), mean)
c1_cluster_means_long <- pivot_longer(c1_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
c1_cluster_means_long$cluster <- as.factor(c1_cluster_means_long$cluster)
plot_cluster_means(c1_cluster_means_long, "Clustering of C1 features")

c2 <- normalized_counts[, names(normalized_counts) %in% as.character(1111:1203)]
c2 <- t(c2)
colnames(c2) <- c("a1", "a2", "b1", "b2", "c1", "c2")
fviz_nbclust(c2, kmeans, method="wss") # Choosing 4
kmeans_out_c2 <- kmeans(c2, centers = 5, nstart = 100)
fviz_cluster(list(data=c2, cluster= kmeans_out_c2$cluster), main = "C2 features cluster means (unscaled)")
c2_cluster_means <- aggregate(c2, by=list(cluster=kmeans_out_c2$cluster), mean)
c2_cluster_means_long <- pivot_longer(c2_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
c2_cluster_means_long$cluster <- as.factor(c2_cluster_means_long$cluster)
plot_cluster_means(c2_cluster_means_long, "Clustering of C2 features")


# Add clusters to long DFs with features. Now we can see which features and can-do statements are in the clusters
a1_long <- add_clusters_to_long(kmeans_out_a1$cluster, a1_long)
a2_long <- add_clusters_to_long(kmeans_out_a2$cluster, a2_long)
b1_long <- add_clusters_to_long(kmeans_out_b1$cluster, b1_long)
b2_long <- add_clusters_to_long(kmeans_out_b2$cluster, b2_long)
c1_long <- add_clusters_to_long(kmeans_out_c1$cluster, c1_long)
c2_long <- add_clusters_to_long(kmeans_out_c2$cluster, c2_long)
feats_level_percent_long <- add_clusters_to_long(kmeans_output$cluster, feats_level_percent_long)
feats_level_percent_long <- add_feature_level(feats_level_percent_long)

