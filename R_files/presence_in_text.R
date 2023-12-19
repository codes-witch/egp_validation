setwd("Documents/corpus_annotation_tp/R_files/")
load(".RData")
source("term_paper_functions.R")
library(ggplot2)
library(gridExtra)
library("dplyr")
library("stringr")
library(tidyr)
load("ef2_short.RData")



##### NEW DATA #####
put_units_and_lang_in_filenames("../data/output/a1/text", ef2_short)
n_texts_level <- count_texts_per_level("../data/output_50k/")
View(n_texts_level)
feats_presence_level_percent_long <- get_feat_presence_in_texts(all_features, "../data/output_50k/")
feats_presence_level_percent_short <- get_feat_presence_in_texts(all_features, "../data/output_50k/", FALSE, TRUE)
feats_presence_level_only <- feats_presence_level_percent_short [,2:ncol(feats_presence_level_percent_short)]


# Get the columns that have all zeros (those features were not found)
zero_feats <- colnames(feats_presence_level_only[,colSums(feats_presence_level_only) == 0])

# Remove all features that were not found
feats_presence_level_percent_long <- subset(feats_presence_level_percent_long , !(feats_presence_level_percent_long$feature %in% zero_feats))
feats_presence_level_percent_short <- feats_presence_level_percent_short[, !colnames(feats_presence_level_percent_short) %in% zero_feats]



# Get the can-do statement for each feature and add to the long dataframes
feats_cando <- read.csv("../egp_list.csv", sep = ";", header = TRUE)
feats_cando <- delete_column(feats_cando, "SubCategory")
feats_cando <- delete_column(feats_cando, "Type")
feats_cando <- delete_column(feats_cando, "Guideword")
feats_cando <- delete_column(feats_cando, "Example")
feats_cando <- delete_column(feats_cando, "Level")

feats_presence_level_percent_long$cando <- feats_cando$Can.do.statement[match(feats_presence_level_percent_long$feature, feats_cando$EGP_ID)] 
feats_presence_level_percent_long$supercategory <- feats_cando$SuperCategory[match(feats_presence_level_percent_long$feature, feats_cando$EGP_ID)]
feats_presence_level_percent_long$subcategory <- feats_cando$SubCategory[match(feats_presence_level_percent_long$feature, feats_cando$EGP_ID)]
feats_presence_level_percent_long$guideword <- feats_cando$Guideword[match(feats_presence_level_percent_long$feature, feats_cando$EGP_ID)]

a1_long <- get_level_feats("A1", feats_presence_level_percent_long)
plot_percentages_level(a1_long, "Presence of A1 constructs per CEFR level")
a2_long <- get_level_feats("A2", feats_presence_level_percent_long)
plot_percentages_level(a2_long[a2_long$total < 0.8,], "Presence of A2 constructs per CEFR level")
b1_long <- get_level_feats("b1", feats_presence_level_percent_long)
plot_percentages_level(b1_long, "Presence of B1 constructs per CEFR level")
b2_long <- get_level_feats("b2", feats_presence_level_percent_long)
plot_percentages_level(b2_long, "Presence of B2 constructs per CEFR level")
c1_long <- get_level_feats("c1", feats_presence_level_percent_long)
c1_long <- c1_long[c1_long$total < 0.2,]
plot_percentages_level(c1_long, "Presence of C1 constructs per CEFR level")
c2_long <- get_level_feats("c2", feats_presence_level_percent_long)
plot_percentages_level(c2_long, "Presence of C2 constructs per CEFR level")


# ------------------------------------------------------------------------
# For clustering:
library(factoextra)

# All the data

# WITHOUT SCALING
transp_feats_presence_level_percent_short <- t(feats_presence_level_percent_short[2:ncol(feats_presence_level_percent_short)])
fviz_nbclust(transp_feats_presence_level_percent_short, kmeans, method = "wss")
#I'm choosing 3 clusters
kmeans_output <- kmeans(transp_feats_presence_level_percent_short, centers = 4, nstart = 100)
# visualize cluster
fviz_cluster(list(data=transp_feats_presence_level_percent_short, cluster = kmeans_output$cluster))

# Find mean of each cluster
cluster_means <- aggregate(transp_feats_presence_level_percent_short, by=list(cluster=kmeans_output$cluster), mean)
colnames(cluster_means) <- c("cluster", "a1", "a2", "b1", "b2", "c1", "c2")
# Make cluster means long to plot them
cluster_means_long <- pivot_longer(cluster_means, cols = !cluster, values_to = "value", names_to = "level")
# Plot cluster means
cluster_means_long$cluster <- as.factor(cluster_means_long$cluster)
plot_cluster_means(cluster_means_long, "Clustering over all features")


# _______________________________________________________
# A1 clustering 
# _______________________________________________________
a1 <- feats_presence_level_percent_short[, names(feats_presence_level_percent_short) %in% as.character(1:109)]
a1 <- t(a1)

a1_scaled <- scale(a1)

# Unscaled
fviz_nbclust(a1, kmeans, method="wss") # I'll choose 3
kmeans_out_a1 <- kmeans(a1, centers = 3, nstart = 100)
kmeans_out_a1_4_centers <- kmeans(a1, centers = 4, nstart = 100)

# Visualize cluster
fviz_cluster(list(data=a1, cluster= kmeans_out_a1$cluster), main = "A1 features cluster means (unscaled)")
fviz_cluster(list(data=a1, cluster = kmeans_out_a1_4_centers$cluster))
# Get the means of the clusters
a1_cluster_means <- aggregate(a1, by=list(cluster=kmeans_out_a1$cluster), mean)
colnames(a1_cluster_means) <- c("cluster", "a1", "a2", "b1", "b2", "c1", "c2")

# Visualize them 
a1_cluster_means_long <- pivot_longer(a1_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
a1_cluster_means_long$cluster <- as.factor(a1_cluster_means_long$cluster)
plot_cluster_means(a1_cluster_means_long, "Clustering of A1 features (no scaling)")



# The rest of levels

# A2
a2 <- t(feats_presence_level_percent_short[, names(feats_presence_level_percent_short) %in% as.character(110:397)])
a2 <- a2[!(row.names(a2) == "275"),]
colnames(a2) <- c("a1", "a2", "b1", "b2", "c1", "c2")
fviz_nbclust(a2, kmeans, method="wss")
kmeans_out_a2 <- kmeans(a2, centers = 3, nstart = 100)
fviz_cluster(list(data=a2, cluster= kmeans_out_a2$cluster), main = "A2 construct clusters (presence)") # Feat 275 is a clear outlier
a2_cluster_means <- aggregate(a2, by=list(cluster=kmeans_out_a2$cluster), mean)
a2_cluster_means_long <- pivot_longer(a2_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
a2_cluster_means_long$cluster <- as.factor(a2_cluster_means_long$cluster)
plot_cluster_means(a2_cluster_means_long, "Clustering of A2 features")

# B1
b1 <- t(feats_presence_level_percent_short[,names(feats_presence_level_percent_short) %in% as.character(401:734)])
colnames(b1) <- c("a1", "a2", "b1", "b2", "c1", "c2")
fviz_nbclust(b1, kmeans, method="wss") # Choosing 3 
kmeans_out_b1 <- kmeans(b1, centers = 3, nstart = 100)
fviz_cluster(list(data=b1, cluster= kmeans_out_b1$cluster), main = "B1 construct clusters")
b1_cluster_means <- aggregate(b1, by=list(cluster=kmeans_out_b1$cluster), mean)
b1_cluster_means_long <- pivot_longer(b1_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
b1_cluster_means_long$cluster <- as.factor(b1_cluster_means_long$cluster)
plot_cluster_means(b1_cluster_means_long, "Clustering of B1 features")

# B2
b2 <- t(feats_presence_level_percent_short[,names(feats_presence_level_percent_short) %in% as.character(739:977)])
b2 <- b2[!(row.names(b2) == "785"),]
colnames(b2) <- c("a1", "a2", "b1", "b2", "c1", "c2")
fviz_nbclust(b2, kmeans, method="wss") # Choosing 5
kmeans_out_b2 <- kmeans(b2, centers = 4, nstart = 100)
fviz_cluster(list(data=b2, cluster= kmeans_out_b2$cluster), main = "B2 construct clusters (presence)")
b2_cluster_means <- aggregate(b2, by=list(cluster=kmeans_out_b2$cluster), mean)
b2_cluster_means_long <- pivot_longer(b2_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
b2_cluster_means_long$cluster <- as.factor(b2_cluster_means_long$cluster)
plot_cluster_means(b2_cluster_means_long, "Clustering of B2 features")


# C1
c1 <- t(feats_presence_level_percent_short[,names(feats_presence_level_percent_short) %in% as.character(982:1105)])
# remove misbehaving feature
c1 <- c1[!(row.names(c1) == "1057"),]
colnames(c1) <- c("a1", "a2", "b1", "b2", "c1", "c2")
fviz_nbclust(c1, kmeans, method="wss") # Choosing 4
kmeans_out_c1 <- kmeans(c1, centers = 4, nstart = 100)
fviz_cluster(list(data=c1, cluster= kmeans_out_c1$cluster), main = "C1 features cluster means (unscaled)")
c1_cluster_means <- aggregate(c1, by=list(cluster=kmeans_out_c1$cluster), mean)
c1_cluster_means_long <- pivot_longer(c1_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
c1_cluster_means_long$cluster <- as.factor(c1_cluster_means_long$cluster)
plot_cluster_means(c1_cluster_means_long, "Clustering of C1 features")

c2 <- t(feats_presence_level_percent_short[,names(feats_presence_level_percent_short) %in% as.character(1111:1203)])
colnames(c2) <- c("a1", "a2", "b1", "b2", "c1", "c2")
fviz_nbclust(c2, kmeans, method="wss") # Choosing 4
kmeans_out_c2 <- kmeans(c2, centers = 4, nstart = 100)
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
feats_presence_level_percent_long <- add_clusters_to_long(kmeans_output$cluster, feats_presence_level_percent_long)
feats_presence_level_percent_long <- add_feature_level(feats_presence_level_percent_long)

# ------------------- Make boxplots ------------------

# get a df where features are matched to the levels they belong to
with_feat_lvl <- add_feature_level(normalized_counts_long)
make_boxpolot_group(with_feat_lvl, ylim_vector = c(0, 100))
