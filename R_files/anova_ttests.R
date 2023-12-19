a1_normalized_counts_students_long$text_level = "A1"
a2_normalized_counts_students_long$text_level = "A2"
b1_normalized_counts_students_long$text_level = "B1"
b2_normalized_counts_students_long$text_level = "B2"
c1_normalized_counts_students_long$text_level = "C1"
c2_normalized_counts_students_long$text_level = "C2"

# put all dataframes of students of different levels together
normalized_counts_students_long <- bind_rows(a1_normalized_counts_students_long, a2_normalized_counts_students_long, b1_normalized_counts_students_long, b2_normalized_counts_students_long, c1_normalized_counts_students_long, c2_normalized_counts_students_long)

# general mean of each feature
normalized_counts_students_long %>%
  filter(text_level == "A1") %>%
  group_by(feature) %>%
  summarise(avg_freq = mean(total))

# get only feature, text_level and total variables
normalized_students <- normalized_counts_students_long %>%
  select(feature, text_level, total)


anova_results <- list()

unique_feats <- unique(normalized_students$feature)

non_significant_feats <- c()
nan_results_feats <- c()
zero_feats_all_learners <- c()

# ANOVA. Check which features have significant differences between their frequencies in levels levels.
for (constr in unique_feats) {
  
  # df with only the current construct. Check whether there are no occurrences of the feature.
  feature_dataframe <- normalized_students %>%
    filter(feature == constr)
  # Keep track of constructs with no occurrences
  if (sum(feature_dataframe$total) == 0){
    print(paste0(constr, " was a 0 feature"))
    zero_feats_all_learners <- append(zero_feats_all_learners, constr)
    
  } else { # Fill in anova result list 
    anova_results[[constr]] <- normalized_students %>%
      filter(feature == constr) %>%
      aov(total ~ text_level, data = .) %>%
      summary()
    
    p_value = anova_results[[constr]][[1]][[5]][1]
  
    if (is.nan(p_value)){ # nan_results_feats will remain empty because we are discarding all features with no occurrences
      nan_results_feats <- append(nan_results_feats, constr)
    } else if (p_value > 0.05) {
      non_significant_feats <- append(non_significant_feats, constr)
    }
  }
  
}

normalized_students <- add_feature_level(normalized_students) 


# For all significant features,  do T-tests to check for significant differences between the feature level and the previous one. 

# All A2 features: are they they significantly different in A2 texts from A1 texts?

significant_a2_feats <- unique(filter(normalized_students, feat_level == "A2", !feature %in% non_significant_feats, !feature %in% zero_feats_all_learners)$feature)
no_signif_diff_a1_a2<- c()
signif_difference_a1_a2 <- c()
no_occurrences_a1_a2 <- c()


levels <- c("C2", "C1", "B2", "B1", "A2", "A1")

for (constr in significant_a2_feats) {
  
  if (sum(filter(normalized_students, text_level %in% c("A1", "A2"), feature == constr)$total) == 0) {
    print(paste0("Contsruct ", constr, " does not appear in any texts written by A1 or A2 students"))
    no_occurrences_a1_a2 <- append(no_occurrences_a1_a2, constr)
  } else {
  
  t_test_result <- normalized_students %>%
    filter(feature == constr, text_level %in% c("A1", "A2")) %>%
    t.test(total ~ text_level, data = .)

  
    if (t_test_result$p.value > 0.05) {
      no_signif_diff_a1_a2<- append(no_difference_a1_a2, constr)
      print(paste(constr, " has no significant difference between A1 and A2"))
    } else {
      signif_difference_a1_a2 <- append(signif_difference_a1_a2, constr)
    }
  }
  
}



  