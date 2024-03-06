# For each student of each level, how many times they used each feature per thousand words
a1_normalized_counts_students_long$text_level = "A1"
a2_normalized_counts_students_long$text_level = "A2"
b1_normalized_counts_students_long$text_level = "B1"
b2_normalized_counts_students_long$text_level = "B2"
c1_normalized_counts_students_long$text_level = "C1"
c2_normalized_counts_students_long$text_level = "C2"

# put all dataframes of students of different levels together
# learnerID, feature, total, text level
normalized_counts_students_long <- bind_rows(a1_normalized_counts_students_long, a2_normalized_counts_students_long, b1_normalized_counts_students_long, b2_normalized_counts_students_long, c1_normalized_counts_students_long, c2_normalized_counts_students_long)

# general mean of each feature
normalized_counts_students_long %>%
  filter(text_level == "A1") %>%
  group_by(feature) %>%
  summarise(avg_freq = mean(total))

# get only feature, text_level and total variables (column)
normalized_students <- normalized_counts_students_long %>%
  select(feature, text_level, total)


anova_results <- list()

unique_feats <- unique(normalized_students$feature)

anova_non_sign_constructs <- c()
anova_sign_constructs <- c()
anova_nan_results_constructs <- c()
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
  
    if (is.nan(p_value)){ # sanity check: anova_nan_results_constructs will remain empty because we are discarding all features with no occurrences
      anova_nan_results_constructs <- append(anova_nan_results_constructs, constr)
    } else if (p_value > 0.05) {
      anova_non_sign_constructs <- append(anova_non_sign_constructs, constr)
    } else {
      anova_sign_constructs <- append(anova_sign_constructs, constr)
    }
  }
  
}

normalized_students <- add_feature_level(normalized_students) 


# For all significant features,  do T-tests to check for significant differences between the feature level and the previous one. 

levels <- c("A1", "A2", "B1", "B2", "C1", "C2")

# store p-values from t-tests
t_tests_p_values <- data.frame(matrix(ncol = 6, nrow = length(anova_sign_constructs)))
names(t_tests_p_values) <- c("feature", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")

# store whether the higher level has significantly higher frequency
t_tests_l2_greater <- data.frame(matrix(ncol = 6, nrow = length(anova_sign_constructs)))
names(t_tests_l2_greater) <- c("feature", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")



# For all relevant constructs, do a t-test between all consecutive levels
for (f in 1:length(anova_sign_constructs)) {
    constr <- anova_sign_constructs[f]
    p_values <- c()
    l2_greater <- c() # Whether the T test estimated a greater mean for the greater level
    for (i in 1:(length(levels) - 1)){
      level1 <- levels[i]
      level2 <- levels[i + 1]
      print(paste0("Construct: ", constr))
      
      appears_in_lev1 <- sum(filter(normalized_students, text_level == level1, feature == constr)$total) > 0
      appears_in_lev2 <- sum(filter(normalized_students, text_level == level2, feature == constr)$total) > 0
      
      # Check that the current feature appears in the level before and in the current level
      if (!appears_in_lev1 | !appears_in_lev2) { 
        if (!appears_in_lev1 & !appears_in_lev2){
          print(paste0("Contsruct ", constr, " does not appear in any texts written by ", level1, " or ", level2, " students"))
        } else if (!appears_in_lev1){
          print(paste0("Contsruct ", constr, " does not appear in any texts written by ", level1, " students"))
        } else if (!appears_in_lev2) {
          print(paste0("Contsruct ", constr, " does not appear in any texts written by ", level2, " students"))
        }
        
        p_values <- append(p_values, NA) 
        l2_greater <- append(l2_greater, NA)
      } else { # It must appear in BOTH levels to be compared
        
        t_test_result <- normalized_students %>%
          filter(feature == constr, text_level %in% c(level1, level2)) %>%
          t.test(total ~ text_level, data = .)
        
        p_values <- append(p_values, t_test_result$p.value)
        if (t_test_result$p.value <= 0.05){
          l2_greater <- append(l2_greater, t_test_result$statistic < 0)
          
          
        } else {
          l2_greater <- append(l2_greater, NA)
        }
        
      }
     
    }
    print(l2_greater)
    construct_pval_row <- append(as.character(constr), p_values)
    construct_l2_greater_row <- append(as.character(constr), l2_greater)
    #names(construct_pval_row) <- names(t_tests_p_values)
    print(construct_pval_row)
    t_tests_p_values[f,] <- construct_pval_row
    t_tests_l2_greater[f, ] <- construct_l2_greater_row
    
}

# We only care about construct-level pairs that have TRUE in t_tests_l2_greater, but we care about their p_value
t_tests_relevant_p_vals <- data.frame(matrix(ncol = 6, nrow = length(anova_sign_constructs)))
names(t_tests_relevant_p_vals) <- c("feature", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")

t_tests_relevant_p_vals$feature <- t_tests_p_values$feature

# populate t_tests_relevant_p_vals with p values, only when they are significant and the second level is of higher frequency than the first one
for (col_idx in 2:ncol(t_tests_relevant_p_vals)) { # the first column has the construct ID, skip
  for (row_idx in 1:nrow(t_tests_relevant_p_vals)) {
    if (!is.na(t_tests_l2_greater[row_idx, col_idx]) && as.logical(t_tests_l2_greater[row_idx, col_idx])) {
      t_tests_relevant_p_vals[row_idx, col_idx] <- t_tests_p_values[row_idx, col_idx]
    }
  }
}


t_tests_relevant_p_vals <- add_feature_level(t_tests_relevant_p_vals)
t_tests_relevant_p_vals <- rename_column(t_tests_relevant_p_vals, 7, "EGP_level")

t_tests_relevant_p_vals <- rename_column(t_tests_relevant_p_vals, 1, "construct_ID")


for (col in 2:6) {
  t_tests_relevant_p_vals[, col] <- as.numeric(t_tests_relevant_p_vals[,col])
}

t_level_predictions <- data.frame(feature = anova_sign_constructs) %>%
  add_feature_level() %>%
  rename(EGP_level = feat_level) %>%
  rename(construct = feature) %>%
  mutate(min_p_pred = NA, first_sign_p_pred = NA, signif_at_EGP_level = NA)


# Find first significant diff
for (r in 1:nrow(t_tests_relevant_p_vals)){
  print(t_tests_relevant_p_vals[r, 1])
  for (c in 2:6){
    if(!is.na(t_tests_relevant_p_vals[r, c])){
      print(t_tests_relevant_p_vals[r, c])
      t_level_predictions$first_sign_p_pred[r] <- str_split_i(colnames(t_tests_relevant_p_vals)[c], "_", 2)
      break
    }
      
  }
}



# find lowest p-value
for (r in 1:nrow(t_tests_relevant_p_vals)){
    if (any(!is.na(t_tests_relevant_p_vals[r, 2:6 ]))){
      # get the name of the column with the minimum p-val
      column_name <- names(t_tests_relevant_p_vals)[apply(t_tests_relevant_p_vals[r, 1:6], 1, which.min)]
      predicted_level <- str_split_i(column_name, "_", 2)
      print(paste("Predicting:", predicted_level))
      
      print(paste("Construct ID:", t_tests_relevant_p_vals$construct_ID[r]))
      
      t_level_predictions[r, "min_p_pred"] <- predicted_level
    }
  
}



