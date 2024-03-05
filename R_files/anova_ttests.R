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

non_significant_feats <- c()
significant_feats <- c()
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
    } else {
      significant_feats <- append(significant_feats, constr)
    }
  }
  
}

normalized_students <- add_feature_level(normalized_students) 


# For all significant features,  do T-tests to check for significant differences between the feature level and the previous one. 

levels <- c("A1", "A2", "B1", "B2", "C1", "C2")

t_tests_p_values <- data.frame(matrix(ncol = 6, nrow = length(significant_feats)))
names(t_tests_p_values) <- c("feature", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")



# For all relevant constructs, do a t-test between all consecutive levels
# TODO significance matters in terms of which one is higher!! You are missing that info by only taking into account the p-value. 
for (f in 1:length(significant_feats)) {
    constr <- significant_feats[f]
    p_values <- c()
    for (i in 1:(length(levels) - 1)){
      level1 <- levels[i]
      level2 <- levels[i + 1]
      print(paste0("Construct: ", constr))
      #print(paste0("Level 1: ", level1))
      #print(paste0("Level 2: ", level2))
      
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
      } else { # It must appear in BOTH levels being compared
      
      t_test_result <- normalized_students %>%
        filter(feature == constr, text_level %in% c(level1, level2)) %>%
        t.test(total ~ text_level, data = .)
      
      p_values <- append(p_values, t_test_result$p.value)
      }
     
    }
    
    construct_pval_row <- append(as.character(constr), p_values)
    #names(construct_pval_row) <- names(t_tests_p_values)
    print(construct_pval_row)
    t_tests_p_values[f,] <- construct_pval_row
    
}



t_tests_p_values <- add_feature_level(t_tests_p_values)
t_tests_p_values <- rename_column(t_tests_p_values, 7, "EGP_level")

t_tests_p_values <- rename_column(t_tests_p_values, 1, "construct_ID")


a2_mismatch <- t_tests_p_values %>%
  filter(EGP_level == "A2", as.numeric(A1_A2) > 0.05)

b1_mismatch <-  t_tests_p_values %>%
  filter(EGP_level == "B1", as.numeric(A2_B1) > 0.05)

b2_mismatch <- t_tests_p_values %>%
  filter(EGP_level == "B2", as.numeric(B1_B2) > 0.05)

c1_mismatch <- t_tests_p_values %>%
  filter(EGP_level == "C1", as.numeric(B2_C1) > 0.05)

c2_mismatch <- t_tests_p_values %>%
  filter(EGP_level == "C2", as.numeric(C1_C2) > 0.05)


# for each construct, get the mean at each level and make a line plot
library(ggplot2)

#for (constr in significant_feats){
normalized_counts_students_long %>%
  group_by(feature, text_level) %>%
  filter(feature == 1111) %>%
  summarise(mean = mean(total)) %>%
    ggplot(aes(x = text_level, y = mean, color = as.factor(feature), group = feature)) +
    geom_line() +
    xlab("Levels") +
    ylab("Mean frequency of construct") +
    scale_x_discrete(
      breaks = c("A1", "A2", "B1", "B2", "C1", "C2"),
      labels = c("A1", "A2", "B1", "B2", "C1", "C2")
    ) +
    ggtitle("Typical C1 construct")
#}

#for (f in c(148, 475, 740, 1057)) {
normalized_counts_students_long %>%
  filter(feature == 1057) %>%
  group_by(learnerID, text_level) %>%
  ggplot(aes(x = text_level, y = total, fill = text_level)) +
  geom_boxplot() +
  xlab("Text Level") +
  ylab("Mean of Total") +
  ggtitle(paste0("Boxplot for feature ", 1057))

for (col in 2:6) {
  t_tests_p_values[, col] <- as.numeric(t_tests_p_values[,col])
}

level_prediction <- data.frame(feature = significant_feats) %>%
  add_feature_level() %>%
  rename(EGP_level = feat_level) %>%
  rename(construct = feature) %>%
  mutate(min_p_pred = NA, first_sign_p_pred = NA, signif_at_EGP_level = NA)

row.names(level_prediction) <- significant_feats

# Find first significant diff
for (r in 1:nrow(t_tests_p_values)){
  for (c in 2:6){
    #print(paste0("Row ", r, " Column ", c))
    #print(t_tests_p_values[r, c])
    
    if (!is.nan(t_tests_p_values[r, c]) && t_tests_p_values[r, c] <= 0.05) {
      #print("Lower than 0.05")
      print(paste0("Construct ID: ", t_tests_p_values$construct_ID[r]))
      #print(paste0("Assigned level: ", names(t_tests_p_values)[c]))
      level_prediction[t_tests_p_values$construct_ID[r], "first_sign_p_pred"] <- str_split_i(names(t_tests_p_values)[c], "_", 2)
      break
    }
  }
}

# find lowest p-value
for (r in 1:nrow(t_tests_p_values)){

    #print(paste0("Row ", r, " Column ", c))
    
    if (any(rowSums(t_tests_p_values[r, 2:6] <= 0.05, na.rm = TRUE))){
      column_name <- names(t_tests_p_values)[apply(t_tests_p_values[r,], 1, which.min)]
      predicted_level <- str_split_i(column_name, "_", 2)
      #print(paste("Predicted level:", predicted_level))
      print(paste("Min p-val:", apply(t_tests_p_values[r,], 1, min)))
      print(paste("Construct ID:", t_tests_p_values$construct_ID[r]))
      #print(paste("EGP level:", t_tests_p_values[r, "EGP_level"]))
      level_prediction[t_tests_p_values$construct_ID[r], "min_p_pred"] <- predicted_level
    }
  
}

# Answer question: is there a significant different at the level the EGP claims that the construct is criterial for?


