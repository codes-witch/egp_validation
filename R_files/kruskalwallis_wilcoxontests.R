library(rstatix)
library(ggpubr)

# Kruskal-Wallis: some constructs might not show significant differences across test levels
kw_results <- list()

kw_non_sign_constructs <- c()
kw_sign_constructs <- c()
kw_nan_results_constructs <- c()



for (constr in unique_feats) {
  
  # df with only the current construct. Check whether there are no occurrences of the feature.
  feature_dataframe <- normalized_students %>%
    filter(feature == constr)
  # Keep track of constructs with no occurrences
  if (constr %in% zero_feats_all_learners ){
    print(paste0(constr, " was a 0 feature"))
    
  } else { # Fill in kw result list 
    kw_results[[constr]] <- normalized_students %>%
      filter(feature == constr) %>%
      kruskal.test(total ~ text_level, data = .)
    
    p_value = kw_results[[constr]]$p.value
    
    if (is.nan(p_value)){ # sanity check: kw_nan_results_constructs will remain empty because we are discarding all features with no occurrences
      kw_nan_results_constructs <- append(kw_nan_results_constructs, constr)
    } else if (p_value > 0.05) {
      kw_non_sign_constructs <- append(kw_non_sign_constructs, constr)
    } else {
      kw_sign_constructs <- append(kw_sign_constructs, constr)
    }
  }
  
}



# For all significant features,  do Wilcoxon Rank Sum tests to check for significant differences between the feature level and the previous one. 

levels <- c("A1", "A2", "B1", "B2", "C1", "C2")

# store p-values from wilcoxon tests
wilcox_p_values <- data.frame(matrix(ncol = 6, nrow = length(kw_sign_constructs)))
names(wilcox_p_values) <- c("feature", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")

# store whether the higher level has significantly higher frequency
wilcox_l2_greater <- data.frame(matrix(ncol = 6, nrow = length(kw_sign_constructs)))
names(wilcox_l2_greater) <- c("feature", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")


# For all relevant constructs, do a t-test between all consecutive levels
for (f in 1:length(kw_sign_constructs)) {
  constr <- kw_sign_constructs[f]
  p_values <- c()
  l2_greater <- c() # Whether the Wilcoxon estimated a greater mean for the greater level
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
      
      # TODO figure out how to get the group with the higher frequency
      wilcox_test_result <- normalized_students %>%
        filter(feature == constr, text_level %in% c(level1, level2)) %>%
        wilcox_test(total ~ text_level, data = .)
      
      p_values <- append(p_values, wilcox_test_result$p.value)
      if (wilcox_test_result$p.value <= 0.05){
        l2_greater <- append(l2_greater, wilcox_test_result$statistic < 0)
        
        
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
