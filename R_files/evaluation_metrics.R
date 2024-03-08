# A file for calculating precision, recall and F1

# Calculate precision for t-tests predictions
install.packages("caret")
library(caret)

t_predicted_first <- t_level_predictions %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(first_sign_p_pred)

t_predicted_first <- factor(t_predicted_first, levels = levels[2:length(levels)])

t_gold_standard <- t_level_predictions %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred) )%>%
  pull(EGP_level)

t_gold_standard <- factor(t_gold_standard, levels = levels[2:length(levels)])

xtab_t_first <- table( t_gold_standard, t_predicted_first )


cm_t_first <- caret::confusionMatrix(xtab_t_first)
cm_t_first

# TODO for the lowest pvalue with t tests

###############  For the wilcoxon tests

wilcox_predicted_first <- wilcox_level_predictions %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(first_sign_p_pred)
wilcox_gold_standard <- wilcox_level_predictions %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(EGP_level)

wilcox_predicted_first <- factor(wilcox_predicted_first, levels = levels[2:length(levels)])


getExactPrecisionAndRecallCefr <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    print("Actual and predicted must be same length")
  }
  
  cefr_levels <- c("A2", "B1", "B2", "C1", "C2")
  result <- data.frame(level = cefr_levels, precision = rep(NA, 5), recall = rep(NA, 5))
  
  for (l in cefr_levels) {
    tp = 0
    fp = 0
    fn = 0
    print(paste("Doing level", l ))
    
    for (i in 1:length(actual)) {
      
      if(actual[i] == l && predicted[i] == l) {
        print("New TP")
        tp <- tp + 1
      } else if (actual[i] == l) {
        print("New FN")
        fn <- fn + 1
      } else if (predicted[i] == l) {
        print("New FP")
        fp <- fp +1
      }
    }
    
    
    if (fp + tp != 0){
      result$precision[result$level == l] <- tp / (fp + tp)
    } else {
      print("Precision will be NA")
    }
    
    if (tp + fn != 0) {
      result$recall[result$level == l] <- tp / (fn + tp)
    } else {
      print("Recall will be NA")
    }
  }
  
  return(result)
}


getExactPrecisionAndRecallCefr(wilcox_gold_standard, wilcox_predicted_first)

