# A file for calculating precision, recall and F1

# Calculate precision, recall and F1 for t-tests predictions
t_1side_predic_first <- t_1side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(first_sign_p_pred)

t_1side_predic_lowest <- t_1side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(min_p_pred)

t_2side_predic_first <- t_2side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(first_sign_p_pred)

t_2side_predic_lowest <- t_2side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(min_p_pred)

t_1side_gold <- t_1side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred) )%>%
  pull(EGP_level)

t_2side_gold <- t_2side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred) )%>%
  pull(EGP_level)

# For the one-sided t-test, the lowest p-value prediction outperforms the first significant value. 
# This is probably mostly because, for most features, the first significant p-value occurs from A1 to A2. 
# Choosing the lowest p-value slightly reduces the number of A2 predictions 
get_exact_precision_recall_f1(t_1side_gold, t_1side_predic_first) 
get_exact_precision_recall_f1(t_1side_gold, t_1side_predic_lowest)

accuracyIsSignificantEGPLevel(t_1side_level_predict)

# For the 2-sided test, the lowest p-value prediction also outperforms 
get_exact_precision_recall_f1(t_2side_gold, t_2side_predic_first)
get_exact_precision_recall_f1(t_2side_gold, t_2side_predic_lowest)

accuracyIsSignificantEGPLevel(t_2side_level_predict)
 


###############  For the wilcoxon tests
# A file for calculating precision, recall and F1

# Calculate precision, recall and F1 for Wilcoxon-tests predictions
wcx_1side_predic_first <- wcx_1side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(first_sign_p_pred)

wcx_1side_predic_lowest <- wcx_1side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(min_p_pred)

wcx_2side_predic_first <- wcx_2side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(first_sign_p_pred)

wcx_2side_predic_lowest <- wcx_2side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(min_p_pred)

wcx_1side_gold <- wcx_1side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred) )%>%
  pull(EGP_level)

wcx_2side_gold <- wcx_2side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred) )%>%
  pull(EGP_level)


get_exact_precision_recall_f1(wcx_1side_gold, wcx_1side_predic_first) 
get_exact_precision_recall_f1(wcx_1side_gold, wcx_1side_predic_lowest)

accuracyIsSignificantEGPLevel(wcx_1side_level_predict)

 
get_exact_precision_recall_f1(wcx_2side_gold, wcx_2side_predic_first)
get_exact_precision_recall_f1(wcx_2side_gold, wcx_2side_predic_lowest)

accuracyIsSignificantEGPLevel(wcx_2side_level_predict)



