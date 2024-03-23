# A file for calculating precision, recall and F1

# Calculate precision, recall and F1 for t-tests predictions
t_1side_predic_first <- t_1side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred)) %>%
  pull(first_sign_p_pred)

t_1side_gold <- t_1side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred) )%>%
  pull(EGP_level)

t_2side_gold <- t_2side_level_predict %>%
  filter(EGP_level != "A1" & !is.na(first_sign_p_pred) )%>%
  pull(EGP_level)

 
get_exact_precision_recall_f1(t_1side_gold, t_1side_predic_first) 
get_neighbors_precision_recall_f1(t_1side_gold, t_1side_predic_first)


accuracyIsSignificantEGPLevel(t_1side_level_predict)




 



