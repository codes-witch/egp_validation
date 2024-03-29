# Get the levels of all constructs:
constr_lvl_df <- data.frame(feature = all_features)
constr_lvl_df <- add_feature_level(constr_lvl_df)

# Get percentage of construct levels: how many f the constructs belong to each level

get_percentage_of_constr_lvl <- function(level, construct_level_df = constr_lvl_df ){
  num_constr_of_level <- construct_level_df %>%
    filter(feat_level == level) %>%
    count()
  
  return(num_constr_of_level/nrow(constr_lvl_df) * 100)
}

get_percentage_of_constr_lvl("A1")
get_percentage_of_constr_lvl("A2")
get_percentage_of_constr_lvl("B1")
get_percentage_of_constr_lvl("B2")
get_percentage_of_constr_lvl("C1")
get_percentage_of_constr_lvl("C2")

total = 0
for (level in c("A1", "A2", "B1", "B2", "C1", "C2")) {
  total = total + get_percentage_of_constr_lvl(level)
}

zero_feats_level <- data.frame(feature = zero_feats_all_learners)
zero_feats_level <- add_feature_level(zero_feats_level)

# How many (in percentage) of the A1 features were not found
n_zero_a1_feats <- zero_a1_feats %>%
  