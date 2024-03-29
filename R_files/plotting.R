# For plotting constructs

#Typical A2 construct
normalized_constr_counts %>%
  get_lineplot(110)

line_plot <- normalized_counts_students_long %>%
  get_lineplot(110)

normalized_counts_students_long %>%
  get_boxplot_construct(110)

normalized_counts_students_long %>%
  filter(feature == 110) %>%
  group_by(text_level) %>%
  summarise(median = median(total), mean=mean(total))


# Save boxplots in a file 
for (construct in t_1side_level_predict$construct) {
  bp <- normalized_counts_students_long %>%
    get_boxplot_construct(construct)
  
  ggsave(paste0("plots/", t_1side_level_predict[t_1side_level_predict$construct == construct, "EGP_level"], "_", construct, "_boxplot.png"), width= 5.40, height = 4.20, plot = bp)
  
  lp <- normalized_constr_counts %>%
    get_lineplot(construct)
  
  ggsave(paste0("plots/", t_1side_level_predict[t_1side_level_predict$construct == construct, "EGP_level"], "_", construct, "_lineplot.png"), width= 5.40, height = 4.20, plot = lp)
}

normalized 
normalized_counts_students_long %>%
  get_boxplot_construct(935)

df_filtered <- normalized_counts_students_long %>%
  filter(feature == constr_id)
