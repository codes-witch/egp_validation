
# for each construct, get the mean at each level and make a line plot
library(ggplot2)

#for (constr in anova_sign_constructs){
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

