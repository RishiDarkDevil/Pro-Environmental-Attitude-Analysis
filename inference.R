# Boxplots

# Gender Boxplot of Total Score
present_t.test <- function(t.test, table_theme = "mBlue", footnote = "", digs = 2) {
  table.fit <- tidy(t.test) %>%
    select(-c("method", "alternative")) %>%
    mutate(across(where(is.numeric), ~ round(., digits = digs)))
  
  table1 <- ggtexttable(table.fit, theme = ttheme(table_theme), rows = NULL) %>%
    tab_add_footnote(text = str_c(t.test$method, ", ", t.test$alternative, footnote), size = 10, face = "italic")
  
  return(table1)
}

present_aov.test <- function(aov.test, table_theme = "mBlue", footnote = "", digs = 2) {
  table.fit <- tidy(aov.test) %>%
    mutate(across(where(is.numeric), ~ round(., digits = digs)))
  
  table1 <- ggtexttable(table.fit, theme = ttheme(table_theme), rows = NULL) %>%
    tab_add_footnote(text = footnote, size = 10, face = "italic")
  
  return(table1)
}

boxplot1 <- data_tot_score %>%
  mutate(Sex = as.factor(Sex), Family = as.factor(Family)) %>%
  ggplot() +
  geom_boxplot(aes(Score, fct_reorder(Sex, Score), fill = Sex)) +
  labs(
    x = "Pro-Environmental Score",
    y = "Sex"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_viridis(discrete = TRUE)


gen_v_tot_score_t_test <- t.test(Score ~ Sex, data = data_tot_score, var.equal = TRUE)
gen_v_tot_score_t_test <- present_t.test(gen_v_tot_score_t_test, footnote = " for Sex vs Total Score")

# Family Boxplot of Total Score
boxplot2 <- data_tot_score %>%
  mutate(Sex = as.factor(Sex), Family = as.factor(Family)) %>%
  ggplot() +
  geom_boxplot(aes(Score, fct_reorder(Family, Score), fill = Family)) +
  labs(
    x = "Pro-Environmental Score",
    y = "Family"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_viridis(discrete = TRUE)


fam_v_tot_score_t_test <- t.test(Score ~ Family, data = data_tot_score)
fam_v_tot_score_t_test <- present_t.test(fam_v_tot_score_t_test, footnote = " for Family vs Total Score")


# Category Scores
boxplot3 <- data_net_score[,3:ncol(data_net_score)] %>%
  gather(everything(), key = "Category", value = "Score") %>%
  mutate(Category = as.factor(Category)) %>%
  ggplot() +
  geom_boxplot(aes(Score, fct_reorder(Category, Score), fill = Category)) +
  labs(
    x = "Pro-Environmental Category Score",
    y = "Question Category"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_viridis(discrete = TRUE)

Cat_Score_aov <- aov(Score ~ Category, data = data_net_score[,3:ncol(data_net_score)] %>%
                       gather(everything(), key = "Category", value = "Score") %>%
                       mutate(Category = as.factor(Category)) %>%
                       filter(Category %in% c("SS", "RES", "CON", "ES")))

Cat_Score_aov <- present_aov.test(Cat_Score_aov, footnote = "One-Factor Anova for SS, RES, CON, ES Scores", digs = 2)

Cat_Score_aov2 <- aov(Score ~ Category, data = data_net_score[,3:ncol(data_net_score)] %>%
                       gather(everything(), key = "Category", value = "Score") %>%
                       mutate(Category = as.factor(Category)) %>%
                       filter(Category %in% c("SA", "PC")))

Cat_Score_aov2 <- present_aov.test(Cat_Score_aov, footnote = "One-Factor Anova for SA, PC Scores", digs = 2)


tests <- ggarrange(gen_v_tot_score_t_test, fam_v_tot_score_t_test, Cat_Score_aov, Cat_Score_aov2, ncol = 1, nrow = 4)

p <- ggarrange(boxplot1, boxplot2, boxplot3, tests, ncol = 2, nrow = 2)

annotate_figure(
  p,
  top = text_grob("Pro-Environmental Category Scores and Total Scores", face = "bold", size = 16)
)
