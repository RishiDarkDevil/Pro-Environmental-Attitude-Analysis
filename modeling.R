library(jtools)

# Linear Model Dependence between the Categories
lin_mod_cats <- Qsn_Name_Cont %>%
  map(~str_c(.,"~.")) %>%
  map(~lm(as.formula(.), data = data_net_score[,3:ncol(data_net_score)]))

sig_lin_mod_cats <- lin_mod_cats %>%
  map(~step(., direction = "both", trace = 0))

sig_lin_mod_cats_summ <- sig_lin_mod_cats %>%
  map(~summary(.))


plot_summs(
  sig_lin_mod_cats,
  ci_level = .99,
  omit.coefs = "(Intercept)",
  model.names = Qsn_Name_Cont, 
  colors = "Rainbow",
  point.size = 6
)  +
  theme(plot.title = element_text(size = 16, hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank(),
        plot.subtitle = element_text(size=13, hjust=0.5, face="italic", color="black")) +
  ggtitle("Linear Model for Dependence of Each Category Score on All Other") +
  labs(subtitle = "Multiple Linear Regression of Each Category Scores on All other Category Scores, including only the significant variables(By both forward and backward model selection)")

# Linear Model Dependence of the Catgories with Total Score  # I realized it makes no sense to do this without scaling
data_net_tot_score <- data_net_score %>%
  add_column(TOT = data_tot_score$Score)
data_net_tot_score

lin_mod_cats_on_tot <- Qsn_Name_Cont %>%
  map(~str_c("TOT~",.)) %>%
  map(~lm(as.formula(.), data = data_net_tot_score[,3:ncol(data_net_tot_score)]))

sig_lin_mod_cats_on_tot <- lin_mod_cats_on_tot %>%
  map(~step(., direction = "both", trace = 0))

sig_lin_mod_cats_on_tot_summ <- sig_lin_mod_cats_on_tot %>%
  map(~summary(.))

lin_mod_cats_on_tot %>%
  map(~summary(.))

data_net_tot_score %>%
  gather(REC, SA, PC, SS, ER, ES, RES, CON, key = "Domains", value = "Score") %>%
  ggplot(aes(TOT, Score, color = Domains)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE, size = 2) +
  labs(
    x = "Total Score(In %)",
    y = "Domain Score(In %)"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
  ggtitle("Regression Fit of Domain Scores on Total Score") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank()
  ) +
  scale_fill_viridis(discrete = TRUE)

# Linear Model Dependence for each item to the Category net score

lin_mod_items_on_cats <- list()
Qsn_Name1 <- c("^REC", "^SA", "^PC", "^SS", "^ER", "^ES", "^RES", "^CON")
for (i in seq_along(Qsn_Name1)) {
  lin_mod_items_on_cats[[i]] <- data_enc_1[,3:ncol(data_enc_1)] %>%
    select(matches(Qsn_Name1[i])) %>%
    gather(everything(), key = "Items", value = "Score", -one_of(Qsn_Name_Cont[i])) %>%
    ggplot(aes(.data[[Qsn_Name_Cont[[i]]]], Score, color = Items)) +
    geom_point() +
    geom_smooth(method = "lm", formula = "y~x", se = FALSE, size = 2) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
    ggtitle(Plot_Nmaes[i]) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.background = element_blank()
    ) +
    scale_fill_viridis(discrete = TRUE)
}

lin_mod_items_on_cats <- ggarrange(
  plotlist = lin_mod_items_on_cats, 
  ncol = 2, nrow = 4
)

annotate_figure(
  lin_mod_items_on_cats,
  left = text_grob("Item Score(In %)", rot = 90),
  bottom = text_grob("Total Score(In %)"),
  top = text_grob("Regression Fit of Item Scores on Domain Score", face = "bold", size = 16)
)
