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
