library(jtools)
library(modelr)

present_R.2 <- function(mods, table_theme = "mBlue", footnote = "Model R-squared", visual = TRUE) {
  mods_summ <- mods %>%
    map(~summary(.))
  R.squared <<- tibble("Model" = Qsn_Name_Cont,  
                      "R_square" = round(mods_summ %>%
                                            map_dbl(~.$r.squared), digits = 2), 
                      "Adj_R_square" = round(mods_summ %>%
                                                map_dbl(~.$adj.r.squared), digits = 2))
  if(!visual){
  p <- ggtexttable(R.squared, theme = ttheme(table_theme), rows = NULL) %>%
    tab_add_footnote(text = footnote, size = 10, face = "italic")}
  else{
    p <- R.squared %>%
      gather(R_square, Adj_R_square, key = "Metric", value = "value") %>%
      mutate(Model = as.factor(Model)) %>%
      ggplot(aes(fct_reorder(Model, value), value, color = Metric, group = Metric)) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
      ggtitle("Model Performance") +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank()
      ) + 
      labs(
        x = "Model",
        y = "Metric Value"
      ) + 
      scale_color_viridis(discrete = TRUE)
  }
  return(p)
}

present_ssr <- function(mods, table_theme = "mBlue", footnote = "Model R-squared", visual = TRUE) {
  mods_summ <- mods %>%
    map(~summary(.))
  SSR <<- tibble("Model" = Qsn_Name_Cont,  
                       "SSR" = round(mods_summ %>%
                                            map_dbl(~sum(.$residuals^2)/.$df[2]), digits = 2))
  if(!visual){
    p <- ggtexttable(SSR, theme = ttheme(table_theme), rows = NULL) %>%
      tab_add_footnote(text = footnote, size = 10, face = "italic")}
  else{
    p <- SSR %>%
      mutate(Model = as.factor(Model)) %>%
      ggplot(aes(fct_reorder(Model, SSR, .desc = TRUE), SSR, group = 1)) +
      geom_line(size = 1.5) +
      geom_point(size = 2.5) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
      ggtitle("Model Residuals") +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank()
      ) + 
      labs(
        x = "Model",
        y = "Sum of Squares Residuals"
      )
  }
  return(p)
}

format_reg_table <- function(model, digs = 2){
  mod <- tidy(model, conf.level = .99, conf.int = TRUE) %>%
    mutate(across(where(is.numeric), ~ round(., digits = digs)))
  mod <- mod %>%
    mutate(estimate = as.character(estimate)) %>%
    mutate(estimate = paste(paste(estimate, ifelse(p.value < 0.05, "*",""), ifelse(p.value < 0.01, "*",""), ifelse(p.value < 0.001, "*",""), sep = ""), paste("[", conf.low, ", ", conf.high, "]", sep = ""), sep = "\n"))
  mod <- mod %>%
    select(term, estimate)
  return(mod)
}

present_reg_mod <- function(model, title = "", table_theme = "mOrange") {
  p <- ggtexttable(format_reg_table(model), theme = ttheme(table_theme), rows = NULL) %>%
    tab_add_title(text = title, face = "bold", padding = unit(0.1, "line")) %>%
    tab_add_footnote(text = paste("*** p < 0.001; ** p < 0.01;", " * p < 0.05; 99% C.I.", sep = "\n"), size = 10, face = "italic")
  return(p)
}

# Linear Model Dependence between the Categories
lin_mod_cats <- Qsn_Name_Cont %>%
  map(~str_c(.,"~.")) %>%
  map(~lm(as.formula(.), data = data_net_score[,3:ncol(data_net_score)]))

sig_lin_mod_cats <- lin_mod_cats %>%
  map(~step(., direction = "both", trace = 0))

sig_lin_mod_cats_summ <- sig_lin_mod_cats %>%
  map(~summary(.))

sig_lin_mod_cats_plot <- plot_summs(
  sig_lin_mod_cats,
  inner_ci_level = .95,
  omit.coefs = "(Intercept)",
  model.names = Qsn_Name_Cont, 
  colors = "Rainbow",
  point.size = 6
)  +
  theme(plot.title = element_text(size = 14, hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank(),
        plot.subtitle = element_text(size=12, hjust=0.5, face="italic", color="black")) +
  ggtitle("Linear Model Coefficients") +
  labs(subtitle = "Multiple Linear Regression Coefficients, including only the significant variables(By both forward and backward model selection)")

# model_present_list <- list()
# for (i in seq_along(Qsn_Name_Cont)) {
#   model_present_list[[i]] <- present_reg_mod(sig_lin_mod_cats[[i]], str_c("Model ", Qsn_Name_Cont[i]))
# }

model_info_table <- sig_lin_mod_cats %>%
  map(~format_reg_table(.)) %>%
  reduce(full_join, by = c('term'))
model_info_table <- model_info_table[2:nrow(model_info_table),]
model_info_table[is.na(model_info_table)] <- " "
colnames(model_info_table) <- c("term", Qsn_Name_Cont)

model_info_table <- ggtexttable(model_info_table, theme = ttheme("mOrange"), rows = NULL) %>%
  tab_add_title(text = "Model Fit Estimates", face = "bold", padding = unit(0.1, "line")) %>%
  tab_add_footnote(text = "*** p < 0.001; ** p < 0.01; * p < 0.05; 99% C.I.", size = 10, face = "italic")

R.2.table <- present_R.2(sig_lin_mod_cats, "mRed", "Model Performance")
Resid.table <- present_ssr(sig_lin_mod_cats, "mRed", "Model Performance")
# Models <- ggarrange(model_info_table, R.2.table, nrow = 1, ncol = 2, widths = c(8,4))
Models <- ggarrange(R.2.table, Resid.table, nrow = 2, ncol = 1)
# p <- ggarrange(sig_lin_mod_cats_plot, Models, ncol = 1, nrow = 2, heights = c(8, 4))

p <- ggarrange(sig_lin_mod_cats_plot, Models, ncol = 2, nrow = 1, widths = c(8, 4))

annotate_figure(
  p,
  top = text_grob("Linear Model for Dependence of Each Category Score on All Other", face = "bold", size = 16)
)

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

sig_lin_mod_cats_on_tot_plot <- data_net_tot_score %>%
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

R.2.table <- present_R.2(sig_lin_mod_cats_on_tot)
Resid.table <- present_ssr(sig_lin_mod_cats_on_tot)

Models <- ggarrange(R.2.table, Resid.table, nrow = 2, ncol = 1)

p <- ggarrange(sig_lin_mod_cats_on_tot_plot, Models, ncol = 2, nrow = 1, widths = c(8, 4))

annotate_figure(
  p,
  top = text_grob("Linear Model for Dependence of Each Domain Scores on Total Score", face = "bold", size = 16)
)


# Linear Model Dependence for each item to the Category net score

data_enc_1 <- data_enc %>%
  bind_cols(data_net_score[,3:ncol(data_net_score)])

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

# Some idea on left over residual explained by the last added domain

lin_mod_tot_on_cats_ex_one <- Qsn_Name_Cont %>%
  map(~str_c("TOT~.-",.)) %>%
  map(~lm(as.formula(.), data = data_net_tot_score[,3:ncol(data_net_tot_score)]))

sig_lin_mod_tot_on_cats_ex_one <- lin_mod_tot_on_cats_ex_one %>%
  map(~step(., direction = "both", trace = 0))

sig_lin_mod_tot_on_cats_ex_one_summ <- sig_lin_mod_tot_on_cats_ex_one %>%
  map(~summary(.))
