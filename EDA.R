library(tidyverse)
library(forcats)
library(stringr)
library(ggpubr)
library(GGally)
library(ggcorrplot)
library(psych)
library(grid)
library(gridExtra)
library(plotly)
library(viridis)
library(broom)

# Summary of the data
desc_summ <- describe(data[,3:ncol(data)])
desc_summ

# Summary of the net_score data
desc_score_summ <- describe(data_net_score[,3:ncol(data_net_score)])
desc_score_summ

# Converting Data from Wide to Long Form
# Factoring the non-reverse score responses
data_long <- data[,3:ncol(data)] %>%
  select(-all_of(rev_order)) %>%
  gather(everything(), key = "Qsn", value = "Resp")
data_long$Resp <- factor(data_long$Resp, levels = levels)
data_long$Resp

# Factoring the reverse score responses
data_long_rev <- data[,rev_order] %>%
  gather(everything(), key = "Qsn", value = "Resp")
data_long_rev$Resp <- factor(data_long_rev$Resp, levels = rev(levels))
data_long_rev$Resp

# Making Stacked Bar Plots for Grouped Qsns and Response
Qsn_Name <- c("^SA.", "^REC.", "^PC.", "^SS.", "^ER.", "^ES.", "^RES.", "^CON.")
Plot_Nmaes <- c("Recycling", "Environmental Safety", "Percieved Control", "Social Support", "Environmental Reductionism", "Environmental Sensitivity", "Reuse", "Conservation")

Plot_Nmaes1 <- c("Environmental Safety", "Recycling", "Percieved Control", "Social Support", "Environmental Reductionism", "Environmental Sensitivity", "Reuse", "Conservation")
gen_stacked_barplots <- function(plot_data = data_long, reverse = FALSE) {
  stacked_bar_plot <<- list()
  loop <- ifelse(reverse, length(rev_order), length(Qsn_Name))
  rows <- ifelse(reverse, 1, 4)
  loop_cols <- ifelse(reverse, rev_order, Qsn_Name)
  for (i in 1:loop) {
    stacked_bar_plot[[i]] <<- plot_data %>%
      filter(str_detect(Qsn, ifelse(reverse, rev_order[i], Qsn_Name[i]))) %>%
      ggplot() +
      geom_bar(aes(Qsn, fill = Resp), position = "fill")  +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
      ggtitle(Plot_Nmaes1[[i]]) +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        legend.position = "none"
      ) +
      scale_fill_viridis(discrete = TRUE) +
      coord_flip()
  }
   
  stacked_bar_plot <- ggarrange(
    plotlist = stacked_bar_plot, 
    ncol = 2, nrow = 4,
    common.legend = TRUE, legend = "bottom"
  )
  
  
  annotate_figure(
    stacked_bar_plot,
    left = text_grob("Question Codes", rot = 90),
    bottom = text_grob("Proportion"),
    top = text_grob("Response Proportions for Each Question", face = "bold", size = 16)
  )
}

# Generate Stacked bar plots for non-reverse scored questions
gen_stacked_barplots()

# Generate Stacked bar plots for reverse scored questions
gen_stacked_barplots(data_long_rev, TRUE)

# Generate Pair Plots for Net Score of Items
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, formula = y~x, size = 1, se = FALSE, ...)
  p
}

# Colored on the basis of Sex
data_net_score %>%
           mutate(Sex = as.factor(Sex)) %>%
           ggpairs(columns = c("REC", "SA", "PC", "SS", "ER", "ES", "RES", "CON"), aes(color = Sex), lower = list(continuous = my_fn)) +
           scale_color_viridis(discrete = TRUE) +
           theme_bw() +
           theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
           ggtitle("Scatterplot Matrix for Total Score of Categories")

# Tile Plots for correlation for each Qsn in each Category
Qsn_Name_Cont <- c("REC", "SA", "PC", "SS", "ER", "ES", "RES", "CON")


corrplots <- list()
for (i in seq_along(Qsn_Name_Cont)) {
  corrplots[[i]] <- data_enc %>%
    select(matches(Qsn_Name[[i]])) %>%
    ggcorr(label = TRUE) +
    # ggpairs(columns = c("REC", "SA", "PC", "SS", "ER", "ES", "RES", "CON"), aes(color = Sex), lower = list(continuous = my_fn)) +
    # scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank(), panel.border = element_blank(), line = element_blank()) +
    ggtitle(Plot_Nmaes[[i]])
    
}

corrplots <- ggarrange(
  plotlist = corrplots, 
  ncol = 2, nrow = 4
)

annotate_figure(
  corrplots,
  top = text_grob("Correlation Plots", face = "bold", size = 16)
)

# Correlation between Net Scores
data_net_score[,3:ncol(data_net_score)] %>%
  ggcorr(label = TRUE) +
  scale_fill_viridis() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank(), panel.border = element_blank(), line = element_blank()) +
  ggtitle("Correlation Plot for Total Scores in Each Category")

# Correlation between All Qsn Scores
data_enc[,3:ncol(data_enc)] %>%
  ggcorr() +
  scale_fill_viridis() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank(), panel.border = element_blank(), line = element_blank()) +
  ggtitle("Correlation Plot for All Questions")


# Fit PDFs to Net Scores

ev_finder <- function(transform = identity) {
  function(f, ..., from = -Inf, to = Inf) {
    integrate(function(x) transform(x) * f(x, ...), from, to)
  }
}

moment_finder <- function(n, c = 0) {
  ev_finder(function(x) (x - c) ^ n)
}

find_mean <- moment_finder(1)
find_variance <- function(f, ...) {
  mu <- find_mean(f, ...)$value
  moment_finder(2, mu)(f, ...)
}

fit_distribution <- function(data, title_name = "Model & Data", qqtitle = NULL){
  fit_g <<- invisible(fitdistrplus::fitdist(data, "gamma"))
  fit_l <<- invisible(fitdistrplus::fitdist(data, "lnorm"))
  fit_w <<- invisible(fitdistrplus::fitdist(data, "weibull"))
  fit_n <<- invisible(fitdistrplus::fitdist(data, "norm"))
  fit_e <<- invisible(fitdistrplus::fitdist(data, "exp"))
  
  plot.legend <- c("gamma", "lognormal", "weibull", "normal", "exponential")
  gof <<- fitdistrplus::gofstat(list(fit_g, fit_l, fit_w, fit_n, fit_e), fitnames = plot.legend)
  
  p1 <- fitdistrplus::denscomp(list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend, fitlwd = c(2,2,2,2,2), xlegend = 0.007, plotstyle = "ggplot") #, fit_e), legendtext = plot.legend, fitlwd = c(3,3.5,4,4.5,1), xlegend = 0.007)

  p2 <- fitdistrplus::qqcomp  (list(fit_g, fit_l, fit_w, fit_n, fit_e), legendtext = plot.legend, plotstyle = "ggplot")
  
  p1 <- p1 +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) + ggtitle(title_name) + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  p2 <- p2  +
    theme(legend.position = "None") + ggtitle(qqtitle) + labs(subtitle = paste(rownames(as.data.frame(gof$ks[which.min(gof$ks)])), "seems to fit the data better.")) + theme(axis.title.x = element_blank(), axis.title.y = element_blank())

  p <- ggarrange(p1, p2, ncol = 2)
  return(p)
}

proper_fit_distribution <- function(data, heading = "Model & Data"){
  
  invisible(capture.output(p <- fit_distribution(data, heading)))
  
  tf <<- (map_df(list(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$estimate), tidy)) %>%
    spread(key = names, value = x) %>% 
    mutate(across(where(is.numeric), ~ round(., digits = 2)))
  
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "exp")
    table <- ggtexttable(tibble(Fit = c(colnames(tf)[1], as.character(tf[1]))), theme = ttheme("mCyan"), rows = NULL)
  else
    table <- ggtexttable(tibble(Fit = c(colnames(tf)[1], as.character(tf[1]),colnames(tf)[2], as.character(tf[2])[1])), theme = ttheme("mCyan"), rows = NULL)
  
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "weibull")
    tablea <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dweibull, scale = as.numeric(tf[1]), shape = as.numeric(tf[2]))$value, digits = 2), "Var", round(find_variance(dweibull, scale = as.numeric(tf[1]), shape = as.numeric(tf[2]))$value, digits = 2))), theme = ttheme("mBlue"), rows = NULL)
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "lnorm")
    tablea <-  ggtexttable(tibble(Moments = c("Mean", round(exp(as.numeric(tf[1]) + 0.5*as.numeric(tf[2])^2), digits = 2), "Var", round(exp(as.numeric(tf[1])*2 + 0.5*4*as.numeric(tf[2])^2) - (exp(as.numeric(tf[1])*1 + 0.5*as.numeric(tf[2])^2))^2, digits = 2))), theme = ttheme("mBlue"), rows = NULL)
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "gamma")
    tablea <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dgamma, rate = as.numeric(tf[1]), shape = as.numeric(tf[2]))$value, digits = 2), "Var", round(find_variance(dgamma, rate = as.numeric(tf[1]), shape = as.numeric(tf[2]))$value, digits = 2))), theme = ttheme("mBlue"), rows = NULL)
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "norm")
    tablea <-  ggtexttable(tibble(Moments = c("Mean", round(find_mean(dnorm, mean = as.numeric(tf[1]), sd = as.numeric(tf[2]))$value, digits = 2), "Var", round(find_variance(dnorm, mean = as.numeric(tf[1]), sd = as.numeric(tf[2]))$value, digits = 2))), theme = ttheme("mBlue"), rows = NULL)
  if(list(fit_g, fit_l, fit_w, fit_n, fit_e)[as.numeric(which.min(gof$ks))][[1]]$distname == "exp")
    tablea <-  ggtexttable(tibble(Moments = c("Mean", round(1/as.numeric(tf[1]), digits = 2), "Var", round(1/(as.numeric(tf[1]))^2, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
  
  table <- ggarrange(table, tablea, nrow = 1, ncol = 2)
  
  data_sum <- describe(data)
    
  tableab <-  ggtexttable(tibble(Obsvd = c("Mean", round(data_sum$mean, digits = 2), "sd", round(data_sum$sd, digits = 2), "median", round(data_sum$median, digits = 2), "skew", round(data_sum$skew, digits = 2)), Data = c("kurt", round(data_sum$kurtosis, digits = 2), "min", round(data_sum$min, digits = 2), "max", round(data_sum$max, digits = 2), "se", round(data_sum$se, digits = 2))), theme = ttheme("mGreen"), rows = NULL)
  
  table <- ggarrange(table, tableab, nrow = 2, heights = c(5, 7))
  
  p <- ggarrange(p, table, nrow = 1, ncol = 2, widths = c(10,1))
  return(p)
}

fit_pdf <- list()
for (i in seq_along(data_net_score[,3:ncol(data_net_score)])) {
  fit_pdf[[i]] <- annotate_figure(
    proper_fit_distribution(data_net_score[[i+2]]),
    top = text_grob(Plot_Nmaes[i], size = 14)
  )
}

fit_pdf <- ggarrange(
  plotlist = fit_pdf, 
  ncol = 1, nrow = 8
)

annotate_figure(
  fit_pdf,
  top = text_grob("MLE Fit of Normal Distribution", face = "bold", size = 16)
)

# Mean and CI of each Qsn for each item

desc_sum <- describe(data_enc[,3:ncol(data_enc)])
desc_sum <- as_tibble(desc_sum) %>%
  mutate(Qsn = rownames(desc_sum))

mean_bar_plot <- list()

for (i in seq_along(Qsn_Name)) {
  mean_bar_plot[[i]] <- desc_sum %>%
    filter(str_detect(Qsn, Qsn_Name[i])) %>%
    ggplot() +
    geom_bar(aes(Qsn, mean, fill = Qsn), stat = "identity")  +
    geom_errorbar(aes(x=Qsn, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.5, size=1.5) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
    ggtitle(Plot_Nmaes1[[i]]) +
    geom_text(aes(Qsn, mean, label=paste("mean=",round(mean, 2),",se=",round(se, 2))), position=position_dodge(width=0.9), hjust = 1.5, color = "black", size = 5) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.background = element_blank(),
      legend.position = "none"
    ) +
    scale_fill_viridis(discrete = TRUE) +
    coord_flip()
}
  
mean_bar_plot <- ggarrange(
  plotlist = mean_bar_plot, 
  ncol = 2, nrow = 4
)
  
  
annotate_figure(
  mean_bar_plot,
  left = text_grob("Question Codes", rot = 90),
  bottom = text_grob("Average Response"),
  top = text_grob("Average Response and Confidence Intervals for Each Question", face = "bold", size = 16)
)

# Mean and CI Bar Plot for Net Scores
desc_score_sum <- as_tibble(desc_score_summ) %>%
  mutate(Qsn = rownames(desc_score_summ))

Q_Name <- c("REC", "SA", "PC", "SS", "ER", "ES", "RES", "CON")

desc_score_sum %>%
  ggplot() +
  geom_bar(aes(Qsn, mean, fill = Qsn), stat = "identity")  +
  geom_errorbar(aes(x=Qsn, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.5, size=1.5) +
  labs(
    x = "Category",
    y = "Average Total Score"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
  ggtitle("Average Response and Confidence Intervals for Each Category") +
  geom_text(aes(Qsn, mean, label=paste("mean=",round(mean, 2),",se=",round(se, 2))), position=position_dodge(width=0.9), hjust = 1.5, color = "black", size = 5) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip()

#Each Qsn Count Balloon Plot
response_count <- data %>%
  select(-c("Sex", "Family")) %>%
  gather(everything(), key = "Qsns", value = "Resp") %>%
  group_by(Qsns, Resp) %>%
  count()

response_count %>%
  ggballoonplot(x = "Resp", y = "Qsns", size = "n", fill = "n") +
  guides(size = "none") +
  scale_fill_viridis() +
  labs(fill = "Count")

# Too Big To Plot
Contingency_Tiles <- list()
i <- 1
var_done <- c()
for(var1 in colnames(data)[3:ncol(data)]) {
  var_done <- c(var_done, var1)
  for (var2 in setdiff(colnames(data)[3:ncol(data)], var_done)) {
    Contingency_Tiles[[i]] <- data %>%
      select(-c("Sex", "Family")) %>%
      group_by(across(all_of(c(var1, var2)))) %>%
      count() %>%
      ggplot(aes(.data[[var1]], .data[[var2]], fill = n)) +
      geom_tile() +
      geom_text(aes(label = n), color = "pink", size = 5) +
      guides(size = "none") +
      labs(fill = "Count")
    i <- i+1
  }
}
length(Contingency_Tiles)

cor.plot(data_enc[,3:ncol(data)])

