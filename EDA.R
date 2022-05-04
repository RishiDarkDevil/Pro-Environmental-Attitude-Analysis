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

gen_stacked_barplots <- function(plot_data = data_long) {
  stacked_bar_plot <- list()
  for (i in seq_along(Qsn_Name)) {
    stacked_bar_plot[[i]] <- plot_data %>%
      filter(str_detect(Qsn, Qsn_Name[i])) %>%
      ggplot() +
      geom_bar(aes(Qsn, fill = Resp), position = "fill")  +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
      ggtitle(Plot_Nmaes[[i]]) +
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
gen_stacked_barplots(data_long_rev)

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
           ggtitle("Scatterplot Matrix for Score of Items")

# Tile Plots for correlation
Qsn_Name_Cont <- c("REC", "SA", "PC", "SS", "ER", "ES", "RES", "CON")


corrplots <- list()
for (i in seq_along(Qsn_Name_Cont)) {
  corrplots[[i]] <- data_enc %>%
    select(contains(Qsn_Name_Cont[[i]])) %>%
    ggcorr(label = TRUE) +
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
