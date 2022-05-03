library(tidyverse)
library(forcats)
library(stringr)
library(ggpubr)
library(GGally)
library(psych)
library(grid)
library(gridExtra)
library(viridis)

# Summary of the data
desc_summ <- describe(data[,3:ncol(data)])
desc_summ

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
gen_stacked_barplots <- function(plot_data = data_long) {
  Qsn_Name <- c("^SA.", "^REC.", "^PC.", "^SS.", "^ER.", "^ES.", "^RES.", "^CON.")
  
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
    top = text_grob("Response Proportions for Each Question")
  )
}

# Generate Stacked bar plots for non-reverse scored questions
gen_stacked_barplots()

# Generate Stacked bar plots for reverse scored questions
gen_stacked_barplots(data_long_rev)
