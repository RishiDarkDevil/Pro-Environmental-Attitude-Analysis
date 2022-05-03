library(tidyverse)
library(forcats)
library(stringr)
library(ggpubr)
library(GGally)
library(psych)
library(grid)
library(gridExtra)

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
Qsn_Name <- c("^REC.", "^SA.", "^PC.", "^SS.", "^ER.", "^ES.", "^RES.", "^CON.")

stacked_bar_plot <- list()
for (i in seq_along(Qsn_Name)) {
  stacked_bar_plot[[i]] <- data_long %>%
    filter(str_detect(Qsn, Qsn_Name[i])) %>%
    ggplot() +
    geom_bar(aes(Qsn, fill = Resp), position = "fill") +
    coord_flip()
}

ggarrange(plotlist = stacked_bar_plot, ncol = 2, nrow = 4)
  
