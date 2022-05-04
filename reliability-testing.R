# Split Data into the respective categories
data_cat_wise <- Qsn_Name %>%
  map(~select(data_enc, matches(.)))

# Reliability Testing
rel_tests <- data_cat_wise %>%
  map(~alpha(., check.keys = TRUE))

rel_tests[[1]]$boot

# Present the Result of Reliability Test
conv_to_tibble <- function(data) {
  data <- as_tibble(data) %>%
    mutate(Qsn = rownames(data)) %>%
    select(Qsn, everything())
  return(data)
}

conv_to_tile <- function(data, omit = NULL, title = NULL, x_lab = NULL, y_lab = NULL) {
  p <- data %>%
    select(-all_of(omit)) %>%
    gather(everything(), key = "key", value = "value", -one_of("Qsn")) %>%
    ggplot(aes(Qsn, key, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, digits = 2)), color = "pink", size = 5) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
    ggtitle(title) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.background = element_blank(),
      legend.position = "none"
    ) + 
    labs(
      x = y_lab,
      y = x_lab
    ) + 
    coord_flip()
  return(p)
}

present_alpha <- function(alpha_mod) {
  t1 <- as_tibble(alpha_mod$total) %>% 
    mutate(across(where(is.numeric), ~ round(., digits = 2)))
  t2 <- conv_to_tibble(alpha_mod$alpha.drop)
  t3 <- conv_to_tibble(alpha_mod$item.stats)
  t4 <- conv_to_tibble(alpha_mod$response.freq)
  
  p1 <- ggtexttable(t1, theme = ttheme("mBlue"), rows = NULL) %>%
    tab_add_footnote(text = "Current Reliability Test Scores", size = 10, face = "italic")
  p2 <- conv_to_tile(t2, title =  "Alpha after Question Drop", y_lab = "Question Dropped", x_lab = "Reliability Measures")
  p3 <- conv_to_tile(t3, "n", title = "Item Statistics", y_lab = "Question Codes", x_lab = "Item-Correlation Measures, Mean(If Dropped) & SD(If Dropped)")
  p4 <- conv_to_tile(t4, "miss", "Response Frequency", "Options", "Question Code")
  
  ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
  
}

present_alpha(rel_tests[[1]])

conv_to_tile(conv_to_tibble(rel_tests[[1]]$item.stats), "n", title = "Item Statistics", y_lab = "Question Codes", x_lab = "Item-Correlation Measures, Mean(If Dropped) & SD(If Dropped)")
conv_to_tile(conv_to_tibble(rel_tests[[1]]$alpha.drop), title =  "Alpha after Question Drop", y_lab = "Question Dropped", x_lab = "Reliability Measures")
