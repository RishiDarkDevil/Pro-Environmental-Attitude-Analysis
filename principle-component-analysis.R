library(FactoMineR)
library(factoextra)
library(corrplot)

pr.out <- prcomp(data_enc[,3:ncol(data_enc)])
biplot(pr.out, scale = 0)

data_long_spread <- data_long %>% 
  count(Qsn, Resp) %>% 
  pivot_wider(names_from=Resp, values_from=n, values_fill=0)
data_long_spread_df <- as.data.frame(data_long_spread[,2:6])
rownames(data_long_spread_df) <- data_long_spread$Qsn

pr.out <- prcomp(data_long_spread_df, scale = TRUE)
biplot(pr.out, scale = 0.5, main = "PCA Biplot on All Items")

data_long_spread2 <- data_long_spread %>%
  mutate(Domain = sub("^([[:alpha:]]*).*", "\\1", data_long_spread$Qsn)) %>%
  select(-Qsn) %>%
  select(Domain, everything()) %>%
  group_by(Domain) %>%
  summarise(
    `Strongly Disagree` = sum(`Strongly Disagree`), 
    Disagree = sum(Disagree),
    Neutral = sum(Neutral),
    Agree = sum(Agree),
    `Strongly Agree` = sum(`Strongly Agree`)
  )

data_long_spread2_df <- as.data.frame(data_long_spread2[,2:6])
rownames(data_long_spread2_df) <- data_long_spread2$Domain

pr.out <- prcomp(data_long_spread2_df, scale = TRUE)
biplot(pr.out, scale = 0.5)


# --------------------------

res.pca <- PCA(data_long_spread_df,graph = TRUE)
fviz_eig(res.pca, addlabels = TRUE) +
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
  )

var <- get_pca_var(res.pca)
var

fviz_pca_var(res.pca, col.var = "black")
corrplot(var$cos2, is.corr=FALSE)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank()
  )


# --------------------------

PCA_plot <- function(data, grp_names = "black", heading = "") { # Pass Data Frame with rownames
  res.pca <- PCA(data, graph = FALSE)
  p1 <- fviz_eig(res.pca, addlabels = TRUE) +
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
    )
  
  var <- get_pca_var(res.pca)
  
  load_contrib <- as_tibble(var$cos2) %>%
    add_column(Response = rownames(var$cos2))
  
  load_contrib <- load_contrib %>%
    gather(everything(), key = "Dim", value = "Contrib", -one_of("Response"))
  
  p2 <- load_contrib %>%
    ggplot(aes(Dim, Response, fill = Contrib, color = Contrib)) +
    geom_tile() +
    geom_text(aes(label = round(Contrib, digits = 2)), color = "pink", size = 5) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
    ggtitle("Loading Contributions") +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.background = element_blank(),
      legend.position = "none"
    ) + 
    labs(
      x = "Dimensions",
      y = "Contribution"
    )
  
  d <- as.data.frame(res.pca$ind$coord)
  d <- as_tibble(d) %>%
    add_column(Domain = rownames(d))
  
  p3 <- fviz_pca_biplot(res.pca, 
                  # Individuals
                  geom.ind = "point",
                  fill.ind = grp_names, col.ind = "black",
                  pointshape = 21, pointsize = 2,
                  repel = TRUE,
                  palette = "jco",
                  # Variables
                  col.var = "contrib",
                  legend.title = list(fill = "Domain", color = "Contrib",
                                      alpha = "Contrib"),
                  title = "PCA Biplot on All Items"
  ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background = element_blank()) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      strip.background = element_blank()
    ) +
    geom_text(data = d, aes(x = Dim.1, y = Dim.2, label = Domain), hjust = -.1, vjust =-.1)
  
  p <- ggarrange(p1, p2, ncol = 2, nrow = 1)
  p <- ggarrange(p3, p, ncol = 1, nrow = 2, heights = c(8, 4))
  p <- annotate_figure(
    p,
    top = text_grob(heading, face = "bold", size = 16)
  )
  return(p)
}

grp_names <- data_long_spread_df %>%
  mutate(Domain = sub("^([[:alpha:]]*).*", "\\1", data_long_spread$Qsn))
grp_names <- grp_names$Domain

PCA_plot(data_long_spread_df, grp_names, "Item Level PCA")

PCA_plot(data_long_spread2_df, heading = "Domain Level PCA")
