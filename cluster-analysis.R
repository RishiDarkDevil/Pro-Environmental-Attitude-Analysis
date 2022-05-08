library(pheatmap)
library(gplots)

# Qsn Clustering
# Trying Euclidean Distance based clustering
data_enc_trans <- as_tibble(t(data_enc[,3:ncol(data_enc)])) %>%
  add_column("Qsn" = colnames(data_enc[,3:ncol(data_enc)])) %>%
  select(Qsn, everything())

par(mfrow = c(1, 3))

data.dist <- dist(data_enc_trans[,2:ncol(data_enc_trans)])
plot(hclust(data.dist), labels = colnames(data_enc)[3:ncol(data_enc)], main = "Complete Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "average"), labels = colnames(data_enc)[3:ncol(data_enc)], main = "Average Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = colnames(data_enc)[3:ncol(data_enc)], main = "Single Linkage", xlab = "", sub = "", ylab = "")

# Trying Jaccard Similarity Distance based Clustering on Questions
sim.jac <- matrix(0, nrow=nrow(data_enc_trans), ncol=nrow(data_enc_trans))
rownames(sim.jac) <- data_enc_trans$Qsn
colnames(sim.jac) <- data_enc_trans$Qsn

pairs <- t(combn(1:nrow(data_enc_trans), 2))

for (i in 1:nrow(pairs)){
  num <- sum(data_enc_trans[pairs[i,1],2:ncol(data_enc_trans)]==data_enc_trans[pairs[i,2],2:ncol(data_enc_trans)], na.rm=T)
  den <- length(union(which(!is.na(data_enc_trans[pairs[i,1],2:ncol(data_enc_trans)])), which(!is.na(data_enc_trans[pairs[i,2],2:ncol(data_enc_trans)]))))
  sim.jac[pairs[i,1],pairs[i,2]] <- num/den
  sim.jac[pairs[i,2],pairs[i,1]] <- num/den  
}

sim.jac[which(is.na(sim.jac))] <- 0
diag(sim.jac) <- 1

data.dist <- sim2dist(sim.jac)

hc2 <- hclust(data.dist, method = "ward.D2")

A2Rplot(
  hc2,
  k = 9,
  boxes = F,
  col.up = "gray50",
  col.down = c("#ff9900", "#4ECDC4", "#556270", "#ff66ff", "#00cc00", "#cc0000", "#cccc00", "#ADFF2F", "#000080"),
  show.labels=T,
  main=NULL
)

# Cosine Similarity
sim.cos <- matrix(0, nrow=nrow(data_enc_trans), ncol=nrow(data_enc_trans))
rownames(sim.cos) <- data_enc_trans$Qsn
colnames(sim.cos) <- data_enc_trans$Qsn

pairs <- t(combn(1:nrow(data_enc_trans), 2))

for (i in 1:nrow(pairs)){
  num <- sum(data_enc_trans[pairs[i,1],2:ncol(data_enc_trans)]*data_enc_trans[pairs[i,2],2:ncol(data_enc_trans)])
  den <- sqrt(sum((data_enc_trans[pairs[i,1],2:ncol(data_enc_trans)])^2)*sum((data_enc_trans[pairs[i,2],2:ncol(data_enc_trans)])^2))
  sim.cos[pairs[i,1],pairs[i,2]] <- num/den
  sim.cos[pairs[i,2],pairs[i,1]] <- num/den  
}

sim.cos[which(is.na(sim.cos))] <- 0
diag(sim.cos) <- 1

data.dist <- sim2dist(sim.cos)

hc1 <- hclust(data.dist, method = "ward.D2")

A2Rplot(
  hc1,
  k = 9,
  boxes = F,
  col.up = "gray50",
  col.down = c("#ff9900", "#4ECDC4", "#556270", "#ff66ff", "#00cc00", "#cc0000", "#cccc00", "#ADFF2F", "#000080"),
  show.labels=T,
  main=NULL
)

# Euclidean Distance based clustering visualization
mat <- as.matrix(data_enc_trans[,2:ncol(data_enc_trans)])
colnames(mat) <- str_c("P", 1:(ncol(data_enc_trans)-1))
rownames(mat) <- data_enc_trans$Qsn
pheatmap(mat, scale = "row")

heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none")

# Pairwise correlation between samples (columns)
cols.cor <- cor(data_enc[,3:ncol(data_enc)], use = "pairwise.complete.obs", method = "pearson")
# Pairwise correlation between rows (Qsns)
rows.cor <- cor(data_enc_trans[,2:ncol(data_enc_trans)], use = "pairwise.complete.obs", method = "pearson")

## Row- and column-wise clustering using correlation 
hclust.col <- hclust(as.dist(1-cols.cor)) 
hclust.row <- hclust(as.dist(1-rows.cor))

A2Rplot(
  hclust.col,
  k = 9,
  boxes = F,
  col.up = "gray50",
  col.down = c("#ff9900", "#4ECDC4", "#556270", "#ff66ff", "#00cc00", "#cc0000", "#cccc00", "#ADFF2F", "#000080"),
  show.labels=T,
  main=NULL
)

# Clustering Based on Correlation for the Questions and Clustering Based on Euclidean Distance for Students
heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none",

          Rowv = as.dendrogram(hclust.col)
)

# Clustering Based on Jaccard for the Questions and Clustering Based on Euclidean Distance for Students
heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none",

          Rowv = as.dendrogram(hc2)
)

# Clustering Based on Euclidean Distance for the Questions and Clustering Based on Euclidean Distance for Students
heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none"
)

# Clustering Based on Cosine Distance for the Questions and Clustering Based on Euclidean Distance for Students
heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none",
          Rowv = as.dendrogram(hc1)
)


# Trying other distance metric

## Row- and column-wise clustering using correlation 

heatmap_dendo_plot <- function(qsn_dist = "euclidean", per_dist = "euclidean") {
  hclust.col <<- hclust(dist(data_enc_trans[,2:ncol(data_enc_trans)], method = qsn_dist)) # clustering of columns(qsns)
  hclust.row <- hclust(dist(data_enc[,3:ncol(data_enc)], method = per_dist)) # clustering of persons(rows)
  heatmap.2(mat, col = bluered(100), 
            trace = "none", density.info = "none",
            Colv = as.dendrogram(hclust.row),
            Rowv = as.dendrogram(hclust.col)
  )
}

heatmap_dendo_plot <- function(per_dist = "euclidean") {
  hclust.row <<- hclust(dist(data_enc[,3:ncol(data_enc)], method = per_dist)) # clustering of persons(rows)
  heatmap.2(mat, col = bluered(100), 
            trace = "none", density.info = "none",
            Colv = as.dendrogram(hclust.row),
            Rowv = as.dendrogram(hc2)
  )
}

# Person-Euclidean, Qsn-Euclidean
heatmap_dendo_plot()

# Person-Euclidean, Qsn-Euclidean
heatmap_dendo_plot("")

A2Rplot(
  hclust.col,
  k = 9,
  boxes = F,
  col.up = "gray50",
  col.down = c("#ff9900", "#4ECDC4", "#556270", "#ff66ff", "#00cc00", "#cc0000", "#cccc00", "#ADFF2F", "#000080"),
  show.labels=T,
  main=NULL
)

# Trying Jaccard Similarity Distance based Clustering on Persons
sim.jac <- matrix(0, nrow=nrow(data_enc), ncol=nrow(data_enc))
rownames(sim.jac) <- str_c("P", 1:(ncol(data_enc_trans)-1))
colnames(sim.jac) <- str_c("P", 1:(ncol(data_enc_trans)-1))

pairs <- t(combn(1:nrow(data_enc), 2))

for (i in 1:nrow(pairs)){
  num <- sum(data_enc[pairs[i,1],3:ncol(data_enc)]==data_enc[pairs[i,2],3:ncol(data_enc)], na.rm=T)
  den <- length(union(which(!is.na(data_enc[pairs[i,1],3:ncol(data_enc)])), which(!is.na(data_enc[pairs[i,2],3:ncol(data_enc)]))))
  sim.jac[pairs[i,1],pairs[i,2]] <- num/den
  sim.jac[pairs[i,2],pairs[i,1]] <- num/den  
}

sim.jac[which(is.na(sim.jac))] <- 0
diag(sim.jac) <- 1

data.dist <- sim2dist(sim.jac)

hc3 <- hclust(data.dist, method = "ward.D2")

heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none",
          Colv = as.dendrogram(hc3),
          Rowv = as.dendrogram(hc2)
)

# ------------------------------------------------------------------
# Domain Clustering
# Trying Euclidean Distance based clustering
data_net_score_trans <- as_tibble(t(data_net_score[,3:ncol(data_net_score)])) %>%
  add_column("Domain" = colnames(data_net_score[,3:ncol(data_net_score)])) %>%
  select(Domain, everything())

par(mfrow = c(1, 3))

data.dist <- dist(data_net_score_trans[,2:ncol(data_net_score_trans)])
plot(hclust(data.dist), labels = colnames(data_net_score)[3:ncol(data_net_score)], main = "Complete Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "average"), labels = colnames(data_net_score)[3:ncol(data_net_score)], main = "Average Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = colnames(data_net_score)[3:ncol(data_net_score)], main = "Single Linkage", xlab = "", sub = "", ylab = "")

# Trying Jaccard Similarity Distance based Clustering on Questions
sim.jac <- matrix(0, nrow=nrow(data_net_score_trans), ncol=nrow(data_net_score_trans))
rownames(sim.jac) <- data_net_score_trans$Domain
colnames(sim.jac) <- data_net_score_trans$Domain

pairs <- t(combn(1:nrow(data_net_score_trans), 2))

for (i in 1:nrow(pairs)){
  num <- sum(data_net_score_trans[pairs[i,1],2:ncol(data_net_score_trans)]==data_net_score_trans[pairs[i,2],2:ncol(data_net_score_trans)], na.rm=T)
  den <- length(union(which(!is.na(data_net_score_trans[pairs[i,1],2:ncol(data_net_score_trans)])), which(!is.na(data_net_score_trans[pairs[i,2],2:ncol(data_net_score_trans)]))))
  sim.jac[pairs[i,1],pairs[i,2]] <- num/den
  sim.jac[pairs[i,2],pairs[i,1]] <- num/den  
}

sim.jac[which(is.na(sim.jac))] <- 0
diag(sim.jac) <- 1

data.dist <- sim2dist(sim.jac)

hc2 <- hclust(data.dist, method = "ward.D2")

A2Rplot(
  hc2,
  k = 2,
  boxes = F,
  col.up = "gray50",
  col.down = c("#ff9900", "#4ECDC4", "#556270", "#ff66ff", "#00cc00", "#cc0000", "#cccc00", "#ADFF2F", "#000080"),
  show.labels=T,
  main=NULL
)

# Cosine Similarity
sim.cos <- matrix(0, nrow=nrow(data_net_score_trans), ncol=nrow(data_net_score_trans))
rownames(sim.cos) <- data_net_score_trans$Domain
colnames(sim.cos) <- data_net_score_trans$Domain

pairs <- t(combn(1:nrow(data_net_score_trans), 2))

for (i in 1:nrow(pairs)){
  num <- sum(data_net_score_trans[pairs[i,1],2:ncol(data_net_score_trans)]*data_net_score_trans[pairs[i,2],2:ncol(data_net_score_trans)])
  den <- sqrt(sum((data_net_score_trans[pairs[i,1],2:ncol(data_net_score_trans)])^2)*sum((data_net_score_trans[pairs[i,2],2:ncol(data_net_score_trans)])^2))
  sim.cos[pairs[i,1],pairs[i,2]] <- num/den
  sim.cos[pairs[i,2],pairs[i,1]] <- num/den  
}

sim.cos[which(is.na(sim.cos))] <- 0
diag(sim.cos) <- 1

data.dist <- sim2dist(sim.cos)

hc1 <- hclust(data.dist, method = "ward.D2")

A2Rplot(
  hc1,
  k = 4,
  boxes = F,
  col.up = "gray50",
  col.down = c("#ff9900", "#4ECDC4", "#556270", "#ff66ff", "#00cc00", "#cc0000", "#cccc00", "#ADFF2F", "#000080"),
  show.labels=T,
  main=NULL
)

# Euclidean Distance based clustering visualization
mat <- as.matrix(data_net_score_trans[,2:ncol(data_net_score_trans)])
colnames(mat) <- str_c("P", 1:(ncol(data_net_score_trans)-1))
rownames(mat) <- data_net_score_trans$Domain
pheatmap(mat, scale = "row")

heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none")

# Pairwise correlation between samples (columns)
cols.cor <- cor(data_net_score[,3:ncol(data_net_score)], use = "pairwise.complete.obs", method = "pearson")
# Pairwise correlation between rows (Qsns)
rows.cor <- cor(data_net_score_trans[,2:ncol(data_net_score_trans)], use = "pairwise.complete.obs", method = "pearson")

## Row- and column-wise clustering using correlation 
hclust.col <- hclust(as.dist(1-cols.cor)) 
hclust.row <- hclust(as.dist(1-rows.cor))

A2Rplot(
  hclust.col,
  k = 5,
  boxes = F,
  col.up = "gray50",
  col.down = c("#ff9900", "#4ECDC4", "#556270", "#ff66ff", "#00cc00", "#cc0000", "#cccc00", "#ADFF2F", "#000080"),
  show.labels=T,
  main=NULL
)

# Clustering Based on Correlation for the Questions and Clustering Based on Euclidean Distance for Students
heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none",
          
          Rowv = as.dendrogram(hclust.col)
)

# Clustering Based on Jaccard for the Questions and Clustering Based on Euclidean Distance for Students
heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none",
          
          Rowv = as.dendrogram(hc2)
)

# Clustering Based on Euclidean Distance for the Questions and Clustering Based on Euclidean Distance for Students
heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none"
)

# Clustering Based on Cosine Distance for the Questions and Clustering Based on Euclidean Distance for Students
heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none",
          Rowv = as.dendrogram(hc1)
)


# Trying other distance metric

## Row- and column-wise clustering using correlation 

heatmap_dendo_plot <- function(qsn_dist = "euclidean", per_dist = "euclidean") {
  hclust.col <<- hclust(dist(data_net_score_trans[,2:ncol(data_net_score_trans)], method = qsn_dist)) # clustering of columns(qsns)
  hclust.row <- hclust(dist(data_net_score[,3:ncol(data_net_score)], method = per_dist)) # clustering of persons(rows)
  heatmap.2(mat, col = bluered(100), 
            trace = "none", density.info = "none",
            Colv = as.dendrogram(hclust.row),
            Rowv = as.dendrogram(hclust.col)
  )
}

heatmap_dendo_plot <- function(per_dist = "euclidean") {
  hclust.row <<- hclust(dist(data_net_score[,3:ncol(data_net_score)], method = per_dist), labels = colnames(data_net_score)[,3:ncol(data_net_score)]) # clustering of persons(rows)
  heatmap.2(mat, col = bluered(100), 
            trace = "none", density.info = "none",
            Colv = as.dendrogram(hclust.row),
            Rowv = as.dendrogram(hc2)
  )
}

heatmap_dendo_plot("maximum")

A2Rplot(
  hclust.col,
  k = 4,
  boxes = F,
  col.up = "gray50",
  col.down = c("#ff9900", "#4ECDC4", "#556270", "#ff66ff", "#00cc00", "#cc0000", "#cccc00", "#ADFF2F", "#000080"),
  show.labels=T,
  main=NULL
)

# Trying Jaccard Similarity Distance based Clustering on Persons
sim.jac <- matrix(0, nrow=nrow(data_net_score), ncol=nrow(data_net_score))
rownames(sim.jac) <- str_c("P", 1:(ncol(data_net_score_trans)-1))
colnames(sim.jac) <- str_c("P", 1:(ncol(data_net_score_trans)-1))

pairs <- t(combn(1:nrow(data_net_score), 2))

for (i in 1:nrow(pairs)){
  num <- sum(data_net_score[pairs[i,1],3:ncol(data_net_score)]==data_net_score[pairs[i,2],3:ncol(data_net_score)], na.rm=T)
  den <- length(union(which(!is.na(data_net_score[pairs[i,1],3:ncol(data_net_score)])), which(!is.na(data_net_score[pairs[i,2],3:ncol(data_net_score)]))))
  sim.jac[pairs[i,1],pairs[i,2]] <- num/den
  sim.jac[pairs[i,2],pairs[i,1]] <- num/den  
}

sim.jac[which(is.na(sim.jac))] <- 0
diag(sim.jac) <- 1

data.dist <- sim2dist(sim.jac)

hc3 <- hclust(data.dist, method = "ward.D2")

heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none",
          Colv = as.dendrogram(hc3),
          Rowv = as.dendrogram(hc2)
)
