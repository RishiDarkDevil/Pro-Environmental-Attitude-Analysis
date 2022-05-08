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

# Trying Jaccard Similarity Distance based Clustering
sim.jac <- matrix(0, nrow=nrow(data_enc_trans), ncol=nrow(data_enc_trans))
rownames(sim.jac) <- data_enc_trans$Qsn
colnames(sim.jac) <- data_enc_trans$Qsn

pairs <- t(combn(1:nrow(data_enc_trans), 2))

for (i in 1:nrow(pairs)){
  num <- sum(data_enc_trans[pairs[i,1],]==data_enc_trans[pairs[i,2],], na.rm=T)
  den <- length(union(which(!is.na(data_enc_trans[pairs[i,1],])), which(!is.na(data_enc_trans[pairs[i,2],]))))
  sim.jac[pairs[i,1],pairs[i,2]] <- num/den
  sim.jac[pairs[i,2],pairs[i,1]] <- num/den  
}

sim.jac[which(is.na(sim.jac))] <- 0
diag(sim.jac) <- 1

data.dist <- sim2dist(sim.jac)

hc2 <- hclust(dist, method = "ward.D2")

A2Rplot(
  hc2,
  k = 8,
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
# Pairwise correlation between rows (genes)
rows.cor <- cor(data_enc_trans[,2:ncol(data_enc_trans)], use = "pairwise.complete.obs", method = "pearson")

## Row- and column-wise clustering using correlation 
hclust.col <- hclust(as.dist(1-cols.cor)) 
hclust.row <- hclust(as.dist(1-rows.cor))

heatmap.2(mat, col = bluered(100), 
          trace = "none", density.info = "none",
          Colv = as.dendrogram(hclust.row),
          Rowv = as.dendrogram(hclust.col)
)
