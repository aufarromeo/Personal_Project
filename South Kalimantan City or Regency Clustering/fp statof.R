library(ggpubr)
library(cluster)
library(factoextra)
library(ggcorrplot)
library(tidyverse)
setwd('C:/Users/ASUS/OneDrive - Institut Teknologi Sepuluh Nopember/Dokumen/Semester 7/Statof/eas')


dfs <- readxl::read_xlsx("data.xlsx") %>% as.data.frame();dfs[-1] <- lapply(dfs[-1], as.numeric) %>% as.data.frame()
dfs
rownames(dfs) <- dfs[[1]];dfs <- dfs[-1]
colnames(dfs)
dfs<-dfs[-1,]
cor_dfs <- cor(dfs);ggcorrplot(cor_dfs, lab = TRUE, type = "lower")
dim(df)
outliers <- function(x, probs = c(0.25, 0.75), labels = NULL) {
  if(length(probs) != 2) stop("Probability length must be 2.")
  Q1 <- quantile(x, probs=min(probs))
  Q3 <- quantile(x, probs=max(probs))
  iqr = Q3-Q1
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  temp_outliers <- x > upper_limit | x < lower_limit
  cat("Upper Limit: ", round(upper_limit, 3), "\t", paste(max(probs) * 100, "%", sep = ""), "\nLower Limit: ", round(lower_limit, 3), "\t", paste(min(probs) * 100, "%", sep = ""), fill = TRUE)
  if(length(which(temp_outliers)) != 0){
    for(i in which(temp_outliers)) cat("Outlier: ", x[i], "\t", labels[i], fill = TRUE)
  } else cat("Tidak ada outlier.\n")
  return(!temp_outliers)
};for(cname in colnames(dfs)){
  cat("\n", cname, "\n")
  outliers(dfs[[cname]], probs = c(0.25, 0.75), labels = rownames(dfs[cname]))
  rm(cname)
}
dfs <- dfs[outliers(dfs$IPM),]

dfs1<-dfs

fviz_nbclust(dfs %>% scale(), kmeans,method = 'silhouette')
fviz_nbclust(dfs %>% scale(), pam)


c1 <- pam(dfs1 %>% scale(), k = 3)
c2 <- kmeans(dfs1 %>% scale(), centers = 2)
c11<-c1
c22<-c2

fviz_cluster(c1, main = "K-Medoids", ggtheme = cowplot::theme_cowplot(font_size = 16))
fviz_cluster(c2, data = dfs1, main = "KMeans", ggtheme = cowplot::theme_cowplot(font_size = 16))
dfs["PAM"] <- c1$clustering %>% as.factor(); dfs["KMeans"] <- c2$cluster %>% as.factor();rm(c1);rm(c2)
dfs
tcname <- colnames(dfs);colnames(dfs) <- make.names(colnames(dfs));par(mfrow = c(2,3));
for(cname in make.names(colnames(dfs[1:6]))){
  idx <- which(colnames(dfs) == cname)
  boxplot(paste0(cname %>% as.symbol(), "~PAM") %>% as.formula(), data = dfs,
          ylab = tcname[idx], xlab = "Cluster (PAM)")
  # cat(deparse(cname), fill = TRUE)
  rm(cname)
};for(cname in make.names(colnames(dfs[1:6]))){
  idx <- which(colnames(dfs) == cname)
  boxplot(paste0(cname %>% as.symbol(), "~KMeans") %>% as.formula(), data = dfs,
          ylab = tcname[idx], xlab = "Cluster (KMeans)")
  # cat(deparse(cname), fill = TRUE)
  rm(cname);rm(idx)
};colnames(dfs) <- tcname;rm(tcname)
dfs
MVN::mvn(dfs[1:6], mvnTest = "royston", univariateTest = "SW", desc = FALSE)
m1 <- manova(dfs[1:6] %>% as.matrix() ~ PAM, data = dfs);summary(m1);summary.aov(m1)
m2 <- manova(dfs[1:6] %>% as.matrix() ~ KMeans, data = dfs);summary(m2);summary.aov(m2)


