#set data
setwd('C:/Users/ASUS/OneDrive - Institut Teknologi Sepuluh Nopember/Dokumen/Semester 7/andat/eas')
#read_file
library(readxl)
library(tidyverse)
df<-read_xlsx('data.xlsx',sheet = 'fix')
df<-data.frame(df)
df

row.names(df)<-(df[,1])
df1<-df[,-1]
head(df1)
library(RColorBrewer)
library(corrplot)
korelasi<-cor(df1)
par(mfrow=c(1,1))
corrplot(korelasi,method = 'pie',
         type='upper',
         col = colorRampPalette(c("darkred","white","midnightblue"))(100))
ggcorrplot::ggcorrplot(korelasi,show.diag = T,lab = T)
#cek outlier

par(mfrow=c(1,1))
df1ks<-scale(df1)
colnames(df1ks)<-c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")
boxplot(scale(df1ks))
boxplot(scale(df1[,1:5]))
boxplot(scale(df1[,6:10]))



outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}
remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}
boxplot(scale(df1))
colnames(df1)
df2<-remove_outliers(df1,cols = c('Persen_HIV'))
df2<-remove_outliers(df2,cols = c('Persen_mati'))
df2<-remove_outliers(df2,cols = c('Persen_COVID'))
boxplot(scale(df2))
dim(df2)
29/38

#terdapat banyak outlier maka digunakan pam
library(factoextra)
library(FactoMineR)
library(cluster)
df1s<-scale(df2)
library(MVN)
par(mfrow=c(1,1))
mvn(df2,multivariatePlot = "qq")
#menentukan jumlah cluster terbaik
korelasi<-cor(df1s)
library(psych)


fviz_nbclust(df1s, pam, method = "silhouette")+
  labs(title ="Silhoutte Score K-Medoids" )

fviz_nbclust(df1s, pam, method = "gap_stat")+
  labs(title ="Silhoutte Score K-Medoids" )

library(NbClust)
library(cluster)
NbClust(df1s,method = "kmeans")
NbClust(df1s,method = "median")

fviz_nbclust(df1s, kmeans, method = "silhouette")+
  labs(title ="Silhoutte Score K-Means" )
# jumlah cluster terbaik adalah 3
krata2<-kmeans(df1s,3)
krata22<-kmeans(df1s,2)
kmedian<-pam(df1s,2)
kmedian2<-pam(df1s,3)
krata2$cluster
kmedian$clustering

fviz_cluster(krata2,data = df1s,ggtheme = cowplot::theme_cowplot(font_size = 16))+
  labs(title = "K-Means Scatterplot")
fviz_cluster(krata22,data = df1s,ggtheme = cowplot::theme_cowplot(font_size = 16))+
  labs(title = "K-Means Scatterplot")
fviz_cluster(kmedian,data = df1s,ggtheme = cowplot::theme_cowplot(font_size = 16))+
  labs(title = "K-Medoids Scatterplot")
fviz_cluster(kmedian2,data = df1s,ggtheme = cowplot::theme_cowplot(font_size = 16))+
  labs(title = "K-Medoids Scatterplot")


mvn(df2)

df3<-data.frame(df2,krata2$cluster,kmedian$clustering,krata22$cluster)
df3
X1<-as.matrix(df3[,c(1:10)])
Y1<-(df3$krata2.cluster)
Y2<-(df3$kmedian.clustering)
Y3<-(df3$krata22.cluster)
m1<-manova((X1)~Y1)
m2<-manova((X1)~Y2)
m3<-manova((X1)~Y3)

par(mfrow=c(2,5))
boxplot(df3$AIR.BERSIH~df3$krata2.cluster)
boxplot(df3$STUNTING~df3$krata2.cluster)
boxplot(df3$Persen_TBC~df3$krata2.cluster)
boxplot(df3$Persen_HIV~df3$krata2.cluster)

boxplot(df3$AKSES.SANITASI.AMAN~df3$krata2.cluster)
boxplot(df3$AHH~df3$krata2.cluster)
boxplot(df3$KB.iud~df3$krata2.cluster)
boxplot(df3$Persen_COVID~df3$krata2.cluster)
boxplot(df3$Persen_Kusta~df3$krata2.cluster)
boxplot(df3$Persen_mati~df3$krata2.cluster)


boxplot(df3$AIR.BERSIH~df3$kmedian.clustering)
boxplot(df3$STUNTING~df3$kmedian.clustering)
boxplot(df3$Persen_TBC~df3$kmedian.clustering)
boxplot(df3$Persen_HIV~df3$kmedian.clustering)

par(mfrow=c(1,1))
boxplot(df3$AKSES.SANITASI.AMAN~df3$kmedian.clustering)
boxplot(df3$AHH~df3$kmedian.clustering)
boxplot(df3$KB.iud~df3$kmedian.clustering)
boxplot(df3$Persen_COVID~df3$kmedian.clustering)
boxplot(df3$Persen_Kusta~df3$kmedian.clustering)
boxplot(df3$Persen_mati~df3$kmedian.clustering)

summary(m1)
summary.aov(m1)
summary(m2)
summary.aov(m2)
summary(m3)
summary.aov(m3)
dim(df2)
length(Y1)
for (cname in make.names(colnames(df2[1:10]))) {
  print(cname)
  print(kruskal.test(df2[,cname]~Y1))
}
for (cname in make.names(colnames(df2[1:10]))) {
  print(cname)
  print(kruskal.test(df2[,cname]~Y2))
}
df3$kmedian.clustering
Y2
for (cname in make.names(colnames(df2[1:10]))) {
  print(cname)
  print(kruskal.test(df2[,cname]~Y3))
}



boxM(df2,grouping = Y3)





help(mvn)
mvn(df2)
library(MVN)
mvn(df2,mvnTest = "mardia",
    univariateTest = 'Lillie')
mvn(df2,mvnTest = "mardia",
    univariateTest = 'SF')
mvn(df2,mvnTest = "mardia",
    univariateTest = 'CVM')
mvn(df2,mvnTest = "mardia",
    univariateTest = 'SW')
mvn(df2,mvnTest = "mardia",
    univariateTest = 'AD')
boxM(df3[,c(1:10)]~df3$krata2.cluster)
boxM(X1,grouping = Y1)
boxM(X1,grouping = Y2)
boxM(X1,grouping = Y3)

dfs<-df3
snam<-c("Angka Harapan Hidup","Akses Sanitasi Aman",
        "Air Bersih","KB IUD","Stunting","Persentase TBC",
        "Persentase HIV","Persentase COVID",
        "Persentase Kusta","Persentase Bayi Mati")
colnames(dfs)<-c("Angka Harapan Hidup","Akses Sanitasi Aman",
                 "Air Bersih","KB IUD","Stunting","Persentase TBC",
                 "Persentase HIV","Persentase COVID",
                 "Persentase Kusta","Persentase Bayi Mati","Kmean3","Kmedian",
                 "Kmean2")
dfs
#write.csv(dfs,"datahasil.csv")
#dfs<-read.csv('datahasil.csv')
dfs
tcname <- colnames(dfs);colnames(dfs) <- make.names(colnames(dfs));par(mfrow = c(2,5));
for(cname in make.names(colnames(dfs[1:10]))){
  idx <- which(colnames(dfs) == cname)
  boxplot(paste0(cname %>% as.symbol(), "~Kmean3") %>% as.formula(), data = dfs,
          ylab = tcname[idx], xlab = "Cluster (Kmean3)",col=c('pink','orange','skyblue'))
  # cat(deparse(cname), fill = TRUE)
  rm(cname)
};for(cname in make.names(colnames(dfs[1:10]))){
  idx <- which(colnames(dfs) == cname)
  boxplot(paste0(cname %>% as.symbol(), "~Kmean2") %>% as.formula(), data = dfs,
          ylab = tcname[idx], xlab = "Cluster (KMeans2)",col=c('pink','orange'))
  # cat(deparse(cname), fill = TRUE)
  rm(cname);rm(idx)
};for(cname in make.names(colnames(dfs[1:10]))){
  idx <- which(colnames(dfs) == cname)
  boxplot(paste0(cname %>% as.symbol(), "~Kmedian") %>% as.formula(), data = dfs,
          ylab = tcname[idx], xlab = "Cluster (Kmedian)",col=c('pink','skyblue'))
  # cat(deparse(cname), fill = TRUE)
  rm(cname)
};colnames(dfs) <- tcname;rm(tcname)
dfs
write.csv(dfs,"hasilclustereasandat.csv")
indo2<-readShapeSpatial('C:/Users/ASUS/OneDrive - Institut Teknologi Sepuluh Nopember/Dokumen/Semester 4/Statistika spasial/final project/gadm36_IDN_2.shp')
Jatim<-subset(indo2,indo2@data$NAME_1=="Jawa Timur")
dim(Jatim@data)
dfix<-read_xlsx('data.xlsx',sheet = 'lokasi')
dfix<-data.frame(dfix)
dfix$Kmean3
for (i in 1:38) {
  if(dfix$Kmean3[i] == 0){
    dfix$w1[i] <- 'darkred'
  }
  else if(dfix$Kmean3[i] == 1){
    dfix$w1[i] <- 'pink'
  }
  else if(dfix$Kmean3[i] == 2){
    dfix$w1[i] <- 'orange'
  }
  else if(dfix$Kmean3[i] == 3){
    dfix$w1[i] <- 'skyblue'
  }
}

dfix$w1
data<-cbind(Jatim,dfix)
par(mfrow=c(1,1))
plot(Jatim,col=data@data$w1,main="K-means 3 Cluster")
legend('topleft',legend=c("Clutser 1", "Cluster 2",'Cluster 3', "Outlier"),lwd=10,
       col=c('pink','orange','skyblue','darkred'))
library(npmv)
#nonpartest(`Angka Harapan Hidup`|dfs$`Akses Sanitasi Aman`|dfs$`Air Bersih`~Kmean3,data = dfs)
#return()


dfklas<-data.frame(df2,krata2$cluster)
library(randomForest)
library(xgboost)
library(caret)

#klasifikasi dengan random forest
dfklas$krata2.cluster<-as.factor(dfklas$krata2.cluster)
m1<-randomForest(krata2.cluster~.,data = dfklas)

cm1<-confusionMatrix(dfklas$krata2.cluster,predict(m1))
plt <- as.data.frame(cm1$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
cm1
ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction",title = "RF Confusion Matrix") +
  scale_x_discrete(labels=c("Cluster_1","Cluster_2","Cluster_3")) +
  scale_y_discrete(labels=c("Cluster_3","Cluster_2","Cluster_1"))

dfxgb<-read.csv('xgb.csv')
dtrain<-xgb.DMatrix(as.matrix(dfxgb[,c(-1,-12)]),label=as.numeric(dfxgb$krata2.cluster))
m2 <- xgboost(data = dtrain,                    # the data   
                 max.depth=3,                       # max depth 
                 nrounds=50,
                 objective='multi:softmax',
                 num_class =3)  



cm2<-confusionMatrix(as.factor(dfxgb$krata2.cluster),as.factor(predict(m2,dtrain)))
plt <- as.data.frame(cm2$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
cm2
ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="darkgreen") +
  labs(x = "Reference",y = "Prediction", title = "XGB Confusion Matrix") +
  scale_x_discrete(labels=c("Cluster_1","Cluster_2","Cluster_3")) +
  scale_y_discrete(labels=c("Cluster_3","Cluster_2","Cluster_1"))


varImpPlot(m1,main="Feature Importance RF")
xgb.importance(m2)
importance_matrix <- xgb.importance(colnames(dfklas[,c(-1,-12,-13)]), model = m2)
xgb.ggplot.importance(importance_matrix)
xgb.plot.shap.summary(as.matrix(dfxgb[,c(-1,-12)]),
                      model = m2,target_class = 0)+
  labs(x="Shap Value",
  title = "SHAP Plot Cluster 1")
xgb.plot.shap.summary(as.matrix(dfxgb[,c(-1,-12)]),
                      model = m2,target_class = 1)+
  labs(x="Shap Value",
       title = "SHAP Plot Cluster 2")
xgb.plot.shap.summary(as.matrix(dfxgb[,c(-1,-12)]),
                      model = m2,target_class = 2)+
  labs(x="Shap Value",
       title = "SHAP Plot Cluster 3")



xgb.plot.shap(as.matrix(dfxgb[,c(-1,-12)]),
              model = m2,top_n = 4,n_col = 2,
              pch = 16,target_class = 0)

xgb.plot.shap(as.matrix(dfxgb[,c(-1,-12)]),
              model = m2,top_n = 4,n_col = 2,
              pch = 16,target_class = 1)

xgb.plot.shap(as.matrix(dfxgb[,c(-1,-12)]),
              model = m2,top_n = 4,n_col = 2,
              pch = 16,target_class = 2)
