library(readxl)
library(ggplot2)
library(GGally)
library(tidyverse)
df = read_xlsx("dataset/DATA SEMIFINAL NSC 2024.xlsx",sheet = "Sheet1")
dfy = read_xlsx("dataset/DATA SEMIFINAL NSC 2024.xlsx",sheet = "Sheet2")
dfy
# Pengaguran terbuka
y = dfy[,c(4:8)]
y1 = y |> stack()
y1
y11 = data.frame(lokasi1 = dfy$Kota_Kabupaten,
                 y = y1)
y11 |> dim()
y11$lokasi_waktu = paste(y11$lokasi1,y11$y.ind)
y11
# rata2 lama sekolah
x1 = df[7:11]
x11= x1 |> stack()
x11 
# rata2 Pengeluaran perkapita tahunan
x2 = df[12:16]
x21 = x2 |> stack()
# rata2 Pengeluaran perkapita tahunan
x3 = df[22:26]
x31 = x3 |> stack()

x = data.frame(x1 = x11$values,
               x2 = x21$values,
               x3 = x31$values,
               lokasi2 = df$Kota_Kabupaten,
               tahun = rep(2019:2023,each = 548))
x |> dim()
x$lokasi_waktu =  paste(x$lokasi2,x$tahun)
y11 |> glimpse()
y11 |> dim()

library(tidyverse)

y11 |> head()
x |> head()

df_1 = inner_join(y11,x)

df_1 |> dim()
lokasi = read_csv("dataset/daftar-nama-daerah.csv")
lokasi1 = lokasi |> dplyr::filter(type == 2) |> dplyr::select(latitude,longitude,name)
colnames(lokasi1)[3] <- "lokasi1" 
lokasi1 |> dim()

s=inner_join(df_1,lokasi1)

full = data.frame(lokasi = s$lokasi1,
                  y = s$y.values,
                  x1 = s$x1,
                  x2 = s$x2,
                  x3 = s$x3,
                  tahun = s$tahun,
                  longitude = s$longitude,
                  latitude = s$latitude)

 
full$longitude  


ggpairs(full[2:5])

full$tahun <- as.character(full$tahun)



p1 = ggplot(data=full,aes(group = tahun,y=y,
                          x=tahun,
                          fill = tahun)) + geom_boxplot()
p2 = ggplot(data=full,aes(group = tahun,y=x1,
                          x=tahun,
                          fill = tahun)) +  geom_boxplot()
p3 = ggplot(data=full,aes(group = tahun,y=x2,
                          x=tahun,
                          fill = tahun)) +  geom_boxplot()
p4 = ggplot(data=full,aes(group = tahun,y=x3,
                          x=tahun,
                          fill = tahun)) +  geom_boxplot()

library(patchwork)
library(cowplot)

plot_grid(p1,p2,p3,p4,
          ncol = 2)


library(rnaturalearth)
library(rnaturalearthdata)

## peta ----
world <- rnaturalearth::ne_countries(
  scale = "medium", 
  returnclass = "sf"
)
full2023 = full |> filter(tahun == 2023)
full2023
peta_y= ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = world, 
    fill = "white"
  ) +
  ggplot2::geom_point(
    data = full2023,
    ggplot2::aes(
      x = longitude,
      y = latitude,
      color = y
    ),
    size = 3.5
  ) +
  ggplot2::scale_color_viridis_c(
    direction = -1, 
    option = "F"
  ) +
  ggplot2::scale_x_continuous(limits = c(94,142)) +
  ggplot2::scale_y_continuous(limits = c(-11,7.5)) + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") +
  ggplot2::ggtitle("Tingkat Pengangguran Terbuka 2023")


peta_x1= ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = world, 
    fill = "white"
  ) +
  ggplot2::geom_point(
    data = full2023,
    ggplot2::aes(
      x = longitude,
      y = latitude,
      color = x1
    ),
    size = 3.5
  ) +
  ggplot2::scale_color_viridis_c(
    direction = -1, 
    option = "magma"
  ) +
  ggplot2::scale_x_continuous(limits = c(94,142)) +
  ggplot2::scale_y_continuous(limits = c(-11,7.5)) +
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") +
  ggplot2::ggtitle("Rata-rata lama sekolah 2023")

peta_x2= ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = world, 
    fill = "white"
  ) +
  ggplot2::geom_point(
    data = full2023,
    ggplot2::aes(
      x = longitude,
      y = latitude,
      color = x2
    ),
    size = 3.5
  ) +
  ggplot2::scale_color_viridis_c(
    direction = -1, 
    option = "D"
  ) +
  ggplot2::scale_x_continuous(limits = c(94,142)) +
  ggplot2::scale_y_continuous(limits = c(-11,7.5)) + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") +
  ggplot2::ggtitle("Pengeluaran Perkapita 2023")

peta_x3= ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = world, 
    fill = "white"
  ) +
  ggplot2::geom_point(
    data = full2023,
    ggplot2::aes(
      x = longitude,
      y = latitude,
      color = x3
    ),
    size = 3.5
  ) +
  ggplot2::scale_color_viridis_c(
    direction = -1, 
    option = "mako"
  ) +
  ggplot2::scale_x_continuous(limits = c(94,142)) +
  ggplot2::scale_y_continuous(limits = c(-11,7.5)) + 
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") +
  ggplot2::ggtitle("Umur Harapan Hidup 2023")


peta_y
cari = full2023 |> filter(longitude > 105,longitude <= 110,
                          latitude < -5, latitude > -10)
cari
min(cari$y)
cari
(peta_y + peta_x1 )/ (peta_x2 + peta_x3)
plot_grid(peta_y,peta_x1,peta_x2,peta_x3,ncol = 2)

## Pemodelan tanpa efek spasial ----
m1 = full |> lm(y~x1+x2+x3,data=_)
summary(m1)
library(car)
library(lmtest)
vif(m1)
bptest(m1)
library(randomForest)
m2 = full |> randomForest(y~x1+x2+x3,data=_)
m2

library(xgboost)
library(tidymodels)

matriks= full[,c(3:5)] |> as.matrix()
dtrain = xgb.DMatrix(matriks, label = full$y)

set.seed(100)
cv = xgb.cv(data = dtrain, nrounds = 100, nthread = 1, nfold = 5,
            max_depth = 3, eta = 1, objective = "reg:squarederror",
            early_stopping_rounds = 1,
            prediction = T)

r21 = rbind(rsq_vec(full$y,m1$fitted.values),
            rsq_vec(full$y,predict(m2)),
            rsq_vec(full$y,cv$pred))
r21
# mse
mse1 = rbind(rmse_vec(full$y,m1$fitted.values)^2,
             rmse_vec(full$y,predict(m2))^2,
             rmse_vec(full$y,cv$pred)^2)

hasilmetric = data.frame(r2 = r21,
                         mse = mse1)
hasilmetric
write.csv(hasilmetric,"hasil2023.csv")

## Pemodelan dengan efek spasial ----

# gwtr
x = full$longitude
y = full$latitude
coord = data.frame(x,y)

# Spatial points (same as coordinates in this example)
library(GWmodel)
coords_sp <- SpatialPointsDataFrame(
  coords = coord,
  data = full[,c(2:5)]  # Include other relevant variables
)
coords_sp
# Define the spatio-temporal bandwidth (st.bw)
# Adjust these values based on your data and understanding
# Include the time attribute (year in this case)
form = y~x1+x2+x3
year = full$tahun |> as.numeric()
#st.bw <- bw.gtwr(formula = form, 
#                data = coords_sp, 
#              adaptive = TRUE,
#               approach="CV",  # or approach="CV"
#             obs.tv = year,
#            longlat = T,
#           kernel = "bisquare")  
st.bw <- bw.gtwr(formula = form, 
                 data = coords_sp, 
                 adaptive = TRUE,
                 approach="CV",  # or approach="CV"
                 obs.tv = year,
                 longlat = T,
                 kernel = "bisquare")


saveRDS(st.bw,"bandwith.rds")
k=readRDS("bandwith.rds")
# Fit GTWR model (now with SpatialPointsDataFrame)
gtwr_model <- gtwr(
  formula = form, 
  data = coords_sp,             # Use the SpatialPointsDataFrame
  obs.tv = year,               # Observation time points
  reg.tv = year,               # Regression time points
  kernel = "bisquare",
  adaptive = TRUE,
  p = 2,                       # Minkowski distance power
  longlat = T,              # Coordinates not in longitude/latitude
  st.bw = k
)
gtwr_model$SDF$x1[2129]/gtwr_model$SDF$x1_SE[2129]

gtwr_model$SDF$x2[2129]/gtwr_model$SDF$x2_SE[2129]

gtwr_model$SDF$x3[2129]/gtwr_model$SDF$x3_SE[2129]

gtwr_model
full |> filter(lokasi == "Bandung")

which(full$lokasi == "Bandung")
full[2129,]


library(yardstick)

rmse_vec(full$y,gtwr_model$SDF$yhat)

prediksi = read.csv("prediksi.csv")
prediksi$y_GTWR = gtwr_model$SDF$yhat
prediksi |> dim()
2455/5*4
prediksi2 = prediksi[c(1964:2455),]
pp1 = ggplot(prediksi2, aes (y = y_RF,x=y)) +
  geom_point() + geom_abline(intercept = 0,
                             slope = 1) +
  ggtitle("Prediksi Random Forest 2023")

pp2 = ggplot(prediksi2, aes (y = y_XGB,x=y)) +
  geom_point() + geom_abline(intercept = 0,
                             slope = 1) +
  ggtitle("Prediksi Xgboost 2023")
pp3 = ggplot(prediksi2, aes (y = y_GTWR,x=y)) +
  geom_point() + geom_abline(intercept = 0,
                             slope = 1)+
  ggtitle("Prediksi GTWR 2023")

plot_grid(pp3,pp2,pp1,ncol = 3)
gtwr_model


hasil2023 = data.frame(rsq = c(rsq_vec(prediksi2$y,prediksi2$y_GTWR),
                              rsq_vec(prediksi2$y,prediksi2$y_RF),
                              rsq_vec(prediksi2$y,prediksi2$y_XGB)),
                       rmse = c(rmse_vec(prediksi2$y,prediksi2$y_GTWR),
                               rmse_vec(prediksi2$y,prediksi2$y_RF),
                               rmse_vec(prediksi2$y,prediksi2$y_XGB)))
write.csv(hasil2023,"hasil2023.csv")

df_box = data.frame(intercept = gtwr_model$SDF$Intercept,
                    beta1 = gtwr_model$SDF$x1,
                    beta2 = gtwr_model$SDF$x2,
                    beta3 = gtwr_model$SDF$x3)
df_box2 = df_box[,c(2:4)] |> stack()
boxplot = df_box2 |> ggplot(data =_,aes(y=values)) +
  geom_boxplot(aes(color = ind))
boxplot
