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

full
library(tidymodels)
library(xgboost)
library(randomForest)
library(kernelshap)
library(shapviz)

recipe1 = full |> recipe(y~x1+x2+x3+latitude+longitude+tahun)
prep = prep(recipe1)

split1 = full %>%  initial_split(0.8)
df_train = training(split1)
df_test = testing(split1)
fold = vfold_cv(df_train)

xgb = boost_tree(tree_depth = tune(),
                 trees = tune(),
                 learn_rate = tune(),
                 loss_reduction = tune()) |> set_mode("regression")  |> set_engine("xgboost")

rf2 = rand_forest(trees = tune()) |> set_mode("regression")  |> set_engine("randomForest")

tune_xgb_wf = workflow() %>%
  add_model(xgb) %>%
  add_recipe(recipe1)

tune_rf2_wf = workflow() %>%
  add_model(rf2) %>%
  add_recipe(recipe1)

grid_boost_tree = grid_regular(tree_depth(),
                               trees(range = c(100L,300L)),
                               learn_rate(),
                               loss_reduction(),
                               levels = 3)

grid_rf = grid_regular(trees(range = c(100L,500L)),
                       levels = 5)


xgb_res = tune_xgb_wf %>%
  tune_grid(resamples = fold,
            grid = grid_boost_tree,
            control = control_grid(verbose = T))

rf2_res = tune_rf2_wf %>%
  tune_grid(resamples = fold,
            grid = grid_rf,
            control = control_grid(verbose = T))
rf2_res$.metrics
best_xgb = xgb_res |> select_best(metric = "rsq")
best_rf2 = rf2_res |> select_best(metric = "rsq")

final_xgb_wf = tune_xgb_wf |> finalize_workflow(best_xgb) |> fit(df_train)
final_rf2_wf = tune_rf2_wf |> finalize_workflow(best_rf2) |> last_fit(split1)

pxgb=predict(final_xgb_wf,full)


1+1

p=extract_fit_parsnip(final_xgb_wf)
p2=extract_fit_parsnip(final_rf2_wf)

mxgb = extract_fit_engine(p)
mrf = extract_fit_engine(p2)

matriks=full[,c(3:8)] |> as.matrix()
matriks
matriks
rsqhasil = c(rsq_vec(full$y,predict(mrf,full)),
rsq_vec(full$y,pxgb$.pred))

rmsehasil = c( 
rmse_vec(full$y,predict(mrf,full)),
rmse_vec(full$y,pxgb$.pred))

hasilml = data.frame(rsqhasil,rmsehasil)
hasilml
full |> dim()
2455/5*4

rsq_vec(full$y[1964:2455],pxgb$.pred[1964:2455])
rsq_vec(full$y[1964:2455],predict(mrf,full)[1964:2455])

write.csv(hasilml,"hasilml.csv")

prediksi = data.frame(y = full$y,
                      y_RF = predict(mrf,full),
                      y_XGB = pxgb$.pred)
write.csv(prediksi,"prediksi.csv")

library(patchwork)

pred1 = prediksi |> ggplot(data =_,aes(x=y,y=y_RF)) + geom_point() +
  geom_abline(intercept = 0,slope = 1) + 
  ggtitle("Prediksi vs Actual Random Forest") +
  ylim(0,15)
pred2 =  prediksi |> ggplot(data =_,aes(x=y,y=y_XGB)) + geom_point() +
  geom_abline(intercept = 0,slope = 1)  + ggtitle("Prediksi vs Actual Xgboost") +
  ylim(0,15)

## Shap Value model 

full$
  matriks
full2 = data.frame(y = full$y,
                   x1 = full$x1,
                   x2 = full$x2,
                   x3 = full$x3,
                   latitude = full$latitude,
                   longitude = full$longitude,
                   tahun = full$tahun)

shap <- shapviz(mxgb, X_pred = as.matrix(full2[,c(2:7)]),bg_X = full[sample(nrow(full),100),])

matriks |> head()
shap

plot_grid(sv_importance(shap, kind = "bar", show_numbers = TRUE) + ggtitle("Shap Importance Xgboost"),
          sv_importance(sv, kind = "bar", show_numbers = TRUE) + ggtitle("Shap Importance Random Forest"))


plot_grid(sv_dependence(shap, "x1", color_var = "x2"),
          sv_dependence(shap, "x2", color_var = "x3"),
          sv_dependence(shap, "x1", color_var = "x3"),
          ncol = 3) + ggtitle("Plot Dependence Random Forest")

plot_grid(sv_dependence(sv, "x1", color_var = "x2"),
          sv_dependence(sv, "x2", color_var = "x3"),
          sv_dependence(sv, "x1", color_var = "x3"),
          ncol = 3) + ggtitle("Plot Dependence Random Forest")

sv_dependence(shap, "sekolah", color_var = "umur")
sv_dependence(shap,c( "latitude","longitude"), color_var = NULL)
?sv_dependence
sv_force(shap, row_id = 1)


sv_force(shap)
sv_waterfall(shap)

s <- kernelshap(mrf, full2[,c(2:7)],  bg_X = full[sample(nrow(full),100),])

sv <- shapviz(s)
sv_importance(sv, kind = "bar", show_numbers = TRUE)
sv_dependence(sv, "x1", color_var = "x2")
sv_dependence(sv, "x1", color_var = "x3")
sv_dependence(sv, "x2", color_var = "x3")

sv_dependence(sv, "x1", color_var = "tahun")
sv_dependence(sv, "x2", color_var = "tahun")
sv_dependence(sv, "x3", color_var = "tahun")

sv_dependence(sv, "pengeluaran", color_var = "umur")
sv_dependence(sv, "sekolah", color_var = "umur")
sv_dependence(sv,c( "latitude","longitude"), color_var = NULL)


library(cowplot)
plot_grid(pred1,pred2)

library(shapviz)
library(kernelshap)

