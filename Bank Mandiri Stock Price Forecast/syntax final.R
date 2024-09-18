getwd()
setwd("C:/Users/ASUS/OneDrive - Institut Teknologi Sepuluh Nopember/Dokumen/Semester 6/Lomba/olympicstat")

library(TSA)
library(tsoutliers)
library(forecast)
library(tseries)
library(lmtest)
library(nortest)
library(xts)
library(CausalImpact)
library(MASS)

df<-read.csv("BMRI.JK.csv")
Diagnostik = function(model){
  par(mfrow=c(2,2))
  
  # Plot sisaan terhadap order
  plot(residuals(model), ylab ='Residuals', type='o'); 
  abline(h=0)
  
  # QQ plot
  qqnorm(residuals(model))
  qqline(residuals(model))
  
  # Histogram sisaan
  hist(model$residuals)
  
  # Plot ACF sisaan
  acf(model$residuals)
  
  # Uji Ljung-Box
  print(Box.test(model$residuals, type = "Ljung-Box"))
  
  # Uji Kolmogorov-Smirnov
  print(ks.test(model$residuals, "pnorm", mean=mean(model$residuals), sd=sd(model$residuals)))
  
  # uji t-test
  print(t.test(model$residuals, mu = 0, alternative = "two.sided"))
}
step<-df[1:360,c("Kode")]
full<-df[1:360,c("Date","Close")]
pre<-df[1:300,c("Date","Close")]
post<-df[301:360,c("Date","Close")]
full2<-df[1:370,c("Date","Close")]
step

fullts<-ts(full$Close)
fullts2<-ts(full2$Close)
prets<-ts(pre$Close)
posts<-ts(post$Close)


par(mfrow=c(1,2))
acf(prets)
pacf(prets)
BoxCox.ar(prets)


preperiod<-c(1, 300)
postperiod<-c(301, 360)
impact<-CausalImpact(full$Close,pre.period = preperiod,post.period = postperiod)
impact
plot(impact)
summary(impact,"report")

out1<-tso(fullts)
plot(out1)

auto.arima(prets)

lambda1<-BoxCox.lambda(prets)
lambda1
prebc<-BoxCox(prets,BoxCox.lambda(prets))
adf.test(prets)
adf.test(prebc)
adf.test(diff(prebc,1))


m1<-Arima(prets,lambda =lambda1,order = c(1,1,0) )
m2<-Arima(prets,lambda =lambda1,order = c(1,1,1) )
m3<-Arima(prets,lambda =lambda1,order = c(2,1,1) )
m4<-Arima(prets,lambda =lambda1,order = c(1,1,2) )
m5<-Arima(prets,lambda =lambda1,order = c(3,1,1) )
m6<-Arima(prets,lambda =lambda1,order = c(1,1,3) )
m7<-Arima(prets,lambda =lambda1,order = c(2,1,2) )

coeftest(m1)
coeftest(m2)
coeftest(m3)
coeftest(m4)
coeftest(m5)
coeftest(m6)
coeftest(m7)


Diagnostik(m2)

m1
m2
m3
m4
m5
m6
m7
forecast(m2)
plot(forecast(m2))
par(mfrow=c(1,1))
accuracy(m1)
accuracy(m2)
accuracy(m3)
accuracy(m4)
accuracy(m5)
accuracy(m6)
accuracy(m7)

AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)
AIC(m5)
AIC(m6)
AIC(m7)

coeftest(m1)
coeftest(m2)
coeftest(m3)
coeftest(m4)
coeftest(m5)
coeftest(m6)
coeftest(m7)

m2

par(mfrow=c(1,2))
acf(diff(prebc,1))
pacf(diff(prebc,1))
eacf(diff(prebc,1))
     
Diagnostik(m1)
Diagnostik(m2)
Diagnostik(m3)
Diagnostik(m4)
Diagnostik(m5)
Diagnostik(m6)
Diagnostik(m7)

par(mfrow=c(1,1))
p1<-forecast(m2,h=30)
plot(p1)
predict(m2,n.ahead = 60)

p2<-forecast(m2,h=60)
plot(p2,ylim = c(2000,4200),col = "red",lwd=2)
lines(fullts,col="black",lty=2,lwd=2)
a<-c(1:360)
full
legend("bottomleft",                           # Add legend to plot
       c ("Arima (1,1,1)","Actual"),
       lwd=2,
       lty=2,
       col = c("red","black"))
o1<-outliers(c("AO"), c(317))
xb<-outliers.effects(o1, length(fullts))

fit1 <- arimax(fullts, order = c(1, 1, 1), 
               xtransf = data.frame(step),transfer = list(c(0, 0)),method = "ML",xreg = xb)
fit2 <- arimax(fullts, order = c(1, 1, 1), 
               xtransf = data.frame(step),transfer = list(c(0, 1)),method = "ML",xreg = xb)
fit3 <- arimax(fullts, order = c(1, 1, 1), 
               xtransf = data.frame(step),transfer = list(c(1, 0)),method = "ML",xreg = xb)

accuracy(fit1)
accuracy(fit2)
accuracy(fit3)

coeftest(fit1)
coeftest(fit2)
coeftest(fit3)



Diagnostik(fit1)

Diagnostik(fit3)

AIC(fit1)
AIC(fit2)
AIC(fit3)
fitted(fit1)
pintervensi<-predict(fit1,newxreg = xb,n.ahead = 10)
pintervensi$se
hasil<-c(2848.468,2848.468,2848.468,2848.468,
         2848.468,2848.468,2848.468,2848.468,2848.468,2848.468)
se2<-c(69.16697, 101.38337, 123.57207, 143.56720, 160.33242, 175.99317, 190.05197,203.34829, 215.68942, 227.45308)

hasil1<-c()
hasil2<-c()

for (i in 1:10) {
  hasil[i]=hasil[i]- 412
  hasil1[i]=hasil[i]+1.96*se2[i]
  hasil2[i]=hasil[i]-1.96*se2[i]
}


s1<-fitted(fit1)
s2<-fitted(fit1)
s3<-fitted(fit1)
k1<-as.ts(hasil)
k2<-as.ts(hasil1)
k3<-as.ts(hasil2)

plot(k1,lwd=2)
lines(k2,lwd=2)
lines(k3,lwd=2)
for (i in 1:10) {
  s1[i+360]==k1[i]
  s2[i+360]==k2[i]
  s3[i+360]==k3[i]
}


plot(s1,lwd=3)
plot(s2,lwd=3)
plot(s3,lwd=3)

par(mfrow=c(1,1))

fit1

plot(y=hasil,x=c(361:370),col="black",lty=2,lwd=2,xlim=c(200,370),ylim=c(0,4000))
lines(y=hasil1,x=c(361:370),col="black",lty=2,lwd=2)
lines(y=hasil2,x=c(361:370),col="black",lty=2,lwd=2)
lines(fitted(fit1),col="red",lty=2,lwd=2)
lines(fullts2,col="blue",lty=3,lwd=2)
legend("bottomleft",                           # Add legend to plot
       c ("Arima (1,1,1) Intervensi (0,12,0)","Actual"),
       lwd=2,
       lty=2,
       col = c("red","blue"))
     
