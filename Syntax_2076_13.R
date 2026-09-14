# Estimasi VaR
library(tseries)
library(rugarch)
library(fGarch)
library(lmtest)
library(forecast)
library(readxl)
# VAR Saham BBCA
set.seed(123)
data <- read_excel('Return.xlsx')
returnBBCA <- data$BBCA_RT
X=cbind(data$IHSG_RT,data$Kurs_Rupiah)
library(tseries)
TBBCA = length(returnBBCA)
window = 500
tau = 0.05
z.alpha = qnorm(tau,0,1)
z.alpha2 = qnorm(1-tau,0,1)
library(rugarch)
Loss.garch=0
Prof.garch=0
VaR.garch=rep(0,TBBCA)
VaR.garch2=rep(0,TBBCA)
#RISIKO
for (i in window:(TBBCA-1)){
  print(i)
  bb=matrix(data$Kurs_Rupiah[(i-window+1):i])
  spec.BBCA=ugarchspec(mean.model=list(armaOrder=c(0,3),include.mean=FALSE,external.regressors=NULL),
                       variance.model=list(garchOrder=c(2,5),external.regressors=bb),
                       distribution.model="norm")
  modelgarch.fit.BBCA=ugarchfit(spec=spec.BBCA,data=returnBBCA[(i-window+1):i],solver="solnp")
  mean.garch = 0
  sd.garch = sigma(modelgarch.fit.BBCA)[window]
  VaR.garch[i+1] = mean.garch+(sd.garch*z.alpha)
  if(VaR.garch[i+1] > returnBBCA[i+1])
    Loss.garch=Loss.garch+1
}
var.garch.out=matrix(VaR.garch[(window+1):TBBCA],ncol=1)
ES.garch=Loss.garch/(TBBCA-window)
win.graph()
return.out=matrix(returnBBCA[(window+1):TBBCA],ncol=1)
VaR.garch.out = matrix(VaR.garch[(window+1):TBBCA],ncol=1)
plot(return.out,col="black",ylab="Return",xlab="Time")
t.garch = matrix(1:nrow(return.out))
dat.garch = matrix(c(t.garch,return.out),ncol=2)
dat.VaR.garch= matrix(c(t.garch, VaR.garch.out),ncol=2)
lines(VaR.garch.out,col="blue",lwd=2)
exceed.garch=matrix(dat.garch[dat.VaR.garch[,2]>dat.garch[,2]],ncol=2)
points(exceed.garch,col="red",cex=0.5,lwd=1,pch=19)
#PROFIT
for (i in window:(TBBCA-1)){
  print(i)
  bb=matrix(data$Kurs_Rupiah[(i-window+1):i])
  spec.BBCA=ugarchspec(mean.model=list(armaOrder=c(0,3),include.mean=FALSE,external.regressors=NULL),
                       variance.model=list(garchOrder=c(2,5),external.regressors=bb),
                       distribution.model="norm")
  modelgarch.fit.BBCA=ugarchfit(spec=spec.BBCA,data=returnBBCA[(i-window+1):i],solver="solnp")
  mean.garch2 = 0
  sd.garch2 = sigma(modelgarch.fit.BBCA)[window]
  VaR.garch2[i+1] = mean.garch2+(sd.garch2*z.alpha2)
  if(VaR.garch2[i+1] < returnBBCA[i+1])
    Prof.garch=Prof.garch+1
}
ES.garch2=Prof.garch/(TBBCA-window)
return.out2=matrix(returnBBCA[(window+1):TBBCA],ncol=1)
VaR.garch.out2=matrix(VaR.garch2[(window+1):TBBCA],ncol=1)
t.garch2 = matrix(1:nrow(return.out2))
dat.garch2 = matrix(c(t.garch2,return.out2),ncol=2)
dat.VaR.garch2 = matrix(c(t.garch2,VaR.garch.out2)
                        ,ncol=2)
lines(VaR.garch.out2,col="green3",lwd=2)
exceed.garch2=matrix(dat.garch2[dat.VaR.garch2[,2]<dat.garch2[,2]],ncol=2)
points(exceed.garch2,col="red",cex=0.5,lwd=1,pch=19)
#
a=cbind(VaR.garch.out,VaR.garch.out2)
#
write.csv(a,"C:/Users/JULI YANDI RAHMAN/Downloads/Kerjaan/Selesai/Project 2706/BBCA_01_b_500.csv")

# VaR Saham BBRI
returnBBRI=data$BBRI_RT
X=cbind(data$IHSG_RT,data$Kurs_Rupiah)
library(tseries)
TBBRI = length(returnBBRI)
window = 500
tau = 0.05
z.alpha = qnorm(tau,0,1)
z.alpha2 = qnorm(1-tau,0,1)
library(rugarch)
Loss.garch=0
Prof.garch=0
VaR.garch=rep(0,TBBRI)
VaR.garch2=rep(0,TBBRI)
#RISIKO
for (i in window:(TBBRI-1))
{
  print(i)
  bb=matrix(data$Kurs_Rupiah[(i-window+1):i])
  spec.BBRI=ugarchspec(mean.model=list(armaOrder=c(2,1),include.mean=FALSE,external.regressors=NULL),
                       variance.model=list(garchOrder=c(4,3),external.regressors=bb),
                       distribution.model="norm")
  modelgarch.fit.BBRI=ugarchfit(spec=spec.BBRI,data=returnBBRI[(i-window+1):i],solver="gosolnp")
  mean.garch = 0
  sd.garch = sigma(modelgarch.fit.BBRI)[window]
  VaR.garch[i+1] = mean.garch+(sd.garch*z.alpha)
  if(VaR.garch[i+1] > returnBBRI[i+1])
    Loss.garch=Loss.garch+1
}
var.garch.out=matrix(VaR.garch[(window+1):TBBRI],ncol=1)
ES.garch=Loss.garch/(TBBRI-window)
win.graph()
return.out=matrix(returnBBRI[(window+1):TBBRI],ncol=1)
VaR.garch.out =
  matrix(VaR.garch[(window+1):TBBRI],ncol=1)
plot(return.out,col="black",ylab=
       "Return",xlab="Time")
t.garch = matrix(1:nrow(return.out))
dat.garch = matrix(c(t.garch,return.out),ncol=2)
dat.VaR.garch= matrix(c(t.garch, VaR.garch.out),ncol=2)
lines(VaR.garch.out,col="blue",lwd=2)
exceed.garch=matrix(dat.garch[dat.VaR.garch[,2]>dat.garch[,2]],ncol=2)
points(exceed.garch,col="red",cex=0.5,lwd=1,pch=19)
#PROFIT
for (i in window:(TBBRI-1))
{
  print(i)
  bb=matrix(data$Kurs_Rupiah[(i-window+1):i])
  spec.BBRI=ugarchspec(mean.model=list(armaOrder=c(2,1),include.mean=FALSE,external.regressors=NULL),
                       variance.model=list(garchOrder=c(4,3),external.regressors=bb),
                       distribution.model="norm")
  modelgarch.fit.BBRI=ugarchfit(spec=spec.BBRI,data=returnBBRI[(i-window+1):i],solver="gosolnp")
  mean.garch2 = 0
  sd.garch2 = sigma(modelgarch.fit.BBRI)[window]
  VaR.garch2[i+1] = mean.garch2+(sd.garch2*z.alpha2)
  if(VaR.garch2[i+1] < returnBBRI[i+1])
    Prof.garch=Prof.garch+1
}
ES.garch2=Prof.garch/(TBBRI-window)
return.out2=matrix(returnBBRI[(window+1):TBBRI],ncol=1)
VaR.garch.out2=matrix(VaR.garch2[(window+1):TBBRI],ncol=
                        1)
t.garch2 = matrix(1:nrow(return.out2))
dat.garch2 = matrix(c(t.garch2,return.out2),ncol=2)
dat.VaR.garch2 = matrix(c(t.garch2,VaR.garch.out2)
                        ,ncol=2)
lines(VaR.garch.out2,col="green3",lwd=2)
exceed.garch2=matrix(dat.garch2[dat.VaR.garch2[,2]<dat.garch2[,2]],ncol=2)
points(exceed.garch2,col="red",cex=0.5,lwd=1,pch=19)
#
a=cbind(VaR.garch.out,VaR.garch.out2)
#
write.csv(a,"C:/Users/JULI YANDI RAHMAN/Downloads/Kerjaan/Selesai/Project 2706/BBRI_01_b_500.csv")

# VaR Saham BMRI
returnBMRI=data$BMRI_RT
X=cbind(data$IHSG_RT,data$Kurs_Rupiah)
library(tseries)
TBMRI = length(returnBMRI)
window = 500
tau = 0.05
z.alpha = qnorm(tau,0,1)
z.alpha2 = qnorm(1-tau,0,1)
library(rugarch)
Loss.garch=0
Prof.garch=0
VaR.garch=rep(0,TBMRI)
VaR.garch2=rep(0,TBMRI)
#RISIKO
for (i in window:(TBMRI-1))
{
  print(i)
  bb=matrix(data$Kurs_Rupiah[(i-window+1):i])
  spec.BMRI=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=FALSE,external.regressors=NULL),
                       variance.model=list(garchOrder=c(2,4),external.regressors=bb),
                       distribution.model="norm")
  modelgarch.fit.BMRI=ugarchfit(spec=spec.BMRI,data=returnBMRI[(i-window+1):i],solver="gosolnp")
  mean.garch = 0
  sd.garch = sigma(modelgarch.fit.BMRI)[window]
  VaR.garch[i+1] = mean.garch+(sd.garch*z.alpha)
  if(VaR.garch[i+1] > returnBMRI[i+1])
    Loss.garch=Loss.garch+1
}
var.garch.out=matrix(VaR.garch[(window+1):TBMRI],ncol=1)
ES.garch=Loss.garch/(TBMRI-window)
win.graph()
return.out=matrix(returnBMRI[(window+1):TBMRI],ncol=1)
VaR.garch.out =
  matrix(VaR.garch[(window+1):TBMRI],ncol=1)
plot(return.out,col="black",ylab=
       "Return",xlab="Time")
t.garch = matrix(1:nrow(return.out))
dat.garch = matrix(c(t.garch,return.out),ncol=2)
dat.VaR.garch= matrix(c(t.garch, VaR.garch.out),ncol=2)
lines(VaR.garch.out,col="blue",lwd=2)
exceed.garch=matrix(dat.garch[dat.VaR.garch[,2]>dat.garch[,2]],ncol=2)
points(exceed.garch,col="red",cex=0.5,lwd=1,pch=19)
#PROFIT
for (i in window:(TBMRI-1))
{
  print(i)
  bb=matrix(data$Kurs_Rupiah[(i-window+1):i])
  spec.BMRI=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=FALSE,external.regressors=NULL),
                       variance.model=list(garchOrder=c(2,4),external.regressors=bb),
                       distribution.model="norm")
  modelgarch.fit.BMRI=ugarchfit(spec=spec.BMRI,data=returnBMRI[(i-window+1):i],solver="gosolnp")
  mean.garch2 = 0
  sd.garch2 = sigma(modelgarch.fit.BMRI)[window]
  VaR.garch2[i+1] = mean.garch2+(sd.garch2*z.alpha2)
  if(VaR.garch2[i+1] < returnBMRI[i+1])
    Prof.garch=Prof.garch+1
}
ES.garch2=Prof.garch/(TBMRI-window)
return.out2=matrix(returnBMRI[(window+1):TBMRI],ncol=1)
VaR.garch.out2=matrix(VaR.garch2[(window+1):TBMRI],ncol=
                        1)
t.garch2 = matrix(1:nrow(return.out2))
dat.garch2 = matrix(c(t.garch2,return.out2),ncol=2)
dat.VaR.garch2 = matrix(c(t.garch2,VaR.garch.out2)
                        ,ncol=2)
lines(VaR.garch.out2,col="green3",lwd=2)
exceed.garch2=matrix(dat.garch2[dat.VaR.garch2[,2]<dat.garch2[,2]],ncol=2)
points(exceed.garch2,col="red",cex=0.5,lwd=1,pch=19)
#
a=cbind(VaR.garch.out,VaR.garch.out2)
#
write.csv(a,"C:/Users/JULI YANDI RAHMAN/Downloads/Kerjaan/Selesai/Project 2706/BMRI_01_b_500.csv")

# VaR Saham BBNI
returnBBNI=data$BBNI_RT
X=cbind(data$IHSG_RT,data$Kurs_Rupiah)
library(tseries)
TBBNI = length(returnBBNI)
window = 500
tau = 0.05
z.alpha = qnorm(tau,0,1)
z.alpha2 = qnorm(1-tau,0,1)
library(rugarch)
Loss.garch=0
Prof.garch=0
VaR.garch=rep(0,TBBNI)
VaR.garch2=rep(0,TBBNI)
#RISIKO
for (i in window:(TBBNI-1))
{
  print(i)
  bb=matrix(data$Kurs_Rupiah[(i-window+1):i])
  spec.BBNI=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=FALSE,external.regressors=NULL),
                       variance.model=list(garchOrder=c(3,6),external.regressors=bb),
                       distribution.model="norm")
  modelgarch.fit.BBNI=ugarchfit(spec=spec.BBNI,data=returnBBNI[(i-window+1):i],solver="solnp")
  mean.garch = 0
  sd.garch = sigma(modelgarch.fit.BBNI)[window]
  VaR.garch[i+1] = mean.garch+(sd.garch*z.alpha)
  if(VaR.garch[i+1] > returnBBNI[i+1])
    Loss.garch=Loss.garch+1
}
var.garch.out=matrix(VaR.garch[(window+1):TBBNI],ncol=1)
ES.garch=Loss.garch/(TBBNI-window)
win.graph()
return.out=matrix(returnBBNI[(window+1):TBBNI],ncol=1)
VaR.garch.out =
  matrix(VaR.garch[(window+1):TBBNI],ncol=1)
plot(return.out,col="black",ylab=
       "Return",xlab="Time")
t.garch = matrix(1:nrow(return.out))
dat.garch = matrix(c(t.garch,return.out),ncol=2)
dat.VaR.garch= matrix(c(t.garch, VaR.garch.out),ncol=2)
lines(VaR.garch.out,col="blue",lwd=2)
exceed.garch=matrix(dat.garch[dat.VaR.garch[,2]>dat.garch[,2]],ncol=2)
points(exceed.garch,col="red",cex=0.5,lwd=1,pch=19)
#PROFIT
for (i in window:(TBBNI-1))
{
  print(i)
  bb=matrix(data$Kurs_Rupiah[(i-window+1):i])
  spec.BBNI=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=FALSE,external.regressors=NULL),
                       variance.model=list(garchOrder=c(3,6),external.regressors=bb),
                       distribution.model="norm")
  modelgarch.fit.BBNI=ugarchfit(spec=spec.BBNI,data=returnBBNI[(i-window+1):i],solver="solnp")
  mean.garch2 = 0
  sd.garch2 = sigma(modelgarch.fit.BBNI)[window]
  VaR.garch2[i+1] = mean.garch2+(sd.garch2*z.alpha2)
  if(VaR.garch2[i+1] < returnBBNI[i+1])
    Prof.garch=Prof.garch+1
}
ES.garch2=Prof.garch/(TBBNI-window)
return.out2=matrix(returnBBNI[(window+1):TBBNI],ncol=1)
VaR.garch.out2=matrix(VaR.garch2[(window+1):TBBNI],ncol=
                        1)
t.garch2 = matrix(1:nrow(return.out2))
dat.garch2 = matrix(c(t.garch2,return.out2),ncol=2)
dat.VaR.garch2 = matrix(c(t.garch2,VaR.garch.out2)
                        ,ncol=2)
lines(VaR.garch.out2,col="green3",lwd=2)
exceed.garch2=matrix(dat.garch2[dat.VaR.garch2[,2]<dat.garch2[,2]],ncol=2)
points(exceed.garch2,col="red",cex=0.5,lwd=1,pch=19)
#
a=cbind(VaR.garch.out,VaR.garch.out2)
#
write.csv(a,"C:/Users/JULI YANDI RAHMAN/Downloads/Kerjaan/Selesai/Project 2706/BBNI_01_b_500.csv")


