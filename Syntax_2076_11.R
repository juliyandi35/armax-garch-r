# Estimasi Parameter Model ARMAX-GARCHX Terbaik pada kelima Saham
returnBBCA=data$BBCA_RT
returnBBRI=data$BBRI_RT
returnBMRI=data$BMRI_RT
returnBBNI=data$BBNI_RT
a=matrix(data$IHSG_RT)
b=matrix(data$Kurs_Rupiah)
X=cbind(a,b)
#BBCA
library(rugarch)
spec.BBCA3=ugarchspec(mean.model=list(armaOrder=c(0,3),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(2,5),external.regressors=X),distribution.model="norm")
garch.fit.BBCA3=ugarchfit(spec=spec.BBCA3,data=returnBBCA,solver="solnp")
garch.fit.BBCA3

#BBCA 3 percobaan di window 250
window = 250
for (i in window:(window+4)){
  returnBBCA1=data$BBCA_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BBCA3=ugarchspec(mean.model=list(armaOrder=c(0,3),include.mean=F, external.regressors=NULL),
                        variance.model=list(garchOrder=c(2,5),external.regressors=X),distribution.model="norm")
  garch.fit.BBCA3=ugarchfit(spec=spec.BBCA3,data=returnBBCA[(i-window+1):i],solver="solnp")
  print(garch.fit.BBCA3)
}
#BBCA 3 percobaan di window 375
window = 375
for (i in window:(window+4)){
  returnBBCA1=data$BBCA_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BBCA3=ugarchspec(mean.model=list(armaOrder=c(0,3),include.mean=F, external.regressors=NULL),
                        variance.model=list(garchOrder=c(2,5),external.regressors=X),distribution.model="norm")
  garch.fit.BBCA3=ugarchfit(spec=spec.BBCA3,data=returnBBCA[(i-window+1):i],solver="solnp")
  print(garch.fit.BBCA3)
}
#BBCA 3 percobaan di window 500
window = 500
for (i in window:(window+4)){
  returnBBCA1=data$BBCA_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BBCA3=ugarchspec(mean.model=list(armaOrder=c(0,3),include.mean=F, external.regressors=NULL),
                        variance.model=list(garchOrder=c(2,5),external.regressors=X),distribution.model="norm")
  garch.fit.BBCA3=ugarchfit(spec=spec.BBCA3,data=returnBBCA[(i-window+1):i],solver="solnp")
  print(garch.fit.BBCA3)
}

#BBRI
spec.BBRI3=ugarchspec(mean.model=list(armaOrder=c(2,1),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(4,3),external.regressors=X),distribution.model="norm")
garch.fit.BBRI3=ugarchfit(spec=spec.BBRI3,data=returnBBRI,solver="gosolnp")
garch.fit.BBRI3

#BBRI 3 percobaan di window 250
window = 250
for (i in window:(window+4)){
  returnBBRI1=data$BBRI_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BBRI3=ugarchspec(mean.model=list(armaOrder=c(2,1),include.mean=F, external.regressors=NULL),
                        variance.model=list(garchOrder=c(4,3),external.regressors=X),distribution.model="norm")
  garch.fit.BBRI3=ugarchfit(spec=spec.BBRI3,data=returnBBRI[(i-window+1):i],solver="gosolnp")
  print(garch.fit.BBRI3)
}
#BBRI 3 percobaan di window 375
window = 375
for (i in window:(window+4)){
  returnBBRI1=data$BBRI_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BBRI3=ugarchspec(mean.model=list(armaOrder=c(2,1),include.mean=F, external.regressors=NULL),
                        variance.model=list(garchOrder=c(4,3),external.regressors=X),distribution.model="norm")
  garch.fit.BBRI3=ugarchfit(spec=spec.BBRI3,data=returnBBRI[(i-window+1):i],solver="gosolnp")
  print(garch.fit.BBRI3)
}
#BBRI 3 percobaan di window 500
window = 500
for (i in window:(window+4)){
  returnBBRI1=data$BBRI_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BBRI3=ugarchspec(mean.model=list(armaOrder=c(2,1),include.mean=F, external.regressors=NULL),
                        variance.model=list(garchOrder=c(4,3),external.regressors=X),distribution.model="norm")
  garch.fit.BBRI3=ugarchfit(spec=spec.BBRI3,data=returnBBRI[(i-window+1):i],solver="gosolnp")
  print(garch.fit.BBRI3)
}

#BMRI
library(rugarch)
spec.BMRI1=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(2,4),external.regressors=X),distribution.model="norm")
garch.fit.BMRI1=ugarchfit(spec=spec.BMRI1,data=returnBMRI,solver="nlminb")
garch.fit.BMRI1

#BMRI 3 percobaan di window 250
window = 250
for (i in window:(window+4)){
  returnBMRI1=data$BMRI_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BMRI1=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=F, external.regressors=NULL),
                        variance.model=list(garchOrder=c(2,4),external.regressors=X),distribution.model="norm")
  garch.fit.BMRI1=ugarchfit(spec=spec.BMRI1,data=returnBMRI[(i-window+1):i],solver="nlminb")
  print(garch.fit.BMRI1)
}
#BMRI 3 percobaan di window 375
window = 375
for (i in window:(window+4)){
  returnBMRI1=data$BMRI_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BMRI1=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=F, external.regressors=NULL),
                        variance.model=list(garchOrder=c(2,4),external.regressors=X),distribution.model="norm")
  garch.fit.BMRI1=ugarchfit(spec=spec.BMRI1,data=returnBMRI[(i-window+1):i],solver="nlminb")
  print(garch.fit.BMRI1)
}
#BMRI 3 percobaan di window 500
window = 500
for (i in window:(window+4)){
  returnBMRI1=data$BMRI_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BMRI1=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=F, external.regressors=NULL),
                        variance.model=list(garchOrder=c(2,4),external.regressors=X),distribution.model="norm")
  garch.fit.BMRI1=ugarchfit(spec=spec.BMRI1,data=returnBMRI[(i-window+1):i],solver="nlminb")
  print(garch.fit.BMRI1)
}


#BBNI
library(rugarch)
spec.BBNI1=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(1,6),external.regressors=X),distribution.model="norm")
garch.fit.BBNI1=ugarchfit(spec=spec.BBNI1,data=returnBBNI,solver="solnp")
garch.fit.BBNI1
#BBNI 3 percobaan di window 250
window = 250
for (i in window:(window+4)){
  returnBBNI1=data$BBNI_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BBNI1=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=FALSE,
                                       external.regressors=NULL),variance.model=list(garchOrder=c(1,6),external.regressors=X),distribution.model="norm"
  )
  garch.fit.BBNI1=ugarchfit(spec=spec.BBNI1,data=returnBBNI[(i-window+1):i],solver="solnp")
  print(garch.fit.BBNI1)
}
#BBNI 3 percobaan di window 375
window = 375
for (i in window:(window+4)){
  returnBBNI1=data$BBNI_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BBNI1=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=FALSE,
                                        external.regressors=NULL),variance.model=list(garchOrder=c(1,6),external.regressors=X),distribution.model="norm"
  )
  garch.fit.BBNI1=ugarchfit(spec=spec.BBNI1,data=returnBBNI[(i-window+1):i],solver="solnp")
  print(garch.fit.BBNI1)
}
#BBNI 3 percobaan di window 500
window = 500
for (i in window:(window+4)){
  returnBBNI1=data$BBNI_RT
  X <- cbind(data$IHSG_RT[(i-window+1):i],data$Kurs_Rupiah[(i-window+1):i])
  spec.BBNI1=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=FALSE,
                                        external.regressors=NULL),variance.model=list(garchOrder=c(1,6),external.regressors=X),distribution.model="norm"
  )
  garch.fit.BBNI1=ugarchfit(spec=spec.BBNI1,data=returnBBNI[(i-window+1):i],solver="solnp")
  print(garch.fit.BBNI1)
}
