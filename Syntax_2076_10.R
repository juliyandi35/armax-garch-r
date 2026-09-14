# Estimasi Parameter Model ARMAX(0,0,3)-GARCHX(0,0,5), ARMAX(0,0,3)-GARCHX (1,0,5), dan ARMAX(0,0,3)-GARCHX(2,0,5) Saham BBCA
data <- read_excel("Return.xlsx")
returnBBCA=data$BBCA_RT
# Calon model (0,3)
# Calon model (0,5),(1,5),(2,5)
#BBCA
library(rugarch)
spec.BBCA1=ugarchspec(mean.model=list(armaOrder=c(0,3),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(0,5),external.regressors=X),distribution.model="norm")
garch.fit.BBCA1=ugarchfit(spec=spec.BBCA1,data=returnBBCA,solver="solnp")
spec.BBCA2=ugarchspec(mean.model=list(armaOrder=c(0,3),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(1,5),external.regressors=X),distribution.model="norm")
garch.fit.BBCA2=ugarchfit(spec=spec.BBCA2,data=returnBBCA,solver="solnp")
spec.BBCA3=ugarchspec(mean.model=list(armaOrder=c(0,3),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(2,5),external.regressors=X),distribution.model="norm")
garch.fit.BBCA3=ugarchfit(spec=spec.BBCA3,data=returnBBCA,solver="solnp")
garch.fit.BBCA1
garch.fit.BBCA2
garch.fit.BBCA3

# Estimasi Parameter Model ARMAX(2,0,1)-GARCHX(2,0,3), ARMAX(2,0,1)-GARCHX (3,0,3), dan ARMAX(2,0,1)-GARCHX(4,0,3) Saham BBRI
returnBBRI=data$BBRI_RT
# Calon model (2,1)
# Calon model (2,3),(3,3),(4,3)
#BBRI
library(rugarch)
spec.BBRI1=ugarchspec(mean.model=list(armaOrder=c(2,1),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(2,3),external.regressors=X),distribution.model="norm")
garch.fit.BBRI1=ugarchfit(spec=spec.BBRI1,data=returnBBRI,solver="gosolnp")
spec.BBRI2=ugarchspec(mean.model=list(armaOrder=c(2,1),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(3,3),external.regressors=X),distribution.model="norm")
garch.fit.BBRI2=ugarchfit(spec=spec.BBRI2,data=returnBBRI,solver="gosolnp")
spec.BBRI3=ugarchspec(mean.model=list(armaOrder=c(2,1),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(4,3),external.regressors=X),distribution.model="norm")
garch.fit.BBRI3=ugarchfit(spec=spec.BBRI3,data=returnBBRI,solver="gosolnp")
garch.fit.BBRI1
garch.fit.BBRI2
garch.fit.BBRI3

# Estimasi Parameter Model ARMAX(2,0,2)-GARCHX(2,0,4), ARMAX(2,0,2)-GARCHX (3,0,5), dan ARMAX(2,0,2)-GARCHX(4,0,5) Saham BMRI
returnBMRI=data$BMRI_RT
# Calon model (2,2)
# Calon model (2,2),(1,4),(1,2)
#BMRI
library(rugarch)
spec.BMRI1=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(2,2),external.regressors=X),distribution.model="norm")
garch.fit.BMRI1=ugarchfit(spec=spec.BMRI1,data=returnBMRI,solver="gosolnp")
spec.BMRI2=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(1,4),external.regressors=X),distribution.model="norm")
garch.fit.BMRI2=ugarchfit(spec=spec.BMRI2,data=returnBMRI,solver="nlminb")
spec.BMRI3=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(1,2),external.regressors=X),distribution.model="norm")
garch.fit.BMRI3=ugarchfit(spec=spec.BMRI3,data=returnBMRI,solver="nlminb")

garch.fit.BMRI1
garch.fit.BMRI2
garch.fit.BMRI3

# Estimasi Parameter Model ARMAX(1,0,2)-GARCHX(1,1,2), ARMAX(1,0,2)-GARCHX (1,0,2), dan ARMAX(1,0,2)-GARCHX(0,1,2) Saham BBNI
returnBBNI=data$BBNI_RT
# Calon model (2,2)
# Calon model (1,6),(2,5),(3,6)
#BBNI
library(rugarch)
spec.BBNI1=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(1,6),external.regressors=X),distribution.model="norm")
garch.fit.BBNI1=ugarchfit(spec=spec.BBNI1,data=returnBBNI,solver="solnp")
spec.BBNI2=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(2,5),external.regressors=X),distribution.model="norm")
garch.fit.BBNI2=ugarchfit(spec=spec.BBNI2,data=returnBBNI,solver="solnp")
spec.BBNI3=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=TRUE, external.regressors=X),
                      variance.model=list(garchOrder=c(3,6),external.regressors=X),distribution.model="norm")
garch.fit.BBNI3=ugarchfit(spec=spec.BBNI3,data=returnBBNI,solver="nlminb")
garch.fit.BBNI1
garch.fit.BBNI2
garch.fit.BBNI3
