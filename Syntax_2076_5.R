# Uji Signifikansi Parameter Model ARMAX Saham BBRI
library(readxl)
data <- read_excel('Return.xlsx')
BBRI <- data$BBRI_RT
X <- cbind(data$IHSG_RT,data$Kurs_Rupiah)
# Calon model (0,4),(1,1),(2,1)
#BBRI
library(rugarch)
spec.BBRI1=ugarchspec(mean.model=list(armaOrder=c(0,4),include.mean=TRUE,external.regressors=X),
                      variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BBRI1=ugarchfit(spec=spec.BBRI1,data=BBRI,solver="nlminb")
spec.BBRI2=ugarchspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE,
                                      external.regressors=X),variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BBRI2=ugarchfit(spec=spec.BBRI2,data=BBRI,solver="lbfgs")
spec.BBRI3=ugarchspec(mean.model=list(armaOrder=c(2,1),include.mean=TRUE,
                                      external.regressors=X),variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BBRI3=ugarchfit(spec=spec.BBRI3,data=BBRI,solver="lbfgs")
garch.fit.BBRI1
garch.fit.BBRI2
garch.fit.BBRI3
