# Uji Signifikansi Parameter Model ARMAX Saham BMRI
library(readxl)
data <- read_excel('Return.xlsx')
BMRI <- data$BMRI_RT
X <- cbind(data$IHSG_RT,data$Kurs_Rupiah)
# Calon model (0,0),(1,1),(2,2)
#BMRI
library(rugarch)
spec.BMRI1=ugarchspec(mean.model=list(armaOrder=c(0,0),include.mean=TRUE,external.regressors=X),
                      variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BMRI1=ugarchfit(spec=spec.BMRI1,data=BMRI,solver="lbfgs")
spec.BMRI2=ugarchspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE,
                                      external.regressors=X),variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BMRI2=ugarchfit(spec=spec.BMRI2,data=BMRI,solver="lbfgs")
spec.BMRI3=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=TRUE,
                                      external.regressors=X),variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BMRI3=ugarchfit(spec=spec.BMRI3,data=BMRI,solver="lbfgs")
garch.fit.BMRI1
garch.fit.BMRI2
garch.fit.BMRI3
