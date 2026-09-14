# Uji Signifikansi Parameter Model ARMAX Saham BBNI
library(readxl)
data <- read_excel('Return.xlsx')
BBNI <- data$BBNI_RT
X <- cbind(data$IHSG_RT,data$Kurs_Rupiah)
# Calon model (0,1),(1,1),(2,2)
#BBNI
library(rugarch)
spec.BBNI1=ugarchspec(mean.model=list(armaOrder=c(0,1),include.mean=TRUE,external.regressors=X),
                      variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BBNI1=ugarchfit(spec=spec.BBNI1,data=BBNI,solver="lbfgs")
spec.BBNI2=ugarchspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE,
                                      external.regressors=X),variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BBNI2=ugarchfit(spec=spec.BBNI2,data=BBNI,solver="lbfgs")
spec.BBNI3=ugarchspec(mean.model=list(armaOrder=c(2,2),include.mean=TRUE,
                                      external.regressors=X),variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BBNI3=ugarchfit(spec=spec.BBNI3,data=BBNI,solver="lbfgs")
garch.fit.BBNI1
garch.fit.BBNI2
garch.fit.BBNI3
