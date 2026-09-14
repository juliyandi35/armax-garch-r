# Uji Signifikansi Parameter Model ARMAX Saham BBCA
library(readxl)
data <- read_excel('Return.xlsx')
BBCA <- data$BBCA_RT
X <- cbind(data$IHSG_RT,data$Kurs_Rupiah)
# Calon model (0,3),(1,3),(0,4)
#BBCA
library(rugarch)
spec.BBCA1=ugarchspec(mean.model=list(armaOrder=c(0,3),include.mean=TRUE,external.regressors=X),
                      variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BBCA1=ugarchfit(spec=spec.BBCA1,data=BBCA,solver="lbfgs")
spec.BBCA2=ugarchspec(mean.model=list(armaOrder=c(1,3),include.mean=TRUE,
                                      external.regressors=X),variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BBCA2=ugarchfit(spec=spec.BBCA2,data=BBCA,solver="lbfgs")
spec.BBCA3=ugarchspec(mean.model=list(armaOrder=c(0,4),include.mean=TRUE,
                                      external.regressors=X),variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.BBCA3=ugarchfit(spec=spec.BBCA3,data=BBCA,solver="lbfgs")
garch.fit.BBCA1
garch.fit.BBCA2
garch.fit.BBCA3
