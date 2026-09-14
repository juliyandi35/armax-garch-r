library(readxl)
data <- read_excel('Return.xlsx')
IHSG <- data$IHSG_RT
X <- cbind(data$IHSG_RT,data$Kurs_Rupiah)

#IHSG
library(rugarch)
spec.IHSG1=ugarchspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE,external.regressors=X),
                      variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.IHSG1=ugarchfit(spec=spec.IHSG1,data=IHSG,solver="lbfgs")
spec.IHSG2=ugarchspec(mean.model=list(armaOrder=c(1,0),include.mean=TRUE,
                                      external.regressors=X),variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.IHSG2=ugarchfit(spec=spec.IHSG2,data=IHSG,solver="lbfgs")
spec.IHSG3=ugarchspec(mean.model=list(armaOrder=c(0,1),include.mean=TRUE,
                                      external.regressors=X),variance.model=list(garchOrder=c(0,0)),distribution.model="norm")
garch.fit.IHSG3=ugarchfit(spec=spec.IHSG3,data=IHSG,solver="lbfgs")
garch.fit.IHSG1
garch.fit.IHSG2
garch.fit.IHSG3
