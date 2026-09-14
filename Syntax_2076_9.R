# Plot ACF dan PACF Residual Kuadrat Model ARMAX
#Saham BBCA ARMAX(0,0,2)
resBBCA=resBBCA1
returnkuadrat1=resBBCA*resBBCA
# win.graph()
par(mfrow=c(2,1))
acf(returnkuadrat1,main="BBCA")
pacf(returnkuadrat1,main="BBCA")
eacf(returnkuadrat1)
# Calon model (0,5),(1,5),(2,5)
#Saham BBRI ARMAX(2,0,1)
resBBRI=resBBRI3
returnkuadrat2=resBBRI*resBBRI
# win.graph()
par(mfrow=c(2,1))
acf(returnkuadrat2,main="BBRI")
pacf(returnkuadrat2,main="BBRI")
eacf(returnkuadrat2)
# Calon model (2,3),(3,3),(4,3)
#Saham BMRI ARMAX(2,0,2)
resBMRI=resBMRI3
returnkuadrat3=resBMRI*resBMRI
# win.graph()
par(mfrow=c(2,1))
acf(returnkuadrat3,main="BMRI")
pacf(returnkuadrat3,main="BMRI")
eacf(returnkuadrat3)
library(forecast)
auto.arima(returnkuadrat3)
# Calon model (2,2),(3,2),(4,5)
#Saham BBNI ARMAX(2,0,2)
resBBNI=resBBNI3
returnkuadrat4=resBBNI*resBBNI
# win.graph()
par(mfrow=c(2,1))
acf(returnkuadrat4,main="BBNI")
pacf(returnkuadrat4,main="BBNI")
eacf(returnkuadrat4)
# Calon model (1,6),(2,5),(3,6)
