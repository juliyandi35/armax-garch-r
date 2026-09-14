# Plot Time Series Harga Saham dan Return Saham Perusahaan, Kurs IDR/USD, dan IHSG
library(readxl)

# Read the data from the Excel file
data <- read_excel("Return.xlsx")

# Extract the variables
Date <- as.Date(data$Tanggal)
BBCA <- as.numeric(data$BBCA)
BBCA2 <- data.frame(Date,BBCA)
BBRI <- as.numeric(data$BBRI)
BBRI2 <- data.frame(Date,BBRI)
BMRI <- as.numeric(data$BMRI)
BMRI2 <- data.frame(Date,BMRI)
BBNI <- as.numeric(data$BBNI)
BBNI2 <- data.frame(Date,BBNI)
IHSG <- as.numeric(data$IHSG)
IHSG2 <- data.frame(Date,IHSG)
RTBBCA <- as.numeric(data$BBCA_RT)
RTBBCA2 <- data.frame(Date,RTBBCA)
RTBBRI <- as.numeric(data$BBRI_RT)
RTBBRI2 <- data.frame(Date,RTBBRI)
RTBMRI <- as.numeric(data$BMRI_RT)
RTBMRI2 <- data.frame(Date,RTBMRI)
RTBBNI <- as.numeric(data$BBNI_RT)
RTBBNI2 <- data.frame(Date,RTBBNI)
RTIHSG <- as.numeric(data$IHSG_RT)
RTIHSG2 <- data.frame(Date,RTIHSG)
Kurs <- data$Kurs_Rupiah
Kurs2 <- data.frame(Date,Kurs)

#Plot Stock price Setiap Perusahaan
plot(BBCA2,ylim=c(0,10000),type="l",lwd=2, col="blue",ylab="Stock Price", xlab="Time",main="Commpany's Stock Prices")
lines(BBRI2,type="l",ylim=c(0,8500),lwd=2,col="red")
lines(BBNI2,type="l",ylim=c(0,8500),lwd=2,col="yellow")
lines(BMRI2,type="l",ylim=c(0,8500),lwd=2,col="black")
lines(IHSG2,type="l",ylim=c(0,8500),lwd=2,col="green")
legend("topright",c("BBCA","BBRI","BBNI","BMRI","IHSG"),text.width = strwidth("0.5,1,1"),lwd=2,col=c("blue","red","yellow","black","green"),bg="white")

#Plot Return BBCA
par(mfrow=c(1,1))
plot(RTBBCA2, type="l", col="black", ylab="Stock Return", xlab="Time",main="BBCA Stock Return")
legend("topright",c("Return BBCA"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")

#Plot Return BBRI
plot(RTBBRI2, type="l", col="black", ylab="Stock Return", xlab="Time",main="BBRI Stock Return")
legend("topright",c("Return BBRI"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")

#Plot Return BBNI
plot(RTBBNI2, type="l", col="black", ylab="Stock Return", xlab="Time",main="BBNI Stock Return")
legend("topright",c("Return BBNI"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")

#Plot Return BMRI
plot(RTBMRI2, type="l", col="black", ylab="Stock Return", xlab="Time",main="BMRI Stock Return")
legend("topright",c("Return BMRI"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")

#Plot IHSG
plot(RTIHSG2, type="l", col="black", ylab="Stock Return", xlab="Time",main="IHSG Stock Return")
legend("topright",c("Return IHSG"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")

#Plot Kurs
plot(Kurs2, type="l", col="black", ylab="Kurs Rupiah", xlab="Time",main="Kurs Rupiah Terhadap Dollar")
legend("topright",c("Kurs Rupiah"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")
