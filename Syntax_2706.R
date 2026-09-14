# Load required packages
library(tseries)
library(rugarch)
library(fGarch)
library(lmtest)
library(forecast)
library(readxl)
library(quantreg)
library(TSA)
# Step 1: Descriptive analysis and time series plot
# Generate example data
set.seed(123)
data <- read_excel('Return.xlsx')
BBCA <- data$BBCA_RT
BBRI <- data$BBRI_RT
BMRI <- data$BMRI_RT
BBNI <- data$BBNI_RT
IHSG <- data$IHSG_RT

# Create a data frame with return data
returns <- data.frame(BBCA, BBRI, BMRI, BBNI, IHSG)

# Descriptive analysis
summary(returns)

# Plot Time Series Harga Saham dan Return Saham Perusahaan, Kurs IDR/USD, dan IHSG
library(readxl)

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

#Plot Stock price Setiap Perusahaan
plot(BBCA2,ylim=c(0,10000),type="l",lwd=2, col="blue",ylab="Stock Price", xlab="Time",main="Company's Stock Prices")
lines(BBRI2,type="l",ylim=c(0,8500),lwd=2,col="red")
lines(BBNI2,type="l",ylim=c(0,8500),lwd=2,col="yellow")
lines(BMRI2,type="l",ylim=c(0,8500),lwd=2,col="black")
lines(IHSG2,type="l",ylim=c(0,8500),lwd=2,col="green")
legend("topright",c("BBCA","BBRI","BBNI","BMRI","IHSG"),text.width = strwidth("0.5,1,1"),lwd=2,col=c("blue","red","yellow","black","green"),bg="white")

#Plot Return BBCA
par(mfrow=c(1,1))
plot(RTBBCA2, type="l", ylim=c(-1000,1000), lwd=2, col="black", ylab="Stock Return", xlab="Time",main="BBCA Stock Return")
legend("topright",c("Return BBCA"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")

#Plot Return BBRI
plot(RTBBRI2, type="l", ylim=c(-1000,1000), lwd=2, col="black", ylab="Stock Return", xlab="Time",main="BBRI Stock Return")
legend("topright",c("Return BBRI"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")

#Plot Return BBNI
plot(RTBBNI2, type="l", ylim=c(-1000,1000), lwd=2, col="black", ylab="Stock Return", xlab="Time",main="BBNI Stock Return")
legend("topright",c("Return BBNI"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")

#Plot Return BMRI
plot(RTBMRI2, type="l", ylim=c(-1000,1000), lwd=2, col="black", ylab="Stock Return", xlab="Time",main="BMRI Stock Return")
legend("topright",c("Return BMRI"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")

#Plot IHSG
plot(RTIHSG2, type="l", ylim=c(-1000,1000), lwd=2, col="black", ylab="Stock Return", xlab="Time",main="IHSG Stock Return")
legend("topright",c("Return IHSG"),lwd=0.5,cex= 1.2,pt.cex = cex,col=c("black"),bg="white")

# Plot ACF dan PACF Return Saham
win.graph()
par(mfrow=c(2,1))
acf(RTBBCA,main="Return BBCA")
pacf(RTBBCA,main="Return BBCA")
win.graph()
par(mfrow=c(2,1))
acf(RTBBRI,main="Return BBRI")
pacf(RTBBRI,main="Return BBRI")
win.graph()
par(mfrow=c(2,1))
acf(RTBBNI,main="Return BBNI")
pacf(RTBBNI,main="Return BBNI")
win.graph()
par(mfrow=c(2,1))
acf(RTBMRI,main="Return BMRI")
pacf(RTBMRI,main="Return BMRI")
win.graph()
par(mfrow=c(2,1))
acf(RTIHSG,main="Return IHSG")
pacf(RTIHSG,main="Return IHSG")

eacf(RTIHSG)
# Calon model yang didapat adalah (0,0,3),(1,0,3),(2,0,3),(3,0,3),(4,0,3)
arima003 <- arima(RTIHSG2,xreg=X.train,order=c(2,0,3),method = "ML")
arima103 <- arima(RTIHSG2,xreg=X.train,order=c(3,0,3),method = "ML")
arima203 <- arima(RTIHSG2,xreg=X.train,order=c(4,0,3),method = "ML")
arima303 <- arima(RTIHSG2,xreg=X.train,order=c(5,0,3),method = "ML")
arima403 <- arima(RTIHSG2,xreg=X.train,order=c(5,0,3),method = "ML")

# Step 2: Window intervals
window_intervals <- c(250, 375, 500)

# Initialize variables for the best model
best_model <- NULL
best_garchx_model_fit <- NULL
best_VaR <- NULL
best_backtest <- Inf

# Initialize a list to store significant test results
significant_tests <- list()

# Initialize a list to store the squared residuals
squared_residuals <- list()

# Initialize lists to store the results
VaR_results <- list()
risk_results <- list()
profit_results <- list()

# Steps 3-7: ARMAX model estimation and selection
for (window in window_intervals) {
  # Select a subset of data for the current window
  subset_data <- returns[(nrow(returns) - window + 1):nrow(returns), ]

  for (i in 1:5) {
    # Fit ARMAX model
    xreg <- as.matrix(subset_data[, -i])
    armax_model <- auto.arima(subset_data[, i], xreg = xreg, seasonal = FALSE,stepwise=FALSE,approximation=FALSE)
    residuals <- residuals(armax_model)
    squared_residuals[[paste0("Model_", window, "_", i)]] <- residuals^2

    # Step 5: Significance test
    coef_test <- coeftest(armax_model)
    white_test <- Box.test(armax_model$residuals, lag = 20, type = "Ljung-Box")
    normality_test <- jarque.bera.test(armax_model$residuals)

    # Store the significant test results in the list
    test_results <- list(
      coef_test = coef_test,
      white_test = white_test,
      normality_test = normality_test
    )

    # Append the test results to the list
    significant_tests[[paste0("Model_", window, "_", i)]] <- test_results
  }

  # Step 7: Choose best ARMAX model by AIC
  best_aic <- Inf

  for (i in 1:5) {
    xreg <- as.matrix(subset_data[, -i])
    armax_model <- auto.arima(subset_data[, i], xreg = xreg, seasonal = FALSE)

    if (AIC(armax_model) < best_aic) {
      best_model <- armax_model
      best_aic <- AIC(armax_model)
    }
  }

  # Steps 8-9: Residuals test and GARCHX model identification
  residuals <- residuals(best_model)
  arch_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")

  garchx_model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
                             distribution.model = "std")

  # Step 11: Significance test for GARCHX model parameters
  garchx_model_fit <- ugarchfit(spec = garchx_model, data = subset_data[, i])

  # Step 12: VaR calculation
  VaR_forecast <- ugarchforecast(garchx_model_fit, n.ahead = 1, n.roll = 0, data = residuals)
  VaR <- as.numeric(sigma(VaR_forecast)) * qnorm(0.05)

  # Step 14: VaR model accuracy with backtesting
  backtest <- sum(residuals < -VaR) / length(residuals)

  # Calculate Risk and Profit
  risk <- sum(residuals[residuals < -VaR])
  profit <- sum(residuals[residuals >= -VaR])

  # Store the results
  VaR_results[[paste0("Model_", window, "_", i)]] <- VaR
  risk_results[[paste0("Model_", window, "_", i)]] <- risk
  profit_results[[paste0("Model_", window, "_", i)]] <- profit

  # Print the results for the best model only
  if (backtest < best_backtest) {
    best_garchx_model_fit <- garchx_model_fit
    best_VaR <- VaR
    best_backtest <- backtest
    best_risk <- risk
    best_profit <- profit

    # Print the test results for the best model
    print("Best Model:")
    print(best_model)
    print("Significance Test Result (GARCHX Model):")
    print(coef_test)
    print("White Noise Test (Ljung-Box):")
    print(arch_test)
    print("Normality Test (Jarque-Bera):")
    print(normality_test)
    print("Value at Risk (VaR):")
    print(best_VaR)
    print("Risk:")
    print(best_risk)
    print("Profit:")
    print(best_profit)
    print("Backtest Result:")
    print(best_backtest)
  }
}

# Print the significant test results for all models
for (model in names(significant_tests)) {
  cat("Model:", model, "\n")
  test_results <- significant_tests[[model]]

  cat("Model Squared Residuals:\n")
  residuals <- squared_residuals[[model]]
  print(residuals)
  cat("\n")

  par(mfrow=c(2,1))
  acf(residuals, main = paste("ACF -", model))
  pacf(residuals, main = paste("PACF -", model))

  # Print the test results
  cat("Significance Test Result:\n")
  print(test_results$coef_test)

  cat("White Noise Test (Ljung-Box):\n")
  print(test_results$white_test)

  cat("Normality Test (Jarque-Bera):\n")
  print(test_results$normality_test)

  cat("Value at Risk (VaR):", VaR_results[[model]], "\n")
  cat("Risk:", risk_results[[model]], "\n")
  cat("Profit:", profit_results[[model]], "\n")

  cat("\n")
}
