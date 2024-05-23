rm(list = ls())

library(quarks)
library(quantmod)
library(moments)
library(tseries)
library(dplyr)
library(xts)
library(forecast)
library(stats)
library(rugarch)
library(PerformanceAnalytics)
library(fGarch)
library(openxlsx)
library(reshape2)

combi_aic <- read.xlsx("Combi_AIC.xlsx")

exceedances_df <- data.frame(Crypto = character(),
                             Distribution = character(),
                             GARCH_Model = character(),
                             Order = character(),
                             Exceedances = integer(),
                             Expected_Exceed = numeric(),
                             LR_uc_Stat = numeric(),
                             LR_uc_p_value = numeric(),
                             LR_cc_Stat = numeric(),
                             LR_cc_p_value = numeric(),
                             stringsAsFactors = FALSE)

window_size <-  365 # Training set size
forecast_length <- 1825 # Testing set size

for (i in 1:nrow(combi_aic)) {
  crypto <- combi_aic[i, "Crypto"]
  garch_model <- combi_aic[i, "Model"]
  dist <- combi_aic[i, "Distribution"]
  order <- as.numeric(unlist(strsplit(as.character(combi_aic[i, "Order"]), "\\.")))
  
  
  if ((crypto == "ETH-USD" && garch_model == "eGARCH" && all(order == c(2, 1)) && dist == "std") ||
      (crypto == "ETH-USD" && garch_model == "gjrGARCH" && all(order == c(1, 1)) && dist == "ged") ||
      (crypto == "ETH-USD" && garch_model == "gjrGARCH" && all(order == c(1, 1)) && dist == "std") ||
      (crypto == "BNB-USD" && garch_model == "gjrGARCH" && all(order == c(1, 1)) && dist == "ged") ||
      (crypto == "BNB-USD" && garch_model == "gjrGARCH" && all(order == c(2, 1)) && dist == "std") ||
      (crypto == "BTC-USD" && garch_model == "eGARCH" && all(order == c(2, 1)) && dist == "ged") ||
      (crypto == "BTC-USD" && garch_model == "eGARCH" && all(order == c(2, 1)) && dist == "std") ||
      (crypto == "BTC-USD" && garch_model == "gjrGARCH" && all(order == c(1, 1)) && dist == "std") ||
      (crypto == "BTC_USD" && garch_model == "gjrGARCH" && dist == "norm")) {
    next
  }
  
  data <- getSymbols(crypto, auto.assign = FALSE, from = "2018-01-01", to = "2023-12-31", src = 'yahoo')
  price <- na.omit(data[, paste(crypto, "Close", sep = ".")])
  ret <- diff(log(price))
  ret <- na.omit(ret)
  colnames(ret) = crypto
  log_returns_xts <- xts(ret, order.by = index(price)[-1])
  
  var_forecasts_df <- data.frame(Date = index(log_returns_xts[(nrow(log_returns_xts) - forecast_length + 1):nrow(log_returns_xts)]))
  
  spec <- ugarchspec(variance.model = list(model = garch_model, garchOrder = order),
                     mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     distribution.model = dist)
  
  forecasts <- ugarchroll(spec, data = log_returns_xts, n.ahead = 1, forecast.length = forecast_length, refit.every = 50, 
                          refit.window = "moving", window.size = window_size, 
                          vaR.alpha = c(0.01), keep.coef = TRUE)
  
  var_forecasts <- as.numeric(forecasts@forecast$VaR[,1])
  var_forecast_column_name <- paste(garch_model, dist, sep = "_")
  out_of_sample_returns <- as.numeric(log_returns_xts[(nrow(log_returns_xts) - forecast_length + 1):nrow(log_returns_xts)])
  
  vartest_result <- VaRTest(alpha = 0.01, actual = out_of_sample_returns, VaR = var_forecasts, conf.level = 0.95)
  
  exceedances <- vartest_result$actual.exceed
  expected_exceed <- vartest_result$expected.exceed
  LR_uc_stat <- vartest_result$uc.LRstat
  LR_uc_p_value <- vartest_result$uc.LRp
  LR_cc_stat <- vartest_result$cc.LRstat
  LR_cc_p_value <- vartest_result$cc.LRp
  
  exceedances_df <- rbind(exceedances_df, data.frame(Crypto = crypto, 
                                                     Distribution = dist, 
                                                     GARCH_Model = garch_model,
                                                     Order = paste(order, collapse = ","),
                                                     Exceedances = exceedances,
                                                     Expected_Exceed = expected_exceed,
                                                     LR_uc_Stat = LR_uc_stat,
                                                     LR_uc_p_value = LR_uc_p_value,
                                                     LR_cc_Stat = LR_cc_stat,
                                                     LR_cc_p_value = LR_cc_p_value))
}

print(exceedances_df)

write.xlsx(exceedances_df, file = "F_Exceedances_Backtest_Results_AIC.xlsx")
