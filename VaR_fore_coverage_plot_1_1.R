rm(list = ls())

library(quarks)
library(quantmod)
library(ggplot2)
library(ggfortify)
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

cryptos <- c("ETH-USD", "BNB-USD", "BTC-USD")
distributions <- c("ged", "std", "norm")
garch_models <- c("sGARCH", "gjrGARCH", "eGARCH")
garch_orders <- list(c(1, 1))

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
forecast_length <- 1825# Testing set size

for (crypto in cryptos) {
  data <- getSymbols(crypto, auto.assign = FALSE, from = "2018-01-01", to = "2023-12-31", src = 'yahoo')
  price <- na.omit(data[, paste(crypto, "Close", sep = ".")])
  ret <- diff(log(price))
  ret <- na.omit(ret)
  colnames(ret) = crypto
  log_returns_xts <- xts(ret, order.by = index(price)[-1])
  
  var_forecasts_df <- data.frame(Date = index(log_returns_xts[(nrow(log_returns_xts) - forecast_length + 1):nrow(log_returns_xts)]))
  
  for (dist in distributions) {
    
    for (garch_model in garch_models) {
      
      for (order in garch_orders) {
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
        
        var_forecasts_df[[var_forecast_column_name]] <- var_forecasts
      }
    }
  }
  
  log_returns_df <- data.frame(Date = index(log_returns_xts[(nrow(log_returns_xts) - forecast_length + 1):nrow(log_returns_xts)]),
                               Log_Returns = as.numeric(log_returns_xts[(nrow(log_returns_xts) - forecast_length + 1):nrow(log_returns_xts)]))
  
  var_forecasts_melted <- melt(var_forecasts_df, id.vars = "Date", variable.name = "Model", value.name = "VaR")
  
  combined_plot <- ggplot() +
    geom_line(data = log_returns_df, aes(x = Date, y = Log_Returns), color = "black") +
    geom_line(data = var_forecasts_melted, aes(x = Date, y = VaR, color = Model)) +
    labs(title = paste("Log Returns and VaR Forecasts for", crypto), x = "Date", y = "Value") +
    theme_minimal() +
    theme(aspect.ratio = 0.8) # Adjust the aspect ratio
  
  
  ggsave(filename = paste0("Log_Returns_and_VaR_1_Forecasts_1_1_test", crypto, ".png"), plot = combined_plot)
}

print(exceedances_df)

write.xlsx(exceedances_df, file = "F_Exceedances_Backtest_Results_BTC.xlsx")
