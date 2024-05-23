rm(list=ls())
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

get_crypto_data <- function(crypto) {
  data <- getSymbols(crypto, auto.assign = FALSE, from = "2018-01-01", to = "2023-12-31", src = 'yahoo')
  price <- na.omit(data[, paste(crypto, "Close", sep = ".")])
  ret <- diff(log(price))
  ret <- na.omit(ret)
  log_returns_xts <- xts(ret, order.by = index(price)[-1])
  colnames(log_returns_xts) <- crypto
  return(log_returns_xts)
}

fit_evaluate_model <- function(data, garchType, order, distribution){
  spec <- ugarchspec(variance.model = list(model = garchType, garchOrder = order),
                     mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     distribution.model = distribution)
  
  fit <- ugarchfit(spec = spec, data = data)
  criteria <- infocriteria(fit)
  return(c(AIC=criteria[1], BIC=criteria[2]))
}

cryptos <- c("ETH-USD", "BNB-USD", "BTC-USD")
models <- c("sGARCH", "eGARCH", "gjrGARCH")  
orders <- list(c(1,1), c(1,2), c(2,1), c(2,2))
distributions <- c("ged", "std", "norm")

results <- list()

for (crypto in cryptos) {
  data <- get_crypto_data(crypto)
  for (model in models) {
    for (dist in distributions) {
      best_aic <- Inf
      best_bic <- Inf
      best_order_aic <- NULL
      best_order_bic <- NULL
      
      for (order in orders) {
        if (crypto == "BTC-USD" && model == "sGARCH" && dist == "norm" && identical(order, c(1, 2))) {
          next
        }
        
        order_label <- paste(order, collapse = ",")
        aic_bic_values <- fit_evaluate_model(data, model, order, dist)
        
        if (aic_bic_values[1] < best_aic) {
          best_aic <- aic_bic_values[1]
          best_order_aic <- order_label
        }
        if (aic_bic_values[2] < best_bic) {
          best_bic <- aic_bic_values[2]
          best_order_bic <- order_label
        }
      }
      results[[paste(crypto, model, dist, "AIC")]] <- c(crypto, model, dist, best_order_aic, best_aic)
      results[[paste(crypto, model, dist, "BIC")]] <- c(crypto, model, dist, best_order_bic, best_bic)
    }
  }
}

results_df <- as.data.frame(do.call(rbind, results), stringsAsFactors = FALSE)
colnames(results_df) <- c("Crypto", "Model", "Distribution", "Best Order", "Value")

write.xlsx(results_df, "F_GARCH_Models_Best_Orders_distributions.xlsx")
