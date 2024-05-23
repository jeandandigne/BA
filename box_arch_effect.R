rm(list=setdiff(ls(), c("jb_results_df","summary_table")))
options(scipen = 999, digits = 3)

library(quantmod)
library(ggplot2)
library(ggfortify)
library(moments)
library(tseries)  
library(dplyr)
library(xts)
library(forecast)
library(stats)


cryptos <- c("ETH-USD", "BNB-USD", "BTC-USD")
log_returns_xts_list <- list()

for(crypto in cryptos) {
  
  data <- getSymbols(crypto, auto.assign=FALSE, from="2018-01-01",to="2023-12-31", src='yahoo')
  
  price <- na.omit(data[, paste(crypto, "Close", sep=".")])
  
  ret <- diff(log(price))
  ret <- na.omit(ret)
  
  mean_ret <- mean(ret)
  residuals <- ret - mean_ret
  
  residuals_xts <- xts(residuals, order.by = index(price)[-1])
  colnames(residuals_xts) <- crypto
  
  log_returns_xts_list[[crypto]] <- residuals_xts
  
}

squared_residuals_list <- lapply(log_returns_xts_list, function(x) x^2)

ljung_box_results <- lapply(squared_residuals_list, function(x) {
  lb_test <- Box.test(x, lag = 20, type = "Ljung-Box")
  return(list(Statistic = lb_test$statistic, P_Value = lb_test$p.value))  
})

results_df <- do.call(rbind, lapply(names(ljung_box_results), function(x) {
  data.frame(Cryptocurrency = x, X_Squared = ljung_box_results[[x]]$Statistic, P_Value = ljung_box_results[[x]]$P_Value)
}))
row.names(results_df) <- NULL

write.xlsx(results_df, "f_ARCH_test.xlsx")


