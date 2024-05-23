rm(list=setdiff(ls(), c("jb_results_df","summary_table")))
options(scipen = 999, digits = 3)

# Load necessary libraries
library(quantmod)
library(ggplot2)
library(ggfortify)
library(moments)
library(tseries)  
library(dplyr)
library(xts)
library(forecast)
library(stats)
library(gridExtra)


cryptos <- c("ETH-USD", "BNB-USD", "BTC-USD")
log_returns_xts_list <- list()

for(crypto in cryptos) {

  data <- getSymbols(crypto, auto.assign=FALSE, from="2018-01-01",to="2023-12-31", src='yahoo')
  
  price <- na.omit(data[, paste(crypto, "Close", sep=".")])
  
  ret <- diff(log(price))
  ret <- na.omit(ret)
  
  log_returns_xts <- xts(ret, order.by = index(price)[-1])
  colnames(log_returns_xts) <- crypto
  
  log_returns_xts_list[[crypto]] <- log_returns_xts
  
}


combined_returns_xts <- do.call(merge, c(log_returns_xts_list, all = TRUE))
combined_returns_df <- as.data.frame(combined_returns_xts)




plot_acf_cryptos <- function(df) {
  for (crypto in colnames(df)) {
    acf_data <- acf(df[[crypto]], main=paste("ACF for", crypto))
    
    autoplot(acf_data) + 
      ggtitle(paste("Acf Function for", crypto)) +
      theme_minimal() +
      labs(y="Autocorrelation", x="Lag")
  }
}

plot_acf_cryptos(combined_returns_df)

