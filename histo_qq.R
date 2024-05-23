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
log_returns_xts_list <- list()

for (crypto in cryptos) {
  
  data <- getSymbols(crypto, auto.assign = FALSE, from = "2018-01-01", to = "2023-12-31", src = 'yahoo')
  
  price <- na.omit(data[, paste(crypto, "Close", sep = ".")])
  
  ret <- diff(log(price))
  ret <- na.omit(ret)
  
  log_returns_xts <- xts(ret, order.by = index(price)[-1])
  colnames(log_returns_xts) <- crypto
  
  log_returns_xts_list[[crypto]] <- log_returns_xts
  
  log_returns_df <- data.frame(Date = index(log_returns_xts), Log_Returns = coredata(log_returns_xts)[,1])
  
  qq_plot <- ggplot(log_returns_df, aes(sample = Log_Returns)) +
    stat_qq(shape = 1, color = "green4", alpha = 0.5) +
    stat_qq_line(color = "red", linetype = "dashed") +
    labs(title = paste("QQ Plot for", crypto), x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
    coord_cartesian(ylim = c(-1.5, 1.5))
  
  ggsave(filename = paste0("QQ_Plot_", crypto, ".png"), plot = qq_plot, width = 8, height = 6)
}


for (crypto in cryptos) {
  
  data <- getSymbols(crypto, auto.assign = FALSE, from = "2018-01-01", to = "2023-12-31", src = 'yahoo')
  
  price <- na.omit(data[, paste(crypto, "Close", sep = ".")])
  
  ret <- diff(log(price))
  ret <- na.omit(ret)
  
  log_returns_xts <- xts(ret, order.by = index(price)[-1])
  colnames(log_returns_xts) <- crypto
  
  log_returns_df <- data.frame(Date = index(log_returns_xts), Log_Returns = coredata(log_returns_xts)[,1])
  
  hist_plot_normal <- ggplot(log_returns_df, aes(x = Log_Returns)) +
    geom_histogram(aes(y = ..density..), bins = 50, color = "black", fill = "grey", alpha = 0.6) +
    stat_function(fun = dnorm, args = list(mean = mean(log_returns_df$Log_Returns), sd = sd(log_returns_df$Log_Returns)), color = "red", linetype = "dashed", size= 1) +
    labs(title = paste("Frequency Plot for", crypto, "with Normal Distribution"), x = "Log Returns", y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 12), axis.text = element_text(size = 10))
  
  ggsave(filename = paste0("Histogram_Normal_", crypto, ".png"), plot = hist_plot_normal, width = 8, height = 6)
}


