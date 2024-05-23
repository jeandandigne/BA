rm(list = ls())

library(quantmod)
library(ggplot2)
library(dplyr)
library(xts)
library(fGarch)  

cryptos <- c("ETH-USD", "BNB-USD", "BTC-USD")
log_returns_xts_list <- list()

for (crypto in cryptos) {
  
  data <- getSymbols(crypto, auto.assign = FALSE, from = "2018-01-01", to = "2023-12-31", src = 'yahoo')
  
  price <- na.omit(data[, paste(crypto, "Close", sep = ".")])
  
  ret <- diff(log(price))
  ret <- na.omit(ret)
  
  log_returns_xts <- xts(ret, order.by = index(price)[-1])
  colnames(log_returns_xts) <- crypto
  

  log_returns_df <- data.frame(Date = index(log_returns_xts), Log_Returns = coredata(log_returns_xts)[,1])
  
  mean_ret <- mean(log_returns_df$Log_Returns)
  sd_ret <- sd(log_returns_df$Log_Returns)
  
  hist_plot_student_t <- ggplot(log_returns_df, aes(x = Log_Returns)) +
    geom_histogram(aes(y = ..density..), bins = 50, color = "black", fill = "gray", alpha = 0.5) +
    stat_function(fun = function(x) dstd(x, mean = mean_ret, sd = sd_ret, nu = 4),
                  color = "green4", size = 1) +
    stat_function(fun = function(x) dnorm(x, mean = mean_ret, sd = sd_ret),
                  color = "red", size = 1, linetype = "dashed") +
    labs(title = paste0(crypto, " Log Returns with Student's t-distribution (shape=4) and Normal Distribution"),
         x = "Log Returns", y = "Density") +
    theme_minimal()+
    scale_color_manual(name = "Distributions",
                       values = c("green4", "red"),
                       labels = c("Student's t-distribution (shape=4)", "Normal Distribution"))
  
  ggsave(filename = paste0("Histogram_Student_t_", crypto, ".png"), plot = hist_plot_student_t, width = 10, height = 6)
  
  hist_plot_ged <- ggplot(log_returns_df, aes(x = Log_Returns)) +
    geom_histogram(aes(y = ..density..), bins = 50, color = "black", fill = "gray", alpha = 0.5) +
    stat_function(fun = function(x) dged(x, mean = mean_ret, sd = sd_ret, nu = 1),
                  color = "blue4", size = 1) +
    stat_function(fun = function(x) dnorm(x, mean = mean_ret, sd = sd_ret),
                  color = "red", size = 1, linetype = "dashed") +
    labs(title = paste0(crypto, " Log Returns with Generalized Error Distribution (GED) (shape=1) and Normal Distribution"),
         x = "Log Returns", y = "Density") +
    theme_minimal()+
    scale_color_manual(name = "Distributions",
                       values = c("blue4", "red"),
                       labels = c("Generalized Error Distribution (shape=1)", "Normal Distribution"))
  
  ggsave(filename = paste0("Histogram_GED_", crypto, ".png"), plot = hist_plot_ged, width = 10, height = 6)
}
