rm(list=ls())

options(scipen = 999, digits = 3)

library(quantmod)
library(ggplot2)
library(ggfortify)
library(moments)
library(tseries)
library(dplyr)
library(xts)


cryptos <- c("ETH-USD", "BNB-USD", "BTC-USD")


summary_stats <- list()
log_returns_xts_list <- list()
jarque_bera_details <- list()  

for(crypto in cryptos) {
 
  data <- getSymbols(crypto, auto.assign=FALSE, from="2018-01-01",to="2023-12-31", src='yahoo')
  
  
  price <- na.omit(data[, paste(crypto, "Close", sep=".")])
  

  ret <- diff(log(price))
  ret <- na.omit(ret)
  
 
  p <- autoplot(ret) + ggtitle(paste(crypto, "Log Returns")) + theme_minimal()
  print(p)  
  

  summary_stats[[crypto]] <- data.frame(
    Crypto = crypto,
    Elements = length(ret),
    Min = min(ret),
    Max = max(ret),
    Mean = mean(ret),
    Median = median(ret),
    Std_Deviation = sd(ret),
    Skewness = skewness(ret),
    Kurtosis = kurtosis(ret)-3  
  )
  
  log_returns_xts <- xts(ret, order.by = index(price)[-1])
  colnames(log_returns_xts) <- crypto
  
  log_returns_xts_list[[crypto]] <- log_returns_xts
  
  jb_test <- jarque.bera.test(ret)
  jarque_bera_details[[crypto]] <- list(
    JB_Statistic = jb_test$statistic,
    JB_P_Value = jb_test$p.value,
    Interpretation = ifelse(jb_test$p.value < 0.05, "Non-normal", "Normal")
  )
}

summary_table <- do.call(rbind, summary_stats)
combined_returns_xts <- do.call(merge, c(log_returns_xts_list, all = TRUE))
combined_returns_df <- as.data.frame(combined_returns_xts)

jb_results_df = do.call(rbind, jarque_bera_details)

print(jb_results_df)

write.xlsx(summary_table, "F_sum_table.xlsx")
write.xlsx(jb_results_df, "F_jb_results.xlsx")
write.csv(jb_results_df, "jb_results.csv")



