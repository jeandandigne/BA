library(ggplot2)
library(reshape2)

models <- c('A-1', 'A-2', 'A-3', 'B-1', 'B-2', 'B-3')
cryptos <- c('BTC-USD', 'ETH-USD', 'BNB-USD')

p_values_uc <- matrix(c(
  0.0018, 0.3928, 0.3928, 0.0000, NA, 0.1969,  # BTC-USD
  0.0114, 0.5275, 0.3928, 0.0000, 0.5275, 0.5275,  # ETH-USD
  0.0865, 0.7661, 0.9530, 0.5275, 0.4302, 0.4302  # BNB-USD
), nrow=3, byrow=TRUE)

p_values_cc <- matrix(c(
  0.0069, 0.3744, 0.3744, 0.0000, NA, 0.2669,  # BTC-USD
  0.0331, 0.4109, 0.3744, 0.0000, 0.6413, 0.4109,  # ETH-USD
  0.1580, 0.8153, 0.8343, 0.6413, 0.6469, 0.6469  # BNB-USD
), nrow=3, byrow=TRUE)

df_uc <- as.data.frame(p_values_uc)
df_cc <- as.data.frame(p_values_cc)

colnames(df_uc) <- models
rownames(df_uc) <- cryptos
df_uc$Crypto <- cryptos

colnames(df_cc) <- models
rownames(df_cc) <- cryptos
df_cc$Crypto <- cryptos

df_uc_melt <- melt(df_uc, id.vars = "Crypto", variable.name = "Model", value.name = "p_value")
df_cc_melt <- melt(df_cc, id.vars = "Crypto", variable.name = "Model", value.name = "p_value")

ggplot(df_uc_melt, aes(x = Model, y = Crypto, fill = p_value)) +
  geom_tile() +
  geom_text(aes(label = round(p_value, 4)), color = "white") +
  scale_fill_gradient(low = "springgreen3", high = "darkgreen", na.value = "grey50") +
  theme_minimal() +
  ggtitle("Unconditional Coverage p-values")

ggplot(df_cc_melt, aes(x = Model, y = Crypto, fill = p_value)) +
  geom_tile() +
  geom_text(aes(label = round(p_value, 4)), color = "white") +
  scale_fill_gradient(low = "springgreen3", high = "darkgreen", na.value = "grey50") +
  theme_minimal() +
  ggtitle("Conditional Coverage p-values")
