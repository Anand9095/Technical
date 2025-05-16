# Install and load necessary packages
if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}
if (!requireNamespace("TTR", quietly = TRUE)) {
  install.packages("TTR")
}
library(quantmod)
library(TTR)

# Define parameters
ticker <- "^GSPC"
start_date <- "1927-12-30"
end_date <- Sys.Date()
rsi_length <- 14
ema_short_length <- 5
ema_mid_length <- 8
ema_long_length <- 13
momentum_length <- 14 # Define momentum length
atr_length <- 14
bb_length <- 20
bb_sd <- 2
macd_fast_length <- 12
macd_slow_length <- 26
macd_signal_length <- 9
stoch_fast_k_length <- 14
stoch_fast_d_length <- 1
stoch_slow_d_length <- 3
adx_length <- 14        # ADX Smoothing (as per your image)
di_length <- 14         # DI Length (as per your image)
output_file <- "C:/r/sp500.csv" # Specify the output file

# Fetch data
getSymbols(ticker, from = start_date, to = end_date, adjust = TRUE)
sp500_data <- GSPC

# Calculate RSI
RSI_values <- RSI(Cl(sp500_data), n = rsi_length)

# Calculate Momentum
momentum_values <- momentum(Cl(sp500_data), n = momentum_length)

# Calculate EMAs on Low prices
ema5_values <- EMA(as.numeric(Lo(sp500_data)), n = ema_short_length)
ema8_values <- EMA(as.numeric(Lo(sp500_data)), n = ema_mid_length)
ema13_values <- EMA(as.numeric(Lo(sp500_data)), n = ema_long_length)

# Calculate Average True Range (ATR)
atr_values <- ATR(HLC(sp500_data), n = atr_length)[, "atr"]

# Calculate Bollinger Bands
bb_values <- BBands(Cl(sp500_data), n = bb_length, sd = bb_sd)
sp500_data$BB_Upper <- bb_values[, "up"]
sp500_data$BB_Middle <- bb_values[, "mavg"]
sp500_data$BB_Lower <- bb_values[, "dn"]
sp500_data$BB_PctB <- bb_values[, "pctB"]

# Calculate Simple Stochastic Oscillator
stoch_values <- stoch(Hi(sp500_data),
                      Lo(sp500_data),
                      Cl(sp500_data),
                      nFastK = stoch_fast_k_length,
                      nFastD = stoch_fast_d_length,
                      nSlowD = stoch_slow_d_length)
sp500_data$Stoch_K <- stoch_values[, "fastK"]
sp500_data$Stoch_D <- stoch_values[, "fastD"]

# Calculate MACD using Close prices
macd_values <- MACD(Cl(sp500_data),
                    nFast = macd_fast_length,
                    nSlow = macd_slow_length,
                    nSig = macd_signal_length)
sp500_data$MACD <- macd_values[, "macd"]
sp500_data$MACDSignal <- macd_values[, "signal"]
sp500_data$MACDHist <- macd_values[, "histogram"]

# Calculate Average Directional Index (ADX)
adx_values <- ADX(HLC(sp500_data), n = adx_length, maType = "SMA") # Using SMA as a common type
sp500_data$ADX <- adx_values[, "ADX"]
sp500_data$DIp <- adx_values[, "DIp"] # Plus Directional Indicator
sp500_data$DIm <- adx_values[, "DIm"] # Minus Directional Indicator

# Calculate Volume Weighted Average Price (VWAP) using HLC
sp500_data$VWAP <- VWAP(price = HLC(sp500_data))

# Add indicators to sp500_data
sp500_data$RSI <- RSI_values
sp500_data$Momentum <- momentum_values
sp500_data$EMA_Low_5 <- ema5_values
sp500_data$EMA_Low_8 <- ema8_values
sp500_data$EMA_Low_13 <- ema13_values
sp500_data$ATR <- atr_values

# Print the first few rows with the indicators
head(sp500_data)

# Print the structure to see the new columns
str(sp500_data)

# Convert xts object to data frame for saving with column names
column_names <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted",
                  "RSI", "Momentum",
                  "EMA_Low_5", "EMA_Low_8", "EMA_Low_13",
                  "ATR", "BB_Upper", "BB_Middle", "BB_Lower", "BB_PctB",
                  "Stoch_K", "Stoch_D",
                  "MACD", "MACDSignal", "MACDHist",
                  "ADX", "DIp", "DIm", "VWAP") # Added ADX, DIp, DIm, VWAP

sp500_df_to_save <- data.frame(Date = index(sp500_data), coredata(sp500_data))
colnames(sp500_df_to_save) <- column_names

# Save the updated data to the specified CSV file, overwriting it
write.csv(sp500_df_to_save, file = output_file, row.names = FALSE, na = "NA")

cat(paste("The S&P 500 historical data with all indicators has been saved to:", output_file, "\n"))

# Try to read the saved CSV and print its structure
cat("\nStructure of the saved CSV file:\n")
tryCatch({
  saved_data <- read.csv(output_file)
  str(saved_data)
}, error = function(e) {
  cat(paste("Error reading the saved CSV:", e$message, "\n"))
})



working till data
