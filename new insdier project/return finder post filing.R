# Load required packages
library(xts)   # For time series manipulation
library(dplyr) # For data manipulation

# Function to calculate 1-day return after an insider filing for each ticker
calculate_1day_return <- function(ticker, stock_data, filings) {
  
  # Subset stock data for the given ticker
  stock_data_ticker <- subset(stock_data, stock_data$ticker == ticker)
  
  if (nrow(stock_data_ticker) == 0) {
    message("No stock data found for ticker: ", ticker)
    return(data.frame(Ticker = ticker, FilingDate = NA, OneDayReturn = NA))
  }
  
  # Convert to xts object for time series manipulation
  stock_xts <- xts(stock_data_ticker[, c("price.close", "price.adjusted")], order.by = as.Date(stock_data_ticker$ref.date))
  
  # Subset insider transactions for the given ticker
  insdr_tr_ticker <- subset(filings, filings$Ticker == ticker)
  insdr_tr_ticker$FilingDate <- as.Date(insdr_tr_ticker$SECForm4)  # Convert to Date
  
  message("Processing ticker: ", ticker, " with ", nrow(insdr_tr_ticker), " filings.")
  
  # Create an empty vector to store 1-day returns
  one_day_returns <- c()
  
  # Loop through each filing date for the given ticker
  for (i in 1:nrow(insdr_tr_ticker)) {
    filing_date <- insdr_tr_ticker$FilingDate[i]
    
    # Check if filing_date exists in stock_xts
    if (!(filing_date %in% index(stock_xts))) {
      message("Filing date ", filing_date, " not found for ticker ", ticker, ". Looking for closest trading day.")
      # Find the closest previous trading day
      previous_trading_day <- tail(index(stock_xts[index(stock_xts) <= filing_date]), 1)
      
      if (length(previous_trading_day) == 0) {
        message("No trading day found before filing date ", filing_date, " for ticker ", ticker)
        one_day_returns <- c(one_day_returns, NA)
        next
      } else {
        filing_date <- previous_trading_day
        message("Using closest previous trading day: ", filing_date)
      }
    }
    
    # Find the next trading day after the filing date
    next_trading_day_index <- which(index(stock_xts) > filing_date)[1]
    
    if (!is.na(next_trading_day_index)) {
      next_trading_day <- index(stock_xts)[next_trading_day_index]
      
      # Extract single values for filing day close and next day close
      filing_day_close <- stock_xts[filing_date, "price.adjusted", drop = FALSE]
      next_day_close <- stock_xts[next_trading_day, "price.adjusted", drop = FALSE]
      
      # Ensure we extract numeric values from the xts object
      filing_day_close <- ifelse(length(filing_day_close) > 0, as.numeric(filing_day_close[1]), NA)
      next_day_close <- ifelse(length(next_day_close) > 0, as.numeric(next_day_close[1]), NA)
      
      # Debugging logs to check values
      message("Ticker: ", ticker, 
              " Filing date: ", filing_date, 
              " Filing day close: ", filing_day_close, 
              " Next trading day: ", next_trading_day, 
              " Next day close: ", next_day_close)
      
      # Ensure both values are not NA before calculating return
      if (!is.na(filing_day_close) && !is.na(next_day_close) && filing_day_close != 0) {
        one_day_return <- (next_day_close - filing_day_close) / filing_day_close  # Updated return formula
        one_day_returns <- c(one_day_returns, one_day_return)
        message("1-day return for filing on ", filing_date, ": ", one_day_return)
      } else {
        message("NA values encountered for filing date: ", filing_date, " or next trading day: ", next_trading_day)
        one_day_returns <- c(one_day_returns, NA)
      }
    } else {
      message("No next trading day found after filing date: ", filing_date, " for ticker ", ticker)
      one_day_returns <- c(one_day_returns, NA)
    }
  }
  
  # Return a data frame with the ticker, filing date, and 1-day returns
  return(data.frame(Ticker = ticker, FilingDate = insdr_tr_ticker$SECForm4, OneDayReturn = one_day_returns))
}

# Example usage:
# Test with a single ticker
single_ticker_result <- calculate_1day_return("INLX", data, ALL)
print(single_ticker_result)

# Iterate over all tickers in the ALL dataframe to calculate 1-day returns
all_one_day_returns <- lapply(unique(ALL$Ticker), function(ticker) {
  calculate_1day_return(ticker, data, ALL)
})

# Combine all results into a single dataframe
all_one_day_returns_df <- do.call(rbind, all_one_day_returns)

# Check if results were generated
if (nrow(all_one_day_returns_df) == 0) {
  message("No results generated.")
} else {
  print(all_one_day_returns_df)
}



