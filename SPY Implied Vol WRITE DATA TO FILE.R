# updated in github
library(quantmod)

quantmod::getSymbols("SPY")
quantmod::getOptionChain("SPY")


getOptionChain_mod <- function(Symbols) {
  # Get real-time quote for the stock
  quote <- getQuote(Symbols)
  # Get the last traded price
  last_price <- 520.4
  # Get option chain
  oc <- getOptionChain(Symbols)
  if (!is.null(oc)) {
    # Define the range for ITM and OTM
    range <- c(last_price - 4, last_price + 4)
    # Filter calls and puts within the defined range
    oc$calls <- oc$calls[oc$calls$Strike >= range[1] & oc$calls$Strike <= range[2], ]
    oc$puts <- oc$puts[oc$puts$Strike >= range[1] & oc$puts$Strike <= range[2], ]
    
  }
  return(oc)
}

# Get option chain data
oc <- getOptionChain_mod("SPY")

# Combine calls and puts into a dataframe
option_data <- rbind(data.frame(Type = "Call", oc$calls),
                     data.frame(Type = "Put", oc$puts))

# Extract relevant data for calls
calls_data <- data.frame(
  Type = "Call",
  Strike = oc$calls$Strike,
  Last = oc$calls$Last,
  Volume = oc$calls$Volume,
  ImpliedVolatility = oc$calls$ImpliedVolatility,
  LastTradeTime = oc$calls$LastTradeTime
)

# Extract relevant data for puts
puts_data <- data.frame(
  Type = "Put",
  Strike = oc$puts$Strike,
  Last = oc$puts$Last,
  Volume = oc$puts$Volume,
  ImpliedVolatility = oc$puts$ImpliedVolatility,
  LastTradeTime = oc$puts$LastTradeTime
)

# Combine calls and puts dataframes
new_option_data <- rbind(calls_data, puts_data)

# Print the dataframe
print(option_data)