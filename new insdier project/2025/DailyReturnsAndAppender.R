require(QuantLib)
require(quantmod)
require(dplyr)


today <- Sys.Date()
yesterday <- Sys.Date() - 1

findDailyReturn <- function(date) {
  
  TABLE$SECForm4 <- as.Date(TABLE$SECForm4)
  TABLE$DailyReturn <- NA
  for (i in (1:nrow(TABLE))) {
    if ( date %in% TABLE$SECForm4[i] && isBusinessDay("UnitedStates/NYSE", date)) {
      
      symbol <- TABLE$Ticker[i]
      
      tryCatch({
        getSymbols(symbol, from = date - 1, to = date, auto.assign = TRUE)
        
        # Print message if successful
        print(paste("Successfully downloaded data for", symbol))
        
      }, error = function(e) {
        # Print a message if there's an error (i.e., symbol not found)
        print(paste("Error downloading data for", symbol, ":", e$message))
      })
      
      return <- dailyReturn(symbol)
      TABLE$DailyReturn[i] <- return
    }
  }
  
}

findDailyReturn(today)