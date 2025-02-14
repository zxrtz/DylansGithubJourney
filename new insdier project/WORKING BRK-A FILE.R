
# require packages
require("stringr");require("data.table");require("BatchGetSymbols")
require("quantmod");require("dplyr");require("PerformanceAnalytics");require("pbapply")
library(timeDate)
library(RQuantLib)
library(xts)
library(zoo)

# Define NYSE holidays for 2024
holiday_dates <- as.Date(c(
  "2024-01-01",  # New Year's Day
  "2024-01-15",  # Martin Luther King Jr. Day
  "2024-02-19",  # Washington's Birthday
  "2024-03-29",  # Good Friday
  "2024-05-27",  # Memorial Day
  "2024-07-04",  # Independence Day
  "2024-09-02",  # Labor Day
  "2024-11-28",  # Thanksgiving Day
  "2024-12-25"   # Christmas Day
))

# Define custom function to check if a date is a business day
is_business_day_custom <- function(date) {
  !weekdays(date) %in% c("Saturday", "Sunday") && !date %in% holiday_dates
}



# subset data for the given ticker
stk_data = subset(data, data$ticker == "BRK-A")
# subset insider transaction
insdr_tr = subset(ALL,ALL$Ticker == "BRK-A")
insdr_tr$SECForm4 = as.Date(insdr_tr$SECForm4, format="%Y-%m-%d %H:%M:%S")
# add Signal == (-1 if Sale) || (1 is Buy/Exercise Options)
stk_data$sig = NA
for (ii in 1:length(insdr_tr$SECForm4)) {
  current_date <- as.Date(insdr_tr$SECForm4[ii])
  if (!is_business_day_custom(current_date)) {
    NEXT <- adjust(calendar = "UnitedStates/NYSE", dates = current_date + 1, bdc = 0)
    iloc <- which(stk_data$ref.date == NEXT)
    
    if (length(iloc) > 0) {
      stk_data$sig[iloc] <- ifelse(insdr_tr$Transaction[ii] == "Sale", -1, 1)
      insdr_tr$SECForm4[ii] <- NEXT
    } else {
      message("No matching date found in stk_data for: ", NEXT)
    }
  } else {
    iloc <- which(stk_data$ref.date == current_date)
    
    if (length(iloc) > 0) {
      stk_data$sig[iloc] <- ifelse(insdr_tr$Transaction[ii] == "Sale", -1, 1)
    } else {
      message("No matching date found in stk_data for: ", current_date)
    }
  }
}
# Lag signal
stk_data$sig <- Lag(stk_data$sig)
# Calculate Return based on Hold Time
# checks to see if multiple transactions exists
if(length(which(!is.na(stk_data$sig))) > 1){
  WHERE = which(!is.na(stk_data$sig))[1]
  stk_data$sig[WHERE:nrow(stk_data)] <- na.locf(stk_data$sig[WHERE:nrow(stk_data)])
  stk_data$sig[is.na(stk_data$sig)] <- 0
  stk_data$strat_ret = stk_data$ret.adjusted.prices * stk_data$sig
  stk_data[is.na(stk_data)]<-0
}else{
  if((which(!is.na(stk_data$sig)) + 7) > nrow(stk_data))
  {
    stk_data$sig[which(!is.na(stk_data$sig)):nrow(stk_data)] <- na.locf(stk_data$sig)
    stk_data$sig[is.na(stk_data$sig)] <- 0
    stk_data$strat_ret = stk_data$ret.adjusted.prices * stk_data$sig
    stk_data[is.na(stk_data)]<-0
  }else{
    WHERE = which(!is.na(stk_data$sig))
    stk_data$sig[WHERE:(WHERE+7)] <- na.locf(stk_data$sig[WHERE:(WHERE+7)])
    stk_data$sig[is.na(stk_data$sig)] <- 0
    stk_data$strat_ret = stk_data$ret.adjusted.prices * stk_data$sig
    stk_data[is.na(stk_data)]<-0
  }
  
}
insdr_tr = insdr_tr[,c("Date","Transaction","NumShares","SECForm4")]
# Add total Shares for each Day - by Transaction Date
insdr_tr = insdr_tr %>% group_by(Date) %>% mutate(Total = sum(NumShares))
# Add total Shares for each Day - by Release Date (SECForm4)
insdr_tr = insdr_tr %>% group_by(SECForm4) %>% mutate(Total2 = sum(NumShares))
# Transaction Dates
tDates = insdr_tr[,c("Date","Transaction","Total")]
tDates = unique(tDates)
# Release Dates
rDates = insdr_tr[,c("SECForm4","Transaction","Total2")]
rDates = unique(rDates)
# change column names
colnames(stk_data)[1:6] = c("price.Open","price.High","price.Low","price.Close",
                            "Volume","price.Adjusted")
# convert to xts
stk_xts = xts(stk_data[,1:5], order.by = as.Date(stk_data$ref.date,format="%Y-%m-%d"))

# new TA - by Transaction Date
stk_xts$stkTA = ifelse(index(stk_xts) == tDates$Date, 
                       ifelse(tDates$Transaction == "Sale",-tDates$Total,tDates$Total),
                       0)
# new TA - by SEC Release Date
stk_xts$stkTA2 = ifelse(index(stk_xts) == rDates$SECForm4, 
                        ifelse(rDates$Transaction == "Sale",-rDates$Total2,rDates$Total2),
                        0)    

# Print unique values in the legend columns
unique(tDates$Transaction)


## *****************
# Save as PDF
## *****************
### CHANGE THIS BACK TO RBITRAGE'S CODE ONCE THERE'S ENOUGH ORDERS
pdf(paste0(getwd(), "/Insiders/", "BRK-A", ".pdf"), paper = "special")
# Plot stock chart
c0 = print(quantmod::chartSeries(stk_xts[,1:5], type = "candlesticks", name = "BRK-A"))
# Add insider transactions - by Transaction Date
c1 = print(quantmod::addTA(stk_xts$stkTA, col = "white", legend = "Buy/Sell"))
# Cumulative Sum - by Transaction Date
c2 = print(quantmod::addTA(cumsum(stk_xts$stkTA), col = "blue", legend = "Cumulative Sum"))
# Add insider transactions - by Release Date
c3 = print(quantmod::addTA(stk_xts$stkTA2, col = "yellow", legend = "Release Date"))
# Add vertical lines for transaction and release dates
c4 = print(quantmod::addLines(v = which(index(stk_xts) %in% tDates$Date), on = -1, col = "white"))
c5 = print(quantmod::addLines(v = which(index(stk_xts) %in% rDates$SECForm4), on = -1, col = "yellow"))
# Close the graphics device to save the PDF
graphics.off()
# return stk_data
stk_data

getwd()




# ORIGINAL PDF CODE **************************************
# save as PDF
# pdf(paste0(getwd(),"/Insiders/","BRK-A",".pdf"),paper = "special")
# plot stock chart
# c0 = print(quantmod::chartSeries(stk_xts[,1:5], type = "candlesticks",name="BRK-A"))
# add insider transactions - by Transaction Date
# c1 = print(quantmod::addTA(stk_xts$stkTA,col="white",legend = tDates$Transaction))
# cumulative Sum - by Transaction Date
# c2 = print(quantmod::addTA(cumsum(stk_xts$stkTA),col="white",legend = tDates$Transaction))
# add insider transactions - by Release Date
# c3 = print(quantmod::addTA(stk_xts$stkTA2,col="yellow",legend = "Release Date"))
# c4 = print(quantmod::addLines(v=which(index(stk_xts) == tDates$Date), on=-1, col = "white"))
# c5 = print(quantmod::addLines(v=which(index(stk_xts) == rDates$SECForm4), on=-1, col = "yellow"))
# graphics.off() 
