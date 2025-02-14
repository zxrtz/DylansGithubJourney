  # require packages
  require("stringr");require("data.table");require("BatchGetSymbols")
  require("quantmod");require("dplyr");require("PerformanceAnalytics");require("pbapply")
  library(timeDate)
  library(RQuantLib)
  library(xts)
  library(zoo)
  
  # SET WD AUTOMATICALLY
  setwd("C:/Users/Dylan/Desktop/STONKS BACKTEST/new insdier project")
  
  # Define the formatVOL function
  formatVOL <- function(x) {
    as.numeric(gsub(",", "", x))
  }
  
  # **********************************************************************************
  #                INSIDER TRANSACTIONS IMPORTING
  # **********************************************************************************
  url <- "https://finviz.com/insidertrading.ashx"
  TABLE <- read_html(url)
  TABLE <- TABLE  %>% 
    html_nodes("table") %>%
    .[[7]] %>%
    html_table(header=TRUE,fill=TRUE)
  
  noms <- names(TABLE)
  noms <- gsub("#","Num",noms)
  noms <- gsub("\\$","Dollar",noms)
  noms <- gsub("\\(","",noms)
  noms <- gsub("\\)","",noms)
  noms <- gsub(" ","",noms)
  colnames(TABLE) <- noms
  TABLE$Date <- as.Date(as.character(TABLE$Date),format="%b %d")
  TABLE$NumShares<- formatVOL(TABLE$NumShares)
  TABLE$ValueDollar<- formatVOL(TABLE$ValueDollar)
  TABLE$NumSharesTotal<- formatVOL(TABLE$NumSharesTotal)
  TABLE$SECForm4 <- as.POSIXct(as.character(TABLE$SECForm4), format ="%b %d %I:%M %p")
  
  # remove dupes
  TABLE <- na.omit(TABLE)
  
  # Define the directory path
  directory_path <- "C:/Users/Dylan/Desktop/STONKS BACKTEST/new insdier project/Filings/"
  
  # Generate a unique filename with a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_name <- paste0("TABLE_", timestamp, ".rds")
  save_path <- file.path(directory_path, file_name)
  
  # Save the TABLE data frame to the specified path
  saveRDS(TABLE, file = save_path)
  
  
  
  
  # list files and subset for December-2020 Transaction only
  FILES = list.files("C:/Users/Dylan/Desktop/STONKS BACKTEST/new insdier project/Filings",full.names = TRUE)
  
  
  # rowbind all transactions and remove duplicates
  ALL = rbindlist(lapply(as.list(FILES), readRDS),use.names = TRUE)
  ALL = unique(ALL)
  # subset ALL to Date Range
  START = "2024-08-1"
  END = Sys.Date()
  ALL = subset(ALL,ALL$Date>=START)
  
  
  # extract unique tickers
  tickers = unique(ALL$Ticker)
  
  # BatchGetSymbols ~ 1064 unique tickers. THIS RETRIEVES ALL TICKER DATA FROM FILINGS.
  data = BatchGetSymbols(tickers=tickers,first.date = START,last.date = END,
                         thresh.bad.data = 0.75,bench.ticker = "^GSPC",
                         type.return = "arit",freq.data = "daily", 
                         how.to.aggregate = "last")
  # saving data for re-run
  saveRDS(data,"stkBATCH_20201130-20210112.rds")
  # data = readRDS("stkBATCH_20201130-20210112.rds")
  # exlcude tickers with bad data
  KEEP = data$df.control[which(data$df.control$threshold.decision == "KEEP"),]
  tickers = KEEP$ticker
  # stock data
  data = data$df.tickers
  
  # *********# *********# *********# *********
  # "KEEP" MATRIX HAS ALL UNIQUE TICKERS.
  # *********# *********# *********# *********
  
  # DV FILTER ******************************
  # Dollar-Volume Penalty
  DV = pblapply(as.list(tickers),function(tic){
    dv = subset(data,data$ticker == tic)
    dv$DVol = dv$price.close * dv$volume
    stk_dv = round(as.data.frame(mean(dv$DVol)),2)
    colnames(stk_dv) = tic
    as.data.frame(cbind(tic,t(stk_dv)))
  })
  DV = rbindlist(DV,use.names = TRUE, fill = TRUE)
  DV$V2 = as.numeric(DV$V2)
  # subset by top 0.5%
  minQ = quantile(DV$V2,seq(0,1,0.005))["99.5%"]
  DV = subset(DV, DV$V2 > minQ)
  
  tickers = DV$tic
  
  # Filter the data where price.adjusted is between $1 and $40
  filtered_data <- data %>%
    filter(price.adjusted >= 1 & price.adjusted <= 150)
  
  # Extract the list of tickers that match the price range
  filtered_tickers <- filtered_data$ticker
  
  # Display the filtered tickers
  filtered_tickers = unique(filtered_tickers)
  print(filtered_tickers)
  print(DV)
  
  # ******************************************************
  #   CONVERT TICKERS INTO CHARTS
  # ******************************************************
  # Create a function to plot Stock Prices Against Insider Transactions
    getInsiderCharts = function(symbol, hold)
    {
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
      stk_data = subset(data, data$ticker == symbol)
      # subset insider transaction
      insdr_tr = subset(ALL,ALL$Ticker == symbol)
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
        if((which(!is.na(stk_data$sig)) + hold) > nrow(stk_data))
        {
          stk_data$sig[which(!is.na(stk_data$sig)):nrow(stk_data)] <- na.locf(stk_data$sig)
          stk_data$sig[is.na(stk_data$sig)] <- 0
          stk_data$strat_ret = stk_data$ret.adjusted.prices * stk_data$sig
          stk_data[is.na(stk_data)]<-0
        }else{
          WHERE = which(!is.na(stk_data$sig))
          stk_data$sig[WHERE:(WHERE+hold)] <- na.locf(stk_data$sig[WHERE:(WHERE+hold)])
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
      pdf(paste0(getwd(), "/Insiders/", symbol, ".pdf"), paper = "special")
      
      # Plot stock chart
      quantmod::chartSeries(stk_xts[,1:5], type = "candlesticks", name = symbol)
      
      # Add insider transactions - by Transaction Date
      quantmod::addTA(stk_xts$stkTA, col = "white", legend = "Buy/Sell")
      
      # Cumulative Sum - by Transaction Date
      quantmod::addTA(cumsum(stk_xts$stkTA), col = "white", legend = "Cumulative Sum")
      
      # Add insider transactions - by Release Date
      quantmod::addTA(stk_xts$stkTA2, col = "yellow", legend = "Release Date")
      
      # Add vertical lines for transaction and release dates
      quantmod::addLines(v = which(index(stk_xts) %in% tDates$Date), on = -1, col = "white")
      quantmod::addLines(v = which(index(stk_xts) %in% rDates$SECForm4), on = -1, col = "yellow")
      
      # Close the graphics device to save the PDF
      graphics.off()
      
      # return stk_data
      stk_data
      
      getwd()
      
      
      # ORIGINAL PDF CODE *************************************
          # IN ORDER TO USE THIS CODE,
          # MUST WAIT 14 DAYS AFTER FILING TO CALL getInsiderCharts FUNCTION FOR THE DESIRED TICKER
          # DO NOT REPLACE until have enough data
      #  ************************************************************************************************************** 
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
      
      
    }
    
  # EXAMPLE CODE -
  if(!file.exists(paste0(getwd(),"/Insiders"))) {dir.create(paste0(getwd(),"/Insiders"))}
  tmp = getInsiderCharts(symbol="BRK-A",hold = 3)

  
  
  
  # example function that converts each ticker from $1-$150 and saves them as a pdf in the WD
  res = pblapply(as.list(filtered_tickers),function(x){
    tmp = try(getInsiderCharts(symbol = x, hold=14))
    if(!inherits(tmp,'try-error')) tmp
  })
  
  # eliminate empty lists
  res = res[lapply(res,length)>0] 