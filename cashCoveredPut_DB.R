require("quantmod");require("data.table");require("lubridate");require("pbapply");require("DBI")
require("RSQLite")

# assign DB Password
PASS <- new.env()
assign("pwd","DB_PASSWORD",envir=PASS)
assign("RHusrnm","RobinHood_UserName",envir=PASS)
assign("RHpwd","RobinHood_Password",envir=PASS)

# add 30 days to get next expiration
DATES = seq.Date(from=as.Date("2021-04-16"), to=Sys.Date()+30, by = "1 day")
# create NA xts to extract option expiration dates
NAxts = xts(rep(NA,length(DATES)), order.by = DATES)
DATES = DATES[options.expiry(NAxts)]
# **********************************************************************************************
#                               function to get Options from MySQL DB
# **********************************************************************************************
getOpsbySymbol = function(ticker,open_date,next_expiry){
  # connect to database
  library(DBI)
  con <- dbConnect(odbc(), Driver = "/usr/local/mysql-connector-odbc-8.0.27-macos11-x86-64bit/lib/libmyodbc8a.so", 
                   Server = "localhost", Database = "DATA", UID = "root", PWD = PASS$pwd, 
                   Port = 3306)
  ops = as.data.frame(dbGetQuery(con,paste0("SELECT * FROM CBOE_OPTIONS WHERE Symbol='",ticker,
                                            "' AND Date='",open_date,
                                            "' AND expiry='",next_expiry,"'", 
                                            "AND flag = 'P' ")))
  
  dbDisconnect(con)
  ops
}
# **********************************************************************************************
#                                       covered put strategy
# **********************************************************************************************
# expiry_dates   : Pass in vector of expiration dates & read in the files/options on those days
# symbol         : Optionable stock/etf symbol
# strikesBelowATM: How many strikes below ATM should be used to write
coveredPutStrat = function(expiry_dates, symbol, strikesBelowATM){
  # pass in the expirations and extract options for ticker 
  ops = lapply(as.list(2:length(expiry_dates)), function(ii){
    # assign open_date & next_expiry (1-month expirations)
    open_date   = expiry_dates[ii-1]
    next_expiry = expiry_dates[ii]
    # read in options
    op = getOpsbySymbol(ticker=symbol,open_date = open_date, next_expiry = next_expiry)
    # get the ATM put i.e. the closest strike to the last traded stock price
    op$stkPrc2strike =  as.numeric(op$strike) - as.numeric(op$stkClose)
    if(strikesBelowATM == 0){
      # eliminate OTM puts
      op = subset(op,op$stkPrc2strike > 0)
      # which has the least difference to the stk Close
      ATM = op[which.min(op$stkPrc2strike),]
    }else{
      # if strikesBelowATM is below MAX strikes available, last strike will be selected:
      if(strikesBelowATM > length(op$stkPrc2strike)){strikesBelowATM = length(op$stkPrc2strike)}
      # get the OTM puts
      ATM = op[which(op$stkPrc2strike < 0),]
      # sort lowest to highest by stkPrc2strike
      ATM = ATM[order(ATM$stkPrc2strike, decreasing = TRUE),]
      # select the put by # of strikes OTM
      ATM = ATM[strikesBelowATM,]
    }
    # extract desired columns
    ATM = ATM[,c("Date","expiry","days2Exp","stkClose","strike","Mid")]
    colnames(ATM) = c("openDate","expiry","days2exp","open_stkPrc","strike","put_premium")
    ATM
  })
  # combine options
  ops = rbindlist(ops,use.names = TRUE, fill = TRUE)
  # get latest quote for the last expiration
  lastPrc = getQuote(symbol)$Last
  # add expiration prices to ops
  ops$prcAtexp = c(ops$open_stkPrc[2:nrow(ops)],lastPrc)
  # calculate the net premium received
  ops$stk2strike = (as.numeric(ops$prcAtexp)-as.numeric(ops$strike))

  ops$netPremium = ifelse(ops$stk2strike>0, 
                          as.numeric(ops$put_premium), 
                          as.numeric(ops$put_premium)+ops$stk2strike)
  # calculate returns
  ops$ccpRet = round(ops$netPremium/ops$strike,4)
  ops$stkRet = round(ops$prcAtexp/ops$open_stkPrc-1,4)
  
  # return results
  ops
}

# test function
tmp0 = coveredPutStrat(expiry_dates = DATES, symbol = "SPY", strikesBelowATM = 0)
tmp1 = coveredPutStrat(expiry_dates = DATES, symbol = "SPY", strikesBelowATM = 1)
# **********************************************************************************************
#                           get covered put strategy returns for symbols
# **********************************************************************************************
#tickers = c("AAPL","SPY","F","EBAY","QQQ","TSLA")

getTickers = function(minPRC,maxPRC, asOf)
{

  library(DBI)
  con <- dbConnect(odbc(), Driver = "/usr/local/mysql-connector-odbc-8.0.27-macos11-x86-64bit/lib/libmyodbc8a.so",
                   Server = "localhost", Database = "DATA", UID = "root", PWD = PASS$pwd,
                   Port = 3306)
  ops = as.data.frame(dbGetQuery(con,paste0("SELECT DISTINCT Symbol,stkClose FROM CBOE_OPTIONS WHERE (stkClose>=",minPRC,
                                            " AND stkClose<=",maxPRC,") AND Date='",asOf,"'")))

  dbDisconnect(con)
  # delete index options
  ops = ops[!str_detect(ops$Symbol,"\\_"),]
  ops
}
tickers = getTickers(minPRC = 10, maxPRC = 30, asOf="2021-12-10")
tickers = as.character(tickers$Symbol)

# apply function to selected symbols
getRets = lapply(as.list(tickers), function(x){
  # print current stock
  cat("\n",x," | #",which(x == tickers))
  # get results
  tmp = try(coveredPutStrat(expiry_dates = DATES,symbol = x, strikesBelowATM = 0),silent = TRUE)
  # if error - OUT (OUTPUT will be null)
  if(inherits(tmp,'try-error') | length(tmp) == 0 | is.null(tmp)){
    OUT <- NULL
  }else{
    if(nrow(tmp) == 0){OUT=NULL}else{
      # get Covered put Summary + Sharpe
                                # sums the difference between the stock price @ exp to strike
      OUT = as.data.frame(cbind(x,round(sum(tmp$stk2strike),2),
                                # sum of all the gross put premium
                                sum(tmp$put_premium), 
                                # sum of the net premium
                                round(sum(tmp$netPremium),2),
                                # average stock price, cash-covered put return, and stock return
                                round(mean(tmp$open_stkPrc),2),round(mean(tmp$ccpRet),4),round(mean(tmp$stkRet),4),
                                # sum of the all the cash-covered return as a %
                                round(sum(tmp$ccpRet),4),
                                # total stock return over the back-test
                                round(tmp$prcAtexp[length(tmp$prcAtexp)]/tmp$prcAtexp[1]-1,4),
                                # number of months that the strategy was back-tested
                                length(tmp$stk2strike),
                                # number of losing months 
                                length(tmp$stk2strike[tmp$stk2strike < 0]),
                                # number of winning months (gross)
                                length(tmp$stk2strike[tmp$stk2strike > 0]),
                                # number of winning months (net)
                                length(tmp$netPremium[tmp$netPremium > 0]),
                                # NET winning Percentage
                                round(length(tmp$netPremium[tmp$netPremium > 0])/ length(tmp$stk2strike),4)
                                ))
      # assign column names
      colnames(OUT) = c("Symbol","mktPoints","grossPremium","netPremium","avgStkPRC",
                        "avgccpRet","avgSTKret","totalccpRet","buyNholdRet","N","nLosing",
                        "nGaining","netGains","Net_pctWin")
      # add Sharpe Ratio
      OUT$ccSharpe = round(mean(tmp$ccpRet)/sd(tmp$ccpRet),2)
      OUT$stkSharpe = round(mean(tmp$stkRet)/sd(tmp$stkRet),2)
    }}
  # return OUT (summary)
  OUT
})

# rbind results
res = rbindlist(getRets,use.names=TRUE,fill=TRUE)
# add efficiency ratio: Net premium / Gross premium
res$eff = round(as.numeric(res$netPremium)/as.numeric(res$grossPremium),4)
# save.image("ccp2021test.RData")
# load("ccp2021test.RData")

# complete backtests only: (N == MAX(N))
ccp = subset(res, res$N == 8)
# strategy sharpe >= 1.00
ccp = subset(ccp, ccp$ccSharpe >=1)
# net Premium >= 8 points ... i.e. at least 1.00/month in NET premium
ccp = subset(ccp, ccp$netPremium >= 8)
# efficiency 
ccp = subset(ccp, ccp$eff >= 0.7)

# function to get premiums for next expiration
getCurrentCCP = function(symbol, expiration, asOf){
  
  library(DBI)
  con <- dbConnect(odbc(), Driver = "/usr/local/mysql-connector-odbc-8.0.27-macos11-x86-64bit/lib/libmyodbc8a.so", 
                   Server = "localhost", Database = "DATA", UID = "root", PWD = PASS$pwd, 
                   Port = 3306)
  ops = as.data.frame(dbGetQuery(con,paste0("SELECT * FROM CBOE_OPTIONS WHERE expiry='",expiration,"'",
                                            " AND Symbol='",symbol,"' AND Date='",asOf,"' AND flag='P'")))
  
  dbDisconnect(con)
  # calculate the difference between the stock and strike
  ops$stk2strk =  as.numeric(ops$strike) - as.numeric(ops$stkClose)
  # eliminate the negative's (OTM)
  ops = subset(ops, as.numeric(ops$stk2strk)>0)
  # ATM
  ATM = ops[which.min(ops$stk2strk),]
  ATM$NetPrem = as.numeric(ATM$Mid) - as.numeric(ATM$stk2strk)
  ATM$PctRet = round(as.numeric(ATM$NetPrem)/as.numeric(ATM$stkClose),4)
  # subset columns
  ATM = ATM[,c("Symbol","expiry","stkClose","strike","Mid","stk2strk","NetPrem","PctRet")]
  ATM
}

# returns a table for next expiration
nextExp = pblapply(as.list(ccp$Symbol), function(x){
  getCurrentCCP(symbol = x, expiration="2021-12-17",asOf = "2021-12-10")
})
# row bind 
nextExp = rbindlist(nextExp, use.names=TRUE, fill=TRUE)
nextExp = nextExp[1:2,]
# *************************************************************************************
#                                 Interactive Brokers
# *************************************************************************************
# function to send orders to Interactive Brokers
coveredputOrders =  function(nextExp, limit){
  
  if(limit==1){
    require("IBrokers")
    tws = twsConnect(port=7497)
    ac <- reqAccountUpdates(tws)
    
    # assign parameters
    stk    = nextExp$Symbol[1] 
    expiry = nextExp$expiry[1]
    strike = nextExp$strike[1]
    lmtPRC = nextExp$Mid[1]   

    # create tws Contract
    contract2 <- twsContract()
    contract2$symbol <- stk
    contract2$currency <- "USD"
    contract2$sectype <- "OPT"
    contract2$expiry <- format(expiry,format="%Y%m%d") # "20211217"
    contract2$right  <- "P"
    contract2$strike <- strike
    contract2$primary<- "NASDAQ"
    CONTRACT <- reqContractDetails(tws,contract2)
    # my option order
    opt = CONTRACT[[1]]$contract
    
    orderID = as.numeric(reqIds(tws))
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(lmtPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    
    # place option order
    placeOrder(tws,opt, myorder)
    
    twsDisconnect(tws)
    
  }
  if(limit==2){
    
    require("IBrokers")
    tws = twsConnect(port=7497)
    ac <- reqAccountUpdates(tws)
    
    # assign parameters
    stk    = nextExp$Symbol[1] 
    expiry = nextExp$expiry[1]
    strike = nextExp$strike[1]
    lmtPRC = nextExp$Mid[1]   
    
    # create tws Contract
    contract2 <- twsContract()
    contract2$symbol <- stk
    contract2$currency <- "USD"
    contract2$sectype <- "OPT"
    contract2$expiry <- format(expiry,format="%Y%m%d") # "20211217"
    contract2$right  <- "P"
    contract2$strike <- strike
    contract2$primary<- "NASDAQ"
    CONTRACT <- reqContractDetails(tws,contract2)
    # my option order
    opt = CONTRACT[[1]]$contract
    
    orderID = as.numeric(reqIds(tws))
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(lmtPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    
    # place option order
    placeOrder(tws,opt, myorder)
    
    twsDisconnect(tws)
    
    Sys.sleep(5)
    
    require("IBrokers")
    tws = twsConnect(port=7497)
    ac <- reqAccountUpdates(tws)
    
    # assign parameters
    stk    = nextExp$Symbol[2] 
    expiry = nextExp$expiry[2]
    strike = nextExp$strike[2]
    lmtPRC = nextExp$Mid[2]   
    
    # create tws Contract
    contract2 <- twsContract()
    contract2$symbol <- stk
    contract2$currency <- "USD"
    contract2$sectype <- "OPT"
    contract2$expiry <- format(expiry,format="%Y%m%d") # "20211217"
    contract2$right  <- "P"
    contract2$strike <- strike
    contract2$primary<- "NASDAQ"
    CONTRACT <- reqContractDetails(tws,contract2)
    # my option order
    opt = CONTRACT[[1]]$contract
    
    orderID = as.numeric(reqIds(tws))
    myorder      = twsOrder(orderId = orderID,orderType = "LMT",lmtPrice = paste(lmtPRC),
                            outsideRTH = "0",action="SELL", totalQuantity = "1", transmit=FALSE)
    
    # place option order
    placeOrder(tws,opt, myorder)
    
    twsDisconnect(tws)
  }
  
  
}
# send orders to Interactive Brokers
Sys.sleep(2); coveredputOrders(nextExp = nextExp, limit=2)
# *************************************************************************************
#                                 Robinhood
# *************************************************************************************
require("RobinHood")
coveredPut_OrdersRH = function(nextExp, limit){
  
  if(limit == 1){
  Symbol = nextExp$Symbol[1]
  expiry = nextExp$expiry[1]
  strike = nextExp$strike[1]
  lmtPRC = nextExp$Mid[1]   
  # establish connection with RH
  RH = RobinHood(username = PASS$RHusrnm, password = PASS$RHpwd)
  # require contract details to get Put option IDs
  ops = get_contracts(RH, chain_symbol = Symbol,type = "put",detail = TRUE)
  # subset desired contract
  ops = subset(ops, ops$expiration_date == expiry & ops$strike_price == strike)
  # place order
  ID1 = place_order_options(RH=RH, option_id = ops$id,direction = "credit",side = "sell",
                            quantity = 1,type = "limit", stop_price = round(lmtPRC/0.05)*0.05,
                            time_in_force = "gtc")
  logout(RH)
  }
  if(limit == 2){
    Symbol = nextExp$Symbol[1]
    expiry = nextExp$expiry[1]
    strike = nextExp$strike[1]
    lmtPRC = nextExp$Mid[1]   
    # establish connection with RH
    RH = RobinHood(username = PASS$RHusrnm, password = PASS$RHpwd)
    # require contract details to get Put option IDs
    ops = get_contracts(RH, chain_symbol = Symbol,type = "put",detail = TRUE)
    # subset desired contract
    ops = subset(ops, ops$expiration_date == expiry & ops$strike_price == strike)
    # place order
    ID1 = place_order_options(RH=RH, option_id = ops$id,direction = "credit",side = "sell",
                              quantity = 1,type = "limit", stop_price = round(lmtPRC/0.05)*0.05,
                              time_in_force = "gtc")
    logout(RH)
    # ************************************************************************************************
    #                             OPTION ORDER # 2
    # ************************************************************************************************
    Symbol = nextExp$Symbol[2]
    expiry = nextExp$expiry[2]
    strike = nextExp$strike[2]
    lmtPRC = nextExp$Mid[2]   
    # establish connection with RH
    RH = RobinHood(username = PASS$RHusrnm, password = PASS$RHpwd)
    # require contract details to get Put option IDs
    ops = get_contracts(RH, chain_symbol = Symbol,type = "put",detail = TRUE)
    # subset desired contract
    ops = subset(ops, ops$expiration_date == expiry & ops$strike_price == strike)
    # place order
    ID1 = place_order_options(RH=RH, option_id = ops$id,direction = "credit",side = "sell",
                              quantity = 1,type = "limit", stop_price = round(lmtPRC/0.05)*0.05,
                              time_in_force = "gtc")
    logout(RH)
    
  }
  
  
}






