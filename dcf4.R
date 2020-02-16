library(xml2)
library(rvest)
library(stringr)

#global variable constants

#################################################
###############  removeRows #####################

removeRows <- function(df, rows_to_remove) {
  df[-which(rownames(df) %in% rows_to_remove),]
}

#################################################
############## get data between elements #####
#[first Start word, last End Word) 
getDataBetween <- function(vec, start_word, end_word) {
  start_index <- which(vec == start_word)[1]
  end_index <- which(vec == end_word)
  end_index <- end_index[length(end_index)]
  vec[(start_index):(end_index-1)]
}

#[first Start word, first End Word) 
getDataBetween2 <- function(vec, start_word, end_word) {
  start_index <- which(vec == start_word)[1]
  end_index <- which(vec == end_word)[1]
  vec[(start_index):(end_index-1)]
}



##################################################
###############    getURL     ####################

#Input:
# source - character - either Nasdaq, Zacks, Yahoo, or WSJ
# financial - character - either 'income statement', 'cash flow statement', 'balance sheet'
# ticker - ticker of company 

#Output: 
# returns url at which information is locatied
getURL <- function(source, financial, ticker) {
  switch(
    source,
    "Nasdaq" = switch(
      financial,
      "income statement" = paste0("https://www.nasdaq.com/symbol/", tolower(ticker), "/financials?query=income-statement"),
      "cash flow statement" = paste0("https://www.nasdaq.com/symbol/", tolower(ticker), "/financials?query=cash-flow"),
      "balance sheet" = paste0("https://www.nasdaq.com/symbol/", tolower(ticker), "/financials?query=balance-sheet")
     ),

    "Zacks" = switch(
      financial,
      "income statement" = paste0("https://www.zacks.com/stock/quote/", toupper(ticker),"/income-statement"),
      "cash flow statement" = paste0("https://www.zacks.com/stock/quote/", toupper(ticker),"/cash-flow-statements"),
      "balance sheet" = paste0("https://www.zacks.com/stock/quote/", toupper(ticker),"/balance-sheet")
    ),
    
    "Yahoo" = switch(
      financial,
      "income statement" = paste0(),
      "cash flow statement" = paste0(),
      "balance sheet" = paste0()
     ),
    
    
    "WSJ" = switch(
      financial,
      "income statement" = paste0(),
      "cash flow statement" = paste0(),
      "balance sheet" = paste0()
    )
    
  )
    
}

########                              ############
##################################################


#################################################
#########         processNSDQ         ###########

#Input: page - read_html(url) - html page from which to scrape
#       strings_to_remove - character vector - remove all elements in this vector from data

#Output: data frame of data, top row being dates with most recent year at the leftmost
#        numbers are in thousands, contain commas and dollar signs, negatives represented by ()


processNSDQ <- function(page, strings_to_remove) {
  num_years_nsdq <- 5
  page <- html_text(html_nodes(page, 
                               xpath = '//*[@id="financials-iframe-wrap"]'))
  page <- gsub("[\r]", '', page) #remove \r characters
  page <- unlist(stringr::str_split(page, "\n")) #split on \n 
  page <- stringr::str_trim(page) #trim white spaces
  page <- page[page != ""] #only keep those that arent blank
  page <- page[!(page %in% c(strings_to_remove))] #remove strings to remove
  print(page)
  page <- page[which(page == "Trend"):length(page)] #start data from 'Trend'
  page <- split(page, ceiling(seq_along(page) / num_years_nsdq))
  page <- t(data.frame(page))
  page
}


########                              ############
##################################################


##################################################
########## getDataNasdaq ########################

#Input: ticker - character
#Output: data frame with relevant data

getDataNasdaq <- function(ticker) {
  
  #load each of the html_pages
  incomeStatementPage <- xml2::read_html(getURL("Nasdaq", "income statement", ticker))
  cashFlowStatementPage <- xml2::read_html(getURL("Nasdaq", "cash flow statement", ticker))
  balanceSheetPage <- xml2::read_html(getURL("Nasdaq", "balance sheet", ticker))

  #set the text that we want to remove 
  is_remove <- c("Operating Expenses")
  cf_remove <- c("Cash Flows-Operating Activities", 
                 "Changes in Operating Activities", 
                 "Cash Flows-Investing Activities", 
                 "Cash Flows-Financing Activities")
  bs_remove <- c("Current Assets", 
                 "Long-Term Assets", 
                 "Current Liabilities", 
                 "Stock Holders Equity")
  
  
  #process all of the data
  issheet <- processNSDQ(incomeStatementPage, is_remove)
  bssheet <- processNSDQ(balanceSheetPage, bs_remove)
  cfsheet <- processNSDQ(cashFlowStatementPage, cf_remove)
  
  bssheet <- bssheet[-1,] # remove the duplicated 'Trend' row which is in issheet
  cfsheet <- cfsheet[-1,] # remove the duplicated 'Trend' row which is in issheet
  
  #put all data into one data frame
  allsheet <- rbind(issheet, cfsheet, bssheet)
  rownames(allsheet) <- as.character(allsheet[,1])
  
  rows_to_take <- c("Trend", "Total Revenue", "Cost of Revenue", "Gross Profit","Research and Development",
                    "Sales, General and Admin." , "Non-Recurring Items" ,  "Other Operating Items" , "Operating Income"  ,
                    "Add'l income/expense items", "Earnings Before Interest and Tax" , "Interest Expense" , "Earnings Before Tax",
                    "Income Tax", "Depreciation" , "Total Current Assets" , "Total Current Liabilities",
                    "Capital Expenditures")
  
  reducedsheet <- allsheet[rows_to_take,-1] #take rows, eliminate first column which just contains duplicated rownames
  colnames(reducedsheet) <- reducedsheet[1,] #change col names to years
  reducedsheet <- reducedsheet[-1,] #remove first row (which contained years)
  
  reducedsheet <- reducedsheet[, rev(1:ncol(reducedsheet))]#format years so that leftmost is the oldest
  
  reducedsheet <- gsub("[,)\\$]", "", reducedsheet) #remove strange text characters
  reducedsheet <- gsub("\\(", "-", reducedsheet) #replace parenthesis with negative sign
  reducedsheet <- as.matrix(reducedsheet) #make it a matrix so we can change class to numeric
  class(reducedsheet) <- "numeric" #change class to numeric
  
  #divide by 1000 to make the numbers in millions
  reducedsheet <- reducedsheet / 1000
  
  #######  add operating expense line #########
  operating_expenses_row <- reducedsheet["Research and Development",] + 
    reducedsheet["Sales, General and Admin.",] + 
    reducedsheet["Non-Recurring Items",] + 
    reducedsheet["Other Operating Items",]
  
  rows_to_remove <- c("Research and Development", "Sales, General and Admin.", "Non-Recurring Items", "Other Operating Items")
  reducedsheet <- removeRows(reducedsheet, rows_to_remove)
  
  opex_line <- which(rownames(reducedsheet) == "Gross Profit")
  reducedsheet <-rbind(reducedsheet[1:opex_line,], operating_expenses_row, reducedsheet[(opex_line+1): nrow(reducedsheet),]) #insert opex line
  rownames(reducedsheet)[opex_line + 1] <- "Operating Expenses"
  ################################################
  
  ########## make subtracted values negative #########
  reducedsheet[c("Cost of Revenue", "Operating Expenses", "Interest Expense", "Income Tax"), ] <-
    reducedsheet[c("Cost of Revenue", "Operating Expenses", "Interest Expense", "Income Tax"), ] * -1
  ####################################################
  
  ########## add change in net working capital ############
  net_working_capital <- reducedsheet["Total Current Assets",] -  reducedsheet["Total Current Liabilities",]
  change_in_nwc <- net_working_capital[-1] - net_working_capital[-ncol(reducedsheet)]
  change_in_nwc <- c(NA, change_in_nwc) #add NA since we cant compute change for the first year
  change_in_nwc <- change_in_nwc * -1 #Make it negative so that we can sum everything
  reducedsheet <- rbind(reducedsheet, change_in_nwc)
  
  reducedsheet <- reducedsheet[-which(rownames(reducedsheet) %in% c("Total Current Assets", "Total Current Liabilities")),]
  
  rownames(reducedsheet)[which(rownames(reducedsheet) == "change_in_nwc")] <- "Negative Change in Net Working Capital"
  
  ########## add new taxes row (unlevered taxes) ##########
  implied_tax <- reducedsheet["Income Tax",] / reducedsheet["Earnings Before Tax",]
  unlevered_tax <- reducedsheet["Earnings Before Interest and Tax",] * implied_tax
  
  
  unlevered_net_income <- reducedsheet["Earnings Before Interest and Tax",] + unlevered_tax
  
  ##############remove remaining unnecessary rows############
  
  reducedsheet <- removeRows(reducedsheet, c("Interest Expense", "Earnings Before Tax", "Income Tax"))
  
  ############## insert new rows #############################
  
  
  reducedsheet <- rbind(reducedsheet, unlevered_tax, unlevered_net_income)
  reducedsheet <- reducedsheet[c(1:7, 11, 12, 8:10),]
  
  ################# rename rows ##############
  final_row_names <- c("Revenue", "COGS", "Gross Profit", "Operating Expenses", 
                       "Operating Income", "Add'l Income", "EBIT", "Unlevered Tax", 
                       "Unlevered Net Income", "Depreciation", "Capital Expenditures",
                       "Negative Change in Net Working Capital")
  
  rownames(reducedsheet) <- final_row_names
  
  free_cash_flow <- reducedsheet["Unlevered Net Income",] + reducedsheet["Depreciation",] + reducedsheet["Capital Expenditures",] + reducedsheet["Negative Change in Net Working Capital",]
  reducedsheet <- rbind(reducedsheet, free_cash_flow)
  rownames(reducedsheet)[nrow(reducedsheet)] <- "Free Cash Flow" #rename last row
  round(reducedsheet, 2)


}


processZacks <- function(page, begin_data_word, end_data_word, begin_date_word, end_date_word) {
  raw_data <- html_text(html_nodes(page, "td")) #get raw data
  data_to_get_dates <- html_text(html_nodes(page, "th")) #get date information
  
  raw_data <- getDataBetween(raw_data, begin_data_word, end_data_word) #relevant income statement data 
  
  dates <- getDataBetween(data_to_get_dates, begin_date_word, end_date_word) #get dates
  dates <- dates[dates != ""] #remove anything blank from dates
  
  raw_data <- split(raw_data, ceiling(seq_along(raw_data) / (length(dates) + 1))) #split data by row
  
  if (end_data_word == "Mortgages") { #if we are working with the balance sheet
    dates <- getDataBetween2(data_to_get_dates, begin_date_word, end_date_word)
    dates <- dates[dates != ""]
    begin <- which(unlist(raw_data) == "Total Current Assets")[1]
    end <- which(unlist(raw_data) == "Total Current Liabilities")[1]
    raw_data <- unlist(raw_data)[c(begin:(begin+length(dates)), end:(end+length(dates)))]
    raw_data <- split(raw_data, ceiling(seq_along(raw_data) / (length(dates) + 1)))
  }
  
  df <- t(data.frame(raw_data)) #turn into a data frame and orient so that rownames on the left

  data_mx <- as.matrix(df[,-1]) #get data that isnt the rownames in the first column
  data_mx <- gsub(",", "", data_mx) #take out commas so we can make the matrix numeric
  class(data_mx) <- "numeric"
  data_mx <- data_mx[, rev(1:ncol(data_mx))] #reverse the order of the matrix

  data_df <- data.frame(data_mx)

  colnames(data_df) <- rev(dates) #assign reversed dates as column names
  rownames(data_df) <- as.character(df[,1]) #assign row names
  
  round(data_df, 2)

}




getNetDebt <- function(ticker) {

  url <- sprintf("https://tradingeconomics.com/%s:us:net-debt", tolower(ticker))
  page <- read_html(url)
  
  #get relevant data
  text <- html_text(html_nodes(page, ".col-xs-6"))
  text <- stringr::str_trim(unlist(stringr::str_split(text, "\r\n")))
  text <- text[text != ""]
  text <- text[2]
  
  #replace the B with billion and M with million
  text <- unlist(str_split(text, " "))[1]
  text <- gsub("B", " billion", text)
  text <- gsub("M", " million", text)
  
  #break up the amount from the denomination
  amount <- as.numeric(unlist(str_split(text, " "))[1])
  denom <- unlist(str_split(text, " "))[2]
  
  #adjust the result
  result <- 0
  if (denom == "million") {
    result <- amount
  }
  if (denom == "billion") {
    result <- (amount * 1000)
  }
  
  result
}

######returns the shares outstanding in millions#############
getSharesOutstanding <- function(ticker) {
    url <- paste0("https://www.nasdaq.com/symbol/", tolower(ticker))
    page <- read_html(url)
    
    #####get all of the data and format it
    data <- html_text(html_nodes(page, "h2+ .span-1-of-2 .fontS14px"))
    data <- gsub("\\r", "", data)
    data <- stringr::str_trim(unlist(stringr::str_split(data, "\\n")))
    data <- data[data != ""]
    
    #######get the market cap
    mcap <- data[which(data == "Market Cap") + 1]
    
    #########get the closing price
    close <- html_text(html_nodes(page, "#qwidget_lastsale"))
    close <- gsub("\\$", "", close)
    close <- gsub(",", "", close)
    close <- stringr::str_trim(close)
    
    mcap <- gsub(",","", mcap)
    mcap <- stringr::str_trim(mcap)
    
    close <- as.numeric(close)
    mcap <- as.numeric(mcap)
    
    ###### calculate the shares outstanding
    (mcap/close) / 1000000
}

getDataZacks <- function(ticker) {
  
  #load each of the html_pages
  incomeStatementPage <- xml2::read_html(getURL("Zacks", "income statement", ticker))
  cashFlowStatementPage <- xml2::read_html(getURL("Zacks", "cash flow statement", ticker))
  balanceSheetPage <- xml2::read_html(getURL("Zacks", "balance sheet", ticker))
  
  #process all of the data
  issheet <- processZacks(incomeStatementPage, "Sales", "Income Before Depreciation & Amortization", "", "Depreciation Footnote")
  cfsheet <- processZacks(cashFlowStatementPage, "Net Income (Loss)", "Issuance (Repurchase) of Capital Stock", "", "")
  bssheet <- processZacks(balanceSheetPage, "Cash & Equivalents", "Mortgages" ,"", "Liabilities & Shareholders Equity")
  
  #####
  names(issheet) <- names(cfsheet)
  names(bssheet) <- names(cfsheet)
  allsheet <- rbind(issheet, cfsheet, bssheet)
  
  
  ####remove rows#######
  
  reducedsheet <- removeRows(allsheet, c("Interest Expense", "Minority Interest", "Investment Gains/Losses" , "Other Income/Charges", 
                                         "Income From Cont. Operations" , "Extras & Discontinued Operations" , "Net Income" , "Net Income (Loss)",
                                         "Net Change from Assets/Liabilities", "Net Cash from Discontinued Operations", "Other Operating Activities",
                                         "Net Cash From Operating Activities", "Acquisition/ Disposition of Subsidiaries", "Investments", "Other Investing Activities", 
                                         "Net Cash from Investing Activities" ))
  
  check <- (sum(is.na(reducedsheet[,ncol(reducedsheet)])) / nrow(reducedsheet)) #remove column if there are two many NAs
  
  if (check > 0.2) {
    reducedsheet <- reducedsheet[,-ncol(r)]
  }
  
  ############make subtracted values negative
  reducedsheet[c("Cost Of Goods", "Selling & Adminstrative & Depr. & Amort Expenses", "Income Taxes"),] <-
    reducedsheet[c("Cost Of Goods", "Selling & Adminstrative & Depr. & Amort Expenses", "Income Taxes"),] * -1
  
  
  ########## add change in net working capital ############
  net_working_capital <- reducedsheet["Total Current Assets",] -  reducedsheet["Total Current Liabilities",]
  change_in_nwc <- net_working_capital[-1] - net_working_capital[-ncol(reducedsheet)]
  change_in_nwc <- unlist(c(NA, change_in_nwc)) #add NA since we cant compute change for the first year
  change_in_nwc <- change_in_nwc * -1 #Make it negative so that we can sum everything
  
  reducedsheet <- rbind(reducedsheet, change_in_nwc)
  
  reducedsheet <- reducedsheet[-which(rownames(reducedsheet) %in% c("Total Current Assets", "Total Current Liabilities")),]
  
  ########## add new taxes row (unlevered taxes) ##########
  implied_tax <- reducedsheet["Income Taxes",] / reducedsheet["Pretax Income",]
  
  ebit <- reducedsheet["Income After Depreciation & Amortization",] + reducedsheet["Non-Operating Income",]
  rownames(ebit) <- "ebit"
  unlevered_tax <- ebit * implied_tax
  rownames(unlevered_tax) <- "unlevered_tax"
  unlevered_net_income <- ebit + unlevered_tax
  rownames(unlevered_net_income) <- "unlevered_net_income"
  ##############remove remaining unnecessary rows############
  
  reducedsheet <- removeRows(reducedsheet, c("Pretax Income", "Income Taxes"))
  
  ############## insert new rows #############################
  
  
  reducedsheet <- rbind(reducedsheet, unlevered_tax, unlevered_net_income, ebit)
  reducedsheet <- reducedsheet[c(1:6, 12, 10, 11, 7, 8, 9),]
  
  ################# rename rows ##############
  final_row_names <- c("Revenue", "COGS", "Gross Profit", "Operating Expenses", 
                       "Operating Income", "Add'l Income", "EBIT", "Unlevered Tax", 
                       "Unlevered Net Income", "Depreciation", "Capital Expenditures",
                       "Negative Change in Net Working Capital")
  
  rownames(reducedsheet) <- final_row_names
  
  free_cash_flow <- reducedsheet["Unlevered Net Income",] + reducedsheet["Depreciation",] + reducedsheet["Capital Expenditures",] + reducedsheet["Negative Change in Net Working Capital",]
  reducedsheet <- rbind(reducedsheet, free_cash_flow)
  rownames(reducedsheet)[nrow(reducedsheet)] <- "Free Cash Flow" #rename last row
  round(reducedsheet, 2)
  
}


getDataYahoo <- function(ticker) {
  
}

getDataWSJ <- function(ticker) {
  
}

getDataSeekingAlpha <- function(ticker) {
  
}
