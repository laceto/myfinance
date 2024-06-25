# https://www.tradingview.com/markets/stocks-italy/market-movers-all-stocks/
library(tidyverse)
table <- read_lines("all shares IT.txt")

sector <- dplyr::tibble(value = table[32:length(table)]) %>% 
  dplyr::filter(
    value != "",
    stringr::str_detect(value, "%")
  )


a <- dplyr::tibble(value = table[32:length(table)]) %>% 
  dplyr::filter(
    value != "",
    stringr::str_detect(value, "%", negate = T),
    stringr::str_detect(value, "Neutral|buy|Buy|Sell|sell", negate = T),
    !stringr::str_sub(value, 1, nchar(value)) %in% LETTERS
  ) %>% 
  dplyr::pull(value)

data.frame(matrix(a, ncol = 2, byrow = TRUE)) %>% head(40)
a <- c(a[1:66], "B", a[67:length(a)])
data.frame(matrix(a, ncol = 2, byrow = TRUE)) %>% head(100)
a <- c(a[1:186], "D", a[187:length(a)])
data.frame(matrix(a, ncol = 2, byrow = TRUE)) %>% head(180)
a <- c(a[1:334], "G", a[335:length(a)])
tickers <- data.frame(matrix(a, ncol = 2, byrow = TRUE))

# tickers <- tickers %>% 
#   dplyr::bind_cols(sector)


# Split the value column by tab character  
df_sector <- strsplit(sector$value, "\t")  

length_split <- sapply(df_sector, length)

remove_last_element <- function(x) {  
  x <- x[-length(x)]  
  return(x)  
} 

remove_last_element <- function(n, x) {  
  
  if(n == 11){
    x <- x[-length(x)]
  }
  
  
  return(x)  
}  

df_sector <- map2(length_split, df_sector, remove_last_element)


# Convert the list to a data frame  
df_sector <- data.frame(do.call(rbind, df_sector))  

# View the resulting data frame  
tickers <- tickers %>%
  dplyr::bind_cols(df_sector)  



fineco_tickers <- readxl::read_xlsx("./stock screener/stocks_fineco_list.xlsx") %>% 
  dplyr::filter(PV == "AFF") %>% 
  dplyr::rename(
    ticker = Symbol,
    name = Description,
    sector = Sector
  ) %>% 
  dplyr::select(ticker, name, sector)



tickers %>% 
  dplyr::rename(
    ticker = X1...1,
    name = X2...2,
    sector = X10
  ) %>% 
  dplyr::select(
    ticker, name, sector
  ) %>% 
  dplyr::mutate(
    ticker = paste0(ticker, ".MI")
  ) %>% 
  dplyr::bind_rows(
    fineco_tickers
  ) %>% 
  dplyr::distinct() %>% 
  openxlsx::write.xlsx("sectors.xlsx")


tickers

library(yahoofinancer)

# download_data <- function(ticker){
# 
#   Sys.sleep(5)
#   data <- Ticker$new(ticker)
#   data <- data$get_history(start = '2016-01-01', interval = '1d')
#   data$ticker = ticker
#   data <- data %>%
#     dplyr::filter(!is.na(volume))
#   name_file <- paste0("./scouting/", ticker, ".xlsx")
#   openxlsx::write.xlsx(data, name_file) 
#   
# }

ticker <- tickers[1]

# library(tidyv)
# Sys.Date() %>% 
#   stringr::str_remove_all("-")

download_data <- function(ticker){
  tryCatch(
    {
      Sys.sleep(5)
      data <- Ticker$new(ticker)
      data <- data$get_history(start = '2016-01-01', interval = '1d')
      data$ticker = ticker
      data <- data %>%
        dplyr::filter(!is.na(volume))
      name_file <- paste0("./scouting/", ticker, ".xlsx")
      openxlsx::write.xlsx(data, name_file) 
      # return(result)
    },
    error=function(e) {
      message('An Error Occurred')
      print(e)
    },
    warning=function(w) {
      message('A Warning Occurred')
      print(w)
      return(NA)
    }
  )
}

download_data("FTSEMIB.MI")
download_data("NOV.DE")
download_data("NVO")
download_data("LLY")
download_data("LLY.DE")
download_data("SGM.MI")

# fineco_tickers <- readxl::read_xlsx("./stock screener/stocks_fineco_list.xlsx") %>% 
#   dplyr::filter(PV == "AFF") %>% 
#   dplyr::pull(Symbol)


# tickers <- tickers %>% 
#   dplyr::rename(
#     ticker = X1...1,
#     name = X2...2,
#     sector = X10
#   ) %>% 
#   dplyr::select(
#     ticker, name, sector
#   ) %>% 
#   dplyr::mutate(
#     ticker = paste0(ticker, ".MI")
#   ) %>% 
#   dplyr::bind_rows(
#     fineco_tickers
#   ) %>% 
#   dplyr::filter(
#     stringr::str_detect(name, "A2A|TAMBURI|RECORDATI|PIAGGIO|MAIRE|MONCLER|CUCIN|UNIE|BUILD|STM|ACEA|BREM|ENEL|ITALGAS|INTERPUMP")
#   ) %>% 
#   dplyr::group_by(ticker) %>% 
#   dplyr::slice(1) %>% 
#   dplyr::pull(ticker)

tickers %>%
  unique() %>% 
  purrr::map(download_data)


read_rds("tickers.rds") %>% 
  unique()  %>% 
  purrr::map(download_data)

# purrr::map(download_data, "STMPA.MI")
  

stock_list_data <- tickers %>% 
  dplyr::rename(
    symbol = X1...1
  ) %>% 
  dplyr::mutate(
    ticker = paste0(symbol, ".MI"),
    num_id = 1:n()
  ) %>% 
  # dplyr::filter(num_id > 122) %>% 
  dplyr::select(symbol, ticker) %>% 
  dplyr::pull(ticker) %>%
  c(fineco_tickers$ticker) %>% 
  unique() %>% 
  purrr::map(download_data)


