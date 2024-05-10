library(yahoofinancer)
library(tidyverse)
list.files(pattern = ".csv")
stock_files <- list.files("screaning_data", pattern = ".csv")
ticker <- stock_files %>% stringr::str_remove(".csv")

download_data <- function(ticker){
  print(ticker)
  Sys.sleep(5)
  data <- Ticker$new(ticker)
  data <- data$get_history(start = '2016-01-01', interval = '1d')
  data$ticker = ticker
  data %>% 
    dplyr::filter(!is.na(volume))
}
stock_data <- purrr::map(ticker, download_data)

save_csv_data <- function(data){
  # data <- stock_data[[8]]
  ticker <- unique(data$ticker)
  print(ticker)
  name_file <- paste0("./screaning_data/", ticker, ".xlsx")
  openxlsx::write.xlsx(data, name_file)
}

purrr::map(stock_data, save_csv_data)

