install.packages("curl")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("lubridate")
# install.packages("magrittr")
# install.packages("purrr")
# install.packages("R6")
# install.packages("stringr")
install.packages("yahoofinancer", dependencies = TRUE)
library(yahoofinancer)

tickers <- readRDS("tickers.rds")
tickers <- tickers[1:10]

download_data <- function(ticker){
  tryCatch(
    {
      Sys.sleep(2)
      print(ticker)
      data <- Ticker$new(ticker)
      # data <- data$get_history(start = Sys.Date(), interval = '1d')

      # data <- data$get_history(start = as.character(as.Date(Sys.Date() - 1)), interval = '1d')
      data <- data$get_history(start = '2016-01-01', interval = '1d')
      data$ticker = ticker
      data <- as.data.frame(subset(data, !is.na(volume)))
      name_file <- paste0("./data/", ticker, ".txt")
      write.table(x = data, file = name_file, sep = "\t", dec = ".", append = TRUE)
      # write.table(data, "./data/data.txt", append = TRUE, row.names = FALSE, col.names = FALSE)
      # Sys.Date()
      # as.character(as.Date(Sys.Date() - 1))
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

lapply(tickers, download_data)