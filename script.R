#install.packages("curl")
#install.packages("openxlsx")
#install.packages("yahoofinancer", dependencies = TRUE)
library(yahoofinancer)

tickers <- readRDS("tickers.rds")
#tickers <- tickers

if (!dir.exists("data")) {
  # Create the directory if it does not exist
  dir.create("data")
}

download_data <- function(ticker){
  tryCatch(
    {
      Sys.sleep(2)
      print(ticker)
      data <- Ticker$new(ticker)
      # data <- data$get_history(start = Sys.Date(), interval = '1d')

      # data <- data$get_history(start = as.character(as.Date(Sys.Date() - 1)), interval = '1d')
      data <- data$get_history(start = '2016-01-01', interval = '1d')
      #data <- data$get_history(start = Sys.Date(), interval = '1d')
      
      data <- as.data.frame(subset(data, !is.na(volume)))
      data$ticker = ticker
      #print(data)
      #name_file <- paste0("./data/", ticker, ".txt")
      name_file <- paste0("./data/", ticker, ".xlsx")
      openxlsx::write.xlsx(data, file = name_file)
      #write.table(x = data, file = name_file, sep = "\t", dec = ".", row.names = FALSE)
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
