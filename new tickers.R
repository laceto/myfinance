

tickers <- readRDS("tickers.rds")

new_tickers <- c('SOUN', 'GTLB', 'SNOW', 'RGTI', 'APP', 'S', 'RKLB', 'PLTR', 'AI', 'PANW', 'IONQ', 'TEM', 'NDX')

tickers <- c(tickers, new_tickers)

saveRDS(tickers, "tickers.rds")
