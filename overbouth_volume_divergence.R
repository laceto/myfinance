# library(TTR)

myRSI <- function (price,n){
  N <- length(price)
  U <- rep(0,N)
  D <- rep(0,N)
  rsi <- rep(NA,N)
  Lprice <- Lag(price,1)
  for (i in 2:N){
    if (price[i]>=Lprice[i]){
      U[i] <- 1
    } else {
      D[i] <- 1
    }
    if (i>n){
      AvgUp <- mean(U[(i-n+1):i])
      AvgDn <- mean(D[(i-n+1):i])
      rsi[i] <- AvgUp/(AvgUp+AvgDn)*100 
    }
  }
  rsi <- reclass(rsi, price)
  return(rsi)
}


volume_divergence <- function(data){
  
  data %>% 
    dplyr::mutate(
      diff_volume = volume - dplyr::lag(volume),
      diff_close = rclose - dplyr::lag(rclose),
      volume_divergence = diff_volume < 0 & diff_close > 0
    ) %>% 
    dplyr::select(-c(diff_volume, diff_close))
  
  
}

mySMA <- function (price,n){
  sma <- c()
  sma[1:(n-1)] <- NA
  for (i in n:length(price)){
    sma[i]<-mean(price[(i-n+1):i])
  }
  sma <- reclass(sma,price)
  return(sma)
}

RSI <- output_signal %>% 
  dplyr::select(rrg, ticker, name, date, volume, rclose) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::mutate(
    RSI = myRSI(rclose, 14),
    close = NULL,
    volume = NULL
  ) %>% 
  dplyr::group_by(rrg, ticker) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::arrange(desc(RSI)) 

output_signal %>% 
  # dplyr::filter(ticker == 'A2A.MI') %>% 
  dplyr::select(marginabile, ticker, name, date, volume, rclose) %>% 
  dplyr::semi_join(bear, 'ticker') %>%
  dplyr::filter(!is.na(marginabile)) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::mutate(
    RSI = myRSI(rclose, 14),
    overbouth = RSI > 70,
    rSMA = mySMA(rclose, 50),
    prev_rclose = dplyr::lag(rclose),
    prev_rSMA = dplyr::lag(rSMA),
    reversal_confirmation = (rclose < rSMA) & (prev_rclose >= prev_rSMA)
  ) %>% 
  dplyr::group_by(ticker) %>% 
  volume_divergence() %>% 
  dplyr::filter(overbouth, volume_divergence, reversal_confirmation) %>% 
  dplyr::arrange(desc(date)) 
  

