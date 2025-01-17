library(TTR)

identify_short_entry <- function(data, nRSI){
  
  nRSI <- unique(data %>% dplyr::pull(nRSI))
  
  data %>% 
    dplyr::mutate(
      RSI = TTR::RSI(close, n = nRSI),
      overbouth = RSI > 70,
      diff_volume = volume - dplyr::lag(volume),
      diff_close = close - dplyr::lag(close),
      volume_divergence = diff_volume < 0 & diff_close > 0
    ) %>% 
    dplyr::select(id, ticker, name, date, volume, close, nRSI, overbouth, volume_divergence)
  
  
}
parameter <- tidyr::expand_grid(
  nRSI = c(3, 7, 14, 21, 28, 50)
) %>% 
  dplyr::mutate(
    id = 1:n()
  )

overbouth_volume_divergence <- output_signal %>% 
  dplyr::select(marginabile, ticker, name, date, volume, close) %>% 
  dplyr::semi_join(bear, 'ticker') %>%
  dplyr::filter(!is.na(marginabile)) %>% 
  tidyr::expand_grid(
    parameter
  ) %>% 
  dplyr::group_split(ticker, id)

overbouth_volume_divergence <- lapply(overbouth_volume_divergence, identify_short_entry, nRSI) %>%
  dplyr::bind_rows() %>% 
  dplyr::filter(overbouth, volume_divergence) %>% 
  dplyr::group_by(ticker, id) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::arrange(desc(date)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(ticker, name, date, nRSI )



