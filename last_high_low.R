change_in_ma <- bull_changes %>% 
  dplyr::select(ticker, date, contains('50100')) %>% 
  tidyr::pivot_longer(cols = c(rsma_50100150, rema_50100150), names_to = 'method', values_to = 'value') %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::arrange(ticker, desc(date)) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::slice_head(n = 1) %>% 
  dplyr::rename(
    change = value,
    date_last_ma_change = date
  )

rl3 <- output_signal %>% 
  dplyr::semi_join(bull) %>% 
  dplyr::select(ticker, name, date, rclose, rh1, rh2, rh3, rh4, rl1, rl2, rl3, rl4, rflr, rclg) %>% 
  # dplyr::select(rrg, ticker, name, date, close, hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  # tidyr::pivot_longer(cols = c(hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg), names_to = 'swing', values_to = 'value') %>% 
  tidyr::pivot_longer(cols = c(rh1, rh2, rh3, rh4, rl1, rl2, rl3, rl4, rflr, rclg), names_to = 'swing', values_to = 'value') %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::filter(stringr::str_detect(swing, "rl3|rflr")) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::arrange(desc(date)) %>%
  dplyr::select(-c(rclose, value)) %>%
  dplyr::rename(
    date_swing = date
  ) %>% 
  dplyr::left_join(readr::read_delim('signals/bull_last_change_signals.txt') %>% 
                     dplyr::select(ticker, name, last_day_score)) %>% 
  dplyr::left_join(
    output_signal %>% 
      dplyr::select(ticker, name, date, rsma_50100150, rema_50100150) %>% 
      dplyr::mutate(
        date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
      ) %>% 
      dplyr::group_by(ticker) %>% 
      dplyr::slice_tail(n = 1) %>% 
      dplyr::select(-date)
  ) %>% 
  dplyr::left_join(change_in_ma) %>% 
  dplyr::left_join(
    readr::read_delim('signals/RSI.txt') %>% 
      dplyr::arrange(ticker, desc(date)) %>% 
      dplyr::select(-rrg, -date, -rclose) %>% 
      dplyr::group_by(ticker) %>% 
      dplyr::slice_head(n = 1)
  ) %>% 
  dplyr::filter(change == 1)

rl3 %>% 
  dplyr::left_join(readr::read_delim('signals/sector_ticker.txt')) %>% 
  dplyr::select(-c(swing, method, change, sector, last_day_score, marginabile)) %>%
  write.table("signals/bull_low3_rsi_score.txt", sep = "\t", dec = ".", row.names = FALSE)
  


change_in_ma <- bear_changes %>% 
  dplyr::select(ticker, date, contains('50100')) %>% 
  tidyr::pivot_longer(cols = c(rsma_50100150, rema_50100150), names_to = 'method', values_to = 'value') %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::arrange(ticker, desc(date)) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::slice_head(n = 1) %>% 
  dplyr::rename(
    change = value,
    date_last_ma_change = date
  )

rh3 <- output_signal %>% 
  dplyr::filter(marginabile == 'si') %>% 
  dplyr::semi_join(bear) %>% 
  dplyr::select(ticker, name, date, rclose, rh1, rh2, rh3, rh4, rl1, rl2, rl3, rl4, rflr, rclg) %>% 
  # dplyr::select(rrg, ticker, name, date, close, hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  # tidyr::pivot_longer(cols = c(hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg), names_to = 'swing', values_to = 'value') %>% 
  tidyr::pivot_longer(cols = c(rh1, rh2, rh3, rh4, rl1, rl2, rl3, rl4, rflr, rclg), names_to = 'swing', values_to = 'value') %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::filter(stringr::str_detect(swing, "rh3|rclg")) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::arrange(desc(date)) %>%
  dplyr::select(-c(rclose, value)) %>%
  dplyr::rename(
    date_swing = date
  ) %>% 
  dplyr::left_join(readr::read_delim('signals/bear_last_change_signals.txt') %>% 
                     dplyr::select(ticker, name, last_day_score)) %>% 
  dplyr::left_join(
    output_signal %>% 
      dplyr::select(ticker, name, date, rsma_50100150, rema_50100150) %>% 
      dplyr::mutate(
        date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
      ) %>% 
      dplyr::group_by(ticker) %>% 
      dplyr::slice_tail(n = 1) %>% 
      dplyr::select(-date)
  ) %>% 
  dplyr::left_join(change_in_ma) %>% 
  dplyr::left_join(
    readr::read_delim('signals/RSI.txt') %>% 
      dplyr::arrange(ticker, desc(date)) %>% 
      dplyr::select(-rrg, -date, -rclose) %>% 
      dplyr::group_by(ticker) %>% 
      dplyr::slice_head(n = 1)
  ) %>% 
  dplyr::filter(change == -1)


rh3 %>% 
  dplyr::left_join(readr::read_delim('signals/sector_ticker.txt')) %>% 
  dplyr::select(-c(swing, method, change, sector, last_day_score, marginabile)) %>%
  write.table("signals/bear_high3_rsi_score.txt", sep = "\t", dec = ".", row.names = FALSE)

