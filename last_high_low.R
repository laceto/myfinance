output_signal %>% 
  dplyr::semi_join(bull) %>% 
  dplyr::select(ticker, name, date, rclose, rh1, rh2, rh3, rh4, rl1, rl2, rl3, rl4, rflr, rclg) %>% 
  # dplyr::select(rrg, ticker, name, date, close, hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  # tidyr::pivot_longer(cols = c(hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg), names_to = 'swing', values_to = 'value') %>% 
  tidyr::pivot_longer(cols = c(rh1, rh2, rh3, rh4, rl1, rl2, rl3, rl4, rflr, rclg), names_to = 'swing', values_to = 'value') %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::filter(stringr::str_detect(swing, "rl|rflr")) %>% 
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
  )




output_signal %>% 
  dplyr::semi_join(bear) %>% 
  dplyr::filter(marginabile == 'si') %>% 
  dplyr::select(rrg, ticker, name, date, close, hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  tidyr::pivot_longer(cols = c(hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg), names_to = 'swing', values_to = 'value') %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::group_by(ticker) %>% 
  # dplyr::slice_tail(n = 1) %>% 
  # dplyr::filter(stringr::str_detect(name, 'BRE'))
  dplyr::filter(stringr::str_detect(swing, "hi2")) %>% 
  dplyr::arrange(desc(date)) %>% View()

# SHORT: ERG 20, TINEXTA 7.8, TERNA 7.8, SNAM 4.3, MARR 10, INWIT 9.5, DANIELI 25, BREMBO 9, BIESSE 7.5


output_signal %>% 
  dplyr::semi_join(bear) %>% 
  dplyr::filter(marginabile == 'si') %>% 
  dplyr::select(rrg, ticker, name, date, close, hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  tidyr::pivot_longer(cols = c(hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg), names_to = 'swing', values_to = 'value') %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::arrange(desc(date))
  


output_signal %>% 
  dplyr::semi_join(bull) %>% 
  dplyr::select(rrg, ticker, name, date, close, hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  tidyr::pivot_longer(cols = c(hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg), names_to = 'swing', values_to = 'value') %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::count(name, swing, sort = T) %>% 
  tidyr::pivot_wider(names_from = swing, values_from = n) %>%
  dplyr::left_join(readr::read_delim('signals/bull_last_change_signals.txt') %>% 
                     dplyr::select(ticker, name, last_day_score))


output_signal %>% 
  dplyr::semi_join(bull) %>%
  # dplyr::filter(marginabile == 'si') %>% 
  dplyr::select(rrg, ticker, name, date, close, volume, hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  tidyr::pivot_longer(cols = c(hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg), names_to = 'swing', values_to = 'value') %>% 
  dplyr::filter(!is.na(value)) %>% 
  # dplyr::group_by(ticker) %>% 
  # dplyr::slice_tail(n = 1) %>% 
  # dplyr::filter(stringr::str_detect(name, 'ENI'))
  dplyr::filter(stringr::str_detect(swing, "lo1")) %>%
  dplyr::filter(volume > 100000) %>% 
  dplyr::arrange(desc(date)) %>% View()
