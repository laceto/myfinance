output_signal %>% 
  dplyr::semi_join(bear) %>% 
  dplyr::filter(marginabile == 'si') %>% 
  dplyr::count(name) %>% View()


output_signal %>% 
  dplyr::semi_join(bull) %>% 
  dplyr::select(rrg, ticker, name, date, close, hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  tidyr::pivot_longer(cols = c(hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg), names_to = 'swing', values_to = 'value') %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::filter(stringr::str_detect(swing, "lo|flr")) %>% 
  dplyr::arrange(desc(date)) %>% 
  View()

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
  dplyr::slice_tail(n = 1) %>% 
  # dplyr::filter(stringr::str_detect(name, 'BRE'))
  dplyr::filter(stringr::str_detect(swing, "hi")) %>% 
  dplyr::arrange(desc(date))


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
  tidyr::pivot_wider(names_from = swing, values_from = n) %>% View()



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
