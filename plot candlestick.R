library(ggplot2)
plot_candlestick <- function(data, n_weeks) {
  
  n_days <- n_weeks * 5
  
  range_days <- data %>%
    tail(n_days) %>%
    dplyr::summarise(
      max_high = max(high),
      min_low  = min(low)
    )
  
  end <- lubridate::as_date(today()-1)
  
  name <- data %>% 
    dplyr::pull(name) %>% 
    unique()
  
  data %>% 
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),
                     colour_up = "darkgreen", colour_down = "darkred", 
                     fill_up  = "darkgreen", fill_down  = "darkred") +
    labs(title = paste0(name, ' at ', n_weeks, ' weeks'), 
         subtitle = " ",
         y = "Closing Price", x = "") + 
    coord_x_date(xlim = c(end - weeks(n_weeks), end),
                 c(range_days$min_low, range_days$max_high)) +
    theme_tq() + 
    geom_line(aes(y=close, color="close"))
}

overbouth_volume_divergence %>% 
  dplyr::count(ticker, name, sort = T) %>% View()

output_signal %>% 
  dplyr::filter(stringr::str_detect(name, 'ERG')) %>%
  dplyr::semi_join(
    overbouth_volume_divergence %>% 
      dplyr::count(ticker, name, sort = T) %>% 
      dplyr::mutate(
        n = NULL
      )
  ) %>% 
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  dplyr::select(name, date, close, open, high, low) %>% 
  dplyr::group_split(name) %>% 
  purrr::map(
    plot_candlestick, n_weeks = 25
  )





output_signal %>% 
  dplyr::semi_join(output_signal %>% 
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
                     dplyr::filter(stringr::str_detect(swing, "hi")) %>% 
                     dplyr::arrange(desc(date)) %>% 
                     dplyr::filter(date >= lubridate::ymd('2024-12-01')) %>% 
                     dplyr::count(ticker), by = join_by(ticker)) %>% 
  # dplyr::filter(date >= lubridate::ymd('2020-12-01')) %>%
  ggplot(aes(x = date, y = close, group = ticker)) +
  geom_line() +
  labs(title = " ", y = "Closing Price", x = "") + 
  facet_wrap(~ ticker, ncol = 4, scale = "free_y") + 
  theme_tq()
