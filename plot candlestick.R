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
    theme_tq()
}

data %>% 
  plot_candlestick(n_weeks = 20)


output_signal %>% 
  dplyr::semi_join(
    overbouth_volume_divergence %>% 
      dplyr::count(ticker, name) %>% 
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
    plot_candlestick, n_weeks = 15
  )



