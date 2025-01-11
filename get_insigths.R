library(purrr)
library(TTR)
library(dplyr)
library(broom)
library(tidyquant)

files_stocks <- list.files("data_proc", full.names = T)

marginabili <- list.files(pattern = "marginabili.xlsx",full.names = T) %>%
  readxl::read_excel() %>%
  dplyr::count(Descrizione, sort = T) %>%
  dplyr::mutate(
    marginabile = "si"
  )

# marginabili
sectors <- list.files(pattern = "sectors.xlsx", full.names = T) %>%
  readxl::read_excel()
# sectors

output_signal <- lapply(files_stocks, read.table, sep = "\t", header = T, dec = ".")
output_signal <- dplyr::bind_rows(output_signal)
# nrow(output_signal)
output_signal <- output_signal %>%
  dplyr::ungroup() %>%
  dplyr::as_tibble()

output_signal <- output_signal %>%
  dplyr::left_join(sectors, by = join_by(ticker)) %>%
  dplyr::left_join(marginabili, by = c("name" = "Descrizione"))
#
get_last_swing <- function(output_signal, swing){

  output_signal %>%
    dplyr::filter(!is.na({{swing}})) %>%
    # dplyr::filter(!is.na({{swing}})) %>%
    dplyr::group_by(ticker) %>%
    filter(row_number()==n()) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::select(ticker, name, date, {{swing}}) %>%
    dplyr::rename(
      date_last_swing = date
    )
}

detect_change <- function(df, regime) {

  df %>%
    dplyr::select(name, ticker, sector, marginabile, date, volume, {{regime}}) %>%
    dplyr::mutate(
      change = 0,
      change = if_else({{regime}} != dplyr::lag({{regime}}), 1, 0)
    ) %>%
    dplyr::filter(change == 1) %>%
    dplyr::slice_tail(n = 1)

}
#
bull <- output_signal %>%
  dplyr::group_by(ticker, name) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::filter(rrg == 1) %>%
  dplyr::select(ticker)

# bull

bull_changes <- output_signal %>%
  dplyr::semi_join(bull) %>%
  dplyr::group_split(ticker) %>%
  purrr::map_df(detect_change, rtt_5020) %>%
  dplyr::arrange(desc(date)) %>% 
  dplyr::bind_rows(
    output_signal %>%
      dplyr::semi_join(bull) %>%
      dplyr::group_split(ticker) %>%
      purrr::map_df(detect_change, rsma_50100150) %>%
      dplyr::arrange(desc(date)),

    output_signal %>%
      dplyr::semi_join(bull) %>%
      dplyr::group_split(ticker) %>%
      purrr::map_df(detect_change, rema_50100150) %>%
      dplyr::arrange(desc(date)),

    output_signal %>%
      dplyr::semi_join(bull) %>%
      dplyr::group_split(ticker) %>%
      purrr::map_df(detect_change, rbo_100) %>%
      dplyr::arrange(desc(date)),
    
    output_signal %>%
      dplyr::semi_join(bull) %>%
      dplyr::group_split(ticker) %>%
      purrr::map_df(detect_change, rrg) %>%
      dplyr::arrange(desc(date))
  ) %>%
  dplyr::arrange(desc(date))



bull_score <- output_signal %>% 
  dplyr::semi_join(bull) %>%
  dplyr::group_by(ticker) %>%
  dplyr::slice_tail(n = 1) %>% 
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>%
  dplyr::group_by(ticker, sector, name, marginabile) %>%
  dplyr::summarise(
    last_day_score = sum(signal),
    last_day_volume = mean(volume)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(last_day_score), desc(last_day_volume))

bull_score_ts <- output_signal %>%  
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::semi_join(bull) %>%
  # dplyr::filter(ticker == 'CFV.MI') %>%
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>%
  dplyr::group_by(ticker, name, date) %>% 
  dplyr::summarise(
    last_day_score = sum(signal, na.rm = T)
  ) %>%
  dplyr::arrange(ticker, name, desc(date)) %>% 
  dplyr::group_by(ticker, name) %>%
  filter(row_number()==1 | row_number()==2) %>% 
  tidyr::pivot_wider(names_from = date, values_from = last_day_score, names_sort = FALSE)
# %>%
#   tidyr::pivot_wider(names_from = date, values_from = last_day_score) %>% View()

bull_swing <- output_signal %>%
  dplyr::semi_join(bull) %>%
  get_last_swing(lo3)

bull_swing_r <- output_signal %>% 
  dplyr::semi_join(bull) %>%
  get_last_swing(rl3) %>% 
  dplyr::rename(
    date_last_swing_r = date_last_swing
  )

bull_tot <- bull_score %>%
  dplyr::left_join(bull_swing, by = join_by(ticker)) %>%
  dplyr::left_join(
    bull_changes %>%
      dplyr::rename(
        date_last_change = date
      ) %>%
      dplyr::select(-volume) %>%
      dplyr::group_by(name) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::arrange(desc(date_last_change))
  ) %>%
  dplyr::arrange(desc(date_last_change), desc(last_day_score), desc(last_day_volume)) %>%
  dplyr::select(-c(rtt_5020:rbo_100))


bear <- output_signal %>%
  dplyr::group_by(ticker, name) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::filter(rrg == -1) %>%
  dplyr::select(ticker)

# bear

bear_changes <- output_signal %>%
  dplyr::semi_join(bear) %>%
  dplyr::group_split(ticker) %>%
  purrr::map_df(detect_change, rtt_5020) %>%
  dplyr::arrange(desc(date)) %>%
  dplyr::bind_rows(
    output_signal %>%
      dplyr::semi_join(bear) %>%
      dplyr::group_split(ticker) %>%
      purrr::map_df(detect_change, rsma_50100150) %>%
      dplyr::arrange(desc(date)),

    output_signal %>%
      dplyr::semi_join(bear) %>%
      dplyr::group_split(ticker) %>%
      purrr::map_df(detect_change, rema_50100150) %>%
      dplyr::arrange(desc(date)),

    output_signal %>%
      dplyr::semi_join(bear) %>%
      dplyr::group_split(ticker) %>%
      purrr::map_df(detect_change, rbo_100) %>%
      dplyr::arrange(desc(date)),
    
    output_signal %>%
      dplyr::semi_join(bear) %>%
      dplyr::group_split(ticker) %>%
      purrr::map_df(detect_change, rrg) %>%
      dplyr::arrange(desc(date))
  ) %>%
  dplyr::arrange(desc(date))


regime_change <- bull_changes %>% 
  dplyr::filter(!is.na(rrg)) %>% 
  dplyr::arrange(rrg) %>% 
  dplyr::bind_rows(
    bear_changes %>% 
      dplyr::filter(!is.na(rrg), !is.na(marginabile)) %>% 
      dplyr::arrange(rrg)
  ) %>%
  dplyr::arrange(desc(date)) %>% 
  dplyr::rename(
    date_regime_change = date
  )

regime_change %>%
  write.table("signals/regime_change.txt", sep = "\t", dec = ".", row.names = FALSE)

changes <- bull_changes %>% 
  dplyr::bind_rows(
    bear_changes
  ) %>% 
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>% 
  dplyr::filter(!is.na(signal)) %>% 
  dplyr::select(ticker, date, method, signal)



bear_score <- output_signal %>%
  dplyr::semi_join(bear) %>%
  dplyr::group_by(ticker) %>%
  dplyr::slice_tail(n = 1) %>%
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>%
  dplyr::group_by(ticker, sector, name, marginabile) %>%
  dplyr::summarise(
    last_day_score = sum(signal),
    last_day_volume = mean(volume)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(last_day_score), desc(last_day_volume))

bear_score_ts <- output_signal %>%  
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::semi_join(bear) %>%
  # dplyr::filter(ticker == 'IF.MI') %>%
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>%
  dplyr::group_by(ticker, name, marginabile, date) %>% 
  dplyr::summarise(
    last_day_score = sum(signal, na.rm = T)
  ) %>%
  dplyr::arrange(ticker, name, desc(date)) %>% 
  dplyr::group_by(ticker, name) %>%
  filter(row_number()==1 | row_number()==2) %>% 
  tidyr::pivot_wider(names_from = date, values_from = last_day_score, names_sort = FALSE) %>%
  dplyr::filter(marginabile == "si")

bear_swing <- output_signal %>%
  dplyr::semi_join(bear) %>%
  get_last_swing(hi3)

bear_swing_r <- output_signal %>% 
  dplyr::semi_join(bear) %>%
  dplyr::filter(!is.na(marginabile)) %>% 
  get_last_swing(rh3) %>% 
  dplyr::rename(
    date_last_swing_r = date_last_swing
  )

bear_tot <- bear_score %>%
  dplyr::left_join(bear_swing, by = join_by(ticker)) %>%
  dplyr::left_join(
    bear_changes %>%
      dplyr::rename(
        date_last_change = date
      ) %>%
      dplyr::select(-volume) %>%
      dplyr::group_by(name) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::arrange(desc(date_last_change))
  ) %>%
  dplyr::arrange(desc(date_last_change), desc(last_day_score), desc(last_day_volume)) %>%
  dplyr::select(-c(rtt_5020:rbo_100))

bull_tot %>%
  dplyr::arrange(desc(last_day_score)) %>%
  write.table("signals/bull_signals.txt", sep = "\t", dec = ".", row.names = FALSE)

bear_tot %>%
  dplyr::filter(marginabile == "si") %>%
  dplyr::arrange(last_day_score) %>%
  write.table("signals/bear_signals.txt", sep = "\t", dec = ".", row.names = FALSE)

last_day = Sys.Date()

output_signal %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(date_date = as.Date(date)) %>% 
  dplyr::filter(date_date == last_day) %>% 
  dplyr::arrange(desc(volume)) %>% 
  dplyr::select(date, ticker, name, volume) %>%
  write.table("signals/last_day_volume.txt", sep = "\t", dec = ".", row.names = FALSE)

bull_tot %>%
  dplyr::select(rrg, ticker, name, last_day_score, date_last_change) %>% 
  dplyr::arrange(desc(date_last_change), desc(last_day_score)) %>%
  write.table("signals/bull_last_change_signals.txt", sep = "\t", dec = ".", row.names = FALSE)

bear_tot %>%
  dplyr::select(rrg, ticker, name, marginabile, last_day_score, date_last_change) %>% 
  dplyr::arrange(desc(date_last_change), last_day_score) %>%
  dplyr::filter(marginabile == "si") %>% 
  write.table("signals/bear_last_change_signals.txt", sep = "\t", dec = ".", row.names = FALSE)

bull_tot %>%
  dplyr::select(rrg, ticker, name, last_day_score, date_last_swing, lo3) %>% 
  dplyr::arrange(desc(date_last_swing)) %>%
  write.table("signals/bull_last_swing.txt", sep = "\t", dec = ".", row.names = FALSE)

bull_swing_r %>%
  write.table("signals/bull_last_swing_r.txt", sep = "\t", dec = ".", row.names = FALSE)

bear_tot %>%
  dplyr::select(rrg, ticker, name, marginabile, last_day_score, date_last_swing, hi3) %>% 
  dplyr::filter(marginabile == "si") %>% 
  dplyr::arrange(desc(date_last_swing)) %>%
  write.table("signals/bear_last_swing.txt", sep = "\t", dec = ".", row.names = FALSE)

bear_swing_r %>%
  write.table("signals/bear_last_swing_r.txt", sep = "\t", dec = ".", row.names = FALSE)

bull_score_ts %>%
  write.table("signals/bull_ts_score.txt", sep = "\t", dec = ".", row.names = FALSE)

bear_score_ts %>%
  write.table("signals/bear_ts_score.txt", sep = "\t", dec = ".", row.names = FALSE)


linear_model <- function(df){
  window_lm <- unique(df$window)
  data <- df %>%
    # output_signal %>% 
    # dplyr::filter(name == "JUVENTUS FC") %>% 
    # dplyr::filter(ticker == "BPE.MI") %>% 
    # dplyr::arrange(desc(date)) %>% 
    dplyr::slice_tail(n=window_lm) %>% 
    dplyr::mutate(time_id = 1:n())
  lm_model <- lm(close ~ time_id, data)
  summary_lm_model <- broom::tidy(summary(lm_model)) %>% 
    dplyr::filter(term == "time_id") %>% 
    dplyr::select(estimate, p.value)
  summary_lm_model2 <- broom::glance(lm_model) %>% 
    dplyr::select(adj.r.squared)
  data.frame(
    rrg = unique(data$rrg),
    ticker = unique(data$ticker),
    window_lm = window_lm,
    marginabile = unique(data$marginabile),
    name = unique(data$name),
    min_date_window = min(data$date)
  ) %>% 
    cbind(summary_lm_model, summary_lm_model2)
}

add_column <- function(df_list, new_col, values_col) {
  
  purrr::pmap(
    
    list(df_list, new_col, values_col),
    
    .f = function(df, new_col, values_col) {
      df %>%
        dplyr::mutate(
          "{new_col}" := values_col
        )
    }
    
  ) %>% 
    dplyr::bind_rows()
  
}

replicate_df <- function(df, n) {
  replicate(n, df, simplify = F)
}

output_signal_lm <- output_signal %>% 
  replicate_df(4) %>% 
  add_column(new_col = "window", values_col = c(7, 15, 30, 45))

lm_mod <- output_signal_lm %>%
  dplyr::group_split(ticker, window) %>% 
  purrr::map_df(linear_model)

lm_mod %>% 
  dplyr::filter(
    rrg == 1, estimate > 0, p.value <= 0.05
  ) %>% 
  dplyr::arrange(window_lm, desc(adj.r.squared)) %>%
  write.table("signals/bull_linear.txt", sep = "\t", dec = ".", row.names = FALSE)

lm_mod %>% 
  dplyr::filter(
    rrg == -1, estimate < 0, p.value <= 0.10, marginabile == "si"
  ) %>% 
  dplyr::arrange(window_lm, desc(adj.r.squared)) %>%
  write.table("signals/bear_linear.txt", sep = "\t", dec = ".", row.names = FALSE)

output_signal %>% 
  dplyr::semi_join(bull, by = join_by(ticker, name)) %>% 
  dplyr::select(rrg, date, ticker, name, marginabile, close) %>% 
  dplyr::mutate(
    daily_return = close / dplyr::lag(close) - 1
  ) %>%
  dplyr::group_by(ticker) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::arrange(daily_return) %>%
  write.table("signals/bull_last_return.txt", sep = "\t", dec = ".", row.names = FALSE)

output_signal %>% 
  dplyr::semi_join(bear, by = join_by(ticker, name)) %>% 
  dplyr::select(rrg, date, ticker, name, marginabile, close) %>% 
  dplyr::mutate(
    daily_return = close / dplyr::lag(close) - 1
  ) %>%
  dplyr::group_by(ticker) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::arrange(desc(daily_return)) %>%
  dplyr::filter(marginabile == "si") %>% 
  write.table("signals/bear_last_return.txt", sep = "\t", dec = ".", row.names = FALSE)

max_equity <- output_signal %>% 
  dplyr::arrange(desc(date)) %>%
  dplyr::group_by(name, ticker) %>% 
  dplyr::slice_head(n=1) %>% 
  dplyr::select(ticker, name, rrg, date, contains("convex"), contains("concave"), contains("equal_weight"), contains("constant")) %>% 
  tidyr::pivot_longer(cols = c(rbo_100_convex:rrg_constant), names_to = "method", values_to = "equity") %>% 
  dplyr::arrange(ticker,desc(equity)) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::slice_head(n = 1) %>% 
  dplyr::arrange(desc(equity)) %>% 
  dplyr::mutate(
    convex = dplyr::if_else(stringr::str_detect(method, "convex"), 1, 0),
    equal_weight = dplyr::if_else(stringr::str_detect(method, "equal_weight"), 1, 0),
    concave = dplyr::if_else(stringr::str_detect(method, "concave"), 1, 0),
    constant = dplyr::if_else(stringr::str_detect(method, "constant"), 1, 0)
  ) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_longer(cols = c(convex:constant), names_to = "equity_method", values_to = "value_equity") %>% 
  dplyr::filter(value_equity == 1) %>% 
  dplyr::mutate(
    method = stringr::str_remove(method, "_convex|_concave|_constant|_equal_weight"),
    value_equity = NULL
  ) %>% 
  dplyr::select(ticker, method, equity)

max_equity %>% 
  write.table("signals/max_equity.txt", sep = "\t", dec = ".", row.names = FALSE)

# max_equity %>% 
#   dplyr::arrange(ticker) %>% 
#   dplyr::semi_join(bull) %>% 
#   dplyr::left_join(bull_changes %>%
#                      dplyr::select(- change, - volume) %>% 
#                      dplyr::rename(
#                        date_last_change = date
#                      ) %>% 
#                      dplyr::group_by(name) %>% 
#                      dplyr::slice_head(n = 1)
#                    ) %>% 
#   dplyr::arrange(desc(date_last_change))

# max_equity %>% 
#   dplyr::arrange(ticker) %>% 
#   dplyr::semi_join(bull) %>% 
#   dplyr::left_join(bull_changes %>%
#                      dplyr::select(- change, - volume) %>% 
#                      dplyr::rename(
#                        date_last_change = date
#                      ) %>% 
#                      dplyr::group_by(name) %>% 
#                      dplyr::slice_head(n = 1)
#   ) %>% 
#   tidyr::pivot_longer(cols = c(rtt_5020:rrg), names_to = 'change_method', values_to = 'change') %>% 
#   dplyr::filter(!is.na(change)) %>% 
#   dplyr::filter(method == change_method) %>% 
#   dplyr::arrange(desc(date_last_change))

bull_score_ts <- output_signal %>%  
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::semi_join(bull) %>%
  # dplyr::filter(ticker == 'IF.MI') %>%
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>%
  dplyr::group_by(ticker, name, marginabile, date) %>% 
  dplyr::summarise(
    last_day_score = sum(signal, na.rm = T)
  ) %>%
  dplyr::arrange(ticker, name, desc(date)) %>% 
  dplyr::filter(marginabile == "si") %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(marginabile)) %>% 
  dplyr::mutate(
    date = lubridate::as_date(date)
  )

bear_score_ts <- output_signal %>%  
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::semi_join(bear) %>%
  # dplyr::filter(ticker == 'IF.MI') %>%
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>%
  dplyr::group_by(ticker, name, marginabile, date) %>% 
  dplyr::summarise(
    last_day_score = sum(signal, na.rm = T)
  ) %>%
  dplyr::arrange(ticker, name, desc(date)) %>% 
  dplyr::filter(marginabile == "si") %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(marginabile)) %>% 
  dplyr::mutate(
    date = lubridate::as_date(date)
  )

bull_last_changes_max_equity <- bull_changes %>%
  # dplyr::filter(!is.na(marginabile)) %>%
  dplyr::select(- change, - volume) %>% 
  dplyr::rename(
    date_last_change = date
  ) %>% 
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>% 
  dplyr::select(ticker, name, date_last_change, method, signal) %>% 
  dplyr::filter(!is.na(signal)) %>% 
  dplyr::semi_join(max_equity %>% 
                     dplyr::arrange(desc(equity)) %>% 
                     dplyr::semi_join(bull) %>% 
                     # dplyr::filter(ticker == 'FCT.MI') %>% 
                     dplyr::left_join(
                       output_signal %>%  
                         dplyr::mutate(date = lubridate::as_date(date)) %>% 
                         dplyr::semi_join(bull) %>%
                         # dplyr::filter(ticker == 'IF.MI') %>%
                         tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>% 
                         dplyr::select(ticker, name, date, method, signal)
                       # %>% 
                       #   dplyr::filter(ticker == 'FCT.MI')
                     ) %>% 
                     dplyr::arrange(desc(date)), by = join_by(ticker, name, method)) %>% 
  dplyr::semi_join(bull)

bull_last_changes_max_equity %>% 
  write.table("signals/max_equity_bull_method.txt", sep = "\t", dec = ".", row.names = FALSE)

bear_last_changes_max_equity <- bear_changes %>%
  dplyr::filter(!is.na(marginabile)) %>%
  dplyr::select(- change, - volume) %>% 
  dplyr::rename(
    date_last_change = date
  ) %>% 
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>% 
  dplyr::select(ticker, name, date_last_change, method, signal) %>% 
  dplyr::filter(!is.na(signal)) %>% 
  dplyr::semi_join(max_equity %>% 
                     dplyr::arrange(desc(equity)) %>% 
                     dplyr::semi_join(bear) %>% 
                     # dplyr::filter(ticker == 'FCT.MI') %>% 
                     dplyr::left_join(
                       output_signal %>%  
                         dplyr::mutate(date = lubridate::as_date(date)) %>% 
                         dplyr::semi_join(bear) %>%
                         # dplyr::filter(ticker == 'IF.MI') %>%
                         tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>% 
                         dplyr::select(ticker, name, date, method, signal)
                       # %>% 
                       #   dplyr::filter(ticker == 'FCT.MI')
                     ) %>% 
                     dplyr::arrange(desc(date)), by = join_by(ticker, name, method)) %>% 
  dplyr::semi_join(bear)

bear_last_changes_max_equity %>% 
  write.table("signals/max_equity_bear_method.txt", sep = "\t", dec = ".", row.names = FALSE)

# max_equity %>% 
#   dplyr::arrange(desc(equity)) %>% 
#   dplyr::semi_join(bear) %>% 
#   dplyr::filter(ticker == 'AMP.MI') %>% 
#   dplyr::left_join(
#     output_signal %>%  
#       dplyr::mutate(date = lubridate::as_date(date)) %>% 
#       dplyr::semi_join(bear) %>%
#       # dplyr::filter(ticker == 'IF.MI') %>%
#       tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>% 
#       dplyr::select(ticker, name, date, method, signal) %>% 
#       dplyr::filter(ticker == 'AMP.MI')
#   ) %>% 
#   dplyr::arrange(desc(date))

# bear_max_equity <- max_equity %>% 
#   # dplyr::filter(ticker == 'A2A.MI') %>% 
#   dplyr::arrange(ticker) %>% 
#   dplyr::semi_join(bear) %>% 
#   dplyr::left_join(bear_changes %>%
#                      # dplyr::filter(ticker == 'A2A.MI') %>% 
#                      dplyr::select(- change, - volume) %>% 
#                      dplyr::rename(
#                        date_last_change = date
#                      )
#                    # %>% 
#                    #   dplyr::group_by(name) %>% 
#                    #   dplyr::slice_head(n = 1)
#   ) %>% 
#   dplyr::arrange(ticker) %>%
#   tidyr::pivot_longer(cols = c(rtt_5020:rrg), names_to = 'change_method', values_to = 'change') %>% 
#   dplyr::filter(!is.na(change)) %>% 
#   dplyr::filter(!is.na(marginabile)) %>% 
#   dplyr::filter(method == change_method) %>% 
#   dplyr::arrange(desc(date_last_change)) %>% 
#   dplyr::select(
#     -c(sector)
#   ) %>% 
#   dplyr::left_join(
#     bear_swing_r
#   ) %>% 
#   dplyr::mutate(
#     date_last_change = lubridate::as_date(date_last_change),
#     date_last_swing_r = lubridate::as_date(date_last_swing_r)
#   ) %>% 
#   dplyr::left_join(bear_score_ts, by = c("date_last_change" = "date", "ticker" = "ticker", "name" = "name")) %>% 
#   dplyr::left_join(bear_score_ts, by = c("date_last_swing_r" = "date", "ticker" = "ticker", "name" = "name")) %>% 
#   dplyr::rename(
#     date_last_change_score = last_day_score.x,
#     date_last_swing_r_score = last_day_score.y
#   ) %>% 
#   dplyr::arrange(desc(date_last_change))
# 
# bear_max_equity %>% 
#   write.table("signals/bear_max_equity.txt", sep = "\t", dec = ".", row.names = FALSE)

# bull_max_equity <- max_equity %>% 
#   # dplyr::filter(ticker == 'A2A.MI') %>% 
#   dplyr::arrange(ticker) %>% 
#   dplyr::semi_join(bull) %>% 
#   dplyr::left_join(bull_changes %>%
#                      # dplyr::filter(ticker == 'A2A.MI') %>% 
#                      dplyr::select(- change, - volume) %>% 
#                      dplyr::rename(
#                        date_last_change = date
#                      )
#                    # %>% 
#                    #   dplyr::group_by(name) %>% 
#                    #   dplyr::slice_head(n = 1)
#   ) %>% 
#   dplyr::arrange(ticker) %>%
#   tidyr::pivot_longer(cols = c(rtt_5020:rrg), names_to = 'change_method', values_to = 'change') %>% 
#   dplyr::filter(!is.na(change)) %>% 
#   dplyr::filter(!is.na(marginabile)) %>% 
#   dplyr::filter(method == change_method) %>% 
#   dplyr::arrange(desc(date_last_change)) %>% 
#   dplyr::select(
#     -c(sector)
#   ) %>% 
#   dplyr::left_join(
#     bull_swing_r
#   ) %>% 
#   dplyr::mutate(
#     date_last_change = lubridate::as_date(date_last_change),
#     date_last_swing_r = lubridate::as_date(date_last_swing_r)
#   ) %>% 
#   dplyr::left_join(bull_score_ts, by = c("date_last_change" = "date", "ticker" = "ticker", "name" = "name")) %>% 
#   dplyr::left_join(bull_score_ts, by = c("date_last_swing_r" = "date", "ticker" = "ticker", "name" = "name")) %>% 
#   dplyr::rename(
#     date_last_change_score = last_day_score.x,
#     date_last_swing_r_score = last_day_score.y
#   )
# 
# bull_max_equity %>% 
#   write.table("signals/bull_max_equity.txt", sep = "\t", dec = ".", row.names = FALSE)


# regime_change %>%
#   dplyr::semi_join(bear) %>% 
#   dplyr::select(ticker, name, date_regime_change) %>% 
#   dplyr::left_join(changes) %>% 
#   dplyr::rename(
#     date_change = date,
#   ) %>% 
#   dplyr::mutate(
#     date_regime_change = lubridate::as_date(date_regime_change),
#     date_change = lubridate::as_date(date_change)
#   ) %>% 
#   dplyr::left_join(bear_score_ts, by = c("date_regime_change" = "date", "ticker" = "ticker", "name" = "name")) %>% 
#   dplyr::rename(
#     date_regime_change_score = last_day_score,
#   ) %>% 
#   dplyr::left_join(bear_score_ts, by = c("date_change" = "date", "ticker" = "ticker", "name" = "name")) %>% 
#   dplyr::rename(
#     date_change_score = last_day_score,
#   )


# bear_swing_r %>% 
#   dplyr::left_join(bear_last_changes_max_equity)
# 
# changes %>% 
#   dplyr::filter(
#     ticker == 'SFER.MI'
#   )

regime_change_followed_signal <- regime_change %>% 
  # dplyr::filter(
  #   ticker == 'SFER.MI'
  # ) %>% 
  dplyr::select(name, ticker, date_regime_change, rrg) %>% 
  dplyr::left_join(
    changes %>% 
      # dplyr::filter(
      #   ticker == 'SFER.MI'
      # ) %>% 
      dplyr::rename(
        date_last_change = date
      )
    ) %>% 
  dplyr::filter(signal == rrg, date_last_change > date_regime_change)

regime_change_followed_signal %>% 
  dplyr::filter(rrg == 1) %>% 
  write.table("signals/bull_regime_change_followed_signal.txt", sep = "\t", dec = ".", row.names = FALSE)

regime_change_followed_signal %>% 
  dplyr::filter(rrg == -1) %>% 
  write.table("signals/bear_regime_change_followed_signal.txt", sep = "\t", dec = ".", row.names = FALSE)

bull_signal_maxequity_after_change_afterregime <- regime_change_followed_signal %>% 
  dplyr::filter(rrg == 1) %>% 
  dplyr::semi_join(
    max_equity, by = join_by(ticker, method)
  ) %>% 
  dplyr::left_join(
    output_signal %>% 
      dplyr::mutate(
        date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
      ) %>% 
      tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>%
      dplyr::group_by(sector, name, ticker, date) %>% 
      dplyr::summarise(
        last_day_score = sum(signal)
      ) %>% 
      dplyr::group_by(sector, name, ticker) %>% 
      dplyr::slice_tail(n = 1)
  ) %>% 
  dplyr::left_join(
    bull_swing_r, by = join_by(name, ticker)
  ) %>% 
  dplyr::select(-c(rrg, method, signal, date, rl3)) %>%
  dplyr::arrange(desc(date_last_change))

bull_signal_maxequity_after_change_afterregime %>% 
  write.table("signals/max_equity_bull_regime_change_followed_signal.txt", sep = "\t", dec = ".", row.names = FALSE)


bear_signal_maxequity_after_change_afterregime <- regime_change_followed_signal %>% 
  dplyr::filter(rrg == -1) %>% 
  dplyr::semi_join(
    marginabili, by = c("name" = "Descrizione")
  ) %>% 
  dplyr::semi_join(
    max_equity, by = join_by(ticker, method)
  ) %>% 
  dplyr::left_join(
    output_signal %>% 
      dplyr::mutate(
        date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
      ) %>% 
      tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>%
      dplyr::group_by(sector, name, ticker, date) %>% 
      dplyr::summarise(
        last_day_score = sum(signal)
      ) %>% 
      dplyr::group_by(sector, name, ticker) %>% 
      dplyr::slice_tail(n = 1)
  ) %>% 
  dplyr::left_join(
    bear_swing_r, by = join_by(name, ticker)
  ) %>% 
  dplyr::select(-c(rrg, method, signal, date, rh3)) %>%
  dplyr::arrange(desc(date_last_change))

bear_signal_maxequity_after_change_afterregime %>% 
  write.table("signals/max_equity_bear_regime_change_followed_signal.txt", sep = "\t", dec = ".", row.names = FALSE)



ptf_signals <- output_signal %>% 
  dplyr::filter(stringr::str_detect(name, 'TENARIS|AVIO|GEFRAN|CUCINELLI|CAMPARI|ITALGAS|INTERPUMP|AMPLIFON')) %>% 
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  # tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>% 
  dplyr::select(date, ticker, name, rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg) %>% 
  dplyr::filter(date == max(date))

ptf_signals %>% 
  write.table("signals/ptf_signals.txt", sep = "\t", dec = ".", row.names = FALSE)


output_signal %>% 
  dplyr::semi_join(ptf_signals, 'ticker') %>% 
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>%
  dplyr::group_by(sector, name, ticker, date) %>% 
  dplyr::summarise(
    last_day_score = sum(signal)
  ) %>% 
  dplyr::arrange(desc(date)) %>% 
  dplyr::group_by(ticker, name) %>%
  dplyr::slice_head(n = 5) %>% 
  tidyr::pivot_wider(names_from = date, values_from = last_day_score) %>% 
  write.table("signals/ptf_ts_score.txt", sep = "\t", dec = ".", row.names = FALSE)

avg_score_sector <- output_signal %>% 
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  # dplyr::filter(date > lubridate::ymd('2024-12-01')) %>%
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100, rrg), names_to = "method", values_to = "signal") %>%
  dplyr::group_by(sector, name, ticker, date) %>% 
  dplyr::summarise(
    last_day_score = sum(signal)
  ) %>% 
  # dplyr::group_by(sector, name, ticker) %>% 
  # dplyr::slice_tail(n = 1) %>% 
  dplyr::group_by(sector, date) %>% 
  dplyr::summarise(
    avg_score = mean(last_day_score)
  )
avg_score_sector %>% 
  dplyr::group_by(sector) %>% 
  dplyr::slice_tail(n = 5)%>% 
  tidyr::pivot_wider(names_from = date, values_from = avg_score) %>% 
  write.table("signals/sector_ts_score.txt", sep = "\t", dec = ".", row.names = FALSE)


avg_score_sector_14 <- avg_score_sector %>% 
  dplyr::group_by(sector) %>% 
  dplyr::slice_tail(n = 14)

sector_trend_score <- avg_score_sector %>% 
  dplyr::group_by(sector) %>% 
  dplyr::slice_tail(n = 14) %>% 
  dplyr::group_by(sector) %>% 
  dplyr::mutate(
    min_date = min(date),
    max_date = max(date),
    avg_score = NULL,
    date = NULL
  ) %>% 
  dplyr::distinct() %>% 
  dplyr::left_join(avg_score_sector_14, by = c('sector', 'min_date' = 'date')) %>% 
  dplyr::left_join(avg_score_sector_14, by = c('sector', 'max_date' = 'date')) %>% 
  dplyr::mutate(
    delta_score = avg_score.y / avg_score.x - 1
  ) %>% 
  dplyr::arrange(delta_score)

sector_trend_score %>% 
  write.table("signals/sector_trend_score.txt", sep = "\t", dec = ".", row.names = FALSE)


stop_loss <- output_signal %>% 
  dplyr::select(rrg, ticker, name, date, high:close, stop_loss, hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  ) %>% 
  tidyr::fill(c(hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg), .direction = 'down') %>% 
  dplyr::rename(
    data = date
  ) %>% 
  dplyr::group_by(ticker, name) %>% 
  tq_mutate(select = c("high", "low", "close"), n=14, mutate_fun = ATR) %>% 
  tq_mutate(select = c("high", "low"), mutate_fun = DonchianChannel) %>%
  tq_mutate(select     = close, 
            mutate_fun = periodReturn, 
            period     = "daily", 
            type       = "log",
            col_rename = "daily.returns") %>% 
  mutate(
    factor = 1,
    sd = runSD(close, n = 14, sample = TRUE, cumulative = FALSE),
    sd_low = close - sd * factor,
    sd_high = close + sd * factor,
    atr_low = close - atr * factor,
    atr_high = close + atr * factor
  ) %>% 
  dplyr::rename(ATR_trueHigh = ATR,
                ATR_trueLow = ATR..1,
                DC_low = DonchianChannel,
                DC_mid = mid,
                DC_high = DonchianChannel..1) %>% 
  dplyr::select(rrg, data, close, factor, atr_low, atr_high, DC_low, DC_high, sd_low, sd_high, stop_loss, hi1, hi2, hi3, hi4, lo1, lo2, lo3, lo4, flr, clg) %>% 
  dplyr::group_by(ticker, name) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::arrange(rrg, ticker)

stop_loss %>%
  write.table("signals/stop_loss.txt", sep = "\t", dec = ".", row.names = FALSE)


library(TTR)
identify_short_entry <- function(data, nRSI, nSMA){
  
  data$RSI <- TTR::RSI(data$close, n = data$nRSI)
  data$overbouth <- data$RSI > 70
  
  data$SMA <- SMA(data$close, n = data$nSMA)
  
  data$volume_divergence <- c(FALSE, diff(data$volume) < 0 & diff(data$close) > 0)
  
  data$reversal_confirmation <- c(FALSE, 
                                  data$close[-1] <= data$SMA[-1] & 
                                    lag(data$close, 1)[-1] >= lag(data$SMA, 1)[-1])
  
  # data$short_entry <- data$overbouth & data$volume_divergence & data$reversal_confirmation
  
  data %>% 
    dplyr::select(id, ticker, name, date, volume, close, nRSI, nSMA, overbouth, SMA, volume_divergence, reversal_confirmation)
  
  
}

parameter <- tidyr::expand_grid(
  nRSI = c(3, 7, 14, 21, 28, 50),
  nSMA = c(50)
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
  dplyr::group_split(ticker, id) %>%
  purrr::map_df(identify_short_entry, nRSI, nSMA) %>% 
  dplyr::filter(overbouth, volume_divergence) %>% 
  dplyr::group_by(ticker, id) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::arrange(desc(date)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(ticker, name, date, nRSI )

overbouth_volume_divergence %>%
  write.table("signals/overbouth_volume_divergence.txt", sep = "\t", dec = ".", row.names = FALSE)

