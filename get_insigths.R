# install.packages("dplyr")
# install.packages("readxl")
library(dplyr)
library(broom)



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

output_signal <- output_signal %>% 
  replicate_df(4) %>% 
  add_column(new_col = "window", values_col = c(7, 15, 30, 45))

lm_mod <- output_signal %>%
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


