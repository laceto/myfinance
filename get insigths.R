library(tidyverse)
library(data.table)

files_stocks <- list.files("data_proc", full.names = T)

marginabili <- list.files(pattern = "marginabili.xlsx",full.names = T) %>% 
  # stringr::str_subset(pattern = "marginabili") %>% 
  readxl::read_excel() %>% 
  dplyr::count(Descrizione, sort = T) %>% 
  dplyr::mutate(
    marginabile = "si"
  )

sectors <- list.files(pattern = "sectors.xlsx", full.names = T) %>% 
  # stringr::str_subset(pattern = "sectors") %>% 
  readxl::read_excel()

output_signal <- lapply(files_stocks, read.table, sep = "\t", header = T, dec = ".")
output_signal <- dplyr::bind_rows(output_signal)

output_signal <- output_signal %>% 
  dplyr::ungroup() %>% 
  dplyr::as_tibble()

output_signal <- output_signal %>% 
  dplyr::left_join(sectors, by = join_by(ticker)) %>% 
  dplyr::left_join(marginabili, by = c("name" = "Descrizione")) 

get_last_swing <- function(output_signal, swing){
  
  output_signal %>% 
    dplyr::filter(!is.na({{swing}})) %>% 
    # dplyr::filter(!is.na({{swing}})) %>% 
    dplyr::group_by(ticker) %>%
    filter(row_number()==n()) %>% 
    dplyr::arrange(desc(date)) %>% 
    dplyr::select(ticker, date, {{swing}}) %>% 
    dplyr::rename(
      date_last_swing = date
    )
}

detect_change <- function(df, regime) {  
  
  df %>% 
    dplyr::select(name, ticker, sector, marginabile, date, volume, rrg, {{regime}}) %>%
    dplyr::mutate(
      change = 0,
      change = if_else({{regime}} != dplyr::lag({{regime}}), 1, 0)
    ) %>% 
    dplyr::filter(change == 1, rrg == {{regime}}) %>% 
    dplyr::slice_tail(n = 3)
  
} 

bull <- output_signal %>% 
  dplyr::group_by(ticker, name) %>% 
  dplyr::slice_tail(n = 1) %>% 
  dplyr::filter(rrg == 1) %>% 
  dplyr::select(ticker)

bull

bull_changes <- output_signal %>% 
  dplyr::semi_join(bull) %>% 
  dplyr::group_split(ticker) %>% 
  purrr::map_df(detect_change, rtt_5020) %>% 
  dplyr::arrange(desc(date)) %>% 
  dplyr::filter(rrg == 1) %>% 
  dplyr::bind_rows(
    output_signal %>% 
      dplyr::semi_join(bull) %>% 
      dplyr::group_split(ticker) %>% 
      purrr::map_df(detect_change, rsma_50100150) %>% 
      dplyr::arrange(desc(date)) %>% 
      dplyr::filter(rrg == 1),
    
    output_signal %>% 
      dplyr::semi_join(bull) %>% 
      dplyr::group_split(ticker) %>% 
      purrr::map_df(detect_change, rema_50100150) %>% 
      dplyr::arrange(desc(date)) %>% 
      dplyr::filter(rrg == 1),
    
    output_signal %>% 
      dplyr::semi_join(bull) %>% 
      dplyr::group_split(ticker) %>% 
      purrr::map_df(detect_change, rbo_100) %>% 
      dplyr::arrange(desc(date)) %>% 
      dplyr::filter(rrg == 1)
  ) %>% 
  dplyr::arrange(desc(date))

bull_score <- output_signal %>% 
  dplyr::semi_join(bull) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::slice_tail(n = 1) %>% 
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100), names_to = "method", values_to = "signal") %>% 
  dplyr::group_by(rrg, ticker, sector, name, marginabile) %>% 
  dplyr::summarise(
    last_day_score = sum(signal),
    last_day_volume = mean(volume)
    ) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(last_day_score), desc(last_day_volume))

bull_swing <- output_signal %>% 
  dplyr::semi_join(bull) %>% 
  get_last_swing(lo3)

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

bear

bear_changes <- output_signal %>% 
  dplyr::semi_join(bear) %>% 
  dplyr::group_split(ticker) %>% 
  purrr::map_df(detect_change, rtt_5020) %>% 
  dplyr::arrange(desc(date)) %>% 
  dplyr::filter(rrg == -1) %>% 
  dplyr::bind_rows(
    output_signal %>% 
      dplyr::semi_join(bear) %>% 
      dplyr::group_split(ticker) %>% 
      purrr::map_df(detect_change, rsma_50100150) %>% 
      dplyr::arrange(desc(date)) %>% 
      dplyr::filter(rrg == -1),
    
    output_signal %>% 
      dplyr::semi_join(bear) %>% 
      dplyr::group_split(ticker) %>% 
      purrr::map_df(detect_change, rema_50100150) %>% 
      dplyr::arrange(desc(date)) %>% 
      dplyr::filter(rrg == -1),
    
    output_signal %>% 
      dplyr::semi_join(bear) %>% 
      dplyr::group_split(ticker) %>% 
      purrr::map_df(detect_change, rbo_100) %>% 
      dplyr::arrange(desc(date)) %>% 
      dplyr::filter(rrg == -1)
  ) %>% 
  dplyr::arrange(desc(date))

bear_score <- output_signal %>% 
  dplyr::semi_join(bear) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::slice_tail(n = 1) %>% 
  tidyr::pivot_longer(cols = c(rtt_5020, rsma_50100150, rema_50100150, rbo_100), names_to = "method", values_to = "signal") %>% 
  dplyr::group_by(rrg, ticker, sector, name, marginabile) %>% 
  dplyr::summarise(
    last_day_score = sum(signal),
    last_day_volume = mean(volume)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(last_day_score), desc(last_day_volume))

bear_swing <- output_signal %>% 
  dplyr::semi_join(bear) %>% 
  get_last_swing(lo3)

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
  write.table("signals/bull_signals.txt", sep = "\t", dec = ".", row.names = T)

bear_tot %>% 
  dplyr::filter(marginabile == "si") %>% 
  dplyr::arrange(last_day_score) %>% 
  write.table("signals/bear_signals.txt", sep = "\t", dec = ".", row.names = T)


