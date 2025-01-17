library(tidyverse)

fondamentali <- readxl::read_excel('data_fondamentali.xlsx') %>% 
  dplyr::rename_with(stringr::str_to_lower) %>% 
  dplyr::select(instrid, description, symbol, capitalization, roe, roi, beta, dividendyield, peratio) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(
    ticker = symbol,
    name = description,
    isin = instrid
  )

ptf_fondamental <- readxl::read_excel('portafoglio-export.xlsx', skip = 2) %>% 
  dplyr::rename_with(stringr::str_to_lower) %>% 
  dplyr::rename_with(function(x) stringr::str_replace_all(x, " di ", "_")) %>% 
  dplyr::rename_with(function(x) stringr::str_replace_all(x, "p.zo", "prezzo")) %>% 
  dplyr::rename_with(function(x) stringr::str_replace_all(x, " ", "_")) %>% 
  dplyr::rename_with(function(x) stringr::str_replace_all(x, "%", "_perc")) %>% 
  dplyr::rename_with(function(x) stringr::str_replace_all(x, "€", "amount")) %>% 
  dplyr::rename(
    ticker = simbolo,
    name = titolo
  ) %>% 
  dplyr::filter(!is.na(ticker)) %>% 
  dplyr::left_join(fondamentali)

ptf_fondamental %>% 
  write.table("signals/ptf_fondamental.txt", sep = "\t", dec = ".", row.names = FALSE)

port <- ptf_fondamental %>% 
  dplyr::select(name, ticker, beta, quantità, prezzo_medio_carico, isin) %>% 
  dplyr::rename(
    Name = name,
    Beta = beta,
    Shares = quantità,
    Cost = prezzo_medio_carico
  ) %>% 
  dplyr::mutate(
    Side = dplyr::if_else(Shares > 0, 1, -1)
  ) %>% 
  dplyr::mutate(
    ticker = dplyr::if_else(ticker == 'NOVC.FRA', 'NOV.DE', ticker)
  )

movimenti <- readxl::read_excel('movimento titoli.xlsx', skip = 5) %>% 
  dplyr::rename_with(stringr::str_to_lower) %>% 
  dplyr::rename_with(function(x) stringr::str_replace_all(x, " di ", "_")) %>% 
  dplyr::rename_with(function(x) stringr::str_replace_all(x, "p.zo", "prezzo")) %>% 
  dplyr::rename_with(function(x) stringr::str_replace_all(x, " ", "_")) %>% 
  dplyr::rename_with(function(x) stringr::str_replace_all(x, "%", "_perc")) %>% 
  dplyr::rename_with(function(x) stringr::str_replace_all(x, "€", "amount")) %>% 
  dplyr::rename(
    name = titolo
  ) 

movimenti_ptf <- movimenti %>% 
  dplyr::semi_join(port, 'isin') %>% 
  dplyr::filter(stringr::str_detect(descrizione, 'Lending', negate = TRUE)) %>% 
  dplyr::mutate(quantita = dplyr::if_else(segno == 'V', -1, 1) * quantita) %>%
  dplyr::arrange(isin, desc(operazione)) %>% 
  dplyr::group_by(name, isin) %>% 
  dplyr::summarise(quantita = sum(quantita)) %>% 
  dplyr::filter(quantita != 0)

date_movimenti <- movimenti %>% 
  dplyr::mutate(
    operazione = lubridate::dmy(operazione)
  ) %>%
  dplyr::filter(stringr::str_detect(descrizione, 'Lending', negate = TRUE)) %>% 
  dplyr::filter(stringr::str_detect(descrizione, 'Leva', negate = TRUE)) %>% 
  dplyr::semi_join(movimenti_ptf, 'name') %>% 
  dplyr::count(operazione, descrizione, name) %>% 
  dplyr::arrange(name, desc(operazione)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(name) %>% 
  dplyr::slice_head(n = 1) %>% 
  dplyr::select(-c(descrizione, n))

port <- port %>% 
  dplyr::left_join(
    date_movimenti, join_by(Name == name)
  ) %>% 
  tidyr::replace_na(list(operazione = lubridate::today()-1)) %>% 
  dplyr::rename(
    date_entry = operazione
  ) %>% 
  dplyr::mutate(
    ticker = dplyr::if_else(ticker == 'NOVC.FRA', 'NOV.DE', ticker)
  )

bmck <- readxl::read_excel('data/FTSEMIB.MI.xlsx') %>% 
  dplyr::select(date, close) %>%
  dplyr::mutate(
    date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
  )

price <- output_signal %>% 
    dplyr::select(date, name, ticker, close) %>% 
    dplyr::mutate(
      date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
    ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-name) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::slice_tail(n = 1)

SL <- readr::read_delim('signals/stop_loss.txt') %>% 
  dplyr::select(ticker, lo3, hi3) %>% 
  dplyr::semi_join(port)


port %>% 
  dplyr::mutate(
    ticker = dplyr::if_else(ticker == 'NOVC.FRA', 'NOV.DE', ticker)
  ) %>% 
  dplyr::left_join(bmck, join_by(date_entry == date)) %>% 
  dplyr::rename(
    Cost_bm = close
  ) %>% 
  dplyr::mutate(
    today = lubridate::today()-1
  ) %>% 
  dplyr::left_join(bmck, join_by(today == date)) %>% 
  dplyr::rename(
    Price_bm = close
  ) %>% 
  dplyr::left_join(price, by = join_by(ticker, today == date)) %>% 
  dplyr::rename(
    Price = close
  ) %>% 
  dplyr::mutate(
    rCost = Cost / Cost_bm,
    rPrice = Price / Price_bm
  ) %>% 
  dplyr::arrange(Name) %>% 
  dplyr::left_join(SL) %>% 
  dplyr::mutate(
    SL = dplyr::if_else(Shares < 0, hi3, lo3),
    hi3 = NULL, 
    lo3 = NULL
  ) %>% 
  openxlsx::write.xlsx('ptf_risk.xlsx')




