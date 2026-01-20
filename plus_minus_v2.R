library(tidyverse)

load_data <- function(){
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
  
  plan(multisession, workers = availableCores() - 1)  # usa tutti i core tranne 1
  output_signal <- files_stocks %>%
    set_names(basename(.)) %>%                    # opzionale: nome lista = nome file
    future_map(~ read_delim(
      .x,
      delim = "\t",
      locale = locale(decimal_mark = "."),
      col_types = cols(),                         # lascia inferire i tipi (o specificali)
      progress = FALSE
    ),
    .options = furrr_options(seed = TRUE))       # importante per riproducibilità
  
  plan(sequential)
  output_signal <- dplyr::bind_rows(output_signal)
  
  # nrow(output_signal)
  output_signal <- output_signal %>%
    dplyr::ungroup() %>%
    dplyr::as_tibble()
  
  output_signal <- output_signal %>%
    dplyr::left_join(sectors, by = join_by(ticker)) %>%
    dplyr::left_join(marginabili, by = c("name" = "Descrizione")) %>% 
    dplyr::mutate(
      date = lubridate::ymd(paste(lubridate::year(date), lubridate::month(date), lubridate::day(date), "-"))
    )
  
  return(list(
    output_signal = output_signal,
    sectors = sectors
  ))
}

load_trades <- function(){
  readxl::read_excel('movimento titoli.xls', skip = 5) %>% 
    dplyr::rename_with(stringr::str_to_lower) %>% 
    dplyr::rename_with(function(x) stringr::str_replace_all(x, " di ", "_")) %>% 
    dplyr::rename_with(function(x) stringr::str_replace_all(x, "p.zo", "prezzo")) %>% 
    dplyr::rename_with(function(x) stringr::str_replace_all(x, " ", "_")) %>% 
    dplyr::rename_with(function(x) stringr::str_replace_all(x, "%", "_perc")) %>% 
    dplyr::rename_with(function(x) stringr::str_replace_all(x, "€", "amount")) %>% 
    dplyr::rename(
      name = titolo
    ) %>% 
    dplyr::arrange(name) %>% 
    dplyr::filter(stringr::str_detect(descrizione, 'Lending', negate = TRUE)) %>% 
    # dplyr::filter(stringr::str_detect(name, 'AVIO')) %>% 
    dplyr::mutate(
      operazione = lubridate::dmy(operazione),
      quantita = dplyr::if_else(segno == 'V', -1, 1) * quantita,
      controvalore = dplyr::if_else(segno == 'V', -1, 1) * controvalore,
      name = stringr::str_replace(name, "\\**", "")
    ) %>% 
    dplyr::arrange(operazione) %>% 
    dplyr::left_join(sectors)
}


split_transactions_by_cumulative_quantity <- function(df) {  
  tryCatch({  
    # Check if required columns are present  
    if (!"quantita" %in% names(df)) {  
      stop("Data frame must contain 'quantita' column.")  
    }  
    
    # Initialize variables  
    cumulative_quantity <- 0  
    transaction_id <- 1  
    split_df <- data.frame()  
    
    # Iterate through each row and split transactions  
    for (i in seq_len(nrow(df))) {  
      current_row <- df[i, ]  
      cumulative_quantity <- cumulative_quantity + current_row$quantita  
      
      # Add transaction ID to the current row  
      current_row$transaction_id <- transaction_id  
      
      # Add row to the new DataFrame  
      split_df <- rbind(split_df, current_row)  
      
      # If cumulative quantity hits zero, increment transaction ID  
      if (cumulative_quantity == 0) {  
        transaction_id <- transaction_id + 1  
      }  
    }  
    
    return(split_df)  
    
  }, error = function(e) {  
    message("An error occurred: ", e$message)  
    return(NULL)  # Return NULL in case of error  
  })  
}  

transactions_per_ticker <- function(df, ticker_value = NULL) {
  print(ticker_value)
  
  df <- df %>% 
    dplyr::filter(stringr::str_detect(name, 'AIIB|BOT|EIB|BTP', negate = TRUE)) %>% 
    dplyr::filter(stringr::str_detect(descrizione, 'Rettifi|CFD|Stacco|Leva|Opzione', negate = TRUE)) %>%
    dplyr::filter(divisa == 'EUR')
  
  # print(ticker) 
  # Filtro per ticker: applicato solo se ticker è fornito e non vuoto
  if (!is.null(ticker_value) & nzchar(ticker_value)) {
    df <- df %>%
      dplyr::filter(stringr::str_detect(ticker, ticker_value))
  }
  
  df %>% 
    dplyr::select(-c(data_valuta, isin, divisa, prezzo, cambio, descrizione)) %>% 
    dplyr::group_by(name, transaction_id) %>% 
    dplyr::mutate(
      transaction_id_trade = 1:n(),
      # trade_type = dplyr::if_else(transaction_id_trade == 1 & segno == 'V', 'short', 'long'),
      cum_quantita = cumsum(quantita),
      trade_status = dplyr::if_else(dplyr::last(cum_quantita) == 0, 'closed', 'open'),
      trade_start_date = min(operazione),
      trade_end_date = max(operazione),
      trade_year = lubridate::year(trade_end_date)
    ) %>% 
    # dplyr::filter(trade_status == 'closed') %>% 
    dplyr::rename(
      op_date = operazione
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-c(trade_status:trade_year)) %>%
    mutate(prezzo_operazione = abs(controvalore / quantita))
}

get_closing_per_ticker <- function(output_signal, ticker_name, min_date, max_date){
  output_signal %>% 
    dplyr::select(ticker, date, close) %>% 
    dplyr::filter(stringr::str_detect(ticker, ticker_name)) %>% 
    dplyr::filter(between(date, min_date, max_date)) %>% 
    dplyr::rename(
      op_date = date,
      close_price = close
    ) 
}

get_stuff <- function(trades, prezzi){
  prezzi %>% 
    dplyr::left_join(trades) %>% 
    # dplyr::filter(!is.na(name)) %>% 
    tidyr::fill(
      c(name, segno, transaction_id, transaction_id_trade, trade_type, cum_quantita, prezzo_operazione), .direction = 'down'
    ) %>% 
    tidyr::replace_na(list(controvalore = 0)) %>% 
    arrange(op_date, transaction_id_trade) %>%   # ordine cronologico rigoroso
    mutate(
      # 1. Flusso di cassa reale (soldi che entrano/escono dal conto)
      cash_flow = -controvalore,                  
      # Es: acquisto (controvalore > 0)  → cash_flow negativo (escono soldi)
      #     vendita  (controvalore < 0) → cash_flow positivo (entrano soldi)
      
      # 2. Variazione della posizione in azioni (tenendo conto del segno)
      shares_delta = case_when(
        segno == "A" ~ +quantita,
        segno == "V" ~ -quantita
      ),
      
      # 3. Controllo coerenza con cum_quantita già presente
      cum_shares_calc = cumsum(shares_delta),     # dovrebbe coincidere con cum_quantita
      
      # 4. Cash cumulato nel tempo
      cum_cash = cumsum(cash_flow),
      
      # 5. Prezzo per il mark-to-market: usiamo prezzo_operazione e lo portiamo avanti
      last_price = close_price,
      last_price = na.locf(last_price, na.rm = FALSE),  # forward fill fino a nuova operazione
      
      # 6. Valore di mercato della posizione aperta (funziona sia long che short!)
      mtm_value = cum_quantita * last_price,      
      # Se cum_quantita > 0 (long)  → positivo e cresce se prezzo sale
      # Se cum_quantita < 0 (short) → negativo e diventa più negativo se prezzo sale (perdita)
      
      # 7. VALORE TOTALE DEL PORTAFOGLIO (equity)
      portfolio_value = cum_cash + mtm_value,
      
      # 8. Opzionale: PnL non realizzato e realizzato separati
      unrealized_pnl = mtm_value,
      realized_pnl   = cum_cash   # approssimazione valida se partiamo da cash iniziale = 0
    ) %>%
    dplyr::filter(!is.na(name)) %>% 
    select(
      op_date, name, transaction_id, trade_type, segno, quantita, prezzo_operazione,
      controvalore, cash_flow, cum_quantita, cum_cash,
      last_price, mtm_value, portfolio_value, unrealized_pnl, realized_pnl
    )
}



library(zoo)        # per na.locf
library(tidyverse)
library(furrr)     # futuro + purrr per map parallelo
library(readr)     # per read_delim più veloce e robusto

data <- load_data()
output_signal = data$output_signal
sectors = data$sectors

all_trades <- load_trades()

split_transactions <- all_trades %>% 
  dplyr::group_split(name, isin) %>% 
  purrr::map(split_transactions_by_cumulative_quantity) %>% 
  dplyr::bind_rows()

trade_type <- split_transactions %>% 
  dplyr::filter(stringr::str_detect(name, 'AIIB|BOT|EIB|BTP', negate = TRUE)) %>% 
  dplyr::filter(stringr::str_detect(descrizione, 'Rettifi|CFD|Stacco|Leva|Opzione', negate = TRUE)) %>%
  # dplyr::filter(stringr::str_detect(name, 'A2A')) %>%
  dplyr::filter(divisa == 'EUR') %>% 
  dplyr::select(-c(data_valuta, isin, divisa, prezzo, cambio, descrizione)) %>% 
  dplyr::group_by(name, transaction_id) %>% 
  dplyr::slice_head(n=1) %>% 
  dplyr::group_by(name, transaction_id) %>% 
  dplyr::mutate(
    trade_type = dplyr::if_else(segno == 'V', 'short', 'long')
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(operazione, name, trade_type) %>% 
  dplyr::rename(
    op_date = operazione
  )

ticker_name <- 'BMED.MI'

trades_ticker <- split_transactions %>% 
  transactions_per_ticker(ticker = ticker_name) %>% 
  dplyr::left_join(trade_type, join_by(name, op_date)) %>% 
  fill(trade_type, .direction = 'down')

prezzi <- output_signal %>% 
  get_closing_per_ticker(ticker_name = ticker_name, min_date =  as.Date('2023-01-02'), max_date =  as.Date('2025-12-25'))

a <- get_stuff(trades = trades_ticker, prezzi = prezzi)

ticker_name <- 'BPE.MI'

trades_ticker <- split_transactions %>% 
  transactions_per_ticker(ticker = ticker_name) %>% 
  dplyr::left_join(trade_type, join_by(name, op_date)) %>% 
  fill(trade_type, .direction = 'down')

prezzi <- output_signal %>% 
  get_closing_per_ticker(ticker_name = ticker_name, min_date =  as.Date('2023-01-02'), max_date =  as.Date('2025-12-25'))

b <- get_stuff(trades = trades_ticker, prezzi = prezzi)

a %>% 
  dplyr::bind_rows(b) %>% 
  dplyr::arrange(op_date) %>% 
  dplyr::group_by(op_date) %>% 
  dplyr::summarise(
    cum_quantita = sum(cum_quantita),
    cum_cash = sum(cum_cash),
    mtm_value = sum(mtm_value),
    portfolio_value = sum(portfolio_value),
  ) %>% view()
