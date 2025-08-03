library(tidyverse)

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
  dplyr::filter(stringr::str_detect(name, 'A2A')) %>% 
  dplyr::mutate(
    operazione = lubridate::dmy(operazione),
    quantita = dplyr::if_else(segno == 'V', -1, 1) * quantita,
    controvalore = dplyr::if_else(segno == 'V', -1, 1) * controvalore
    ) %>% 
  dplyr::arrange(operazione) %>% 
  dplyr::group_by(name) %>% 
  dplyr::mutate(
    cum_cvt = cumsum(controvalore),
    cum_qnt = cumsum(quantita),
    trade = dplyr::if_else(cum_qnt == 0, 1, 0)
  )


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

# Example usage  
df <- data.frame(  
  operazione = c("V", "V", "A", "A", "A", "V", "V"),  
  data_valuta = as.Date(c("2024-04-02", "2024-04-23", "2024-04-23", "2024-04-23", "2025-06-02", "2025-07-09", "2025-07-10")),  
  quantita = c(-1000, -1000, 1000, 1000, 550, -200, -350),  
  prezzo = c(1.66, 1.80, 1.81, 1.78, 2.29, 2.18, 2.18)  
)  

split_transactions <- split_transactions_by_cumulative_quantity(df)
split_transactions


df <- readxl::read_excel('movimento titoli.xls', skip = 5) %>% 
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
    controvalore = dplyr::if_else(segno == 'V', -1, 1) * controvalore
  ) %>% 
  dplyr::arrange(operazione)

split_transactions <- df %>% 
  dplyr::group_split(name, isin) %>% 
  purrr::map(split_transactions_by_cumulative_quantity) %>% 
  dplyr::bind_rows()


# split_transactions <- split_transactions_by_cumulative_quantity(df)
# split_transactions

split_transactions %>% 
  # dplyr::filter(stringr::str_detect(name, 'MONCL')) %>% 
  dplyr::group_by(name, transaction_id) %>% 
  dplyr::mutate(
    transaction_id_trade = 1:n(),
    trade_type = dplyr::if_else(transaction_id_trade == 1 && segno == 'V', 'short', 'long'),
    cum_quantita = cumsum(quantita),
    trade_status = dplyr::if_else(dplyr::last(cum_quantita) == 0, 'closed', 'open'),
    trade_start_date = min(operazione),
    trade_end_date = max(operazione),
    trade_year = lubridate::year(trade_end_date)
  ) %>% 
  # dplyr::group_split(transaction_id) %>% 
  dplyr::filter(trade_status == 'closed') %>% 
  dplyr::group_by(name, transaction_id, trade_year, trade_type, segno) %>% 
  dplyr::summarise(
    controvalore = sum(controvalore)
  ) %>% 
  tidyr::pivot_wider(names_from = segno, values_from = controvalore) %>% 
  dplyr::mutate(
    plus = dplyr::if_else(trade_type == 'long', -V - A, -V - A)
  ) %>% 
  dplyr::group_by(trade_year) %>% 
  dplyr::summarise(
    plus = sum(plus)
  )


split_transactions %>% 
  # dplyr::filter(stringr::str_detect(name, 'MONCL')) %>% 
  dplyr::group_by(name, transaction_id) %>% 
  dplyr::mutate(
    transaction_id_trade = 1:n(),
    trade_type = dplyr::if_else(transaction_id_trade == 1 && segno == 'V', 'short', 'long'),
    cum_quantita = cumsum(quantita),
    trade_status = dplyr::if_else(dplyr::last(cum_quantita) == 0, 'closed', 'open'),
    trade_start_date = min(operazione),
    trade_end_date = max(operazione),
    trade_year = lubridate::year(trade_end_date)
  ) %>% 
  # dplyr::group_split(transaction_id) %>% 
  dplyr::filter(trade_status == 'closed') %>% 
  dplyr::group_by(name, transaction_id, trade_year, trade_type, segno) %>% 
  dplyr::summarise(
    controvalore = sum(controvalore)
  ) %>% 
  tidyr::pivot_wider(names_from = segno, values_from = controvalore) %>% 
  dplyr::mutate(
    plus = dplyr::if_else(trade_type == 'long', -V - A, -V - A)
  ) %>% 
  dplyr::group_by(trade_year, transaction_id, name) %>% 
  dplyr::summarise(
    pl = sum(plus)
  ) %>% 
  dplyr::mutate(
    result_type = dplyr::if_else(pl > 1, 'plus', 'minus')
  ) %>% 
  dplyr::group_by(result_type) %>% 
  dplyr::summarise(
    pl = sum(pl),
    num_pl = n()
  ) %>% 
  dplyr::mutate(
    mean_pl = pl / num_pl
  )
  



split_transactions %>% 
  # dplyr::filter(stringr::str_detect(name, 'MONCL')) %>% 
  dplyr::group_by(name, transaction_id) %>% 
  dplyr::mutate(
    transaction_id_trade = 1:n(),
    trade_type = dplyr::if_else(transaction_id_trade == 1 && segno == 'V', 'short', 'long'),
    cum_quantita = cumsum(quantita),
    trade_status = dplyr::if_else(dplyr::last(cum_quantita) == 0, 'closed', 'open'),
    trade_start_date = min(operazione),
    trade_end_date = max(operazione),
    trade_year = lubridate::year(trade_end_date)
  ) %>% 
  dplyr::group_by(operazione) %>% 
  dplyr::summarise(controvalore = sum(controvalore)) %>% 
  dplyr::mutate(
    cumsum(controvalore)
  ) %>% View()

library(ggplot2)
library(scales)

split_transactions %>% 
  # dplyr::filter(stringr::str_detect(name, 'MONCL')) %>% 
  dplyr::group_by(name, transaction_id) %>% 
  dplyr::mutate(
    transaction_id_trade = 1:n(),
    trade_type = dplyr::if_else(transaction_id_trade == 1 && segno == 'V', 'short', 'long'),
    cum_quantita = cumsum(quantita),
    trade_status = dplyr::if_else(dplyr::last(cum_quantita) == 0, 'closed', 'open'),
    trade_start_date = min(operazione),
    trade_end_date = max(operazione),
    trade_year = lubridate::year(trade_end_date)
  ) %>% 
  # dplyr::group_split(transaction_id) %>% 
  dplyr::filter(trade_status == 'closed') %>% 
  dplyr::group_by(name, transaction_id, trade_year, trade_type, segno) %>% 
  dplyr::summarise(
    controvalore = sum(controvalore)
  ) %>% 
  tidyr::pivot_wider(names_from = segno, values_from = controvalore) %>% 
  dplyr::mutate(
    result = dplyr::if_else(trade_type == 'long', -V - A, -V - A),
    result_perc = result / dplyr::if_else(trade_type == 'long', A, -V)
  ) %>% 
  ggplot(aes(x = result_perc)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
  labs(
    title = "Distribuzione dei rendimenti",
    x = "Rendimento (%)",
    y = "Numero di operazioni"
  ) +
  theme_minimal()

ptf <- split_transactions %>% 
  # dplyr::filter(stringr::str_detect(name, 'MONCL')) %>% 
  dplyr::group_by(name, transaction_id) %>% 
  dplyr::mutate(
    transaction_id_trade = 1:n(),
    trade_type = dplyr::if_else(transaction_id_trade == 1 && segno == 'V', 'short', 'long'),
    cum_quantita = cumsum(quantita),
    trade_status = dplyr::if_else(dplyr::last(cum_quantita) == 0, 'closed', 'open'),
    trade_start_date = min(operazione),
    trade_end_date = max(operazione),
    trade_year = lubridate::year(trade_end_date)
  )

ptf %>% 
  dplyr::filter(name == 'ACEA', transaction_id == 1) %>% 
  dplyr::arrange((trade_start_date)) %>% 
  dplyr::select(transaction_id,name,  trade_start_date, operazione, segno, quantita, prezzo, controvalore, ) %>% 
  dplyr::left_join(
    output_signal %>% 
      dplyr::semi_join(ptf, join_by(name, date >= operazione)) %>% 
      dplyr::select(date, name, ticker, close)
  ) %>% 
  dplyr::group_by(name, transaction_id) %>% 
  dplyr::filter(date <= operazione) %>% 
  dplyr::mutate(
    
  )

prezzi <- ptf %>% 
  dplyr::filter(stringr::str_detect(name, 'FINCAN')) %>%
  dplyr::group_by(name, descrizione, transaction_id) %>% 
  dplyr::mutate(
    transaction_id_trade = 1:n(),
    trade_type = dplyr::if_else(transaction_id_trade == 1 && segno == 'V', 'short', 'long'),
    cum_quantita = cumsum(quantita),
    trade_status = dplyr::if_else(dplyr::last(cum_quantita) == 0, 'closed', 'open'),
    trade_start_date = min(operazione),
    trade_end_date = max(operazione),
    trade_year = lubridate::year(trade_end_date)
  ) %>%
  dplyr::select(operazione, name, descrizione, transaction_id, trade_year, trade_type, trade_status, segno, prezzo, quantita) %>% 
  dplyr::arrange(desc(operazione))

prezzo_medio_carico <- function(quantita, prezzi) {
  if (length(quantita) != length(prezzi)) {
    stop("Le lunghezze di 'quantita' e 'prezzi' devono essere uguali.")
  }
  prezzo_medio <- sum(quantita * prezzi) / sum(quantita)
  return(prezzo_medio)
}

prezzi %>% 
  dplyr::count(name, descrizione, transaction_id, segno) %>% 
  dplyr::filter(n > 1) %>% 
  right_join(prezzi) %>% 
  dplyr::filter(!is.na(n)) %>% 
  dplyr::arrange(descrizione, transaction_id, operazione) %>% 
  dplyr::group_split(name, descrizione, transaction_id)
  

ptf %>% 
  dplyr::filter(stringr::str_detect(name, 'CAMPARI')) %>%
  dplyr::group_by(name, descrizione, transaction_id) %>% 
  dplyr::mutate(
    transaction_id_trade = 1:n(),
    trade_type = dplyr::if_else(transaction_id_trade == 1 && segno == 'V', 'short', 'long'),
    cum_quantita = cumsum(quantita),
    trade_status = dplyr::if_else(dplyr::last(cum_quantita) == 0, 'closed', 'open'),
    trade_start_date = min(operazione),
    trade_end_date = max(operazione),
    trade_year = lubridate::year(trade_end_date)
  ) %>% 
  dplyr::arrange(desc(operazione)) %>% 
  dplyr::group_by(name, descrizione, transaction_id, trade_year, trade_type, trade_status, segno) %>% 
  dplyr::summarise(
    controvalore = sum(controvalore)
  ) %>% 
  dplyr::left_join(prezzi)

# %>% 
#   tidyr::pivot_wider(names_from = segno, values_from = controvalore) %>% 
#   dplyr::mutate(
#     plus = dplyr::if_else(trade_type == 'long', -V - A, -V - A)
#   )



