## This function is a wrapper around s_run_strategy().
#  It runs (without saving to disk) a selected subset of strategies and
#  aggregates their results in form of: 
# - average daily returns,
# - absolute return throughout the analysed investment horizon,
# - relative return w.r.t. B&H investment,
# - relative return w.r.t. compound Rf return.

source("src/function-s_run_strategy.R")
source("src/include-s_helper_functions.R")

s_get_results <- function(df_, ma_freq_, tc_enabled_) {
  
  
  ## EXTRACT PRODUCT CODE FROM INPUT DF NAME
  product_code <- gsub(".*\\$", "", deparse(substitute(df_)))
  
  
  
  ## DEFINE ALL STRATEGY COMBINATIONS
  strat_specs <- crossing(
    "ma_freq" = ma_freq_,       # e.g. c("ma", "quima")
    "ma_type" = c("SMA", "WMA", "EMA"),
    "st_lag" = c(1),
    "lt_lag" = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90,
                 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 
                 200, 210, 220, 230, 240, 250),
    "ma_band" = c(0, 0.005, 0.01, 0.015, 0.025, 0.03, 0.035, 0.04, 
                  0.045, 0.05, 0.055, 0.06, 0.065, 0.07, 0.075),
    "tc_enabled" = tc_enabled_  # boolean
    ) %>%
    filter(st_lag < lt_lag)
  
  
  
  ## RUN ALL STRATEGY COMBINATIONS ON INPUT
  strat_list <- pmap(strat_specs, 
                     s_run_strategy, 
                     df = df_)
  
  
  
  ## NAME LIST ELEMENTS
  names(strat_list) <- strat_specs %>% 
    unite("name") %>% 
    unlist()
  
  
  
  ## CALCULATE EQUITY LINES AND OFFLOAD THE LIST
  strat_list <- lapply(strat_list, function(list_df) {
    
    last_sig <- list_df %>%
      select(contains("sig")) %>%
      unlist(use.names = F) %>%
      subset(. != 0) %>%
      last() %>% ifelse(is.na(.) || length(.) == 0, 0, .)
    
    sig_sum <- list_df %>% 
      select(contains("sig")) %>%
      unlist(use.names = F) %>%
      sum(., na.rm = T)
    
    list_df <- list_df %>%
      mutate(pval = c(pval[-n()], if_else(last_sig == -1L | sig_sum == 0, 
                                          pval[n()], pval[n()] * (1 - Tc[n()]))), 
             pval_bnh = c(Close[-n()] * (1 - Tc[1]), 
                          Close[n()]  * (1 - Tc[1]) * (1 - Tc[n()])), 
             pval_rfr = cumprod(c(Open[1], if_else(.data$Rf > 0, 
                                                   1 + .data$Rf/25200, 1)))[-1]
             # across(across(contains("pval"),
             #               list(RCs = ~ (.x - lag(.x)) / lag(.x)),
             #               .names = "{.fn}_{.col}"
             #               # .names = paste0("{.fn}", gsub("^[^_]*", "", "{.col}"))
             # ))
      ) %>% select(-contains(c("Volume", "_ma", "_qma_", "last_", 
                               "Open", "High", "Low", "Close", "Tc")), 
                   -Rf)
  }) # exit lapply()
  
  
  
  ## SUMMARISE RESULTS
  strat_results <- strat_list %>% 
    bind_rows(.id = "strat") %>% 
    group_by(strat) %>% 
    summarise(
      across(contains("pval"),
             list(aRC = ~ get_aRC(.)),
             .names = "{.fn}_{.col}"
             # list(aRC = ~ (.[length(.)] / .[1]) ^ (252/length(.)) - 1), 
             # .names = paste0("{.fn}", gsub("^[^_]*", "", "{.col}"))
      ),
      aSD = get_aSD(pval), 
      IR = get_IR(pval, pval_bnh), 
      SR = get_IR(pval, pval_rfr), 
      # SR = get_SR(pval), # without reference to Rf
      MD = get_MD(pval), 
      MDD = max(get_MDD(pval)), 
      across(contains("_sig_"),
             list(sigs  = ~ unlist(., use.names = F) %>% subset(. != 0) %>% length()),
             .names = "{.fn}_{.col}"),
      nObs = n(), 
    ) %>% 
    mutate(sigs_total = rowSums(across(contains("_sig_")))) %>% 
    select(-(contains("_sig_") & !sigs_total))%>% 
    rename_with(., ~ gsub("aRC_pval", "aRC", .), contains("aRC_pval")) %>% 
    separate("strat", into = c("freq","type","st_lag","lt_lag","band","tc_bool"), 
             sep = "_", convert = T)
  
  
  ## DRAWDOWNS
  ## EXTRACT 2 TOP, 2 BOTTOM and a MIDDLE STRATEGY RESULTS
  dat_drawdowns <- strat_results %>% 
    arrange(., desc(IR)) %>% 
    filter(
      row_number() > max(row_number()) - 2 |
        row_number() < 3 |
        row_number() == round(0.5 * max(row_number()))) %>% 
    select(-c(8,9,15))
  
  ## PULL A NAMES VECTOR FOR SUBSETTING
  drawdowns_names <- dat_drawdowns %>% 
    select(1:6) %>% 
    unite(strat) %>% 
    unlist(., use.names = F)
  ## TRANSFORM NAMES FOR CONSISTENT VISUALISATION IN DYGRAPHS LEGEND
  drawdowns_legend <- dat_drawdowns %>% 
    select(2:5) %>%     # (1:5) - w/ freq parameter (longer)
    unite(., strat, sep = ", ") %>% 
    unlist(., use.names = F) %>% 
    paste0("(", ., ")")
  
  ## PULL STRATEGIES BY NAMES VECTOR FROM THE LIST OF STRATEGY DFs
  drawdowns_input <- sapply(drawdowns_names, function(name_) {
    
    strat_list[[name_]] %>% 
      select(Date, pval)
    
  }, simplify = FALSE) %>%
    set_names(drawdowns_legend) %>% 
    imap(.x = ., ~ set_names(.x, c("Date", .y))) %>% 
    reduce(inner_join, by = "Date") %>% 
    inner_join(x = ., 
               y = strat_list[[1]] %>% 
                 select(Date, pval_bnh), 
               by = "Date") %>% 
    rename_with(., ~ paste("(Buy-and-hold)"), contains("pval_bnh"))
  
  ## CONSTRUCT DRAWDOWNS
  # drawdowns_static <- {
  #   
  #   tmp <-
  #     drawdowns_input %>%
  #     select(-Date) %>%
  #     mutate(across(everything(), function(x) xts::diff.xts(x) / xts::lag.xts(x))) %>%
  #     xts::xts(., 
  #              order.by =  drawdowns_input$Date)
  #   
  #   chart.Drawdown(tmp, legend.loc = "bottomleft") 
  #   
  # }
  
  plot_drawdowns <- drawdowns_input %>% 
    select(-Date) %>% 
    mutate(across(everything(), function(x) xts::diff.xts(x) / xts::lag.xts(x))) %>% 
    xts::xts(., 
             order.by =  drawdowns_input$Date) %>% 
    Drawdowns() %>% 
    dygraph(.) %>% 
    dyAxis("y", label = "Drawdown", valueRange = c(-1, 0.15)) %>%
    dyLegend(width = 1000) %>%
    # dyRangeSelector(., height = 40) %>% 
    dyOptions(colors = c("orange", "blue", "green", "gray", "gray", "red")) %>%
    dyCSS("src/style-dygraphs.css")
  # dySeries("Buy-and-hold", color = "red") %>% 
  # dyAxis("y", valueRange = c(-1, 0.15))
  
  ## FILL THE DRAWDOWNS TABLE WITH BH DATA
  bh_results <- strat_list[[1]] %>% 
    select(Date, pval_bnh, pval_rfr) %>% 
    summarise(
      strat = paste("(Buy-and-hold)"),
      aRC = get_aRC(pval_bnh),
      aSD = get_aSD(pval_bnh),
      IR = NA,
      SR = get_IR(pval_bnh, pval_rfr),
      MD = get_MD(pval_bnh),
      MDD = max(get_MDD(pval_bnh)),
      sigs_total = NA
    )
  
  dat_drawdowns <- dat_drawdowns %>% 
    select(-tc_bool) %>% 
    mutate(freq = toupper(freq)) %>% 
    unite(., strat, 1:5, sep = ", ") %>% 
    mutate(strat = paste0("(", strat, ")")) %>% 
    bind_rows(., bh_results) %>% 
    mutate(ID = seq_len(nrow(.)), .before = strat)
  
  ## SAVE PLOT TO HTML, THEN SCREENCAP AND SAVE TO PNG
  h_ensure_dir("data/results/static")
  html_dir <- paste0("data/results/static/",
                     toupper(product_code), "_",
                     ma_freq_, "_",
                     if_else(tc_enabled_ == T, "tc", "notc"),
                     "_drawdowns.html")
  plot_dir <- paste0("data/results/static/",
                     toupper(product_code), "_",
                     ma_freq_, "_",
                     if_else(tc_enabled_ == T, "tc", "notc"),
                     "_drawdowns.png")
  
  saveWidget(plot_drawdowns, html_dir, selfcontained = FALSE)
  width <- 1080
  height <- 610
  webshot(html_dir, file = plot_dir,
          cliprect = c(10, 30, width+50, height+50),
          vwidth = width, vheight = height )
  
  ## CONSTRUCT RETURN OBJECT (results)
  results <- lst("name" = product_code,
                 "results" = strat_results,
                 "drawdowns_data" = dat_drawdowns,
                 "drawdowns_plot" = plot_drawdowns,
                 # "strategies" = strat_list # Careful - potentially large files
                 )
  
  
  
  ## SAVE RESULTS TO DISK (for convenience on first generation)
  h_ensure_dir("data/results")
  write_rds(results, paste0("data/results/",
                            product_code, "_",
                            ma_freq_, "_",
                            if_else(tc_enabled_ == T, "tc", "notc"),
                            "_results.Rds"))

  
  
  ## RETURN THE RESULTS
  return(results)
  
}
