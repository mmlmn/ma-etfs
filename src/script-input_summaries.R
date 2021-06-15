##----------------------------------------------------------------------------##
## CHUNK: etfs-summary
if(!exists("data/results/etfs_summary.rds")) {
  
  etfs_summary_dat <- etfs %>%
    bind_rows(.id = "ETF") %>%
    group_by(ETF) %>%
    mutate(pval_bnh = c(Close[-n()] * (1 - Tc[1]),
                        Close[n()]  * (1 - Tc[1]) * (1 - Tc[n()])),
           pval_rfr = cumprod(c(Open[1], if_else(.data$Rf > 0,
                                                 1 + .data$Rf/25200, 1)))[-1]
    ) %>%
    summarise(First_Date = Date[1],
              Last_Date = Date[length(Date)],
              N = n(),
              TR = (pval_bnh[length(pval_bnh)] - pval_bnh[1]) / pval_bnh[1],
              aRC = get_aRC(pval_bnh),
              aSD = get_aSD(pval_bnh),
              SR = get_IR(pval_bnh, pval_rfr),
              MD = get_MD(pval_bnh),
              MDD = max(get_MDD(pval_bnh)),
              Skewness = skewness((pval_bnh - lag(pval_bnh)) / lag(pval_bnh), na.rm = T),
              Kurtosis = kurtosis((pval_bnh - lag(pval_bnh)) / lag(pval_bnh), na.rm = T)
    ) # exit summarise()
  
  plot_etf_theme <- theme_minimal() + 
    theme(# plot.title = element_text(color = "blue", size = 14),
          axis.title = element_text(face = "plain", color = "black"), 
          axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, vjust = -1), 
          axis.title.y = element_text(size = 14, vjust = 2),
          legend.title = element_blank())
  
  plot_etfw20l <- ggplot(data = etfs$ETFW20L, aes(x = Date, y = Close)) +
    geom_line() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    geom_hline(yintercept = etfs$ETFW20L$Close[1], colour = "red2", linetype = "dotted") +
    xlab("Date") +
    ylab("Close price") +
    # labs(title = "Product: ETFW20L") +
    plot_etf_theme
  
  plot_etfdax <- ggplot(data = etfs$ETFDAX, aes(x = Date, y = Close)) +
    geom_line() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    geom_hline(yintercept = etfs$ETFDAX$Close[1], colour = "red2", linetype = "dotted") +
    xlab("Date") +
    ylab("Close price") +
    # labs(title = "Product: ETFDAX") +
    plot_etf_theme
  
  plot_etfsp500 <- ggplot(data = etfs$ETFSP500, aes(x = Date, y = Close)) +
    geom_line() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    geom_hline(yintercept = etfs$ETFSP500$Close[1], colour = "red2", linetype = "dotted") +
    xlab("Date") +
    ylab("Close price") +
    # labs(title = "Product: ETFSP500") +
    plot_etf_theme
  
  etfs_summary <- lst(
    "data" = etfs_summary_dat,
    "plot_etfw20l" = plot_etfw20l,
    "plot_etfdax" = plot_etfdax,
    "plot_etfsp500" = plot_etfsp500
    )
  
  rm(plot_etf_theme, plot_etfw20l, plot_etfdax, plot_etfsp500)
  
  write_rds(etfs_summary, "data/results/etfs_summary.rds")
  
}

etfs_summary <- read_rds("data/results/etfs_summary.rds")



##----------------------------------------------------------------------------##




##------------------------------------------------------------------------------#
## CHUNK: parameters-summary
if(!exists("data/results/parameters_summary_dat.rds")) {
  
  parameters <- crossing(
    "ma_type" = c("SMA", "WMA", "EMA"),
    "st_lag" = c(1),
    "lt_lag" = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90,
                 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 
                 200, 210, 220, 230, 240, 250),
    "ma_band" = c(0, 0.005, 0.01, 0.015, 0.025, 0.03, 0.035, 0.04, 
                  0.045, 0.05, 0.055, 0.06, 0.065, 0.07, 0.075)
  ) %>% 
    filter(st_lag < lt_lag)
  
  
  # Column 3 - ugly, could use is.numeric() etc.
  parameters_col3 <- c(2, length(unique(parameters$ma_type)),
                       length(unique(parameters$st_lag)),
                       length(unique(parameters$lt_lag)),
                       length(unique(parameters$ma_band)))
  # Column 4
  parameters_col4 <- c(NA, NA, round(median(parameters$st_lag), 2),
                       round(median(parameters$lt_lag), 2),
                       round(median(parameters$ma_band), 2))
  
  parameters_summary_dat <- tribble(
    ~Variable,         ~ValueRange,            
    "Frequency",       "MA, QUIMA",          
    "Type",            "SMA, WMA, EMA",       
    "Short-term lag",  "1",   
    "Long-term lag",   "5, 10, 20, ..., 250",
    "Band",            "0, 0.005, ..., 0.075"
  ) %>%
    bind_cols(N = parameters_col3, MeanValue = parameters_col4)
  
  rm(parameters_col3, parameters_col4)
  
  write_rds(parameters_summary_dat, "data/results/parameters_summary_dat.rds")
  
}

parameters_summary_dat <- read_rds("data/results/parameters_summary_dat.rds")

##----------------------------------------------------------------------------##

