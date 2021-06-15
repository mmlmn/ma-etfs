
##----------------------------------------------------------------------------##
{ # Environment setup
  
  # READ RAW DATA
  etfs <- readRDS("data/input/etfs.rds")
  
  # INCLUDE
  required <- list(pkgs = c("Rcpp", "tidyverse", "TTR", "tidyquant", 
                            "PerformanceAnalytics", "gridExtra", "dygraphs", 
                            "kableExtra", "wesanderson", "htmlwidgets",
                            "webshot", "formattable"), 
                   srcs = c("include-s_helper_functions.R",
                            "script-input_summaries.R",
                            "function-s_run_strategy.R",  # Rcpp compilation
                            "function-s_get_results.R", 
                            "function-s_analyse_results.R"),
                   dirs = c("data/input", "data/results", "data/results/static")
  )
  
  sapply(required$pkgs, function(vec) {
    if(!require(vec, character.only = T)) { 
      install.packages(vec); library(vec, character.only = T)}})
  
  sapply(required$srcs, function(vec) {
    source(paste0("src/", vec))})
  
  sapply(required$dirs, h_ensure_dir)
  
}
##----------------------------------------------------------------------------##





##----------------------------------------------------------------------------##
# To get results of a single strategy, simply run

s_run_strategy(df = etfs$ETFSP500,
               ma_freq = "ma",    # Trading frequency: "ma" / "quima". Note: lowercase
               ma_type = "SMA",   # MA type: "SMA" / "WMA" / "EMA". Note: uppercase
               st_lag = 1,        # Short-term lag
               lt_lag = 20,       # Long-term lag
               ma_band = 0.03,    # Band value
               tc_enabled = TRUE) # Transaction cost switch
# This function returns a data frame with a "pval" column -- 
# -- a strategy's portfolio value at the end of each day
#    for a standard MA strategy on the product, using SMA
#    with a 1-day short MA, 20-day long MA, a band of 0.03
#    and trading costs enabled.
##----------------------------------------------------------------------------##


##----------------------------------------------------------------------------##
# To run a set of predefined strategies, execute

etfw20l_quima_tcon_results <- s_get_results(df_ = etfs$ETFW20L,
                                            ma_freq_ = "quima",
                                            tc_enabled_ = TRUE)

# This function returns a *list* with aggregated results 
# for a given product and trading frequency. It also produces a drawdowns graph
# for a subset of 2 top, 2 bottom an a middle strategy.
# It contains a predefined set of strategies available in the
# internal 'strat_specs' crossing. To change it, simply add or remove parameters
# from the parameter vectors used in the crossing function.
# Careful! Running many strategies requires free memory 
# and may cause system instability, especially on low-resource systems.
##----------------------------------------------------------------------------##



##----------------------------------------------------------------------------##
# To analyse output from the s_get_results() function, run s_analyse_results()
# using a list produced by the s_get_results() function.
# For example, you could run

etfw20l_quima_tcon_analysis <- s_get_results(df_ = etfs$ETFW20L,
                                             ma_freq_ = "quima",
                                             tc_enabled_ = FALSE) %>% 
  s_analyse_results()

# or

etfw20l_quima_tcon_analysis <- s_analyse_results(etfw20l_quima_tcon_results)

# This function analyses the results of the strategies constructed with the
# given parameter pool, producing tables, IR plots and heatmaps.
##----------------------------------------------------------------------------##