## This is the R-side strategy execution function.
#
#  It takes the following inputs:
#  - <df>: a data frame with
#          "Open", "High", "Low", "Close", "Rf" and "Tc" columns at the minimum,
#  - <ma_freq>: the strategy signal-generation frequency;
#          should be "ma" or "quima"
#  - <ma_type>: the type of generated MA series;
#          should be "SMA", "WMA" or "EMA"
#  - <st_lag>: the short-term lag length;
#  - <lt_lag>: the long-term lag length;
#          should in principle be greater than 1 and greater than st_lag,
#  - <ma_band>: floating-point value of a signal generation band;
#  - <tc_enabled>: a bool control for enabling/disabling transaction costs.
#
#  It returns <df> mutated with:
#  - 2 MA columns ("St_ma" and "Lt_ma"),
#  - 4 additional intraday MA columns ("quima"-only),
#  - 1 or 4 signal columns,
#  - 1 strategy portfolio value column.


source("src/function-s_calculate_ma.R")
Rcpp::sourceCpp('src/function-s_generate_trades.cpp')

s_run_strategy <- function(df, ma_freq, ma_type, st_lag, lt_lag,
                           ma_band, tc_enabled) {
  
  if(st_lag >= lt_lag || lt_lag == 1) { 
    stop("\nMoving average parameter error: 
         \nLag length error;\nlt_lag must be greater than 1\nst_lag must be smaller than lt_lag.")
  }
  
  
  
  df %>% 
    mutate(Tc = ifelse(tc_enabled == TRUE, Tc, 0)) %>% 
    mutate(St_ma = s_calculate_ma(.$Close, ma_type, st_lag)) %>% 
    mutate(Lt_ma = s_calculate_ma(.$Close, ma_type, lt_lag)) %>% 
    mutate(s_generate_trades(., ma_freq, ma_type, ma_band))
  
  
}
