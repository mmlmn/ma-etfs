## INCLUDE

## DEFINE PERFORMANCE FUNCTIONS
get_aRC <- function(pv, scale = 252){ # annualised rate of return
  (pv[length(pv)] / pv[1]) ^ (scale/length(pv)) - 1
}

get_aSD <- function(pv, scale = 252){ # annualised standard deviation
  ((pv - lag(pv)) / lag(pv)) %>% sd(., na.rm = T) * sqrt(scale)
}

get_SR <- function(pv, scale = 252){ # Sharpe Ratio (Rf = 0)
  get_aRC(pv, scale) / get_aSD(pv, scale)
} # NOTE: TO CALCULATE SR in the STUDY, WE USE get_IR() and refer it to risk-free rate

get_IR <- function(pv, pvb, scale = 252) { # Information ratio (wrt benchmark/Rf)
  rc <- (pv - lag(pv)) / lag(pv)
  rcb <- (pvb - lag(pvb)) / lag(pvb)
  
  (prod(1 + rc, na.rm = T)^(scale/length(rc)) - 1 - 
      (prod(1 + rcb, na.rm = T)^(scale/length(rcb)) - 1)) / 
    (sd(rc - rcb, na.rm = T) * sqrt(scale))
}

get_MD <- function(pv){
  ((pv - lag(pv)) / lag(pv)) %>% PerformanceAnalytics::maxDrawdown() %>% 
    suppressWarnings()
}

get_MDD <- function(pv, scale = 252) {
  
  current_max <- pv[1]
  LD          <- rep(NA, length(pv))
  LD[1]       <- 0
  
  for (i in 2:length(pv)) {
    if (pv[i] >= current_max) {
      
      current_max <- pv[i]
      LD[i] <- 0
      
    } else {
      
      LD[i] <- LD[i-1] + 1
      
    }
  }
  
  LD / scale
  
}



## DEFINE OTHER

## Ensure a directory for storing data exists or create it if it does not.
#  Should be relative path if used from within a project, i.e.
#  "data/raw" or "data/results".
h_ensure_dir <- function(rel_path) {
  
  ifelse(!dir.exists(file.path(getwd(), rel_path)), 
         dir.create(file.path(getwd(), rel_path), recursive = TRUE), 
         FALSE)
}




h_numformat <- function(val) { 
  sub("^(-?)0.", "\\1.", sprintf("%.2f", val))
}




# h_express_strategy <- function(freq_, type_, st_lag_, lt_lag_, band_) {
#   expr(
#     paste("(",
#           (!!if_else(freq_ == "ma", "MA", "QUIMA"))[F], ", ",
#           (!!type_)[T], ", ",
#           (!!st_lag_)[S],  ", ", 
#           (!!lt_lag_)[L],  ", ", 
#           (!!band_)[B],  ")"
#     )
#   )
# }
