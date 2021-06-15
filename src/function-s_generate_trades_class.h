#pragma once
// [[Rcpp::plugins(cpp14)]]
#include <string>
#include <algorithm>
#include <cmath>
#include <Rcpp.h>



class ma_strat { 
  
private:
  // Input: R DataFrame-derived
  Rcpp::NumericVector st_ma_;       // [data] short-term MA vector (== "Close" for st_lag_ = 1)
  Rcpp::NumericVector lt_ma_;       // [data] long-term MA vector
  Rcpp::NumericMatrix quotes_;      // [data] daily quotations matrix
  Rcpp::NumericVector rf_;          // [data] risk-free rates
  Rcpp::NumericVector tc_;          // [data] transaction costs
  int n_;                           // [data] number of observations
  int st_lag_;                      // [strat/user] short-term lag
  int lt_lag_;                      // [strat/user] long-term lag
  // Input: other parameters
  std::string ma_freq_;             // [strat/user] MA trading frequency ["ma" or "quima"]
  std::string ma_type_;             // [strat/user] MA type ["SMA", "WMA" or "EMA"]
  double ma_band_;                  // [strat/user] MA band value for signal generation sensitivity
  // bool gap_enabled_;                // [strat/user] control for opening gaps - NOT IMPLEMENTED
  Rcpp::NumericMatrix quima_st_ma_; // [strat/user] intraday-adjusted moving averages
  
  
  // Private helper functions
  Rcpp::NumericMatrix extract_quotes(const Rcpp::DataFrame& r_df) const;
  int check_lag_length(const Rcpp::NumericVector& vec) const;
  double adjust_rfr(double rf) const;
  double adjust_ma(int row, int col, double k) const;
  Rcpp::NumericMatrix gen_intra_ma() const;
  
  // Private signal-generation functions
  Rcpp::IntegerVector run_ma() const;
  Rcpp::IntegerMatrix run_quima() const;
  
  // Private portfolio value-generation functions
  Rcpp::NumericVector pval_ma(const Rcpp::IntegerVector& signals) const;
  Rcpp::NumericVector pval_quima(const Rcpp::IntegerMatrix& signals) const;
  
  
public:
  // Ctor
  ma_strat(const Rcpp::DataFrame& r_df,
           std::string ma_freq,
           std::string ma_type,
           double ma_band);
  
  // Public strategy execution function
  Rcpp::DataFrame run_strat() const;
  
};
