#include "function-s_generate_trades_class.h"



// [[Rcpp::export]]
Rcpp::DataFrame s_generate_trades(Rcpp::DataFrame r_df,
                                  std::string ma_freq,
                                  std::string ma_type,
                                  double ma_band) {
  
  // Instantiate an object of ma_strat class
  ma_strat strat = ma_strat(r_df, ma_freq, ma_type, ma_band);
  
  // Return a complete strategy result
  return strat.run_strat();
  
}
