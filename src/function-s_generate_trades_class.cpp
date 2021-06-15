#include "function-s_generate_trades_class.h"
// [[Rcpp::plugins(cpp14)]]


// ma_strat::ma_strat(const Rcpp::DataFrame& r_df,
//                    std::string ma_freq,
//                    std::string ma_type,
//                    double ma_band)
// 
//   : st_ma_{r_df["Ma_st"]},
//     lt_ma_{r_df["Ma_lt"]},
//     quotes_{extract_quotes(r_df)},
//     rf_{r_df["Rf"]},
//     tc_{r_df["Tc"]},
//     n_{r_df.nrow()},
//     st_lag_{check_lag_length(st_ma_)},
//     lt_lag_{check_lag_length(lt_ma_)},
//     ma_freq_{ma_freq},
//     ma_type_{ma_type},
//     ma_band_{ma_band},
//     quima_st_ma_{ma_freq == "quima" ? gen_intra_ma() : Rcpp::no_init(1, 1)} {}


ma_strat::ma_strat(const Rcpp::DataFrame& r_df,
                   std::string ma_freq,
                   std::string ma_type,
                   double ma_band) {
  
  st_ma_ = r_df["St_ma"];
  lt_ma_ = r_df["Lt_ma"];
  quotes_ = extract_quotes(r_df);
  rf_ = r_df["Rf"];
  tc_ = r_df["Tc"];
  n_ = r_df.nrow();
  st_lag_ = check_lag_length(st_ma_);
  lt_lag_ = check_lag_length(lt_ma_);
  ma_freq_ = ma_freq;
  ma_type_ = ma_type;
  ma_band_ = ma_band;
  quima_st_ma_ = ma_freq == "quima" ? gen_intra_ma() : Rcpp::no_init(1, 1);
  
}


Rcpp::NumericMatrix ma_strat::extract_quotes(const Rcpp::DataFrame& r_df) const {
  
  Rcpp::NumericVector op = r_df["Open"];
  Rcpp::NumericVector hi = r_df["High"];
  Rcpp::NumericVector lo = r_df["Low"];
  Rcpp::NumericVector cl = r_df["Close"];
  
  return Rcpp::cbind(op, hi, lo, cl);
  
}


int ma_strat::check_lag_length(const Rcpp::NumericVector& vec) const {
  
  auto iter = std::find_if(vec.begin(), vec.end(), [](auto a) { return a != 0.0; });
  
  
  if (iter == vec.end())
    return 1;
  else
    return (1 + static_cast<int>(std::distance(vec.begin(), iter)));
  
}


double ma_strat::adjust_rfr(double rf) const {
  
  
  if (std::islessequal(rf, 0.0)) // '<=' should be safe too
    return 0.0;
  else
    return rf/25200.0;
  
}


double ma_strat::adjust_ma(int row, int col, double k) const {
  
  double intra_readj_ma = 0.0;
  
  
  return intra_readj_ma = st_ma_[row] + k * (quotes_(row, col) - quotes_(row, 3));
  
}


Rcpp::NumericMatrix ma_strat::gen_intra_ma() const {
  
  double k = ma_type_ == "SMA" ? 1.0 / st_lag_ : 2.0 / (st_lag_ + 1);
  int frow = ma_type_ == "EMA" ? st_lag_ : st_lag_ - 1;
  int nrow = n_;                              // number of observations
  int ncol = quotes_.ncol() - 1;              // we adjust only what's necessary (OHL, -C)
  Rcpp::NumericMatrix ohl_ma(nrow, ncol);     // (n x 3) matrix: OHL adjustment
  
  if (ma_type_ == "EMA") {
    
    for (int j = 0; j < ncol; ++j) { // first non-zero row is filled with mean values of close+intraday
      
      ohl_ma(st_lag_ - 1, j) = adjust_ma(st_lag_ - 1, j, 1.0 / st_lag_);
      
    }
  }
  
  for (int i = frow; i < nrow; ++i) {
    for (int j = 0; j < ncol; ++j) {
      
      ohl_ma(i, j) = adjust_ma(i, j, k);
      
    }
  }
  
  Rcpp::NumericMatrix intra_ma = Rcpp::cbind(ohl_ma, st_ma_);
  
  return intra_ma;
  
}


Rcpp::IntegerVector ma_strat::run_ma() const {
  
  Rcpp::IntegerVector ma_sigs(n_);
  bool hold = 0;
  
  for (int i = lt_lag_; i < n_; ++i) {
    
    
    if (st_ma_[i-1] <= lt_ma_[i-1] * (1 + ma_band_) && // first-order golden cross
        st_ma_[i]   >  lt_ma_[i]   * (1 + ma_band_) && // second-order golden cross
        hold == 0) { 
      
      ma_sigs[i] = 1;
      hold = 1;
      
    } else if (
        st_ma_[i-1] >= lt_ma_[i-1] * (1 - ma_band_) && // first-order death cross
        st_ma_[i]   <  lt_ma_[i]   * (1 - ma_band_) && // second-order death cross
        hold == 1) { 
      
      ma_sigs[i] = -1;
      hold = 0;
      
    } // else ma_sigs[i] = 0; // NumericVectors initialised with 0's already
  }
  
  
  return ma_sigs;
  
}


Rcpp::IntegerMatrix ma_strat::run_quima() const {
  
  Rcpp::IntegerMatrix quima_sigs(n_, 4);
  bool hold = 0;
  int col_index = 0;
  
  // if (gap_enabled_ == 1) {
  //   Rcpp::Rcout << "temporary - decide about opening gaps";
  // }
  
  for (int i = lt_lag_; i < n_; ++i) {
    
    Rcpp::NumericVector q_row = quima_st_ma_(i, Rcpp::_);
    
    
    if (st_ma_[i-1] <= lt_ma_[i-1] * (1 + ma_band_) && hold == 0) {
      
      auto cross_iter = std::find_if(q_row.begin(), q_row.end(), [=](auto a) { 
        return a > lt_ma_[i-1] * (1 + ma_band_);});
      
      
      if (cross_iter != q_row.end()) {
        
        col_index = static_cast<int>(std::distance(q_row.begin(), cross_iter));
        
        if (col_index == 1 && // && lt_ma_[i-1] < quotes_(i, 1) unnecessary
            lt_ma_[i-1] < quotes_(i-1, 3)) {
          
          quima_sigs(i, col_index) = 0;
          hold = 0;
          
        } else {
          
          quima_sigs(i, col_index) = 1;
          hold = 1;
          
          if(st_ma_[i] < lt_ma_[i] * (1 + ma_band_)) { // flip back if reverted at close
            
            quima_sigs(i, 3) = -1;
            hold = 0;
            
          }
        }
      }
      
    } else if (st_ma_[i-1] >= lt_ma_[i-1] * (1 - ma_band_) && hold == 1) {
      
      auto cross_iter = std::find_if(q_row.begin(), q_row.end(), [=](auto a) { 
        return a < lt_ma_[i-1] * (1 - ma_band_);});
      
      
      if (cross_iter != q_row.end()) {
        
        col_index = static_cast<int>(std::distance(q_row.begin(), cross_iter));
        
        
        if (col_index == 2 && // && lt_ma_[i-1] > quotes_(i, 2) unnecessary
            lt_ma_[i-1] > quotes_(i-1, 3)) {
          
          quima_sigs(i, col_index) = 0;
          hold = 1;
          
        } else {
          
          quima_sigs(i, col_index) = -1;
          hold = 0;
          
          if (st_ma_[i] > lt_ma_[i] * (1 - ma_band_)) { // flip back if reverted at close
            
            quima_sigs(i, 3) = 1;
            hold = 1;
            
          }
        }
      }
    }
  }
  
  
  return quima_sigs;
  
}


Rcpp::NumericVector ma_strat::pval_ma(const Rcpp::IntegerVector& signals) const {
  
  bool hold = 0;
  double psize = 0.0;
  int last_sig = 0;
  
  Rcpp::NumericVector pval(n_);
  pval[0] = quotes_(0, 0) * (1 + adjust_rfr(rf_[0]));
  
  
  for (int i = 1; i < lt_lag_; ++i) { // risk-free returns before trading begins
    pval[i] = pval[i-1] * (1 + adjust_rfr(rf_[i]));
  }
  
  
  auto update_pval = [&](int i) {
    
    
    if (last_sig == 0 && hold == 0) {
      
      pval[i] = pval[i-1] * (1 + adjust_rfr(rf_[i]));
      
    } else if (last_sig == 1) {
      
      psize = pval[i-1] * (1 + adjust_rfr(rf_[i])) / quotes_(i, 3) * (1 - tc_[i]);
      pval[i] = psize * quotes_(i, 3);
      hold = 1;
      
    } else if (last_sig == 0 && hold == 1) {
      
      pval[i] = psize * quotes_(i, 3);
      
    } else { // if (last_sig == -1) {
      
      pval[i] = psize * quotes_(i, 3) * (1 - tc_[i]);
      hold = 0;
      
    }
  };
  
  
  for (int i = lt_lag_; i < n_; ++i) {
    
    last_sig = signals[i];
    update_pval(i);
    
  }
  
  
  return pval;
  
}


Rcpp::NumericVector ma_strat::pval_quima(const Rcpp::IntegerMatrix& signals) const {
  
  bool hold = 0;
  double psize = 0.0;
  double last_pval = 0.0;
  int last_sig = 0;
  int col_index = 0;
  
  Rcpp::NumericVector pval(n_);
  pval[0] = quotes_(0, 0) * (1 + adjust_rfr(rf_[0]));
  
  
  for (int i = 1; i < lt_lag_; ++i) { // risk-free returns before trading begins
    pval[i] = pval[i-1] * (1 + adjust_rfr(rf_[i]));
  }
  
  
  auto update_pval = [&](int i) {
    
    if (last_sig == 0 && hold == 0) {
      
      pval[i] = pval[i-1] * (1 + adjust_rfr(rf_[i]));
      
    } else if (last_sig == 0 && hold == 1) {
      
      pval[i] = psize * quotes_(i, 3);
      
    } else {
      
      double exec_quote = [&] {
        if (col_index == 0 || col_index == 3)
          return quotes_(i, col_index);
        else
          return lt_ma_[i-1];}();
      
      if (last_sig == 1) {
        
        psize = last_pval / exec_quote * (1 - tc_[i]);
        pval[i] = psize * quotes_(i, 3);
        hold = 1;
        
      } else if (last_sig == -1) {
        
        pval[i] = psize * exec_quote * (1 - tc_[i]);
        hold = 0;
        
      }
    }
  };
  
  
  for (int i = lt_lag_; i < n_; ++i) {
    
    
    Rcpp::IntegerVector sig_row = signals(i, Rcpp::_);
    
    auto sig_iter = std::find_if(sig_row.begin(), sig_row.end(), [](auto sig) {
      return sig != 0; }); // iter to first non-zero value in a row
    
    
    if (sig_iter != sig_row.end()) { // if non-zero signal within row range
      
      col_index = static_cast<int>(std::distance(sig_row.begin(), sig_iter));
      last_sig = sig_row[col_index];
      last_pval = pval[i-1];
      update_pval(i);
      
      
      if (last_sig * (-1) == sig_row[3]) { // if non-zero signal reverted at close
        
        col_index = 3;
        last_sig = sig_row[3];
        last_pval = pval[i];
        update_pval(i);
        
      } else if (last_sig == -1 && col_index == 0) { // if death cross found at Open and not reverted at Close
        
        pval[i] *= (1 + adjust_rfr(rf_[i])); // eligible for rf gains
        
      }
      
    } else { // row of 0's: either staying out or remaining on the market
      
      last_sig = 0;
      update_pval(i);
      
    }
  }
  
  return pval;
  
}


Rcpp::DataFrame ma_strat::run_strat() const {
  
  
  if (ma_freq_ == "ma") {
    
    Rcpp::IntegerVector ma_signals = run_ma();
    Rcpp::NumericVector portf_val = pval_ma(ma_signals);
    
    
    return Rcpp::DataFrame::create(Rcpp::_["ma_sig_cl"] = ma_signals,
                                   Rcpp::_["pval"] = portf_val);
    
  } else {
    
    Rcpp::IntegerMatrix quima_signals = run_quima();
    Rcpp::NumericVector portf_val = pval_quima(quima_signals);
    
    Rcpp::NumericVector qma_st_op = quima_st_ma_(Rcpp::_, 0);
    Rcpp::NumericVector qma_st_hi = quima_st_ma_(Rcpp::_, 1);
    Rcpp::NumericVector qma_st_lo = quima_st_ma_(Rcpp::_, 2);
    
    Rcpp::IntegerVector qma_sig_op = quima_signals(Rcpp::_, 0);
    Rcpp::IntegerVector qma_sig_hi = quima_signals(Rcpp::_, 1);
    Rcpp::IntegerVector qma_sig_lo = quima_signals(Rcpp::_, 2);
    Rcpp::IntegerVector qma_sig_cl = quima_signals(Rcpp::_, 3);
    
    
    return Rcpp::DataFrame::create(Rcpp::_["st_qma_op"] = qma_st_op,
                                   Rcpp::_["st_qma_hi"] = qma_st_hi,
                                   Rcpp::_["st_qma_lo"] = qma_st_lo,
                                   Rcpp::_["st_qma_cl"] = st_ma_,
                                   Rcpp::_["qma_sig_op"] = qma_sig_op,
                                   Rcpp::_["qma_sig_hi"] = qma_sig_hi,
                                   Rcpp::_["qma_sig_lo"] = qma_sig_lo,
                                   Rcpp::_["qma_sig_cl"] = qma_sig_cl,
                                   Rcpp::_["pval"] = portf_val);
    
  }
}
