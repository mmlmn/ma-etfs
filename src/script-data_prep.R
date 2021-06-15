##------------------------------------------------------------------------------#
## CHUNK: data-prep


# # ### CONTENT GENERATION
# ## GENERATE STRATEGY RESULTS (incl. drawdowns)
# etfw20l_ma_tc_results <- s_get_results(etfs$ETFW20L, "ma", T)
# etfdax_ma_tc_results <- s_get_results(etfs$ETFDAX, "ma", T)
# etfsp500_ma_tc_results <- s_get_results(etfs$ETFSP500, "ma", T)
# etfw20l_quima_tc_results <- s_get_results(etfs$ETFW20L, "quima", T)
# etfdax_quima_tc_results <- s_get_results(etfs$ETFDAX, "quima", T)
# 
# etfw20l_ma_notc_results <- s_get_results(etfs$ETFW20L, "ma", F)
# etfdax_ma_notc_results <- s_get_results(etfs$ETFDAX, "ma", F)
# etfsp500_ma_notc_results <- s_get_results(etfs$ETFSP500, "ma", F)
# etfw20l_quima_notc_results <- s_get_results(etfs$ETFW20L, "quima", F)
# etfdax_quima_notc_results <- s_get_results(etfs$ETFDAX, "quima", F)
# 
# 
# # ## ANALYSE & PRODUCE CONTENT
# etfw20l_ma_tc_analysis <- s_analyse_results(etfw20l_ma_tc_results)
# etfdax_ma_tc_analysis <- s_analyse_results(etfdax_ma_tc_results)
# etfsp500_ma_tc_analysis <- s_analyse_results(etfsp500_ma_tc_results)
# etfw20l_quima_tc_analysis <- s_analyse_results(etfw20l_quima_tc_results)
# etfdax_quima_tc_analysis <- s_analyse_results(etfdax_quima_tc_results)
# 
# etfw20l_ma_notc_analysis <- s_analyse_results(etfw20l_ma_notc_results)
# etfdax_ma_notc_analysis <- s_analyse_results(etfdax_ma_notc_results)
# etfsp500_ma_notc_analysis <- s_analyse_results(etfsp500_ma_notc_results)
# etfw20l_quima_notc_analysis <- s_analyse_results(etfw20l_quima_notc_results)
# etfdax_quima_notc_analysis <- s_analyse_results(etfdax_quima_notc_results)



## LOAD SAVED STRATEGY RESULTS
etfw20l_ma_tc_results <- read_rds("data/results/ETFW20L_ma_tc_results.Rds")
etfdax_ma_tc_results <- read_rds("data/results/ETFDAX_ma_tc_results.Rds")
etfsp500_ma_tc_results <- read_rds("data/results/ETFSP500_ma_tc_results.Rds")
etfw20l_quima_tc_results <- read_rds("data/results/ETFW20L_quima_tc_results.Rds")
etfdax_quima_tc_results <- read_rds("data/results/ETFDAX_quima_tc_results.Rds")

etfw20l_ma_notc_results <- read_rds("data/results/ETFW20L_ma_notc_results.Rds")
etfdax_ma_notc_results <- read_rds("data/results/ETFDAX_ma_notc_results.Rds")
etfsp500_ma_notc_results <- read_rds("data/results/ETFSP500_ma_notc_results.Rds")
etfw20l_quima_notc_results <- read_rds("data/results/ETFW20L_quima_notc_results.Rds")
etfdax_quima_notc_results <- read_rds("data/results/ETFDAX_quima_notc_results.Rds")

## LOAD SAVED ANALYSIS CONTENT
etfw20l_ma_tc_analysis <- read_rds("data/results/ETFW20L_ma_tc_analysis.Rds")
etfdax_ma_tc_analysis <- read_rds("data/results/ETFDAX_ma_tc_analysis.Rds")
etfsp500_ma_tc_analysis <- read_rds("data/results/ETFSP500_ma_tc_analysis.Rds")
etfw20l_quima_tc_analysis <- read_rds("data/results/ETFW20L_quima_tc_analysis.Rds")
etfdax_quima_tc_analysis <- read_rds("data/results/ETFDAX_quima_tc_analysis.Rds")

etfw20l_ma_notc_analysis <- read_rds("data/results/ETFW20L_ma_notc_analysis.Rds")
etfdax_ma_notc_analysis <- read_rds("data/results/ETFDAX_ma_notc_analysis.Rds")
etfsp500_ma_notc_analysis <- read_rds("data/results/ETFSP500_ma_notc_analysis.Rds")
etfw20l_quima_notc_analysis <- read_rds("data/results/ETFW20L_quima_notc_analysis.Rds")
etfdax_quima_notc_analysis <- read_rds("data/results/ETFDAX_quima_notc_analysis.Rds")

##------------------------------------------------------------------------------#


##------------------------------------------------------------------------------#
## CHUNK: *-quima-*-type

etfw20l_quima_tc_type_dat <- bind_rows("ma" = etfw20l_ma_tc_analysis$dat_type_rank, 
                                       "quima" = etfw20l_quima_tc_analysis$dat_type_rank, .id = "freq") %>% 
  arrange(id, freq, desc(avg.IR)) %>% 
  select(-c(freq, id))

etfdax_quima_tc_type_dat <- bind_rows("ma" = etfdax_ma_tc_analysis$dat_type_rank, 
                                      "quima" = etfdax_quima_tc_analysis$dat_type_rank, .id = "freq") %>% 
  arrange(id, freq, desc(avg.IR)) %>% 
  select(-c(freq, id))




etfw20l_quima_notc_type_dat <- bind_rows("ma" = etfw20l_ma_notc_analysis$dat_type_rank, 
                                       "quima" = etfw20l_quima_notc_analysis$dat_type_rank, .id = "freq") %>% 
  arrange(id, freq, desc(avg.IR)) %>% 
  select(-c(freq, id))

etfdax_quima_notc_type_dat <- bind_rows("ma" = etfdax_ma_notc_analysis$dat_type_rank, 
                                      "quima" = etfdax_quima_notc_analysis$dat_type_rank, .id = "freq") %>% 
  arrange(id, freq, desc(avg.IR)) %>% 
  select(-c(freq, id))

##------------------------------------------------------------------------------#


# # WRITE AUTO-WRAPPER
# lapply(paths, function(path_) {
# 
#   if(!exists(path_)) {
#     # etfw20l_ma_tc_results <- s_get_results(etfs$ETFW20L, "ma", T)
#     s_get_results
#   }
# 
# 
# 
# })
# 
# pwalk(paths, function(row) {
#   
#   if(!exists(df$path)) {
#     s_get_results(row$)
#   }
#   
# })
# 
# paths <- crossing(
#   folder = "data/results/",
#   etf = names(etfs),
#   freq = c("_ma", "_quima"),
#   tc_enabled = c("_tc", "_notc"),
#   file = c("_results")
# ) %>%
#   filter(!(etf == "ETFSP500" & freq == "_quima")) %>%
#   unite(., path, sep = "", remove = F) %>% 
#   select(path, etf, freq, tc_enabled)
# 


