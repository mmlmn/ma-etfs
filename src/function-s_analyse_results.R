



s_analyse_results <- function(list_) {
  
  
  ## DEFINE HELPER FUNCTIONS
  fill_missing <- function(df) { # rank table completion
    
    ma_types <- c("SMA", "WMA", "EMA")
    which_missing <- !(ma_types %in% df$type)
    
    if (any(which_missing)) {
      
      missing_types <- ma_types[which_missing]
      
      df <- Reduce(function(df, x) {
        add_row(df, .after = nrow(df))
      },
      1:length(missing_types),
      init = df)
      
      df$type[is.na(df$type)] <- missing_types
      df
      
    } else df
  }
  
  
  
  ## SET UP PLOT PALETTES
  palette_disc <- wes_palette("BottleRocket2", 3, type = "discrete")
  palette_cont <- wes_palette("Zissou1", 100, type = "continuous")
  
  
  
  ## CONSTRUCT MA TYPE RANK TABLE OF ALL STRATEGIES
  # For positive IR values
  type_rank_op <- list_[[2]] %>%
    filter(IR > 0 & sigs_total > 0) %>% 
    group_by(type) %>% 
    summarise(avg.IR = mean(IR, na.rm = T),
              avg.SR = mean(SR, na.rm = T),
              avg.aRC = mean(aRC, na.rm = T),
              avg.aSD = mean(aSD, na.rm = T),
              avg.sigs = mean(sigs_total, na.rm = T),
              ltlag.min = min(lt_lag, na.rm = T),
              ltlag.max = max(lt_lag, na.rm = T),
              ltlag.avg = mean(lt_lag, na.rm = T),
              ltlag.med = median(lt_lag, na.rm = T),
              band.min = min(band, na.rm = T),
              band.max = max(band, na.rm = T),
              band.avg = mean(band, na.rm = T), 
              strat.count = n(), 
              pool.share = 100 * n() / nrow(list_[[2]])
              # print.share = sprintf("%0.2f\\\\%%", pool.share * 100)
    ) %>% 
    arrange(desc(avg.IR)) %>% 
    fill_missing()
  
  # For IR values above the third quartile Q3
  type_rank_q3 <- list_[[2]] %>% 
    filter(IR > quantile(IR, 0.75)  & sigs_total > 0) %>% 
    group_by(type) %>% 
    summarise(avg.IR = mean(IR, na.rm = T),
              avg.SR = mean(SR, na.rm = T),
              avg.aRC = mean(aRC, na.rm = T),
              avg.aSD = mean(aSD, na.rm = T),
              avg.sigs = mean(sigs_total, na.rm = T),
              ltlag.min = min(lt_lag, na.rm = T),
              ltlag.max = max(lt_lag, na.rm = T),
              ltlag.avg = mean(lt_lag, na.rm = T),
              ltlag.med = median(lt_lag, na.rm = T),
              band.min = min(band, na.rm = T),
              band.max = max(band, na.rm = T),
              band.avg = mean(band, na.rm = T), 
              strat.count = n(), 
              pool.share = 100 * n() / nrow(list_[[2]])
              # print.share = sprintf("%0.2f\\\\%%", pool.share * 100)
    ) %>% 
    arrange(desc(avg.IR))
  
  # For IR values below the first quartile Q1
  type_rank_q1 <- list_[[2]] %>% 
    filter(IR < quantile(IR, 0.25)  & sigs_total > 0) %>% 
    group_by(type) %>% 
    summarise(avg.IR = mean(IR, na.rm = T),
              avg.SR = mean(SR, na.rm = T),
              avg.aRC = mean(aRC, na.rm = T),
              avg.aSD = mean(aSD, na.rm = T),
              avg.sigs = mean(sigs_total, na.rm = T),
              ltlag.min = min(lt_lag, na.rm = T),
              ltlag.max = max(lt_lag, na.rm = T),
              ltlag.avg = mean(lt_lag, na.rm = T),
              ltlag.med = median(lt_lag, na.rm = T),
              band.min = min(band, na.rm = T),
              band.max = max(band, na.rm = T),
              band.avg = mean(band, na.rm = T), 
              strat.count = n(), 
              pool.share = 100 * n() / nrow(list_[[2]])
              # print.share = sprintf("%0.2f\\\\%%", pool.share * 100)
    ) %>% 
    arrange(desc(avg.IR))
  
  
  dat_type_rank <- bind_rows("1_op_IR" = type_rank_op, 
                             "2_q3_IR" = type_rank_q3, 
                             "3_q1_IR" = type_rank_q1,
                             .id = "id")
  
  # Seemingly, this has to be done in R Markdown file directly, leaving for now
  tab_type_rank <- dat_type_rank %>% 
    select(-id) %>%
    kbl(., "latex", caption = "Strategy results - ETFW20L", 
        col.names = c("MA Type", "IR", "SR", "aRC", "aSD", "Sigs.", 
                      "Min", "Max", "Mean", "Med.", # long-term lag
                      "Min", "Max", "Mean", # band value
                      # "Range", "Mean($Q_{3}$)", # short-term lag
                      # "Range", "Mean($Q_{3}$)", # long-term lag
                      # "Mean($\\Omega$)", # band value
                      "N", "Share ($\\%$)"),
        digits = c(0, 3, 3, 3, 3, 0, 0, 0, 0, 0, 2, 2, 2, 0, 2), 
        format.args = list(scientific = F),
        escape = F,
        booktabs = T
    ) %>%
    # mutate(across(everything(), ~column_spec(
    #   .x, c(2:13),
    #   color = case_when(
    #     is.na(.x) ~ "gray",
    #     .x < 0    ~ "red",
    #     T     ~ "black"
    # )))) %>%
    kable_paper() %>%
    kable_styling(latex_options = c("scale_down", "striped", "hold_position"),
                  full_width = F
    ) %>% 
    pack_rows(group_label = "Panel A. Filter: IR greater than 0", 1, 3,
              latex_gap_space = "0.6em",
              escape = F, bold = F, italic = T
    ) %>%
    pack_rows(group_label = "Panel B. Filter: IR greater than $Q_{3}$", 4, 6,
              latex_gap_space = "0.6em",
              escape = F, bold = F, italic = T
    ) %>%
    pack_rows(group_label = "Panel C. Filter: IR smaller than $Q_{1}$", 7, 9,
              latex_gap_space = "0.6em",
              escape = F, bold = F, italic = T
    ) %>%
    add_header_above(c(" " = 6, "Long-term lag" = 4, "Band" = 3, " " = 2)) %>%
    add_header_above(c(" " = 1, "Performance" = 5, "Parameters" = 7, "Descriptive" = 2)) %>%
    # column_spec(1, border_right=T) %>%
    # column_spec(6, border_right=T) %>%
    # column_spec(12, border_right=T) %>%
    footnote(c("table footnote"), general_title = "", threeparttable = T)
  
  
  
  ## RETURN 10 BEST-PERFORMING STRATEGIES BY MA TYPE
  dat_type_top10 <- list_[[2]] %>% 
    filter(sigs_total > 0) %>% 
    group_by(type) %>% 
    slice_max(IR, n = 10, with_ties = FALSE) %>% 
    arrange(desc(IR, type))
  
  
  
  ## PLOT IR SCORE BY LAG DIFF AND TYPE
  lag_ir_style <- theme_minimal() +
    theme(
      # plot.title = element_text(color = "blue", size = 11),
      # plot.subtitle = element_text(size = 10),
      # plot.caption = element_text(size = 11), 
      axis.title = element_text(face = "plain", color = "black"), 
      axis.text.x = element_text(size = 12), 
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 13, vjust = -1), 
      axis.title.y = element_text(size = 13, vjust = 2), 
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13), 
      legend.key.size = unit(12, "points"),
      legend.position = "top")
  
  # For positive IR
  plot_lag_ir <- list_[[2]] %>% 
    filter(IR > 0 & sigs_total > 0) %>% 
    group_by(type, lt_lag) %>% 
    mutate(group_count = n(),
           IR_score = mean(IR, na.rm = T) * group_count
    ) %>% 
    ungroup() %>% 
    mutate(IR_score_std = (IR_score - min(IR_score)) / (max(IR_score) - min(IR_score))
           ) %>% 
    ggplot(aes(x = lt_lag, y = IR_score_std, color = type)) +
    geom_point() + 
    scale_colour_manual(name = "MA Type", 
                        values = palette_disc,
                        # values = c("#FF0B40", "#005A9E", "#FFE100"),
                        breaks = c("SMA", "WMA", "EMA")
    ) +
    geom_smooth(span = 1, 
                # size = 1,
                # method='loess', formula = y~poly(x,2)
                method = "loess", formula = "y ~ x"
    ) +
    labs(
      # title = paste(list_[[1]], " - IR Score by long-short lag spread"),
      # subtitle = "Differentiated by MA type",
      # caption = "Source: own calculation",
      x = "Long-short lag length spread",
      y = "Information Ratio Score") +
    lag_ir_style
  
  
  plot_lag_irb <- list_[[2]] %>% 
    filter(IR > 0 & sigs_total > 0 & band >= 0.04) %>% 
    group_by(type, lt_lag) %>% 
    mutate(group_count = n(),
           IR_score = mean(IR, na.rm = T) * group_count
    ) %>% 
    ungroup() %>% 
    mutate(IR_score_std = (IR_score - min(IR_score)) / (max(IR_score) - min(IR_score))
    ) %>% 
    ggplot(aes(x = lt_lag, y = IR_score_std, color = type)) +
    geom_point() + 
    scale_colour_manual(name = "MA Type", 
                        values = palette_disc,
                        breaks = c("SMA", "WMA", "EMA")
    ) +
    geom_smooth(span = 1, 
                # size = 1,
                # method='loess', formula = y~poly(x,2)
                method = "loess", formula = "y ~ x"
    ) +
    labs(
      # title = paste(list_[[1]], " - IR Score by long-short lag spread"),
      # subtitle = "Differentiated by MA type",
      # caption = "Source: own calculation",
      x = "Long-short lag length spread",
      y = "Information Ratio Score") +
    lag_ir_style
  
  
  # For the whole sample
  
  plot_lag_irws <- list_[[2]] %>% 
    filter(sigs_total > 0) %>% 
    group_by(type, lt_lag) %>% 
    mutate(group_count = n(),
           IR_score = mean(IR, na.rm = T) * group_count
    ) %>% 
    ggplot(aes(x = lt_lag, y = IR_score, color = type)) +
    geom_point() + 
    scale_colour_manual(name = "MA Type", 
                        values = palette_disc,
                        breaks = c("SMA", "WMA", "EMA")
    ) +
    geom_hline(yintercept = 0) + 
    geom_smooth(span = 1, 
                # size = 1,
                method='loess', formula = y~poly(x,2)
                # method = "loess", formula = "y ~ x"
    ) +
    labs(
      # title = paste(list_[[1]], " - IR Score by long-short lag spread (whole sample, band >= 0.01)"),
      # subtitle = "Differentiated by MA type",
      # caption = "Source: own calculation",
      x = "Long-short lag length spread",
      y = "Information Ratio Score") +
    lag_ir_style
    
  
  
  plot_lag_irwsb <- list_[[2]] %>% 
    filter(sigs_total > 0 & band >= 0.04) %>% 
    group_by(type, lt_lag) %>% 
    mutate(group_count = n(),
           IR_score = mean(IR, na.rm = T) * group_count
    ) %>% 
    ggplot(aes(x = lt_lag, y = IR_score, color = type)) +
    geom_point() + 
    scale_colour_manual(name = "MA Type", 
                        values = palette_disc,
                        breaks = c("SMA", "WMA", "EMA")
    ) +
    geom_smooth(span = 1, 
                # size = 1,
                method='loess', formula = y~poly(x,2)
                # method = "loess", formula = "y ~ x"
    ) +
    geom_hline(yintercept = 0) + 
    labs(
      # title = paste(list_[[1]], " - IR Score by long-short lag spread (whole sample, band >= 0.01)"),
      # subtitle = "Differentiated by MA type",
      # caption = "Source: own calculation",
      x = "Long-short lag length spread",
      y = "Information Ratio Score") +
    lag_ir_style
  
  
  
  
  
  heat_ir <- list_[[2]] %>% 
    select(type, st_lag, lt_lag, band, tc_bool, IR, sigs_total)
    # group_by(type) %>% 
    # mutate(IR_std = (IR - min(IR)) / (max(IR) - min(IR)))
  heat_ir_sma <- heat_ir %>% filter(type == "SMA")
  heat_ir_wma <- heat_ir %>% filter(type == "WMA")
  heat_ir_ema <- heat_ir %>% filter(type == "EMA")
  
  heat_ir_style <- theme(plot.title = element_text(color = "blue", size = 10),
                         # plot.subtitle = element_text(size = 8), 
                         # plot.caption = element_text(size = 6), 
                         axis.title = element_text(face = "plain", color = "black"), 
                         axis.text.x = element_text(size = 8, angle = 90), 
                         axis.text.y = element_text(size = 8),
                         axis.title.x = element_text(size = 9, vjust = -1), 
                         axis.title.y = element_text(size = 9, vjust = 2), 
                         legend.text = element_text(size = 9), 
                         legend.key.size = unit(9, "points"), 
                         legend.title = element_blank())
  
  
  x_breaks_b <- unique(heat_ir$lt_lag)
  y_breaks_b <- unique(heat_ir$band)
  
  y_scale_b <- scale_y_discrete(name = "Band value [bp]",
                                breaks = y_breaks_b,
                                labels = y_breaks_b * 10000
                                # labels = paste(y_breaks_b * 10000, "bp")
                                )
  
  ma_freq_ <- list_[[2]]$freq[1]
  st_lag_ <- list_[[2]]$st_lag[1]
  # ma_band_ <- list_[[2]]$band
  # ma_type_ <- list_[[2]]$type[1]
  
  plot_heat_b_sma <- heat_ir_sma %>% 
    ggplot(., aes(as.factor(lt_lag), 
                  as.factor(band))) +
    geom_tile(aes(fill = IR)) +
    geom_text(data = subset(heat_ir_sma, IR >= 0), 
              aes(label = h_numformat(round(IR, 2))), size = 2.5) +
    geom_text(data = subset(heat_ir_sma, IR < 0), 
              aes(label = h_numformat(abs(round(IR, 2)))), size = 2.5, colour = "red2") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, 
                         limit = c(quantile(heat_ir_sma$IR, probs = 0, names = F), 
                                   quantile(heat_ir_sma$IR, probs = 1, names = F))) +
    scale_x_discrete(name = "Long-term lag length",
                     breaks = x_breaks_b,
                     labels = x_breaks_b) + 
    labs(
      # title = paste0("Information ratio (", 
      #                if_else(ma_freq_ == "ma", "MA", "QUIMA"),  
      #                " vs BH)"), 
      title = expr(
        paste("Product: ", !!list_[[1]],
              " | Strategy: (", 
              (!!if_else(ma_freq_ == "ma", "MA", "QUIMA"))[F], ", ",
              SMA[T], ", ",
              (!!st_lag_)[S],  ", ", 
              x[L], ", ", 
              y[B], ")"))
      # caption = "Source: own calculation"
      ) +
    heat_ir_style +
    y_scale_b
  
  
  plot_heat_s_sma <- heat_ir_sma %>% 
    ggplot(., aes(as.factor(lt_lag), 
                  as.factor(band))) +
    geom_tile(aes(fill = sigs_total)) +
    geom_text(data = subset(heat_ir_sma, sigs_total == 0), 
              aes(label = sigs_total), size = 2.5, colour = "red2") +
    geom_text(data = subset(heat_ir_sma, sigs_total > 0), 
              aes(label = sigs_total), size = 2.5, colour = "black") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = mean(heat_ir_sma$sigs_total), 
                         limit = c(quantile(heat_ir_sma$sigs_total, probs = 0, names = F), 
                                   quantile(heat_ir_sma$sigs_total, probs = 0.98, names = F))) +
    scale_x_discrete(name = "Long-term lag length",
                     breaks = x_breaks_b,
                     labels = x_breaks_b) + 
    labs(
      # title = "Number of signals", 
      title = expr(
        paste("Product: ", !!list_[[1]],
              " | Strategy: (", 
              (!!if_else(ma_freq_ == "ma", "MA", "QUIMA"))[F], ", ",
              SMA[T], ", ",
              (!!st_lag_)[S],  ", ", 
              x[L], ", ", 
              y[B], ")"))
      # caption = "Source: own calculation"
      ) +
    heat_ir_style +
    y_scale_b
  
  
  plot_heat_b_wma <- heat_ir_wma %>% 
    ggplot(., aes(as.factor(lt_lag), 
                  as.factor(band))) +
    geom_tile(aes(fill = IR)) +
    geom_text(data = subset(heat_ir_wma, IR >= 0),
              aes(label = h_numformat(round(IR, 2))), size = 2.5) +
    geom_text(data = subset(heat_ir_wma, IR < 0),
              aes(label = h_numformat(abs(round(IR, 2)))), size = 2.5, colour = "red2") +
    # scale_fill_gradientn(colours = plasma(30)) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, 
                         limit = c(quantile(heat_ir_wma$IR, probs = 0, names = F), 
                                   quantile(heat_ir_wma$IR, probs = 1, names = F))) +
    scale_x_discrete(name = "Long-term lag length",
                     breaks = x_breaks_b,
                     labels = x_breaks_b) + 
    labs(
      # title = paste0("Information ratio (", 
      #                if_else(ma_freq_ == "ma", "MA", "QUIMA"),  
      #                " vs BH)"), 
      title = expr(
        paste("Product: ", !!list_[[1]],
              " | Strategy: (", 
              (!!if_else(ma_freq_ == "ma", "MA", "QUIMA"))[F], ", ",
              WMA[T], ", ",
              (!!st_lag_)[S],  ", ", 
              x[L], ", ", 
              y[B], ")"))
      # caption = "Source: own calculation"
      ) +
    heat_ir_style +
    y_scale_b
  
  
  plot_heat_s_wma <- heat_ir_wma %>% 
    ggplot(., aes(as.factor(lt_lag), 
                  as.factor(band))) +
    geom_tile(aes(fill = sigs_total)) +
    geom_text(data = subset(heat_ir_wma, sigs_total == 0), 
              aes(label = sigs_total), size = 2.5, colour = "red2") +
    geom_text(data = subset(heat_ir_wma, sigs_total > 0), 
              aes(label = sigs_total), size = 2.5, colour = "black") +
    # scale_fill_gradientn(colours = plasma(30, direction = -1)) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = mean(heat_ir_wma$sigs_total), 
                         limit = c(quantile(heat_ir_wma$sigs_total, probs = 0, names = F), 
                                   quantile(heat_ir_wma$sigs_total, probs = 0.98, names = F))) +
    scale_x_discrete(name = "Long-term lag length",
                     breaks = x_breaks_b,
                     labels = x_breaks_b) + 
    labs(
      # title = "Number of signals", 
      title = expr(
        paste("Product: ", !!list_[[1]],
              " | Strategy: (", 
              (!!if_else(ma_freq_ == "ma", "MA", "QUIMA"))[F], ", ",
              WMA[T], ", ",
              (!!st_lag_)[S],  ", ", 
              x[L], ", ", 
              y[B], ")"))
      # caption = "Source: own calculation"
      ) +
    heat_ir_style +
    y_scale_b
  
  
  plot_heat_b_ema <- heat_ir_ema %>% 
    ggplot(., aes(as.factor(lt_lag), 
                  as.factor(band))) +
    geom_tile(aes(fill = IR)) +
    geom_text(data = subset(heat_ir_ema, IR >= 0), 
              aes(label = h_numformat(round(IR, 2))), size = 2.5) +
    geom_text(data = subset(heat_ir_ema, IR < 0), 
              aes(label = h_numformat(abs(round(IR, 2)))), size = 2.5, colour = "red2") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, 
                         limit = c(quantile(heat_ir_ema$IR, probs = 0, names = F), 
                                   quantile(heat_ir_ema$IR, probs = 1, names = F))) +
    scale_x_discrete(name = "Long-term lag length",
                     breaks = x_breaks_b,
                     labels = x_breaks_b) + 
    labs(
      # title = paste0("Information ratio (", 
      #                if_else(ma_freq_ == "ma", "MA", "QUIMA"),  
      #                " vs BH)"), 
      title = expr(
        paste("Product: ", !!list_[[1]],
              " | Strategy: (", 
              (!!if_else(ma_freq_ == "ma", "MA", "QUIMA"))[F], ", ",
              EMA[T], ", ",
              (!!st_lag_)[S],  ", ", 
              x[L], ", ", 
              y[B], ")"))
      # caption = "Source: own calculation"
      ) +
    heat_ir_style +
    y_scale_b
  
  
  plot_heat_s_ema <- heat_ir_ema %>% 
    ggplot(., aes(as.factor(lt_lag), 
                  as.factor(band))) +
    geom_tile(aes(fill = sigs_total)) +
    geom_text(data = subset(heat_ir_ema, sigs_total == 0), 
              aes(label = sigs_total), size = 2.5, colour = "red2") +
    geom_text(data = subset(heat_ir_ema, sigs_total > 0), 
              aes(label = sigs_total), size = 2.5, colour = "black") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = mean(heat_ir_ema$sigs_total), 
                         limit = c(quantile(heat_ir_ema$sigs_total, probs = 0, names = F), 
                                   quantile(heat_ir_ema$sigs_total, probs = 0.98, names = F))) +
    scale_x_discrete(name = "Long-term lag length",
                     breaks = x_breaks_b,
                     labels = x_breaks_b) + 
    labs(
      # title = "Number of signals", 
      title = expr(
        paste("Product: ", !!list_[[1]],
              " | Strategy: (", 
              (!!if_else(ma_freq_ == "ma", "MA", "QUIMA"))[F], ", ",
              EMA[T], ", ",
              (!!st_lag_)[S],  ", ", 
              x[L], ", ", 
              y[B], ")"))
      # caption = "Source: own calculation"
      ) +
    heat_ir_style +
    y_scale_b
    
  
  
  
  
  
  
  ## CONSTRUCT RETURN OBJECT (analysis)
  analysis <- lst(
    "product_code" = list_[[1]],
    "dat_type_rank" = dat_type_rank,
    "tab_type_rank" = tab_type_rank,
    "dat_type_top10" = dat_type_top10,
    "plot_lag_IR" = plot_lag_ir,
    "plot_lag_IRb" = plot_lag_irb,
    "plot_lag_IRws" = plot_lag_irws,
    "plot_lag_IRwsb" = plot_lag_irwsb,
    "plot_heat_sma" = plot_heat_b_sma,
    "plot_heat_smas" = plot_heat_s_sma,
    "plot_heat_wma" = plot_heat_b_wma,
    "plot_heat_wmas" = plot_heat_s_wma,
    "plot_heat_ema" = plot_heat_b_ema,
    "plot_heat_emas" = plot_heat_s_ema,
    # "plot_heat_IRsma" = grid.arrange(plot_heat_b_sma, plot_heat_s_sma, nrow = 2),
    # "plot_heat_IRwma" = grid.arrange(plot_heat_b_wma, plot_heat_s_wma, nrow = 2), 
    # "plot_heat_IRema" = grid.arrange(plot_heat_b_ema, plot_heat_s_ema, nrow = 2)
  )
  
  
  
  ## SAVE THE ANALYSIS TO DISK (for convenience on first generation)
  write_rds(analysis, paste0("data/results/",
                             list_[[1]], "_",
                             list_[[2]]$freq[1], "_",
                             if_else(list_[[2]]$tc_bool[1] == T, "tc", "notc"),
                             "_analysis.Rds"))
  
  
  
  ## RETURN THE ANALYSIS
  return(analysis)
  
  
}

