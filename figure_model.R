# functions make figure_model panel
make_figure_model = function(){
  figure_model_prediction() # saves on its own
  p = figure_model_pairs()
  scale_save(p, 'figure_model_pairs', 14, 14, 1)
}

figure_model_prediction = function(){
  # Make model prediction ribbons overlayed with control subject binned choice data  
  df = read.csv('csv/preprocessed_good_trials.csv') %>% filter(better_side != 2)
  subj_list = unique(df$subjid)
  for (animal in subj_list){
    subj_df = df %>% filter(subjid == animal)
    load(sprintf('fits/%s_pred.RData', animal))
    # extract simulated n_chose_lott and confidence intervals 
    draws = rstan::extract(fit)
    nch_df = as.data.frame(t(draws$pred_n_chose_high))
    pred_df = subj_df %>% data_grid(s = s_binned, better_side = better_side, rew_multi = rew_multi, n_trials = 200)
    colnames(pred_df) = paste0('pred_', colnames(pred_df))
    pred_df = pred_df %>% mutate(y = rowMeans(nch_df)/pred_n_trials, 
                                 ymin_80 = apply(nch_df, 1, quantile, 0.1)/pred_n_trials,  
                                 ymax_80 = apply(nch_df, 1, quantile, 0.9)/pred_n_trials,
                                 ymin_95 = apply(nch_df, 1, quantile, 0.025)/pred_n_trials,  
                                 ymax_95 = apply(nch_df, 1, quantile, 0.975)/pred_n_trials,
                                 ymin_99 = apply(nch_df, 1, quantile, 0.005)/pred_n_trials,  
                                 ymax_99 = apply(nch_df, 1, quantile, 0.995)/pred_n_trials)
    # plot
    rho = mean(draws$rho)
    sigma = mean(draws$sigma)
    qt = quantile(subj_df$log2_sound, probs = seq(0, 1, 0.1))
    n_trials = subj_df %>% group_by(better_side) %>% tally() %>% pull(n) # high, low, none
    qt = quantile(subj_df$log2_sound, probs = seq(0, 1, 0.1))
    p = ggplot(subj_df) + theme_classic(BASE_SIZE) +
      geom_ribbon(pred_df, mapping = aes(x = pred_s, ymin = ymin_80, ymax = ymax_80, fill = as.factor(pred_better_side)), alpha = 0.6) +
      geom_ribbon(pred_df, mapping = aes(x = pred_s, ymin = ymin_95, ymax = ymax_95, fill = as.factor(pred_better_side)), alpha = 0.3) +
      geom_ribbon(pred_df, mapping = aes(x = pred_s, ymin = ymin_99, ymax = ymax_99, fill = as.factor(pred_better_side)), alpha = 0.1) +
      stat_summary_bin(mapping = aes(x = log2_sound, y = chose_high, color = as.factor(better_side)), breaks = qt, fun.data = bino, geom = 'pointrange', size = 0.5) +
      geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
      geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) + 
      scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
      scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
      annotate("text", label = unique(subj_df$subjid), fontface = 2, x = 3.7, y = 0.4) + 
      #annotate("text", label = paste0('n=', n_trials[1]), x = 3.7, y = 0.3, color = better_side_colors$High) +  
      #annotate("text", label = paste0('n=', n_trials[3]), x = 3.7, y = 0.2, color = better_side_colors$None) +
      #annotate("text", label = paste0('n=', n_trials[2]), x = 3.7, y = 0.1, color = better_side_colors$Low) +  
      xlab(expression(log2(Frequency))) + ylab("P(Chose High)") +
      scale_color_manual(values = c('#81B628', '#F37970','azure4'), name = 'Flash on') + 
      scale_fill_manual(values = c('#81B628', '#F37970', 'azure4'), name = 'Flash on') +
      #scale_color_manual(values = c('#F37970','#81B628', 'azure4'), name = 'Flash on') + # for 2078, 2124
      #scale_fill_manual(values = c('#F37970', '#81B628', 'azure4'), name = 'Flash on') + # for 2078, 2124
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none')
    scale_save(p, sprintf('%d_model', animal), 8, 8, 1)
  }
}

figure_model_pairs = function(control_ticks = TRUE){
  # aggregated pairs plot using individual fits
  # multiple_datasets: when true, it makes figure5b
  df = read.csv('csv/bdt_fits.csv')
  stat_df = df %>% group_by(subjid) %>% summarise(mean_rho = mean(rho), min_rho = quantile(rho, 0.05), max_rho = quantile(rho, 0.95), 
                                                  mean_sigma = mean(sigma), min_sigma = quantile(sigma, 0.05), max_sigma = quantile(sigma, 0.95), 
                                                  mean_omega.1 = mean(omega.1), min_omega.1 = quantile(omega.1, 0.05), max_omega.1 = quantile(omega.1, 0.95), 
                                                  mean_omega.2 = mean(omega.2), min_omega.2 = quantile(omega.2, 0.05), max_omega.2 = quantile(omega.2, 0.95), 
                                                  mean_omega.3 = mean(omega.3), min_omega.3 = quantile(omega.3, 0.05), max_omega.3 = quantile(omega.3, 0.95))
  stat_df$subjid = factor(stat_df$subjid)
  
  params = c('rho', 'sigma', 'omega.1', 'omega.2', 'omega.3')
  param_combos = as.data.frame(expand.grid(params, params))
  diagnal = seq(1, dim(param_combos)[1], length(params)+1)
  # define parameter-specific limits and breaks
  lower_limits = list('rho' = 0, 'sigma' = 0, 'omega.1' = 0, 'omega.2' = 0, 'omega.3' = 0)
  upper_limits = list('rho' = 1, 'sigma' = 1, 'omega.1' = 1, 'omega.2' = 1, 'omega.3' = 1)
  lower_breaks = list('rho' = 0, 'sigma' = 0, 'omega.1' = 0, 'omega.2' = 0, 'omega.3' = 0)
  middle_breaks = list('rho' = 0.5, 'sigma' = 0.5, 'omega.1' = 0.5, 'omega.2' = 0.5, 'omega.3' = 0.5)
  upper_breaks = list('rho' = 1, 'sigma' = 1, 'omega.1' = 1, 'omega.2' = 1, 'omega.3' = 1)
  # make the 'pairs' plot
  for (i in 1:dim(param_combos)[1]){
    raw_x = as.character(param_combos[i,1])
    raw_y = as.character(param_combos[i,2])
    if (i %in% c(2,3,4,5,8,9,10,14,15,20)){
      P = P + plot_spacer()
    } else{
      if (i %in% diagnal){ # population posterior 
        pop_param_df = as.data.frame(as_vector(df %>% select(contains(raw_x))))
        colnames(pop_param_df) = 'pop_param'
        p = ggplot(pop_param_df, aes(x = pop_param)) + theme_classic(base_size = BASE_SIZE) +
          geom_density(alpha = 0.5, fill = 'azure4') +
          geom_vline(xintercept = apply(pop_param_df, 2, median), color = 'black', size = 1) + 
          xlab(TeX(latex_params[raw_x])) + ylab(TeX(latex_params[raw_y])) +
          theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 5, size = 12))
      } else{ # scatter plot with individual parameter means
        xmin = paste0('min_', raw_x)
        xmax = paste0('max_', raw_x)
        ymin = paste0('min_', raw_y)
        ymax = paste0('max_', raw_y)
        x = paste0('mean_', raw_x)
        y = paste0('mean_', raw_y)
        p = ggplot(stat_df, aes_string(x = x, y = y, ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, color = 'subjid')) +
          scale_color_manual(values = subj_colors) +
          theme_classic(base_size = BASE_SIZE) +
          geom_point(size = 1) + geom_errorbar(size = 0.5) + geom_errorbarh(size = 0.5) + 
          xlab(TeX(latex_params[raw_x])) + ylab(TeX(latex_params[raw_y])) +
          theme(legend.position = 'none', axis.text.x = element_text(angle = 90, hjust = 5))
      }
      # control ticks label
      if (control_ticks){
        if (!(i %in% diagnal)){
          p = p + scale_y_continuous(limits = c(lower_limits[[raw_y]], upper_limits[[raw_y]]), 
                                     breaks = c(lower_breaks[[raw_y]], middle_breaks[[raw_y]], upper_breaks[[raw_y]])) +
            scale_x_continuous(limits = c(lower_limits[[raw_x]], upper_limits[[raw_x]]), 
                               breaks = c(lower_breaks[[raw_x]], middle_breaks[[raw_x]], upper_breaks[[raw_x]]))
        } else if (i %in% diagnal){
          p = p + scale_x_continuous(limits = c(lower_limits[[raw_x]], upper_limits[[raw_x]]), 
                                     breaks = c(lower_breaks[[raw_x]], middle_breaks[[raw_x]], upper_breaks[[raw_x]]))
        }
      }
      # control x and y ticks
      if (i %% length(params) != 1){ # if not in the first column
        p = p + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
      }
      if (i <= length(params)^2 - length(params)){ # if not in the last row
        p = p + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
      }
      if (i %in% c(2,3,4,5,8,9,10,11,12,14,15,16,17,18)){
        p = p + theme(axis.text.x = element_blank())
      }
      if (i %in% c(3,4,5,8,9,10,12,14,15,17,18,20,22,23,24)){
        p = p + theme(axis.text.y = element_blank())
      }
      if (i %in% diagnal){
        p = p + theme(axis.text.x = element_text())
      }
      if (i == 1){
        p = p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
        P = p
      } else{
        P = P + p
      }
    }
  }
  P = P + plot_layout(ncol = length(params), nrow = length(params))
  return(P)
}

figure_model_sigmoid = function(){
  # makes table from sigmoid fits in latex syntax
  df = read.csv('csv/sigmoid_fits.csv') %>% mutate(subjid = as.factor(subjid))
  df$better_side_s = factor(df$better_side_s, c('Low', 'None', 'High'))
  params = c('x0', 'b', 'w1', 'w2')
  for (param in params){
    p = ggplot(df, aes_string(x = 'subjid', y = param, fill = 'better_side_s')) + theme_classic(BASE_SIZE) +
      geom_col(position = position_dodge(0.8), color = 'black', alpha = 0.8) +
      scale_fill_manual(values = better_side_colors) +
      xlab(" ") + theme(legend.position = 'none', axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))
    if (param == 'x0'){P = p} else{P = P + p}
  }
  P = P + plot_layout(ncol = 2, nrow = 2)
  scale_save(P, 'figure_model_sigmoid', 20, 10, 1)
  return(P)
}

figure_model_sigmoid_table = function(){
  # makes table from sigmoid fits in latex syntax
  df = read.csv('csv/sigmoid_fits.csv')
  for (subj in unique(df$subjid)){
    subj_df = df %>% filter(subjid == subj)
    cat(sprintf('\\multirow{3}{4em}{\\textbf{%d}} & None & %.2f & %.2f & %.2f & %.2f \\\\
            & High & %.2f & %.2f & %.2f & %.2f \\\\
            & Low & %.2f & %.2f & %.2f & %.2f \\\\
            \\midrule', subj,  
                subj_df$x0[subj_df$better_side_s == 'None'], subj_df$b[subj_df$better_side_s == 'None'], subj_df$w1[subj_df$better_side_s == 'None'], subj_df$w2[subj_df$better_side_s == 'None'], 
                subj_df$x0[subj_df$better_side_s == 'High'], subj_df$b[subj_df$better_side_s == 'High'], subj_df$w1[subj_df$better_side_s == 'High'], subj_df$w2[subj_df$better_side_s == 'High'], 
                subj_df$x0[subj_df$better_side_s == 'Low'], subj_df$b[subj_df$better_side_s == 'Low'], subj_df$w1[subj_df$better_side_s == 'Low'], subj_df$w2[subj_df$better_side_s == 'Low']), 
        '\n')
  }
}
