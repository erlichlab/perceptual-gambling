# supplementary figures

figureS_task_param = function(){
  # examples showing how different task parameters: sigma_s or rew_multi affects the behavior
  # maybe: 2083, 10, 1; 2083, 7, 1.5; 2143, 10, 0.3; 2143, 15, 0.3
  df = read.csv('csv/preprocessed_trials.csv')
  p1 = plot_pg(df, 2083, rewmulti = 10, sigmas = 1)
  scale_save(p1, '2083_10_1', 8, 8, 1)
  p2 = plot_pg(df, 2083, rewmulti = 7, sigmas = 1.5)
  scale_save(p2, '2083_7_1.5', 8, 8, 1)
  p3 = plot_pg(df, 2143, rewmulti = 10, sigmas = 0.3)
  scale_save(p3, '2143_10_0.3', 8, 8, 1)
  p4 = plot_pg(df, 2143, rewmulti = 15, sigmas = 0.3)
  scale_save(p4, '2143_15_0.3', 8, 8, 1)
}

figureS_RT = function(individual = TRUE){
  # RT plot for the population and each subject
  # thin colored lines = each subject, thick line = population average
  df = read.csv('csv/preprocessed_good_trials.csv') %>% filter(better_side_s != 'Both') %>% 
    mutate(log_RT = log(resp_time))
  if (individual){
    subj_list = unique(df$subjid)
    alphas_list = seq(0.1, 1, by = 1 / length(subj_list))
    for (i in 1:length(subj_list)){
      # fit LM on individual dataset
      subj_df = df %>% filter(subjid == subj_list[i])
      ind_pred_df = data_grid(subj_df, log2_sound = seq_range(log2_sound, by = 0.01), better_side_s = better_side_s, chose_high = chose_high)
      m1 = lm('log_RT ~ log2_sound*better_side_s*chose_high', subj_df)
      ind_pred_df$pred = predict(m1, ind_pred_df, type = 'response', allow.new.levels=TRUE)
      # plot
      qt = quantile(subj_df$log2_sound, probs = seq(0, 1, 0.15))
      label_df = data.frame(label = c(sprintf('%d trials', nrow(subj_df %>% filter(chose_high == 0))),
                                      sprintf('%d trials', nrow(subj_df %>% filter(chose_high == 1)))),
                            chose_high = c(0, 1))
      p = ggplot() + theme_classic(base_size = BASE_SIZE) +
        stat_summary_bin(subj_df, mapping = aes(x = log2_sound, y = log_RT, color = better_side_s), fun.data = mean_se, breaks = qt, geom = 'pointrange') +
        geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) +
        scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
        xlab(expression(log2(Frequency))) + ylab("log(RT)") +
        ggtitle(subj_list[i]) +
        scale_color_manual(values = c('#F37970', '#81B628', 'azure4'), name = 'Flash on') +
        theme(legend.position = 'bottom') + facet_wrap(~chose_high, scales = 'free')
      scale_save(p, sprintf('%d_RT', subj_list[i]), 15, 10, 1)
    }
  } else{
    m0 = lm('log_RT ~ log2_sound*better_side_s*chose_high', df)
    #m1 = lmer('log_RT ~ log2_sound*better_side_s + (log2_sound*better_side_s | subjid)', df %>% filter(chose_high == 1))
    pop_pred_df = data_grid(df, log2_sound = seq_range(log2_sound, by = 0.01), better_side_s = better_side_s, chose_high = chose_high)
    pop_pred_df$pred = predict(m0, pop_pred_df, type = 'response', allow.new.levels=TRUE)
    # plot
    qt = quantile(df$log2_sound, probs = seq(0, 1, 0.15))
    label_df = data.frame(label = c(sprintf('%d trials', nrow(df %>% filter(chose_high == 0))), 
                                    sprintf('%d trials', nrow(df %>% filter(chose_high == 1)))), 
                          chose_high = c(0, 1))
    p = ggplot() + theme_classic(base_size = BASE_SIZE) +
      stat_summary_bin(df, mapping = aes(x = log2_sound, y = log_RT, color = better_side_s), fun.data = mean_se, breaks = qt, geom = 'pointrange') +
      geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) + 
      scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
      xlab(expression(log2(Frequency))) + ylab("log(RT)") +
      scale_color_manual(values = c('#F37970', '#81B628', 'azure4'), name = 'Flash on') +
      theme(legend.position = 'none') + 
      facet_wrap(~chose_high) +
      geom_text(label_df, mapping = aes(x = 3.5, y = -0.7, label = label), size = 5)
    scale_save(p, 'RT', 15, 10, 1)
    return(p)
  }
}

figureS_sanity = function(N = 20){
  # Generate model parameters from the prior range, simulate dataset, and see if the model can recover them accurately
  # returns a scatter plot of predictions vs. true values
  params_df = data.frame()
  params = c('rho', 'sigma', 'omega_rational', 'omega_high', 'omega_low')
  # define parameter-specific limits and breaks
  lower_limits = list('rho' = 0, 'sigma' = 0, 'omega_rational' = 0, 'omega_high' = 0, 'omega_low' = 0)
  upper_limits = list('rho' = 2, 'sigma' = 0.6, 'omega_rational' = 1, 'omega_high' = 1, 'omega_low' = 1)
  lower_breaks = list('rho' = 0, 'sigma' = 0, 'omega_rational' = 0, 'omega_high' = 0, 'omega_low' = 0)
  middle_breaks = list('rho' = 1, 'sigma' = 0.3, 'omega_rational' = 0.5, 'omega_high' = 0.5, 'omega_low' = 0.5)
  upper_breaks = list('rho' = 2, 'sigma' = 0.6, 'omega_rational' = 1, 'omega_high' = 1, 'omega_low' = 1)
  for (n in 1:N){
    # generate parameters
    true_params = list()
    true_params$rho = exp(rnorm(1, log(1), 0.3))
    true_params$sigma = exp(rnorm(1, log(0.3), 0.1))
    omegas = as.numeric(rdirichlet(n = 1, alpha = c(6, 2, 2)))
    true_params$omega_1 = omegas[1] # rational
    true_params$omega_2 = omegas[2] # high
    true_params$omega_3 = omegas[3] # low
    # create synthetic data
    df = sim_subject(n_trials = 3000, sigma_s = 1, rew_multi = 5, params_list = true_params)
    # fit model
    fit = fit_bdt(df, save = FALSE)
    # append model estimates (maximum log likelihood)
    draws = as.data.frame(rstan::extract(fit))
    max_lp = which.max(draws$lp__)
    temp_df = data.frame(true = c(true_params$rho, true_params$sigma, true_params$omega_1, true_params$omega_2, true_params$omega_3),
                         params = params, 
                         mlp = c(draws$rho[max_lp], draws$sigma[max_lp], draws$omega.1[max_lp], draws$omega.2[max_lp], draws$omega.3[max_lp]), 
                         n = n)
    params_df = rbind(params_df, temp_df)
  }
  # make the scatter plots
  params_df$n = as.factor(params_df$n)
  for (i in 1:length(params)){
    param = params[i]
    this_df = params_df %>% filter(params == param)
    corr = cor.test(this_df$true, this_df$mlp)
    p = ggplot(this_df, aes(x = true, y = mlp, color = n)) + theme_classic(base_size = BASE_SIZE) +
      geom_point() + xlab(TeX(latex_params[param])) +
      theme(legend.position = 'none') +
      ylab('Model Estimate') + geom_abline(slope = 1, linetype =  'dashed', alpha = 0.4) +
      geom_smooth(method = lm, formula = y ~ x, color = 'gray', size = 0.2) +
      annotate("text", label = sprintf('R = %.3f', corr$estimate), x = middle_breaks[[param]], y = 0.1) + 
      scale_x_continuous(limits = c(lower_limits[[param]], upper_limits[[param]]),
                         breaks = c(lower_breaks[[param]], middle_breaks[[param]], upper_breaks[[param]])) +
      scale_y_continuous(limits = c(lower_limits[[param]], upper_limits[[param]]),
                         breaks = c(lower_breaks[[param]], middle_breaks[[param]], upper_breaks[[param]])) # add an abline 
    if (i == 1){P = p} 
    else{p = p + theme(axis.title.y = element_blank())
    P = P + p}
  }
  P = P + plot_layout(ncol = length(params))
  scale_save(P, 'figureS_sanity', 20, 6, scale = 1)
}

figureS_bdt_shift = function(this_sigma = 0.3){
  # show that BDT only allows for horizontal shift by changing rho
  df = sim_subject(params_list = list(sigma = this_sigma, rho = 1, omega_1 = 0.8, omega_2 = 0.1, omega_3 = 0.1)) 
  df$better_side = as.factor(df$better_side)
  p = ggplot(df) + theme_classic(BASE_SIZE) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
    geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) + 
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
    geom_line(mapping = aes(x = s, y = p_chose_high, color = better_side), alpha = 1, size = 1) +
    xlab(expression(log2(Frequency))) + ylab("P(Chose High)") +
    scale_color_manual(values = c('#81B628', '#F37970', 'azure4'), name = 'Flash on') +
    theme(legend.position = 'none')
  scale_save(p, sprintf('figureS_shift_%.1f', this_sigma), 8, 8, scale = 1)
  return(p)
}

figureS_sigmoid = function(){
  # plots sigmoid prediction for each animal and saves it
  draws_df = read.csv('csv/sigmoid_fits.csv')
  df = read.csv('csv/preprocessed_good_trials.csv')
  subj_list = unique(draws_df$subjid)
  for (subj in subj_list){
    subj = 2143
    subj_df = df %>% filter(subjid == subj)
    sim_df = data.frame()
    for (flash_side in c('Low', 'None', 'High')){
      this_draws_df = draws_df %>% filter(subjid == subj) %>% filter(better_side_s == flash_side)
      sim_df = rbind(sim_df, sigmoid_agent(w1 = this_draws_df$w1, w2 = this_draws_df$w2, x0 = this_draws_df$x0, b = this_draws_df$b) %>% 
                       mutate(subjid = subj, better_side_s = flash_side))
    }
    qt = quantile(subj_df$log2_sound, probs = seq(0, 1, 0.1))
    p = ggplot(subj_df) + theme_classic() +
      stat_summary_bin(mapping = aes(x = log2_sound, y = chose_high, color = better_side_s), fun.data = bino, breaks = qt, geom = 'pointrange') +
      geom_line(sim_df, mapping = aes(x = s, y = y, color = better_side_s), alpha = 1, size = 1) +
      geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
      geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) + 
      scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
      scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
      annotate("text", label = unique(subj_df$subjid), fontface = 2, x = 3.7, y = 0.25) + 
      xlab(expression(log2(Frequency))) + ylab("P(Chose High)") +
      scale_color_manual(values = c('#F37970', '#81B628', 'azure4'), name = 'Flash on') +
      theme(legend.position = 'none')
      #theme(legend.position = 'none', axis.title = element_blank())
    scale_save(p, sprintf('%s_sigmoid', subj), 8, 8, 1)
  }
}