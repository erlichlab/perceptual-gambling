make_figure_optimality = function(){
  p = figure_optimality_tradeoff()
  scale_save(p, 'figure_optimality_tradeoff', 20, 15)
  p = figure_optimality()
  scale_save(p, 'figure_model_optimality', 24, 10)
}

figure_optimality_tradeoff = function(n_trials = 1000, sigmas = 1, rewmulti = 5, sim = FALSE){
  # make a heatmap between different levels of rho and sigma, color by normalized reward using the same dataset
  if (sim){
    sigmas = seq(0.1, 0.5, by = 0.1)
    rhos = seq(0.1, 1.5, by = 0.1)
    df = data.frame(s = gen_sound(n_trials, 1),
                    sigma_s = sigmas,
                    rew_multi = rewmulti,
                    better_side = sample(c(0, 1, 9), n_trials, replace = TRUE))
    reward_df = data.frame()
    for (this_rho in rhos){
      for (this_sigma in sigmas){
        this_df = sim_subject(n_trials = n_trials,
                              params_list = list(sigma = this_sigma, rho = this_rho, omega_1 = 1, omega_2 = 0, omega_3 = 0), df = df)
        this_reward = sum(this_df$reward)
        temp_df = data.frame(sigma = this_sigma, rho = this_rho, reward = this_reward)
        reward_df = rbind(reward_df, temp_df)
      }
    }
    write.csv(reward_df, file = 'csv/rho_sigma_tradeoff.csv')
  } else{
    reward_df = read.csv('csv/rho_sigma_tradeoff.csv')
  }
  # get subjects fits
  fit_df = read.csv('csv/bdt_fits.csv')
  fit_df = fit_df %>% group_by(subjid) %>% mutate(max_lp = which.max(lp__)) %>% 
    summarise(rho = round(mean(rho[max_lp]), 1), sigma = round(mean(sigma[max_lp]), 1))
  fit_df$rho[fit_df$rho == 0] = 0.1
  reward_df = reward_df %>% group_by(sigma) %>% mutate(norm_reward = reward / max(reward))
  reward_df2 = reward_df %>% group_by(sigma) %>% summarise(best_rho = rho[which.max(reward)])
  p = ggplot() + theme_classic(BASE_SIZE) +
    geom_tile(reward_df, mapping = aes(x = rho, y = sigma, fill = norm_reward), color = NA) +
    geom_point(fit_df, mapping = aes(x = rho, y = sigma, color = as.factor(subjid)), shape = 15, size = 14) +
    geom_point(reward_df2, mapping = aes(x = best_rho, y = sigma), color = 'indianred', shape = 0, size = 14) +
    xlab(TeX(latex_params[['rho']])) + ylab(TeX(latex_params[['sigma']])) +
    scale_fill_gradient(low = "white", high = "#868B8E", name = 'Normalized Reward') +
    #scale_color_manual(values = c('#74BDCB', '#FFA384', '#EFE7BC', '#A3EBB1', '#D8A7B1', '#887BB0', '#FDB750'), name = 'Subject') +
    scale_color_manual(values = subj_colors, name = 'Subject') +
    theme(legend.position = 'right', axis.text.x = element_text(angle = 0, hjust = 0, vjust = 0),
          axis.title = element_text(size = 20)) 
  return(p)
}

figure_optimality = function(){
  # how sub-optimal are these animals compared to a fully rational, risk-neutral agent with the fit sigma_p?
  # using the rewards they can get 
  if (!file.exists('csv/reward_df.csv')){
    fit_df = read.csv('csv/bdt_fits.csv')
    df = read.csv('csv/preprocessed_good_trials.csv')
    tradeoff_df = read.csv('csv/all_tradeoff.csv')
    # find MAP sigma_p estimates for each animal
    fit_df = fit_df %>% group_by(subjid) %>% mutate(max_lp = which.max(lp__)) %>% 
      summarise(rho = mean(rho[max_lp]), sigma = mean(sigma[max_lp]), omega_1 = mean(omega.1[max_lp]), omega_2 = mean(omega.2[max_lp]), omega_3 = mean(omega.3[max_lp]))
    reward_df = data.frame()
    for (subj in good_list){
      subj_df = df %>% filter(subjid == subj) %>% mutate(s = log2_sound)
      # optimal_agent: same sigma as the animal, but best rho according to modelling results
      subj_sigma = fit_df$sigma[fit_df$subjid == subj]
      best_rho = as.numeric(tradeoff_df %>% filter(sigma == round(subj_sigma, 1)) %>% summarise(rho = rho[which.max(reward)]))
      syn_df1 = bdt_agent(df = subj_df, 
                          params_list = list(rho = best_rho, sigma = subj_sigma, omega_1 = 1, omega_2 = 0, omega_3 = 0))
      # agent 2: same rho and sigma as the animal, fully rational
      syn_df2 = bdt_agent(df = subj_df, 
                          params_list = list(rho = fit_df$rho[fit_df$subjid == subj], sigma = subj_sigma, omega_1 = 1, omega_2 = 0, omega_3 = 0))
      # agent 3: same rho, sigma and bias as the animal
      syn_df3 = bdt_agent(df = subj_df, 
                          params_list = list(rho = fit_df$rho[fit_df$subjid == subj], sigma = subj_sigma, 
                                             omega_1 = fit_df$omega_1[fit_df$subjid == subj], 
                                             omega_2 = fit_df$omega_2[fit_df$subjid == subj], 
                                             omega_3 = fit_df$omega_3[fit_df$subjid == subj]))
      # plot the real subject and 'optimal subject' together
      # m0 = glm('chose_high ~ log2_sound*better_side_s', subj_df, family = binomial)
      # pred_df = data_grid(subj_df, log2_sound = seq_range(log2_sound, by = 0.001), better_side_s = better_side_s)
      # pred_df$pred = predict(m0, pred_df, type = 'response')
      # qt = quantile(df$log2_sound, probs = seq(0, 1, 0.1))
      # p = ggplot(subj_df) + theme_classic(BASE_SIZE) +
      #   geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
      #   stat_summary_bin(mapping = aes(x = log2_sound, y = chose_high, color = better_side_s), fun.data = bino, breaks = qt, geom = 'pointrange') +
      #   geom_line(pred_df, mapping = aes(x = log2_sound, y = pred, color = better_side_s), size = 1) +
      #   geom_line(opt_df, mapping = aes(x = log2_sound, y = p_chose_high, color = better_side_s), alpha = 0.7, size = 1) +
      #   geom_line(sub_opt_df, mapping = aes(x = log2_sound, y = p_chose_high, color = better_side_s), alpha = 0.4, size = 1) +
      #   geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) + 
      #   scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
      #   scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
      #   xlab(expression(log2(Frequency))) + ylab("P(Chose High)") +
      #   scale_color_manual(values = c('#F37970', '#81B628', 'azure4'), name = 'Flash on') +
      #   theme(legend.position = 'none')
      # scale_save(p, sprintf('%d_opt', subj), 8, 8, 1)
      # calculate how much reward
      syn_df1 = compute_reward(syn_df1)
      syn_df2 = compute_reward(syn_df2)
      syn_df3 = compute_reward(syn_df3)
      # save reward information
      temp_df = data.frame(subjid = subj, reward = c(sum(subj_df$reward), sum(syn_df1$reward), sum(syn_df2$reward), sum(syn_df3$reward)), 
                           type = c('Actual', 'Optimal', 'Rational', 'Estimated'), n_trials = nrow(subj_df))
      reward_df = rbind(reward_df, temp_df)
    }
    write.csv(reward_df, file = 'csv/reward_df.csv')
  } else{
    reward_df = read.csv('csv/reward_df.csv') %>% group_by(subjid) %>% mutate(norm_reward = reward / max(reward))
    reward_df$type = factor(reward_df$type, c('Actual', 'Estimated', 'Rational', 'Optimal'))
  }
  p = ggplot(reward_df, aes(x = as.factor(subjid), y = norm_reward, fill = type)) + theme_classic(BASE_SIZE) +
    geom_col(position = position_dodge(0.8), color = 'black', alpha = 0.8) +
    xlab(' ') + ylab('Normalized Reward') + scale_fill_manual(values = c('#DEE2EC', '#95cfe7','#0C6980', 'black'), name = '') +
    theme(legend.position = 'bottom')
  return(p)
}