# functions related to Bayesian decision theory analysis 

sim_subject = function(n_trials = 3000,  
                       sigma_s = 1,
                       rew_multi = 5,
                       params_list = list(sigma = 0.3, rho = 1, 
                                          omega_1 = 0.8, omega_2 = 0.1, omega_3 = 0.1),
                       df = NULL){
  # create synthetic subject data using bdt_agent with specified parameters
  base_reward = 10
  high_reward = base_reward * rew_multi
  if (is.null(df)){
    df = data.frame(s = gen_sound(n_trials, sigma_s),
                    sigma_s = sigma_s,
                    rew_multi = rew_multi,
                    better_side = sample(c(0, 1, 9), n_trials, replace = TRUE))
  }
  df = bdt_agent(params_list, df)
  
  # calculate reward
  df = df %>% mutate(pitch_high = s > 3, 
                     low_reward = case_when((pitch_high == 0 & better_side != 0) ~ base_reward, 
                                            (pitch_high == 0 & better_side == 0) ~ high_reward, 
                                            pitch_high == 1 ~ 0), 
                     high_reward = case_when((pitch_high == 1 & better_side != 1) ~ base_reward, 
                                            (pitch_high == 1 & better_side == 1) ~ high_reward, 
                                            pitch_high == 0 ~ 0), 
                     reward = (1 - p_chose_high) * low_reward + p_chose_high * high_reward)
  # bin log2_sound into n_bins
  n_bins = 30
  breaks = seq(2, 4, by = 0.1)
  labels = c()
  for (i in 1:length(breaks)-1){
    labels = append(labels, (breaks[i] + breaks[i+1])/2)
  }
  df = df %>% mutate(s_binned = cut(s, breaks, labels, include.lowest = TRUE))
  df$s_binned = as.numeric(levels(df$s_binned))[df$s_binned]
  df$best_sigma_s = df$sigma_s
  return(df)
}

bdt_agent = function(params_list, df){
  # params_list must include: rho, sigma, omega_1, omega_2, omega_3
  # df must contain these columns: sigma_s, rew_multi, s, better_side
  sigma_s = unique(df$sigma_s)
  rew_multi = unique(df$rew_multi)
  
  sigma = params_list$sigma # perceptual noise
  rho = params_list$rho # risk attitude?
  omega_1 = params_list$omega_1 # omega_rational
  omega_2 = params_list$omega_2 # omega_high
  omega_3 = params_list$omega_3 # omega_low
  
  omega_asy = 0
  sigma_asy = 0
  if ('omega_1_low' %in% names(params_list)){
    omega_asy = 1
  }
  if ('sigma_low' %in% names(params_list)){
    sigma_asy = 1
  }
  
  # step 1: class prior 
  x = seq(2, 4, 0.001)
  p_L = 0.5 # class prior of low sound
  p_H = 0.5 # class prior of high sound
  
  # step 2: class-conditioned stimulus distribution
  gsd = dnorm(x, mean = 3, sd = sigma_s) # general stimulus distribution
  ccsd_L = append(gsd[1:1000], rep(0, 1001)) # class-conditioned stimnulus distribution of low
  ccsd_H = append(rep(0, 1001), gsd[1001:2000]) # class-conditioned stimnulus distribution of high
  
  n_trials = dim(df)[1]
  df$p_chose_high = 9
  df$chose_high = 9
  for (tx in 1:n_trials){
    # step 3: percept given stimulus
    if (sigma_asy){ # if we allow sigma to change based on flashing condition
      if (df$better_side[tx] == 9){ # none
        p_x_given_s = dnorm(x, df$s[tx], sigma_none) # probability of internal percept given the stimulus
      } else if (df$better_side[tx] == 0){ # low
        p_x_given_s = dnorm(x, df$s[tx], sigma_low) # probability of internal percept given the stimulus
      } else if (df$better_side[tx] == 1){ # high
        p_x_given_s = dnorm(x, df$s[tx], sigma_high) # probability of internal percept given the stimulus
      }
    } else{
      p_x_given_s = dnorm(x, df$s[tx], sigma) # probability of internal percept given the stimulus
    }
    # step 4: likelihood
    p_L_given_x = p_L * sum(ccsd_L * p_x_given_s) # likelihood of low given percept x
    p_H_given_x = p_H * sum(ccsd_H * p_x_given_s) # likelihood of high given percept x
    # step 5: apply Bayesian decision rule
    lambdas = compute_action_cost(df$better_side[tx], rew_multi)
    lambda_L = lambdas$lambda_L
    lambda_H = lambdas$lambda_H
    d_H = log(lambda_H^rho / lambda_L^rho) - log(p_H_given_x / p_L_given_x)
    # step 6: mix the weights of three agents and get p_choose_H
    p_rational = 1 / (1 + exp(d_H))
    if (omega_asy){ # if we allow omega to change based on flashing condition
      if (df$better_side[tx] == 9){ # none
        df$p_chose_high[tx] = p_rational * params_list$omega_1_none + 1*params_list$omega_2_none + 0*params_list$omega_3_none
      } else if (df$better_side[tx] == 0){ # low
        df$p_chose_high[tx] = p_rational * params_list$omega_1_low + 1*params_list$omega_2_low + 0*params_list$omega_3_low
      } else if (df$better_side[tx] == 1){ # high
        df$p_chose_high[tx] = p_rational * params_list$omega_1_high + 1*params_list$omega_2_high + 0*params_list$omega_3_high
      }
    } else{
      df$p_chose_high[tx] = p_rational * omega_1 + 1 * omega_2 + 0 * omega_3
    }
    df$chose_high[tx] = rbinom(1, 1, df$p_chose_high[tx])
  }
  return(df)
}

compute_action_cost = function(better_side, rew_multi){
  # better_side: 0 = low, 1 = high, 9 = none
  # output: lambda_L = cost of choosing L if H is correct, lambda_H = cost of choosing H if L is correct
  cost_table = data.frame(lambda_H = c(rew_multi,1,1), lambda_L = c(1,rew_multi,1))
  rownames(cost_table) = c('low', 'high', 'none')
  if (better_side == 0){
    return(as.list(cost_table['low',]))
  } else if (better_side == 1){
    return(as.list(cost_table['high',]))
  } else if (better_side == 9){
    return(as.list(cost_table['none',]))
  }
}

gen_sound = function(n_trials = 1000, sigma_s = 0.5){
  # generate sound stimulus from a truncated Gaussian
  sounds = rep(0, n_trials)
  for (tx in 1:n_trials){
    sounds[tx] = 0
    while (sounds[tx] > 4 | sounds[tx] < 2){
      sounds[tx] = rnorm(1, 3, sigma_s)
    }
  }
  return(sounds)
}

#df$sigma_s_index = df %>% group_by(sigma_s) %>% group_indices()
# ccsd_L = list()
# ccsd_H = list()
# for (sigma_s_index in sort(unique(df$sigma_s_index))){
#   sigma_s = unique(df$sigma_s[df$sigma_s_index == sigma_s_index])
#   gsd = dnorm(x, mean = 3, sd = sigma_s) # general stimulus distribution 
#   ccsd_L[paste0(sigma_s)] = list(append(gsd[1:1000], rep(0, 1001)))
#   ccsd_H[paste0(sigma_s)] = list(append(rep(0, 1001), gsd[1001:2000]))
# }

fit_bdt = function(df, pred = FALSE, sigma_asy = 0, omega_asy = 0, save = FALSE){
  # binomialize df
  #df = df %>% select(!'n')
  bino_df = df %>% group_by(better_side, rew_multi, s_binned) %>% 
    add_tally() %>% summarise(n_trials = mean(n), n_chose_high = sum(chose_high)) %>% 
    ungroup() %>% rename(s = s_binned)
  if (pred){
    pred_df = df %>% data_grid(s = s_binned, better_side = better_side, rew_multi = rew_multi, n_trials = 200)
    colnames(pred_df) = paste0('pred_', colnames(pred_df))
    fit_model = bdt_bino_pred_model
    fname = sprintf('fits/%d_pred.RData', unique(df$subjid))
    data = append(as.list(bino_df), as.list(pred_df))
    data$P = dim(pred_df)[1]
  } else{
    fit_model = bdt_bino_model
    fname = sprintf('fits/%d.RData', unique(df$subjid))
    data = as.list(bino_df)
  }
  # create lists of CCSD for different sigma_s groups
  x = seq(2, 4, 0.001)
  gsd = dnorm(x, mean = 3, sd = unique(df$best_sigma_s)) # general stimulus distribution 
  ccsd_L = append(gsd[1:1000], rep(0, 1001))
  ccsd_H = append(rep(0, 1001), gsd[1001:2000])
  # append data to the list
  data$T = dim(bino_df)[1]
  data$x = x
  data$X = length(x)
  data$ccsd_L = ccsd_L
  data$ccsd_H = ccsd_H
  # for toggle models
  #data$sigma_asy = sigma_asy
  #data$omega_asy = omega_asy
  # fit model
  fit = sampling(fit_model, data = data, refresh = 1, chains = 4, warmup = 200, iter = 300, seed = SEED, init = 'random')
  check_hmc_diagnostics(fit)
  if (save){
    save(fit, file = fname)
  }
  return(fit)
}

fit_animals = function(pred = FALSE){
  # fit bdt.stan to this animal's dataset and save fit
  df = read.csv('csv/preprocessed_good_trials.csv') %>% filter(better_side != 2) %>% select(!'n')
  subj_list = unique(df$subjid)
  #subj_list = c(2143, 2123)
  for (subj in subj_list){
    cat(sprintf('Fitting bdt.stan on %d....\n', subj))
    fit_bdt(df %>% filter(subjid == subj), pred)
  }
}

plot_syn = function(df){
  # makes a plot to analyse perceptual gambling behavior given subjid
  df$better_side = as.factor(df$better_side)
  n_trials = dim(df)[1]
  qt = quantile(df$s, probs = seq(0, 1, 0.1))
  p = ggplot(df) + theme_classic(BASE_SIZE) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
    geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) + 
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
    stat_summary_bin(mapping = aes(x = s, y = chose_high, color = as.factor(better_side)), fun.data = bino, breaks = qt, geom = 'pointrange') +
    geom_line(mapping = aes(x = s, y = p_chose_high, color = as.factor(better_side)), alpha = 1, size = 1) +
    #annotate("text", label = paste0('n=', n_trials), x = 3.5, y = 0.1) + 
    xlab(expression(log2(Frequency))) + ylab("P(Chose High)") +
    scale_color_manual(values = c('#F37970', 'azure4', '#81B628'), name = 'Flash on') +
    theme(legend.position = 'none')
  return(p)
}
