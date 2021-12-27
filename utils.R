# utility functions
COMPILE_MODELS = 0
SEED = 12345
BASE_SIZE = 15
ANNOTATION_SIZE = 5

# hyperparameters
good_list = c(2077, 2078, 2083, 2085, 2109, 2124, 2143) # list of good subjects
rm_list = list('2077' = 5,  # list of reward multipliers
               '2078' = 5, 
               '2083' = 5, 
               '2085' = 5,
               '2109' = 25,
               '2124' = 20, 
               '2143' = 15)

sigma_s_list = list('2077' = 1, # list of sigma_s
                    '2078' = 1, 
                    '2083' = 1, 
                    '2085' = 1,
                    '2109' = 0.3,
                    '2124' = 0.3, 
                    '2143' = 0.3)

prop_list = list('2077' = 0.65, # one side prob
                 '2078' = 0.7, 
                 '2083' = 0.4, 
                 '2085' = 0.4,
                 '2109' = 0.35,
                 '2124' = 0.5, 
                 '2143' = 0.4)
                 
better_side_colors = list('High' = '#F37970', 'None' = 'azure4', 'Low' = '#81B628') # red, gray, green

# model parameters
params = c('rho', 'sigma', 'omega_rational', 'omega_low', 'omega_high')
latex_params = c('rho' ='$\\rho$', 'sigma' = '$\\sigma_{p}$', 
                 'omega_rational' = '$\\omega_{rational}$', 
                 'omega_low' = '$\\omega_{low}$', 
                 'omega_high' = '$\\omega_{high}$', 
                 'omega.1' = '$\\omega_{rational}$', 
                 'omega.2' = '$\\omega_{low}$', 
                 'omega.3' = '$\\omega_{high}$')

subj_colors = c("#67595E", # brown
                '#FFA384', # salmon
                '#478C5C', # green
                '#189AB4', # blue
                '#C197D2', # lavender
                '#FAD02C', # yellow
                '#FFC5D0') # rose
                

# load models
if (COMPILE_MODELS){
  sigmoid_model = stan_model(file = 'sigmoid.stan')
  #bdt_model = stan_model(file = 'bdt.stan')
  bdt_bino_model = stan_model(file = 'bdt_bino.stan')
  bdt_bino_pred_model = stan_model(file = 'bdt_bino_pred.stan')
  #bdt_toggle_model = stan_model(file = 'bdt_toggle.stan')
  #bdt_toggle_cv_model = stan_model(file = 'bdt_toggle_cv.stan')
}

scale_save = function(p, name, width = 4, height = 4, scale = 1){
  # ggsave wrapper
  p = p + theme(text = element_text(size = BASE_SIZE), 
                axis.text = element_text(size = BASE_SIZE), 
                legend.text = element_text(size = BASE_SIZE))
  fname = sprintf('plots/%s.pdf', name)
  ggsave(filename = fname, device = "pdf", width = width, height = height, scale = scale, unit = 'cm')
}

se = function(x){
  return(sd(x) / sqrt(length(x)))
}

bino = function(x){
  out = binom.test(sum(x), length(x))
  df = data.frame(y = mean(x), ymin = out$conf.int[1], ymax = out$conf.int[2])
  return(df)
}

plot_pg = function(df, subj, rewmulti, sigmas, history = FALSE){
  # makes a plot to analyse perceptual gambling behavior given subjid
  df = df %>% filter(subjid == subj) %>% filter(rew_multi == rewmulti) %>% filter(sigma_s == sigmas)
  if (history){
    m0 = glm('chose_high ~ log2_sound*better_side_s*prev_outcome', df, family = binomial)
    #m5 = glm('chose_high ~ log2_sound + better_side_s + prev_reward', df, family = binomial)
    avg_pred_df = data_grid(df, log2_sound = seq_range(log2_sound, by = 0.001), better_side_s = better_side_s, prev_reward = prev_reward)
  } else{
    m0 = glm('chose_high ~ log2_sound*better_side_s', df, family = binomial)
    # a = anova(m0, m1)
    # p = 1 - pchisq( abs(a$Deviance[2]), abs(a$Df[2]))
    avg_pred_df = data_grid(df, log2_sound = seq_range(log2_sound, by = 0.001), better_side_s = better_side_s)
  }
  avg_pred_df$pred = predict(m0, avg_pred_df, type = 'response')
  n_trials = dim(df)[1]
  qt = quantile(df$log2_sound, probs = seq(0, 1, 0.1))
  n_trials = df %>% group_by(better_side) %>% tally() %>% pull(n) # high, low, none
  p = ggplot(df) + theme_classic() +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
    geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) + 
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
    #annotate("text", label = paste0('n=', n_trials), x = 3.5, y = 0.1) + 
    annotate("text", label = unique(df$subjid), fontface = 2, x = 3.5, y = 0.3) + 
    annotate("text", label = paste0('n=', n_trials[1]), x = 3.5, y = 0.2, color = better_side_colors$High) +  
    annotate("text", label = paste0('n=', n_trials[3]), x = 3.5, y = 0.1, color = better_side_colors$None) +
    annotate("text", label = paste0('n=', n_trials[2]), x = 3.5, y = 0, color = better_side_colors$Low) +  
    xlab(expression(log2(Frequency))) + ylab("P(Chose High)") +
    scale_color_manual(values = c('#F37970', '#81B628', 'azure4'), name = 'Flash on') +
    scale_alpha_manual(values = c(0.2, 0.4, 1)) + 
    theme(legend.position = 'none')
  if (history){
    p = p + stat_summary_bin(mapping = aes(x = log2_sound, y = chose_high, color = as.factor(better_side)), fun.data = bino, breaks = qt, geom = 'pointrange') +
      geom_line(avg_pred_df, mapping = aes(x = log2_sound, y = pred, color = better_side)) +
      facet_grid(~as.factor(prev_reward))
  } else{
    p = p + stat_summary_bin(mapping = aes(x = log2_sound, y = chose_high, color = better_side_s), fun.data = bino, breaks = qt, geom = 'pointrange') +
      geom_line(avg_pred_df, mapping = aes(x = log2_sound, y = pred, color = better_side_s), alpha = 1, size = 1)
  }
  print(summary(m0))
  return(p)
}

plotRT = function(df, subj, cutoff_threshold = 0.15){
  # make a plot for RT given subjid
  subj_df = df %>% filter(subjid == subj) %>% arrange(sessid)
  unique_sessids = unique(subj_df$sessid)
  cutoff = round(length(unique_sessids) * cutoff_threshold)
  subj_df = subj_df %>% filter(! sessid %in% unique_sessids[1:cutoff]) %>% # remove the first 15% sessions 
    filter(! sessid %in% unique_sessids[(length(unique_sessids) - cutoff):length(unique_sessids)]) # remove the last 15% sessions
  
  n_trials = dim(subj_df)[1]
  qt = quantile(subj_df$log2_sound, probs = seq(0, 1, 0.15))
  p = ggplot(subj_df) + theme_classic() +
    geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) + 
    stat_summary_bin(mapping = aes(x = log2_sound, y = log(resp_time), color = better_side), fun.data = 'mean_se', breaks = qt, geom = 'pointrange') +
    scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
    xlab(expression(log2(Frequency))) + ylab('log(RT)') +
    scale_color_manual(values = c('indianred', 'azure4', 'forestgreen'), name = 'Flash on') +
    theme(legend.position = 'bottom') + ggtitle(subj) + facet_grid(~choice)
  return(p)
}

plotGroup = function(df){
  subj_list = unique(df$subjid)
  for (subj in subj_list){
    p = plotPG(df, subj)
    ggsave(sprintf('plots/%d.png', subj), device = 'png', width = 5, height = 5, scale = 0.7)
  }
}

computeELPD = function(log_lik_heldout){
  # adapted from https://github.com/stan-dev/stancon_talks/blob/master/2017/Contributed-Talks/07_nicenboim/kfold.Rmd
  # The ELPD is the theoretical expected log pointwise predictive density for a new dataset
  # the loglik from the fit is a n_iter x T matrix, the pointwise is 1 x T vector, elpd is the sum of pointwise
  library(matrixStats)
  logColMeansExp = function(x) {
    S = nrow(x)
    colLogSumExps(x) - log(S)
  }
  pointwise = matrix(logColMeansExp(log_lik_heldout), ncol = 1)
  colnames(pointwise) = "elpd"
  elpd = sum(pointwise)
  se_elpd = sqrt(ncol(log_lik_heldout) * var(pointwise))
  out = list('pointwise' = pointwise,
             'elpd' = elpd,
             'se_elpd' = se_elpd)
  return(structure(out, class = 'S4'))
}

kfold_toggle = function(K_FOLD = 10, model = 'bdt'){
  # the generic function for manual K-fold cross validation  
  df = read.csv('csv/preprocessed_trials.csv') %>% filter(better_side != 2) %>% select(!'n')
  subj_list = unique(df$subjid)
  # first find the portion size for each subject
  portion_df = df %>% group_by(subjid) %>% tally() %>% 
    mutate(portion = round(n/K_FOLD), test_begin = 1, test_end = test_begin + portion)
  # initialize elpd container for this model's kfold
  elpd_df = data.frame()
  subj_list = c(2124)
  for (sx in 1:length(subj_list)){
    subj = subj_list[sx]
    for (k in 1:K_FOLD){
      df$holdout = 0 # 1 means to be heldout, 0 means to be used for fitting
      test_begin = portion_df$test_begin[portion_df$subjid == subj]
      test_end = portion_df$test_end[portion_df$subjid == subj]
      df$holdout[df$subjid == subj][test_begin:test_end] = 1
      # update test_begin and test_end number for the next fold
      portion_df$test_begin[portion_df$subjid == subj] = test_end
      if (k == K_FOLD - 1){
        portion_df$test_end[portion_df$subjid == subj] = portion_df$n[portion_df$subjid == subj]
      } else {
        portion_df$test_end[portion_df$subjid == subj] = test_end + portion_df$portion[portion_df$subjid == subj]
      }
      # format it into binomial version
      all_df = df %>% filter(subjid == subj) %>% group_by(holdout, better_side, rew_multi, s_binned) %>% 
        add_tally() %>% summarise(n_trials = mean(n), n_chose_high = sum(chose_high)) %>% 
        ungroup() %>% rename(s = s_binned)
      train_df = all_df[all_df$holdout == 0, ]
      test_df = all_df[all_df$holdout == 1, ]
      # create lists of CCSD for different sigma_s groups
      x = seq(2, 4, 0.001)
      subj_df = df %>% filter(subjid == subj)
      gsd = dnorm(x, mean = 3, sd = unique(subj_df$best_sigma_s)) # general stimulus distribution 
      ccsd_L = append(gsd[1:1000], rep(0, 1001))
      ccsd_H = append(rep(0, 1001), gsd[1001:2000])
      # append data to the list
      train_data = as.list(train_df)
      colnames(test_df) = paste0('test_', colnames(test_df))
      test_data = as.list(test_df)
      data = append(train_data, test_data)
      data$T = dim(train_df)[1]
      data$D = dim(test_df)[1]
      data$x = x
      data$X = length(x)
      data$ccsd_L = ccsd_L
      data$ccsd_H = ccsd_H
      # toggle
      data$sigma_asy = str_detect(model, 'sigma')
      data$omega_asy = str_detect(model, 'omega')
      # fit the model
      cat(sprintf('Fitting %d/%d fold on %d using %s...\n', k, K_FOLD, subj, model))
      fit = sampling(bdt_toggle_cv_model, data = data, seed = SEED, refresh = 1, warmup = 500, iter = 700, init = 'random')
      check_hmc_diagnostics(fit)
      # expand binomial elpd to bernoulli elpd by the number of trials 
      ll_draws = as.data.frame(rstan::extract(fit, pars = 'log_lik'))
      for (i in 1:dim(ll_draws)[2]){
        temp = as.data.frame(matrix(rep(ll_draws[,i], test_df$test_n_trials[i]), ncol=test_df$test_n_trials[i]))
        if (i == 1){
          k_bernoulli_ll_df = temp # n_iter x n_trials log_likelihood for this test set
        } else{
          k_bernoulli_ll_df = cbind(k_bernoulli_ll_df, temp)
        }
      }
      if (k == 1){
        bernoulli_ll_df = k_bernoulli_ll_df
      } else{
        bernoulli_ll_df = cbind(bernoulli_ll_df, k_bernoulli_ll_df)
      }
    }
    #save(bernoulli_ll_df, file = sprintf("fits/%s_%d_ll.RData", model, subj))
    # compute ELPD on log predictive posterior density aka. log likelihood on the test data
    elpd = computeELPD(as.matrix(bernoulli_ll_df))
    elpd_df = rbind(elpd_df, data.frame(subjid = subj, elpd = elpd$elpd, elpd_se = elpd$se_elpd))
  }
  write.csv(elpd_df, sprintf("csv/%s_2124_kfold.csv", model))
  return(elpd_df)
}

sigmoid_agent = function(w1 = 0.1, w2 = 0, x0 = 0, b = 0){
  # makes plots see how changing parameters change psychometric curve in this sigmoid function
  s  = gen_sound(1000, 0.3)
  y = w1 / (1 + exp(-b*(s - x0))) + (1 - w1) * w2
  
  df = data.frame(s = s, y = y)
  # p = ggplot(df, aes(x = s, y = y)) + geom_line() +
  #   scale_x_continuous(breaks = c(2, 3, 4), limits = c(2, 4)) +
  #   scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1))
  return(df)
}

fit_sigmoid = function(){
  # fit the 4-parameter sigmoid model to each subject's dataset under three flashing conditions to quantify the ch
  df = read.csv('csv/preprocessed_good_trials.csv')
  subj_list = unique(df$subjid)
  draws_df = data.frame()
  for (subj in subj_list){
    for (flash_side in c('Low', 'None', 'High')){
      this_df = df %>% filter(subjid == subj) %>% filter(better_side_s == flash_side)
      data = as.list(this_df %>% select(c('log2_sound', 'chose_high')) %>% rename(s = log2_sound))
      data$T = nrow(this_df)
      fit = sampling(sigmoid_model, data = data, refresh = 1, chains = 4, warmup = 200, iter = 400, seed = SEED, init = 'random')
      check_hmc_diagnostics(fit)
      draws = as.data.frame(rstan::extract(fit)) 
      max_lp = which.max(draws$lp__) 
      draws = draws[max_lp, ] %>% mutate(subjid = subj, better_side_s = flash_side)
      draws_df = rbind(draws_df, draws)
    }
  }
  write.csv(draws_df, 'csv/sigmoid_fits.csv')
  return(draws_df)
}