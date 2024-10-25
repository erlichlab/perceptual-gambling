# functions to make figure_behavior panels
make_figure_behavior = function(){
  p = figure_behavior_example(2143) 
  scale_save(p, 'figure_behavior_example', 8, 8, 1)
  p = figure_behavior_violin(2143)
  scale_save(p, 'figure_behavior_violin', 8, 8, 1)
  p = figure_behavior_difficulty_schematic()
  scale_save(p, 'figure_behavior_difficulty_schematic', 10, 8, 1)
  p = figure_behavior_difficulty()
  scale_save(p, 'figure_behavior_difficulty', 10, 8, 1)
  p = figure_behavior_population() 
  scale_save(p, 'figure_behavior_population', 8, 8, 1)
  #p = figure_behavior_RT() 
  #scale_save(p, 'figure_behavior_RT', 12, 10, 1)
}

figure_behavior_example = function(animal = 2143, show_sessions = FALSE, n_sessions = 10){
  # Example psychometric curve from one example animal
  # thick line = average performance, thin line = session performance
  subj_df = read.csv('csv/preprocessed_good_trials.csv') %>% filter(subjid == animal)
  
  m0 = glm('chose_high ~ log2_sound*better_side_s', subj_df, family = binomial)
  avg_pred_df = data_grid(subj_df, log2_sound = seq_range(log2_sound, by = 0.001), better_side_s = better_side_s)
  avg_pred_df$pred = predict(m0, avg_pred_df, type = 'response')
  
  n_trials = subj_df %>% group_by(better_side_s) %>% tally() %>% pull(n) # high, low, none
  qt = quantile(subj_df$log2_sound, probs = seq(0, 1, 0.1))
  p = ggplot() + theme_classic() +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
    geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) + 
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
    annotate("text", label = unique(subj_df$subjid), fontface = 2, x = 3.7, y = 0.25) + 
    annotate("text", label = paste0('n=', n_trials[1]), x = 3.7, y = 0.2, color = better_side_colors$High) +  
    annotate("text", label = paste0('n=', n_trials[3]), x = 3.7, y = 0.15, color = better_side_colors$None) +
    annotate("text", label = paste0('n=', n_trials[2]), x = 3.7, y = 0.1, color = better_side_colors$Low) +  
    xlab(expression(log2(Frequency))) + ylab("P(Chose High)") +
    scale_color_manual(values = c('#F37970', '#81B628', 'azure4'), name = 'Flash on') +
    theme(legend.position = 'none')
  if (show_sessions){
    for (sessidx in sample(unique(subj_df$sessid), n_sessions)){
      sess_df = subj_df %>% filter(sessid == sessidx)
      m1 = glm('chose_high ~ log2_sound*better_side_s', sess_df, family = binomial)
      sess_pred_df = data_grid(sess_df, log2_sound = seq_range(log2_sound, by = 0.001), better_side_s = better_side_s)
      sess_pred_df$pred = predict(m1, sess_pred_df, type = 'response', allow.new.levels=TRUE)
      p = p + geom_line(sess_pred_df, mapping = aes(x = log2_sound, y = pred, color = better_side_s), alpha = 0.4, size = 0.5)
    }
  }
  p = p + stat_summary_bin(subj_df, mapping = aes(x = log2_sound, y = chose_high, color = better_side_s), fun.data = bino, breaks = qt, geom = 'pointrange') +
    geom_line(avg_pred_df, mapping = aes(x = log2_sound, y = pred, color = better_side_s), alpha = 1, size = 1)
  return(p) 
}

figure_behavior_violin = function(animal = 2143){
  # violin plot showing the distribution of % choosing high by condition across sessions
  subj_df = read.csv('csv/preprocessed_good_trials.csv') %>% filter(subjid == animal)
  subj_df$better_side_s = factor(subj_df$better_side_s, c('Low', 'None', 'High'))
  
  violin_df = data.frame()
  for (sessidx in unique(subj_df$sessid)){
    sess_df = subj_df %>% filter(sessid == sessidx)
    temp_df = sess_df %>% group_by(better_side_s) %>% summarise(mean_chose_high = mean(chose_high))
    violin_df = rbind(violin_df, temp_df)
  }
  p = ggplot(violin_df, aes(x = better_side_s, y = mean_chose_high*100, fill = better_side_s, color = better_side_s)) + theme_classic(BASE_SIZE) +
    geom_violin(alpha = 0.4) + geom_jitter(height = 0, width = 0.1) +
    geom_hline(yintercept = 50, linetype = 'dashed', alpha = 0.4) + 
    scale_fill_manual(values = c('#81B628',  'azure4', '#F37970')) + 
    scale_color_manual(values = c('#81B628',  'azure4', '#F37970')) + 
    scale_y_continuous(breaks = c(20, 50, 80)) +
    xlab(" ") + ylab("% Chose High") + theme(legend.position = 'none')
  return(p)
}

figure_behavior_difficulty_schematic = function(){
  # plots a schematic bell curve for easy - medium - hard illustration
  df = data.frame(log2_sound = rnorm(1000, 3, 1)) %>% 
    mutate(b1 = quantile(log2_sound, probs = c(0.2)), 
           b2 = quantile(log2_sound, probs = c(0.4)), 
           b3 = quantile(log2_sound, probs = c(0.6)), 
           b4 = quantile(log2_sound, probs = c(0.8))) %>% 
    mutate(difficulty = case_when(log2_sound < b1 ~ 'Easy', 
                                  log2_sound >= b4 ~ 'Easy', 
                                  (log2_sound >= b1 & log2_sound < b2) ~ 'Medium', 
                                  (log2_sound >= b3 & log2_sound < b4) ~ 'Medium', 
                                  (log2_sound >= b2 & log2_sound < b3) ~ 'Hard'))
  p = ggplot(df, aes(x = log2_sound, fill = difficulty)) + theme_classic(BASE_SIZE) + 
    geom_density() + xlab(expression(log2(Frequency))) + ylab("Probability") +
    geom_vline(xintercept = 3, linetype = 'dashed') +
    scale_x_continuous(breaks = c(2, 3, 4)) + scale_y_continuous(breaks = c(0, 0.5, 1)) 
  return(p)
}

figure_behavior_difficulty = function(){
  # Do animals shift towards the flashing side more in difficult trials than medium, easy trials?
  # 1. For each session, get mean percentage choosing high from 9 condition: diffculty (easy, medium, hard) x better_side
  # 2. For each difficulty, subtract delta_low = low - none, delta_high = high - none, and get delta = mean(delta_low + delta_high)
  # 3. Now we have 3, plot all the points for each animal (LMM for each animal)
  # Plot points connecting each session 
  df = read.csv('csv/preprocessed_good_trials.csv')
  df$better_side_s = factor(df$better_side_s, c('Low', 'None', 'High'))
  # find sigma_p for each animal
  fit_df = read.csv('csv/bdt_fits.csv')
  fit_df = fit_df %>% group_by(subjid) %>% mutate(max_lp = which.max(lp__)) %>% 
    summarise(sigma_p = mean(sigma[max_lp]))
  df = merge(df, fit_df, by = 'subjid')
  # bin trials based on log2_sound into easy, medium, hard difficulty levels
  b1 = 0.426
  b2 = 0.95
  a = df %>% group_by(subjid, sessid) %>% mutate(z_score = (log2_sound - 3) / sigma_p) %>% 
    mutate(difficulty = case_when((z_score >= -b1 & z_score < b1) ~ '3',
                                  (z_score < -b1 & z_score >= -b2) ~ '2_low', 
                                  (z_score >= b1 & z_score < b2) ~ '2_high', 
                                  z_score < -b2 ~ '1_low', 
                                  z_score >= b2 ~ '1_high')) %>% 
    group_by(subjid, sessid, better_side_s, difficulty) %>% summarise(mean_chose_high = mean(chose_high)) %>% ungroup()
  #a$mean_chose_high[a$mean_chose_high == 0] = 0.01
  delta_df = data.frame()
  for (sessidx in unique(a$sessid)){
    tryCatch(
      expr = {
        b = a %>% filter(sessid == sessidx)
        temp_df = data.frame(sessid = rep(sessidx, 5), subjid = rep(unique(b$subjid), 5),
                             difficulty = c('1_high', '1_low', '2_high', '2_low', '3'),
                             # how much flashing low/high side shifts the choices for each difficulty
                             delta_low = abs(b %>% filter(better_side_s == 'Low') %>% pull(mean_chose_high) - b %>% filter(better_side_s == 'None') %>% pull(mean_chose_high)),
                             delta_high = abs(b %>% filter(better_side_s == 'High') %>% pull(mean_chose_high) - b %>% filter(better_side_s == 'None') %>% pull(mean_chose_high)))
        temp_df = temp_df %>% mutate(delta = (delta_low + delta_high)/2) # get the mean shift by flashing lights
        # now aggregate the mean of easy and medium deltas
        temp_df2 = data.frame(sessid = rep(sessidx, 3), subjid = rep(unique(b$subjid), 3),
                              difficulty = c('Easy', 'Medium', 'Hard'),
                              delta = c( (temp_df %>% filter(difficulty == '1_high') %>% pull(delta) + temp_df %>% filter(difficulty == '1_low') %>% pull(delta)) / 2,
                                         (temp_df %>% filter(difficulty == '2_high') %>% pull(delta) + temp_df %>% filter(difficulty == '2_low') %>% pull(delta)) / 2,
                                         temp_df %>% filter(difficulty == '3') %>% pull(delta)))
        delta_df = rbind(delta_df, temp_df2)
      },
      error = function(e){
        message("This session sucks.")
      }
    )
  }
  # aggregate the sessions
  subj_delta_df = delta_df %>% group_by(subjid, difficulty) %>% summarise(delta_mean = mean(delta), delta_se = se(delta))
  subj_delta_df$difficulty = factor(subj_delta_df$difficulty, c('Easy', 'Medium', 'Hard')) 
  #m0 = lm('delta ~ difficulty', data = delta_df %>% filter(subjid == 2077))
  m1 = lmer('delta ~ difficulty + (difficulty | subjid)', data = delta_df)
  m1 = lmer('delta ~ difficulty + (difficulty | subjid)', data = delta_df %>% filter(difficulty %in% c('Medium', 'Hard')))
  #m1 = lmer('delta ~ difficulty + (difficulty | subjid)', data = delta_df %>% filter(difficulty %in% c('Easy', 'Hard')))
  p = ggplot(subj_delta_df, aes(x = difficulty, y = delta_mean*100, group = as.factor(subjid), ymin = delta_mean*100 - delta_se*100, ymax = delta_mean*100 + delta_se*100, color = difficulty)) + 
    theme_classic(BASE_SIZE) + 
    geom_line(position = position_dodge(0.3)) + 
    geom_point(position = position_dodge(0.3), size = 2) + 
    geom_errorbar(width = 0, position = position_dodge(0.3)) +
    scale_y_continuous(breaks = c(10, 25, 40)) + 
    scale_color_manual(values = c('#cdcfd0', '#a4a8aa', '#7c8185', '#434648')) + # light to dark
    xlab(" ") + ylab("% Chose High") + theme(legend.position = 'none')
  return(p)
}

figure_behavior_population = function(){
  # Psychometric curves for the population
  # thin colored lines = each subject, thick line = population average
  df = read.csv('csv/preprocessed_good_trials.csv') %>% filter(better_side != 2)
  # normalize by baseline RT for each subject
  #x = df %>% group_by(subjid, better_side_s) %>% summarise(baseline_RT = mean(resp_time)) %>% filter(better_side_s == 'None')
  #df = merge(df, x, by = c'subjid')
  # GLMM fit
  df$better_side_s = factor(df$better_side_s, c('None', 'Low', 'High'))
  m0 = glm('chose_high ~ log2_sound*better_side_s', df, family = binomial)
  pop_pred_df = data_grid(df, log2_sound = seq_range(log2_sound, by = 0.01), better_side_s = better_side_s)
  pop_pred_df$pred = predict(m0, pop_pred_df, type = 'response', allow.new.levels=TRUE)
  
  qt = quantile(df$log2_sound, probs = seq(0, 1, 0.1))
  p = ggplot() + theme_classic(base_size = BASE_SIZE) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
    geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) + 
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
    annotate("text", label = sprintf('%d trials \n %d sessions \n %d animals', dim(df)[1], length(unique(df$sessid)), length(good_list)), x = 3.7, y = 0.2, size = ANNOTATION_SIZE) + 
    xlab(expression(log2(Frequency))) + ylab("P(Chose High)") +
    scale_color_manual(values = c('#F37970', '#81B628', 'azure4'), name = 'Flash on') +
    theme(legend.position = 'none')
  subj_list = unique(df$subjid)
  alphas_list = seq(0.1, 1, by = 1 / length(subj_list))
  for (i in 1:length(subj_list)){
    subj_df = df %>% filter(subjid == subj_list[i])
    ind_pred_df = data_grid(subj_df, log2_sound = seq_range(log2_sound, by = 0.01), better_side_s = better_side_s)
    m1 = glm('chose_high ~ log2_sound*better_side_s', subj_df, family = binomial)
    ind_pred_df$pred = predict(m1, ind_pred_df, type = 'response', allow.new.levels=TRUE)
    p = p + geom_line(ind_pred_df, mapping = aes(x = log2_sound, y = pred, color = better_side_s), alpha = alphas_list[i])
  }
  p = p + stat_summary_bin(df, mapping = aes(x = log2_sound, y = chose_high, color = better_side_s), fun.data = bino, bins = 7, geom = 'pointrange') +
    geom_line(pop_pred_df, mapping = aes(x = log2_sound, y = pred, color = better_side_s), alpha = 1, size = 2)
  return(p)
}

figure_behavior_RT = function(){
  # summary of population RT 
  df = read.csv('csv/preprocessed_good_trials.csv') %>% filter(better_side != 2)
  df$log_RT = log(df$resp_time)
  rt_df = df %>% group_by(subjid, better_side_s, log2_sound > 3) %>% summarise(mean_RT = mean(log_RT), se_RT = se(log_RT))
}

figure_behavior_history = function(){
  # plot psychometric curves based on previous trial's difficulty and it has to be correct?
  df = read.csv('csv/preprocessed_good_trials.csv')
  df = df %>% mutate(prev_flash = lag(better_side_s), prev_correct = lag(correct_choice), prev_choice = lag(chose_high), 
                     prev_reward = lag(reward)) %>% 
    mutate(prev_outcome = case_when(prev_choice == 0 & prev_reward > 0 ~ 'Low win',
                                    prev_choice == 0 & prev_reward == 0 ~ 'Low lose',
                                    prev_choice == 1 & prev_reward > 0 ~ 'High win',
                                    prev_choice == 1 & prev_reward == 0 ~ 'High lose')) #%>% filter(prev_correct == 1)
  df$prev_outcome = factor(df$prev_outcome, c('Low win', 'Low lose', 'High lose', 'High win'))
  df$prev_outcome = factor(df$prev_outcome, c('Low lose', 'Low win', 'High lose', 'High win'))
  
  m0 = glm('chose_high ~ log2_sound * prev_outcome', df, family = binomial)
  #m1 = glmer('chose_high ~ log2_sound + prev_choice + (log2_sound + prev_choice | subjid)', df, family = binomial, nAGQ = 0)
  #m1 = glmer('chose_high ~ log2_sound + prev_outcome + (log2_sound + prev_outcome | subjid)', df, family = binomial, nAGQ = 0)
  for (subj in good_list){
    subj_df = df %>% filter(subjid == subj)
    m0 = glm('chose_high ~ log2_sound + prev_outcome', subj_df, family = binomial)
    save(m0, file = sprintf("%d_history.RData", subj))
    pred_df = data_grid(subj_df, log2_sound = seq_range(log2_sound, by = 0.001), prev_outcome = prev_outcome)
    pred_df$pred = predict(m0, pred_df, type = 'response')
    qt = quantile(subj_df$log2_sound, probs = seq(0, 1, 0.1))
    p = ggplot(subj_df) + theme_classic(BASE_SIZE) +
      stat_summary_bin(mapping = aes(x = log2_sound, y = chose_high, color = prev_outcome), fun.data = bino, breaks = qt, geom = 'pointrange', size = 0.3) +
      geom_line(pred_df, mapping = aes(x = log2_sound, y = pred, color = prev_outcome), size = 0.5) +
      #annotate("text", label = unique(subj_df$subjid), fontface = 2, x = 3.7, y = 0.25) +
      geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
      geom_vline(xintercept = 3, linetype = 'dashed',  alpha = 0.4) +
      scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
      scale_x_continuous(limits = c(2, 4), breaks = c(2, 3, 4)) +
      xlab(expression(log2(Frequency))) + ylab("P(Chose High)") +
      #scale_color_manual(values = c( '#81B628', '#F37970'), name = 'Previous Choice') +
      scale_color_manual(values = c('#81b628', '#9ea73a', '#d7885e', '#f37970'), name = 'Previous outcome') +
      #theme(legend.position = 'none', axis.title = element_blank())
      theme(legend.position = 'none')
    scale_save(p, sprintf("%d_history", subj), 8, 8, 1)
  }
  return(p)
}
