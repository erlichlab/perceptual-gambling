# functions for behavioral analysis 
# history analysis !

getTrials = function(){
  # in general, this should be run once and it will save a csv
  con = elConnect()
  # get the subjids that have enough sessions in hedging
  sqlquery = "SELECT subjid, COUNT(subjid) AS n_sess FROM beh.sessview WHERE settings_name = 'hedging' AND subjid > 2000 GROUP BY subjid HAVING n_sess > 10"
  subjids = suppressWarnings(dbGetQuery(con, sqlquery))$subjid
  # get their qualified sessids
  subjid_str = reduce(subjids,function(s1,s2){paste(s1,",",s2)})
  sqlquery = sprintf("select sessid from beh.sessview where subjid IN (%s) AND settings_name = 'hedging' AND total_profit > 1000", subjid_str)
  sessids = suppressWarnings(dbGetQuery(con, sqlquery))$sessid
  # get trials data from these sessions
  sessid_str = reduce(sessids,function(s1,s2){paste(s1,",",s2)})
  sqlquery = sprintf("select * from proto.perceptgam_view where sessid IN (%s)", sessid_str)
  df = suppressWarnings(dbGetQuery(con, sqlquery))
  dbDisconnect(con)
  write.csv(df, 'csv/raw_trials.csv')
  return(df)
}

preprocess_pg = function(){
  df = read.csv('csv/raw_trials.csv')
  # preprocess df from getTrials
  df = df %>% mutate(is_odd = subjid %% 2) %>% 
    filter(subjid %in% good_list) %>%
    mutate(chose_high = case_when(correct_choice == 1 & pitch_high == 1 ~ 1,
                                  correct_choice == 1 & pitch_high == 0 ~ 0,
                                  correct_choice == 0 & pitch_high == 1 ~ 0,
                                  correct_choice == 0 & pitch_high == 0 ~ 1)) %>% 
    mutate(chose_right = case_when(is_odd == 1 & chose_high == 1 ~ 1, # for odd subjid: high = R, low = L
                                   is_odd == 1 & chose_high == 0 ~ 0, 
                                   is_odd == 0 & chose_high == 1 ~ 0, # for low subjid: high = L, low = R
                                   is_odd == 0 & chose_high == 0 ~ 1)) %>%  
    mutate(choice = ifelse(chose_high == 1, 'High', 'Low')) %>% 
    filter(init_time != 0) %>% filter(resp_time != 0) %>% # remove timeout trials
    filter(better_side != 2) %>% 
    mutate(better_side_s = case_when(is_odd == 1 & better_side == 0 ~ 'Low',
                                     is_odd == 1 & better_side == 1 ~ 'High', # for odd subjid: high = R, low = L
                                     is_odd == 0 & better_side == 0 ~ 'High', # for low subjid: high = L, low = R
                                     is_odd == 0 & better_side == 1 ~ 'Low',
                                     better_side == 9 ~ 'None')) %>% 
    group_by(sessid) %>% # find session-specific rew_multi of better_side
    mutate(rew_multi = max(reward) / min(reward[reward > 0])) %>% ungroup() %>% 
    mutate(prev_better_side = lag(better_side)) %>%  # add previous better_side
    mutate(prev_reward = lag(reward)) %>%  # add previous reward
    mutate(prev_choice = lag(choice)) %>% # add previous choice
    # mutate(prev_outcome = case_when(prev_better_side == 'None' & prev_reward == 0 & prev_choice == 'High' ~ 'high_incorrect', # no flash, low correct, choose high
    #                                 prev_better_side == 'None' & prev_reward == 0 & prev_choice == 'Low' ~ 'low_incorrect', # no flash, high correct, choose low
    #                                 prev_better_side == 'None' & prev_reward > 0 & prev_choice == 'High' ~ 'high_correct', # no flash, high correct, choose high
    #                                 prev_better_side == 'None' & prev_reward > 0 & prev_choice == 'Low' ~ 'low_correct', # no flash, low correct, choose low
    #                                 prev_better_side == 'High' & prev_reward == 0 & prev_choice == 'High' ~ 'high_lose', # high flash, low correct, choose high
    #                                 prev_better_side == 'High' & prev_reward > 0 & prev_choice == 'Low' ~ 'low_correct', # high flash, low correct, choose low
    #                                 prev_better_side == 'High' & prev_reward > 0 & prev_choice == 'High' ~ 'high_win', # high flash, high correct, choose high
    #                                 prev_better_side == 'High' & prev_reward == 0 & prev_choice == 'low' ~ 'low_incorrect', # high flash, high correct, choose low
    #                                 prev_better_side == 'Low' & prev_reward == 0 & prev_choice == 'Low' ~ 'low_lose', # low flash, high correct, choose low
    #                                 prev_better_side == 'Low' & prev_reward > 0 & prev_choice == 'Low' ~ 'low_win', # low flash, low correct, choose low
    #                                 )) %>% 
  arrange(subjid, trialid)
  write.csv(df, 'csv/preprocessed_trials.csv')
  return(df)
}

get_sigma_s = function(){
  # extract sigma_s from met.subjet_settings for each session
  con = elConnect()
  df = read.csv('csv/preprocessed_trials.csv') # only use the good ones
  info_df = df %>% mutate(session_date = as.Date(trialtime)) %>% group_by(subjid, sessid, session_date) %>% tally()
  settings_df = data.frame()
  for (i in 1:nrow(info_df)){
    sqlquery = sprintf("SELECT * FROM met.subject_settings WHERE DATE(settings_date) = '%s' AND subjid = %s",
                       info_df$session_date[i], info_df$subjid[i])
    settings_df = rbind(settings_df, suppressWarnings(dbGetQuery(con, sqlquery)))
  }
  settings_df = settings_df %>% mutate(session_date = as.Date(settings_date)) %>%
    filter(settingsid %in% c(275, 337)) %>% # settings for hedging and hedging_rand
    filter(!is.na(settings_data)) %>% filter(saved_by_experid == 0)
  dbDisconnect(con)
  write.csv(settings_df, 'csv/raw_settings.csv')
  return(settings_df)
}

append_sigma_s = function(){
  # append sigma_s informatin to the good trials
  df = read.csv('csv/preprocessed_trials.csv') # only use the good ones
  info_df = df %>% mutate(session_date = as.Date(trialtime)) %>% group_by(subjid, sessid, session_date) %>% tally()
  settings_df = read.csv('csv/raw_settings.csv')
  settings_df$settings_data = as.character(levels(settings_df$settings_data))[settings_df$settings_data]
  # extract sigma_s and append it to preprocessed_trials.csv
  settings_df$sigma_s = 0
  for (i in 1:nrow(settings_df)){
    settings_df$sigma_s[i] = fromJSON(settings_df$settings_data[i])$vals$sigma_s
  }
  settings_df = settings_df %>% group_by(subjid, session_date) %>% summarise(sigma_s = mean(sigma_s))
  info_df = merge(info_df, settings_df, by = c('subjid', 'session_date'))
  df = merge(df, info_df, by = c('subjid', 'sessid'))
  write.csv(df, 'csv/preprocessed_trials.csv')
  return(df)
}

preprocess_pg2 = function(){
  # bin log2_sound into n_bins for each subject's dataset
  # and find the most frequent sigma_s and the sigma_s
  df = read.csv('csv/preprocessed_trials.csv')
  subj_list = unique(df$subjid)
  breaks = seq(2, 4, by = 0.1)
  labels = c()
  for (i in 1:length(breaks)-1){
    labels = append(labels, (breaks[i] + breaks[i+1])/2)
  }
  out_df = data.frame()
  for (subj in subj_list){
    subj_df = df %>% filter(subjid == subj)
    subj_df = subj_df %>% mutate(s_binned = cut(log2_sound, breaks, labels, include.lowest = TRUE))
    subj_df$s_binned = as.numeric(levels(subj_df$s_binned))[subj_df$s_binned]
    out_df = rbind(out_df, subj_df)
  }
  # use the most frequent sigma_s
  info_df = df %>% group_by(subjid, sigma_s, rew_multi) %>% tally()
  info_df = info_df %>% group_by(subjid) %>% summarise(best_sigma_s = sigma_s[which.max(n)]) 
  out_df = merge(out_df, info_df, by = 'subjid')
  write.csv(out_df, 'csv/preprocessed_trials.csv')
  return(out_df)
}

preprocess_pg3 = function(){
  # select only the good task parameter combination for each animal
  df = read.csv('csv/preprocessed_trials.csv')
  good_df = data.frame()
  for (subj in good_list){
    subj_df = df %>% filter(subjid == subj) %>% filter(rew_multi == rm_list[[as.character(subj)]]) %>% 
      filter(sigma_s == sigma_s_list[[as.character(subj)]]) %>% 
      filter(is.na(flash_level) | flash_level == 0) # some subjects have different flash levels
    good_df = rbind(good_df, subj_df)
  }
  write.csv(good_df, 'csv/preprocessed_good_trials.csv')
  return(good_df)
}

