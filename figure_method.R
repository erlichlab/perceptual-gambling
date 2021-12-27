# functions make figure_method panels
make_figure_method = function(){
  p = figure_method_ccsd()
  scale_save(p, 'figure_method_b', 10, 6, 1)
  p = figure_method_observation()
  scale_save(p, 'figure_method_c', 10, 6, 1)
}

figure_method_ccsd = function(){
  x = seq(1.5, 4.5, by = 0.01)
  ccsd = dnorm(x, mean = 3, sd = 0.5)
  ccsd[x < 2] = 0
  ccsd[x > 4] = 0
  df = data.frame(x = x, ccsd1 = ccsd)
  p = ggplot(df) + theme_classic() +
    geom_line(aes(x = x, y = ccsd), size = 1) + geom_vline(xintercept = 3, linetype = 'dashed') +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
    xlab('log2(Frequency)') + ylab('Probability')
  return(p)
}

figure_method_observation = function(){
  x = seq(1.5, 4.5, by = 0.01)
  obs1 = dnorm(x, mean = 2.4, sd = 0.1)
  obs2 = dnorm(x, mean = 2.4, sd = 0.3)
  df = data.frame(x = x, obs1 = obs1, obs2 = obs2)
  p = ggplot(df) + theme_classic() +
    geom_line(aes(x = x, y = obs1), size = 1) + 
    geom_line(aes(x = x, y = obs2), size = 1) + 
    geom_vline(xintercept = 2.4, linetype = 'dashed') +
    xlab('s') + ylab('p(x|s)') +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank())
  return(p)
}