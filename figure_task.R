# functions for figure_task
make_figure_task = function(){
  p = figure_task_ccsd()
  scale_save(p, 'figure_task_c', 10, 6, 1)
}

figure_task_ccsd = function(){
  # example of how different sigma_s can affect frequency distribution
  x = seq(1.5, 4.5, by = 0.01)
  ccsd1 = dnorm(x, mean = 3, sd = 0.5)
  ccsd2 = dnorm(x, mean = 3, sd = 1)
  ccsd1[x < 2] = 0
  ccsd1[x > 4] = 0
  ccsd2[x < 2] = 0
  ccsd2[x > 4] = 0
  df = data.frame(x = x, ccsd1 = ccsd1, ccsd2 = ccsd2)
  p = ggplot(df) + theme_classic() +
    geom_line(aes(x = x, y = ccsd1), size = 1) +
    geom_line(aes(x = x, y = ccsd2), size = 1) + geom_vline(xintercept = 3, linetype = 'dashed') +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) + 
    xlab('log2(Frequency)') + ylab('Probability')
  return(p)
}