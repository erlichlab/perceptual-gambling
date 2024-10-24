// model fitting a four-parameter sigmoid to the Perceptual Gambling task
// x0 = the inflection point, indicates the value of s when y = 0.5
// b = the slope of the sigmoid, 0 = flat, higher more step function
// 1 - w1 = total lapse rate, 0 = no lapse, 1 = all lapse
// w2 = the fraction of lapses are low

data {
  int<lower=0> T; // Number of trials we have
  vector[T] s; // Stimulus frequency in log2 space
  int chose_high[T]; // binary
}
parameters {
  real<lower=0, upper=1> w1; // lower lapse
  real<lower=0, upper=1> w2; // upper lapse
  real x0; // horizontal shift
  real b; // 
}
model {
  w1 ~ normal(0, 0.1);
  w2 ~ normal(0, 0.5);
  x0 ~ normal(0, 0.5);
  b ~ normal(0, 0.5);
  
  for(t in 1:T){
    chose_high[t] ~ bernoulli(w1 / (1 + exp(-b*(s[t] - x0))) + (1 - w1) * w2);
  }
}
