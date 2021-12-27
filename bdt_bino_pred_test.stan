// model fitting Bayesian Decision Theory to the Perceptual Gambling task
// binomial version with binned stimulus
functions {
  real choiceprob(real sigma, real rho_low, real rho_high, real s, real better_side, real rew_multi, int X, vector x, vector ccsd_L, vector ccsd_H) {
    vector[X] p_x_given_s; // p(x|s), subject's observation
    real p_L_given_x; // p(C = L | x), posterior of C = L
    real p_H_given_x; // p(C = R | x), posterior of C = R
    real lambda_L; // action cost of choosing low when high is correct
    real lambda_H; // action cost of choosing high when low is correct
    real d_H; // posterior-weighted decision variable 
    real y; // probability of choosing high 

    for (i in 1:X){
      p_x_given_s[i] = normal_lpdf(x[i] | s, sigma);
    }
    p_x_given_s = exp(p_x_given_s);
    p_L_given_x = sum(p_x_given_s .* ccsd_L);
    p_H_given_x = sum(p_x_given_s .* ccsd_H);
    
    // Find the action cost
    if (better_side == 0){ // Low side is better
      lambda_L = 1;
      lambda_H = rew_multi;
    } else if (better_side == 1){ // High side is better
      lambda_L = rew_multi;
      lambda_H = 1;
    } else if (better_side == 9){ // No flash here
      lambda_L = 1;
      lambda_H = 1;
    }
    
    // Get the decision variable with action cost integrated 
    d_H = log(lambda_H^rho_high / lambda_L^rho_low) - log(p_H_given_x / p_L_given_x);
    // Convert d_H into a probability
    y = 1 / (1 + exp(d_H));
    return y;
  }
}
data {
  int<lower=0> T; // Number of trial types we have
  int<lower=0> X; // Length of CCSD vectors
  vector[X] x; // just x
  vector[X] ccsd_L; // Class-conditioned stimulus distribution for low category
  vector[X] ccsd_H; // Class-conditioned stimulus distribution for high category
  vector[T] better_side; // 0 = low, 1 = high, 9 = none
  vector[T] rew_multi; // reward multiplier of the flashing side
  vector[T] s; // Stimulus frequency in log2 space
  int n_chose_high[T]; // number chose high frequency for this trial type
  int n_trials[T]; // total number of trials for this trial type
  // below is for prediction
  int<lower=0> P; // Number of trial types for prediction
  vector[P] pred_better_side; // 0 = low, 1 = high, 9 = none
  vector[P] pred_rew_multi; // reward multiplier of the flashing side
  vector[P] pred_s; // Stimulus frequency in log2 space
  int pred_n_trials[P]; // total number of trials for this trial type
}
parameters {
  real rho_low_raw; // risk preference for low
  real rho_high_raw; // risk preference for high
  real<lower=0> sigma; // perceptual noise
  simplex[3] omega; // baseline fractions of being rational, high and low agent
  simplex[3] omega_low; // when better side is low
  simplex[3] omega_high; // when better side is high
  // real omega_1_base; // rational agent, before logit
  // real omega_2_base; // weight of low agent in low vs. high, before logit
}
transformed parameters{
  real<lower=0> rho_low;
  real<lower=0> rho_high;
  // real<lower=0> omega_1;
  // real<lower=0> omega_2;
  
  rho_low = exp(rho_low_raw);
  rho_high = exp(rho_high_raw);
  // omega_1 = inv_logit(omega_1_base);
  // omega_2 = inv_logit(omega_2_base);
}
model {
  real p_rational; 
  
  rho_low_raw ~ normal(log(1), 0.3);
  rho_high_raw ~ normal(log(1), 0.3);
  sigma ~ lognormal(log(0.3), 0.1);
  omega ~ dirichlet([6, 2, 2]');
  omega_low ~ dirichlet([6, 2, 2]');
  omega_high ~ dirichlet([6, 2, 2]');
  // omega_1_base ~ normal(0.5, 0.3);
  // omega_2_base ~ normal(0, 2);
  
  for(t in 1:T){
    p_rational = choiceprob(sigma, rho_low, rho_high, s[t], better_side[t], rew_multi[t], X, x, ccsd_L, ccsd_H);
    // print("s is ", s[t], " better_side is ", better_side[t], " p_chose_high is ", p_rational, 
    // " rho is", rho, " sigma is ", sigma);
    if (better_side[t] == 0){ // low
      n_chose_high[t] ~ binomial(n_trials[t], omega_low[1]*p_rational + omega_low[2]*1 + omega_low[3]*0);
    } else if (better_side[t] == 1){ // high
      n_chose_high[t] ~ binomial(n_trials[t], omega_high[1]*p_rational + omega_high[2]*1 + omega_high[3]*0);
    } else{
      n_chose_high[t] ~ binomial(n_trials[t], omega[1]*p_rational + omega[2]*1 + omega[3]*0);
    }
    // n_chose_high[t] ~ binomial(n_trials[t], omega_1*p_rational + (1 - omega_1)*omega_2);
  }
}
generated quantities{
  real p_rational; 
  vector[P] pred_n_chose_high;
  
  for(p in 1:P){
    p_rational = choiceprob(sigma, rho_low, rho_high, pred_s[p], pred_better_side[p], pred_rew_multi[p], X, x, ccsd_L, ccsd_H);
    if (pred_better_side[p] == 0){ // low
      pred_n_chose_high[p] = binomial_rng(pred_n_trials[p], omega_low[1]*p_rational + omega_low[2]*1 + omega_low[3]*0);
    } else if (pred_better_side[p] == 1){ // high
      pred_n_chose_high[p] = binomial_rng(pred_n_trials[p], omega_high[1]*p_rational + omega_high[2]*1 + omega_high[3]*0);
    } else{
      pred_n_chose_high[p] = binomial_rng(pred_n_trials[p], omega[1]*p_rational + omega[2]*1 + omega[3]*0);
    }
  }
}
