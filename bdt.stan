// model fitting Bayesian Decision Theory to the Perceptual Gambling task
functions {
  real choiceprob(real sigma, real rho, real s, real better_side, real rew_multi, int X, vector x, vector ccsd_L, vector ccsd_H) {
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
    d_H = log(lambda_H^rho / lambda_L^rho) - log(p_H_given_x / p_L_given_x);
    // Convert d_H into a probability
    y = 1 / (1 + exp(d_H));
    return y;
  }
}
data {
  int<lower=0> T; // Number of trials we have
  int<lower=0> X; // Length of CCSD vectors
  real<lower=0> rew_multi; // rew_multi of the flashing side
  vector[X] x; // just x
  vector[X] ccsd_L; // Class-conditioned stimulus distribution for low category
  vector[X] ccsd_H; // Class-conditioned stimulus distribution for high category
  vector[T] s; // Stimulus frequency in log2 space
  vector[T] better_side; // 0 = low, 1 = high, 9 = none
  // vector[T] prev_outcome; // -1 = lose, 1 = win, 0 = surebet
  int chose_high[T]; // binary
}
parameters {
  real rho_raw; // risk preference
  real<lower=0> sigma; // perceptual noise
  // simplex[3] omega; // weighing vector of the rational, lottery and sure-bet agent
  real omega_1_base; // rational agent, before logit
  real omega_2_base; // weight of low agent in low vs. high, before logit
}
transformed parameters{
  real<lower=0> rho;
  real<lower=0> omega_1;
  real<lower=0> omega_2;
  
  rho = exp(rho_raw);
  omega_1 = inv_logit(omega_1_base);
  omega_2 = inv_logit(omega_2_base);
}
model {
  real p_rational; 
  
  rho_raw ~ normal(log(1), 0.1);
  sigma ~ lognormal(log(0.3), 0.1);
  omega_1_base ~ normal(0.5, 0.3);
  omega_2_base ~ normal(0, 2);
  // omega ~ dirichlet(omega_alphas);
  
  for(t in 1:T){
    p_rational = choiceprob(sigma, rho, s[t], better_side[t], rew_multi, X, x, ccsd_L, ccsd_H);
    // print("s is ", s[t], " better_side is ", better_side[t], "p_chose_lott is ", p_chose_lott);
    chose_high[t] ~ bernoulli(omega_1*p_rational + (1 - omega_1)*omega_2);
  }
}
