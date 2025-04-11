data {
  int<lower=0> n;
  int<lower=0,upper=1> y[n];
  int<lower=0> k;
  matrix[n, k] X;
}
parameters {
  vector[k] beta;
  real beta0;
  real<lower=0,upper=1> pi_mix;  
  real mu1; 
  real<lower=0> sigma1; 
  real mu2;
  real<lower=0> sigma2; 
}
model {
  // Priors (ejemplo)
  beta ~ normal(0, 5);
  beta0 ~ normal(0, 5);
  // pi_mix ~ beta(1,1) implícito
  mu1 ~ normal(0, 5);
  sigma1 ~ cauchy(0, 2);
  mu2 ~ normal(0, 5);
  sigma2 ~ cauchy(0, 2);

  for(i in 1:n){
    real z = beta0 + X[i] * beta;
    real p1 = Phi((z - mu1)/sigma1);
    real p2 = Phi((z - mu2)/sigma2);
    real p  = pi_mix * p1 + (1 - pi_mix) * p2;
    y[i] ~ bernoulli(p);
  }
}
generated quantities {
  vector[n] prob;
  for(i in 1:n){
    real z = beta0 + X[i] * beta;
    real p1 = Phi((z - mu1)/sigma1);
    real p2 = Phi((z - mu2)/sigma2);
    prob[i] = pi_mix * p1 + (1 - pi_mix) * p2;
  }
}

