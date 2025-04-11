functions {
  real laplace_cdf(real z, real mu, real b) {
    if (z < mu)
      return 0.5 * exp((z - mu)/b);
    else
      return 1.0 - 0.5 * exp(-(z - mu)/b);
  }
}
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
  real<lower=0> b1;
  real mu2; 
  real<lower=0> b2;
}
model {
  // Priors (ejemplo)
  beta ~ normal(0, 5);
  beta0 ~ normal(0, 5);
  mu1 ~ normal(0, 5);
  b1 ~ cauchy(0,2);
  mu2 ~ normal(0, 5);
  b2 ~ cauchy(0,2);

  for(i in 1:n){
    real z = beta0 + X[i]*beta;
    real p1 = laplace_cdf(z, mu1, b1);
    real p2 = laplace_cdf(z, mu2, b2);
    real p  = pi_mix * p1 + (1 - pi_mix) * p2;
    y[i] ~ bernoulli(p);
  }
}
generated quantities {
  vector[n] prob;
  for(i in 1:n){
    real z = beta0 + X[i]*beta;
    real p1 = laplace_cdf(z, mu1, b1);
    real p2 = laplace_cdf(z, mu2, b2);
    prob[i] = pi_mix * p1 + (1 - pi_mix) * p2;
  }
}


