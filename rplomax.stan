functions {
  real reverse_power_lomax_cdf(real z, real lambda) {
    return 1.0 - (1.0 / pow(1.0 + fabs(z), lambda));
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
  real<lower=0> lambda;
}
model {
  beta ~ normal(0, 5);
  beta0 ~ normal(0, 5);
  lambda ~ cauchy(0, 2);

  for(i in 1:n){
    real z = beta0 + X[i] * beta;
    real p = reverse_power_lomax_cdf(z, lambda);
    y[i] ~ bernoulli(p);
  }
}
generated quantities {
  vector[n] prob;
  for(i in 1:n){
    real z = beta0 + X[i] * beta;
    prob[i] = reverse_power_lomax_cdf(z, lambda);
  }
}



