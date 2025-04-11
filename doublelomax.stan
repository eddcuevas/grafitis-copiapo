functions {
  real double_lomax_cdf(real z, real lambda) {
    if(z > 0) {
      return pow(1.0 - 0.5 / (1.0 + z), lambda);
    } else {
      return pow(0.5 / (1.0 - z), lambda);
    }
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

  for(i in 1:n) {
    real z = beta0 + X[i] * beta;
    real p = double_lomax_cdf(z, lambda);
    y[i] ~ bernoulli(p);
  }
}
generated quantities {
  vector[n] prob;
  for(i in 1:n) {
    real z = beta0 + X[i] * beta;
    prob[i] = double_lomax_cdf(z, lambda);
  }
}



