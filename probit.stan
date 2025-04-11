data {
  int<lower=0> n;
  int<lower=0,upper=1> y[n];
  int<lower=0> k;
  matrix[n, k] X;
}
parameters {
  vector[k] beta; 
  real beta0;
}
model {
  // Priors
  beta ~ normal(0, 5);
  beta0 ~ normal(0, 5);

  // Verosimilitud con link probit (CDF normal estándar)
  for(i in 1:n){
    real z = beta0 + X[i] * beta;
    y[i] ~ bernoulli(Phi(z));
  }
}
generated quantities {
  vector[n] prob;
  for(i in 1:n){
    real z = beta0 + X[i] * beta;
    prob[i] = Phi(z);
  }
}

