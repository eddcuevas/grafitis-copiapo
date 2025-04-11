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

  // Verosimilitud usando link logit
  for(i in 1:n) {
    y[i] ~ bernoulli_logit(beta0 + X[i] * beta);
  }
}
generated quantities {
  vector[n] prob;
  for(i in 1:n){
    prob[i] = inv_logit(beta0 + X[i] * beta);
  }
}
