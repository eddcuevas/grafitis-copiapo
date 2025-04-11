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
  beta ~ normal(0, 5);
  beta0 ~ normal(0, 5);

  for(i in 1:n){
    real z = beta0 + X[i] * beta;
    real p = exp(-exp(-z)); // cdf Gumbel
    y[i] ~ bernoulli(p);
  }
}
generated quantities {
  vector[n] prob;
  for(i in 1:n){
    real z = beta0 + X[i] * beta;
    prob[i] = exp(-exp(-z));
  }
}

