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

  // Verosimilitud cauchit
  for(i in 1:n){
    real z = beta0 + X[i] * beta;
    real p = (1.0 / pi()) * atan(z) + 0.5;
    y[i] ~ bernoulli(p);
  }
}
generated quantities {
  vector[n] prob;
  for(i in 1:n){
    real z = beta0 + X[i] * beta;
    prob[i] = (1.0 / pi()) * atan(z) + 0.5;
  }
}

