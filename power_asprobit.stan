functions {
  // CDF Skew Normal real (ver #8). Aquí hacemos un placeholder.
  real skewnormal_cdf(real x, real alpha) {
    // Implementar correctamente o usar aproximación.
    // Placeholder simplificado:
    return Phi(x) - alpha * 0.01;
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
  real alpha;      // asimetría
  real<lower=0> lambda;  // potencia
}
model {
  beta ~ normal(0, 5);
  beta0 ~ normal(0, 5);
  alpha ~ normal(0,5);
  lambda ~ cauchy(0,2);

  for(i in 1:n){
    real z = beta0 + X[i]*beta;
    real base_cdf = skewnormal_cdf(z, alpha);
    real p = pow(base_cdf, lambda);
    y[i] ~ bernoulli(p);
  }
}
generated quantities {
  vector[n] prob;
  for(i in 1:n){
    real z = beta0 + X[i]*beta;
    real base_cdf = skewnormal_cdf(z, alpha);
    prob[i] = pow(base_cdf, lambda);
  }
}



