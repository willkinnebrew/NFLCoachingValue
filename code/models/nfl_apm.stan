//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=1> N;               // number of plays
  int<lower=1> P;               // number of players
  matrix[N, P] X;               // design matrix
  vector[N] y;                  // EPA
}

parameters {
  real alpha;                   // baseline EPA
  vector[P] beta;               // player APM
  real<lower=0> sigma;          // noise
  real<lower=0> tau;            // shrinkage
}

model {
  // Priors
  alpha ~ normal(0, 0.5);
  tau   ~ normal(0, 0.5);
  beta  ~ normal(0, tau);
  sigma ~ normal(0, 1);

  // Likelihood
  y ~ normal(alpha + X * beta, sigma);
}
