data {
  real y[3];
  real x[3];
  real sigma_e;
  real new_x;
}

parameters {
  vector[2] alpha;
}

model {
  real mu;
  for (i in 1:3) {
    mu = alpha[1] + alpha[2] * x[i];
    y[i] ~ normal(mu, sigma_e);
  }
}

generated quantities {
  real y_pred;
  y_pred = normal_rng(alpha[1] + alpha[2] * new_x, sigma_e);
}
