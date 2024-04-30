data {
  int<lower=0> N; // number of data rows
  int<lower=0> K1; // number of predictors other
  int<lower=0> K2; // number of predictors parties combination
  int<lower=0> K3; // number of predictors party
  int<lower=0> L; // number of groups
  array[L] int s; // group sizes
  matrix[N, K1 + K2 + K3 + 1] X; // predictor matrix
  array[L] int y; // outcome vector

  int<lower=0> N_new; // number of rows for prediction 
  int<lower=0> L_new; // number of groups for prediction
  array[L_new] int s_new; // group sizes for prediction
  matrix[N_new, K1 + K2 + K3 + 1] X_new; // data for prediciton
}
parameters {
  vector[K1] beta; // beta
  vector[K2] beta_parties; // beta
  vector[K3] beta_party; // beta
  real<lower=0>  tau;
  real<lower=0>  nu;
  real beta_afd;
}
model {
  int pos;
  pos = 1;
  for (l in 1:L) {
    y[l] ~ categorical(softmax(X[pos:(pos+s[l]-1)] * append_row(append_row(append_row(beta, beta_parties), beta_party), beta_afd)));
    pos = pos + s[l];
  }
  target += normal_lpdf(to_vector(beta) | 0, 5); // prior
  target += normal_lpdf(to_vector(beta_parties) | 0, tau); // prior
  target += normal_lpdf(to_vector(beta_party) | 0, nu); // prior
  target += student_t_lpdf(beta_afd | 3, 0, 20); // prior
  target += normal_lpdf(tau | 0, 3); // prior
  target += normal_lpdf(nu | 0, 3); // prior
}
generated quantities {
  vector[N_new] y_new;
  // y_new = softmax(X_new * append_row(append_row(append_row(beta, beta_parties), beta_party), beta_afd)); // predict
  int pos;
  pos = 1;
  for (l in 1:L_new) {
    y_new[pos:(pos+s_new[l]-1)] = softmax(X_new[pos:(pos+s_new[l]-1)] * append_row(append_row(append_row(beta, beta_parties), beta_party), beta_afd));
    pos = pos + s_new[l];
  }
}
