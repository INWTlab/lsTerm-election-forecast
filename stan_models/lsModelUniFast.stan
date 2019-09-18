data {
  int<lower=0> NTOTAL;
  int<lower=0> YTOTAL;
  int<lower=0> NPollsters;
  int<lower=0> NParties;
  int<lower=0> NElections;
  int matchedDates[NTOTAL];
  vector[NTOTAL] pollData[NParties];
  matrix[NTOTAL,NPollsters] IMatrix;
  matrix[YTOTAL,NElections] ElectionMatrix;
  vector[YTOTAL] govMatrix[NParties];
  vector[NTOTAL] Missing[NParties];
}

parameters {
  vector[NParties] y_start;
  vector[NParties] pe_start;
  real<lower=0.75, upper = 0.99> theta;
  real<lower=0, upper = theta> theta2;
  real<lower=0.1, upper = 1> alpha;
  vector[NPollsters] housebias[NParties];
  vector<lower=0>[NPollsters] sigma_sd[NParties];
  vector[NElections] pollError[NParties];
  real<lower=0> tau;
  real<lower=0> tau2;
  real<lower=0, upper = 0.05> tau3;
  real<lower=0> tau4;
  real<lower=-0.003, upper = 0.003> opposition;
  real<lower=-0.003, upper = 0.003> government;
  vector[YTOTAL] epsilon[NParties];
  vector<lower=0, upper = 0.1>[NParties] sigma_shift;
  real<lower=0.002>  sigma_pollbias;
  real<lower=0> WT;
  real<lower=0> WT2;
}
transformed parameters{
  vector[YTOTAL] y[NParties];
  vector[YTOTAL] eps[NParties];
  vector[YTOTAL] w[NParties];
  
  real eta;
  real nu;
  
  for(i in 1:NParties){
    y[i,1]         =  y_start[i];

    eta = 0;
    nu = 0;
    eps[i]  = epsilon[i] * sqrt(WT) * sigma_shift[i] + opposition + govMatrix[i] * government;
    
    for(n in 2:YTOTAL){
      y[i,n]         = y[i,n-1]  + eps[i,n] + nu - eta;
      nu = (nu + eps[i,n]) * theta2;
      eta = eta * theta + (alpha * (nu + eps[i,n])) * (1 - theta);
    }
    w[i] = y[i] + ElectionMatrix * pollError[i] * sqrt(WT2) * sigma_pollbias;
  }
}
model {
  sigma_shift ~ normal(0, tau3);
  sigma_pollbias ~ normal(0.025, 0.0125);
  pe_start ~ normal(0, tau4);
  y_start ~ normal(0, 2);
  government ~ normal(0, 0.0005);
  opposition ~ normal(0, 0.0005);
  tau2 ~ normal(0, 0.05);
  theta ~ beta(10, 3);
  theta2 ~ beta(3, 3);
  alpha ~ beta(5, 5);
  tau ~ normal(0, 0.05);
  tau3 ~ normal(0, 0.03);
  tau4 ~ normal(0, 0.05);
  WT ~ scaled_inv_chi_square(5,1);
  WT2 ~ scaled_inv_chi_square(5,1);
  
  for(i in 1:NParties){
    housebias[i] ~ normal(0, tau);
    epsilon[i] ~ normal(0, 1);
    pollError[i] ~ normal(0, 1);
    sigma_sd[i] ~ normal(0.05, tau2);
    pollData[i] ~ normal(w[i,matchedDates] + IMatrix * housebias[i], IMatrix * sigma_sd[i] + 1E-4 + Missing[i] * 1E4);
  }
} 
