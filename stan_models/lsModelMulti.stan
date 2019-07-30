data {
  int<lower=0> NTOTAL;
  int<lower=0> YTOTAL;
  int<lower=0> NBTW;
  int<lower=0> NPollsters;
  int<lower=0> NParties;
  int AllDates[NTOTAL];
  vector[NTOTAL] poll[NParties];
  matrix[NTOTAL,NPollsters] IMatrix;
  vector[YTOTAL] govMatrix[NParties];
  vector[NTOTAL] Missing[NParties];
  vector[YTOTAL] BTWIndikator3;
  vector[YTOTAL] BTWIndikator2;
}

parameters {
  vector[NParties] y_start;
  vector[NParties] pe_start;
  real<lower=0.70, upper = 0.99> theta;
  real<lower=0, upper = theta> theta2;
  real<lower=0, upper = 1> alpha;
  vector[NPollsters] housebias[NParties];
  vector<lower=0>[NPollsters] sigma_sd[NParties];
  real<lower=0> tau;
  real<lower=0> tau2;
  real<lower=0> tau3;
  real<lower=0> tau4;
  real<lower=-0.003, upper = 0.003> opposition;
  real<lower=-0.003, upper = 0.003> government;
  vector[NParties] epsilon[YTOTAL];
  vector[NParties] epsilonPolls[YTOTAL];
  cholesky_factor_corr[NParties] Eps_corr;
  cholesky_factor_corr[NParties] EpsPoll_corr;
  
  real<lower=0> WT;
  real<lower=0> WT2;

  vector<lower=0>[NParties] sigma_shift;
  real<lower=0.002>  sigma_pollbias;
}
transformed parameters{
  vector[YTOTAL] y[NParties];
  vector[YTOTAL] pollError[NParties];
  vector[YTOTAL] w[NParties];
  
  real substract;
  real adds;
  real eps;
  
  for(i in 1:NParties){
    y[i,1]         =  y_start[i];
    pollError[i,1] =  pe_start[i];
    
    substract = 0;
    adds = 0;
    
    for(n in 2:YTOTAL){
      eps = epsilon[n,i] * sqrt(WT)  * sigma_shift[i] + opposition + govMatrix[i,n] * government;
      y[i,n]         = y[i,n-1]  + eps + adds - substract;
      pollError[i,n] =  0.98 * pollError[i,n-1] * BTWIndikator2[n] + epsilonPolls[n,i] * sqrt(WT2) * sigma_pollbias;
      adds = (adds + eps) * theta2;
      substract = substract * theta + (alpha * (adds + eps)) * (1 - theta);
    }
    w[i] = y[i] + BTWIndikator3 .* pollError[i];
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
  Eps_corr ~ lkj_corr_cholesky(1.0);
  EpsPoll_corr ~ lkj_corr_cholesky(1.0);
  
  WT ~ scaled_inv_chi_square(5,1);
  WT2 ~ scaled_inv_chi_square(5,1);
  
  for(i in 1:YTOTAL){
    epsilon[i] ~ multi_normal_cholesky(rep_vector(0, NParties), Eps_corr);
    epsilonPolls[i] ~ multi_normal_cholesky(rep_vector(0, NParties), EpsPoll_corr);
  }
  
  for(i in 1:NParties){
    housebias[i] ~ normal(0, tau);
    sigma_sd[i] ~ normal(0.05, tau2);
    poll[i] ~ normal(w[i,AllDates] + IMatrix * (housebias[i] - mean(housebias[i])), IMatrix * sigma_sd[i] + 1E-4 + Missing[i] * 1E4);
  }
} 
