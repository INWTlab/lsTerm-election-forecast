data {
  int<lower=0> NTOTAL;
  int<lower=0> NElections;
  int<lower=0> YTOTAL;
  int<lower=0> NPollsters;
  int<lower=0> NParties;
  int matchedDates[NTOTAL];
  vector[NTOTAL] weight;
  vector[NTOTAL] pollData;
  matrix[NTOTAL, NPollsters * NParties] IMatrix;
  matrix[NTOTAL, NElections * NPollsters * NParties] IMatrixEl;
  vector[YTOTAL] govMatrix[NParties];
  matrix[YTOTAL,NElections] ElectionMatrix;
  vector[NParties] Zero[YTOTAL];
  vector[NParties] Zero2[NElections];
}
transformed data {
  vector[rows(csr_extract_w(IMatrix))] Aw1;
  int Av1[rows(Aw1)];
  int Au1[size(csr_extract_u(IMatrix))];
  vector[rows(csr_extract_w(IMatrixEl))] Aw2;
  int Av2[rows(Aw2)];
  int Au2[size(csr_extract_u(IMatrixEl))];

  Aw1 = csr_extract_w(IMatrix);
  Av1 = csr_extract_v(IMatrix);
  Au1 = csr_extract_u(IMatrix);
  Aw2 = csr_extract_w(IMatrixEl);
  Av2 = csr_extract_v(IMatrixEl);
  Au2 = csr_extract_u(IMatrixEl);

}
parameters {
  vector[NParties] y_start;
  real<lower=0, upper = 1> phi;
  real<lower=0, upper = 0.95> theta;
  real<lower=0, upper = 1> theta2;
  real<lower=0, upper = 2> alpha;
  vector<lower=-0.5, upper = 0.5>[NPollsters * NParties] housebias;
  vector<lower=-0.5, upper = 0.5>[NElections * NPollsters * NParties] housebiasEl;
  real<lower=0, upper = 0.15> tau;
  real<lower=0, upper = 0.15> tau2;
  real<lower=0, upper = 0.2> muPollster;
  real<lower=0, upper = 0.1> sdPollster;
  vector<lower=0, upper = 5>[NParties] sigma_sdParty;
  vector<lower=0, upper = 0.2>[NPollsters] sigma_sdPollster;
  real<lower=0, upper = 0.15> sigma_sdsd;
  real<lower=0, upper = 0.1> sdshift;
  real<lower=0, upper = 0.15> sdpbias;
  real<lower=0, upper = 0.2> mushift;
  real<lower=0, upper = 0.3> mupbias;
  real<lower=-0.05, upper = 0.05> opposition;
  real<lower=-0.05, upper = 0.05> government;
  vector[NParties] epsilon[YTOTAL];
  vector[NParties] pollError[NElections];
  cholesky_factor_corr[NParties] Eps_corr;
  cholesky_factor_corr[NParties] EpsPoll_corr;
  vector<lower=0, upper = 0.1>[NParties] sigma_shift;
  vector<lower=0, upper = 0.5>[NParties]  sigma_pollbias;
}
transformed parameters{
  vector[YTOTAL] y[NParties];
  vector[YTOTAL * NParties] w;
  vector[YTOTAL] eps[NParties];
  vector[NTOTAL] mu;
  vector<lower=0>[NTOTAL] sigma;
  vector[NPollsters * NParties] sigma_sd;
  matrix[NParties, NParties] sigmaEps;
  real eta;
  real nu;
  sigmaEps = crossprod(diag_pre_multiply(sigma_shift, Eps_corr));

  for(i in 1:NParties){
    y[i,1]         =  y_start[i];
    eta = 0;
    nu = 0;
    eps[i] = (to_vector(epsilon[,i]) + opposition + govMatrix[i] * government);
    
    sigma_sd[((i-1) * NPollsters + 1) : (i * NPollsters)] = sigma_sdParty[i] * sigma_sdPollster;

    for(n in 2:YTOTAL){
      y[i,n]         = y[i,n-1]  + eps[i,n] + nu - eta + phi * eps[i, n-1];
      eta = eta * theta + (alpha * nu) * (1 - theta);
      nu = (nu + eps[i,n] + phi * eps[i, n-1]) * theta2;
    }
    w[((i-1) * YTOTAL + 1) : (i * YTOTAL)] = y[i] + ElectionMatrix * to_vector(pollError[,i]);
  }
    mu = w[matchedDates] + 
    csr_matrix_times_vector(NTOTAL, NPollsters * NParties, Aw1, Av1, Au1, housebias) + 
    csr_matrix_times_vector(NTOTAL, NPollsters * NParties * NElections, Aw2, Av2, Au2, housebiasEl);
    sigma = csr_matrix_times_vector(NTOTAL, NPollsters * NParties, Aw1, Av1, Au1, sigma_sd) + 0.002;
}
model {
  sigma_shift ~ normal(mushift, sdshift);
  sigma_pollbias ~ normal(mupbias, sdpbias);
  y_start ~ normal(-1, 3);
  government ~ normal(0, 0.01);
  opposition ~ normal(0, 0.01);
  alpha ~ normal(0.75, 0.25);
  theta ~ normal(0.75, 0.25);
  phi ~ normal(0.5, 0.25);
  theta2 ~ normal(0.75, 0.25);
  tau ~ normal(0, 0.05);
  tau2 ~ normal(0, 0.05);
  mushift ~ normal(0.005, 0.0025);
  mupbias ~ normal(0.1, 0.075);
  sdshift ~ normal(0, 0.005);
  sdpbias ~ normal(0, 0.03);
  Eps_corr ~ lkj_corr_cholesky(1);
  EpsPoll_corr ~ lkj_corr_cholesky(1);
  muPollster ~ normal(0.1, 0.05);
  sdPollster ~ normal(0.05, 0.025);
  epsilon ~ multi_student_t(4, Zero, sigmaEps);
  pollError ~ multi_normal_cholesky(Zero2, diag_pre_multiply(sigma_pollbias, EpsPoll_corr));
  sigma_sdPollster ~ normal(muPollster, sdPollster);
  sigma_sdParty ~ normal(1, 0.5);
  housebias ~ normal(0, tau);
  housebiasEl ~ normal(0, tau2);
  pollData ~ normal(mu, sigma);
  //   for(n in 1:NTOTAL){
  //   target += normal_lpdf(pollData[n] | mu[n] , sigma[n]) * weight[n];
  // }
}  
