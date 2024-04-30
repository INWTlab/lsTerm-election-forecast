data {
  int<lower=0> NTOTAL;
  int<lower=0> NElections;
  int<lower=0> YTOTAL;
  int<lower=0> NPollsters;
  int<lower=0> NParties;
  array[NTOTAL] int matchedDates;
  vector[NTOTAL] weight;
  vector[NTOTAL] pollData;
  vector[NTOTAL] roundError;
  matrix[NTOTAL, NPollsters * NParties] IMatrix;
  matrix[NTOTAL, NElections * NPollsters * NParties] IMatrixEl;
  array[NParties] vector[YTOTAL] govMatrix;
  matrix[YTOTAL,NElections - 1] ElectionMatrix;
  vector[YTOTAL] ElectionVector;
  array[YTOTAL] vector[NParties] Zero;
  array[NElections - 1] vector[NParties] Zero2;
}
transformed data {
  real theta0 = 0.7;
  real theta1 = 0.95;
  real nuT = 3.5;
  vector[rows(csr_extract_w(IMatrix))] Aw1;
  array[rows(Aw1)] int Av1;
  array[size(csr_extract_u(IMatrix))] int Au1;
  vector[rows(csr_extract_w(IMatrixEl))] Aw2;
  array[rows(Aw2)] int Av2;
  array[size(csr_extract_u(IMatrixEl))] int Au2;
  
  profile("transformed data"){
  Aw1 = csr_extract_w(IMatrix);
  Av1 = csr_extract_v(IMatrix);
  Au1 = csr_extract_u(IMatrix);
  Aw2 = csr_extract_w(IMatrixEl);
  Av2 = csr_extract_v(IMatrixEl);
  Au2 = csr_extract_u(IMatrixEl);
  }
}
parameters {
  vector[NParties] y_start;
  real<lower=0, upper = 1> theta2;
  real<lower=0, upper = 1> alpha;
  real<lower=0, upper = 0.8> alpha2;
  vector<lower=-0.5, upper = 0.5>[NPollsters * NParties] housebias;
  vector<lower=-0.5, upper = 0.5>[NElections * NPollsters * NParties] housebiasEl;
  real<lower=0, upper = 0.15> tau;
  real<lower=0, upper = 0.15> tau2;
  real<lower=0, upper = 0.2> muPollster;
  real<lower=0, upper = 0.1> sdPollster;
  vector<lower=0, upper = 5>[NParties] sigma_sdParty;
  real<lower=0, upper = 0.075> sdshift;
  real<lower=0, upper = 0.175> sdpbias;
  real<lower=0, upper = 0.15> mushift;
  real<lower=0, upper = 0.3> mupbias;
  real<lower=-0.05, upper = 0.05> opposition;
  real<lower=-0.05, upper = 0.05> government;
  array[YTOTAL] vector[NParties] epsilon;
  array[NElections-1] vector[NParties] pollError;
  cholesky_factor_corr[NParties] Eps_corr;
  cholesky_factor_corr[NParties] EpsPoll_corr;
  vector[NPollsters] sigma_sdPollster_raw;
  vector[NParties] sigma_shift_raw;
  vector[NParties] sigma_pollbias_raw;
  vector<lower=0>[YTOTAL] u;
}
transformed parameters{
  array[NParties] vector[YTOTAL] y;
  vector[YTOTAL * NParties] w;
  array[NParties] vector[YTOTAL] eps;
  vector[NTOTAL] mu;
  vector<lower=0>[NTOTAL] sigma;
  vector[NPollsters * NParties] sigma_sd;
  vector<lower=0>[NPollsters] sigma_sdPollster;
  vector<lower=0>[NParties] sigma_shift;
  vector<lower=0>[NParties] sigma_pollbias;
  real eta;
  real eta2;
  real nu;
  profile("transformed parameters") {
  sigma_shift = sigma_shift_raw * sdshift + mushift;
  sigma_pollbias = sigma_pollbias_raw * sdpbias + mupbias;
  sigma_sdPollster = sigma_sdPollster_raw * sdPollster + muPollster;
  for(i in 1:NParties){
    y[i,1]         =  y_start[i];
    eta = 0;
    eta2 = 0;
    nu = 0;
    eps[i] = ((to_vector(epsilon[,i]) .* sqrt(u)) + opposition + govMatrix[i] * government) * sqrt(1-square(theta2));
    sigma_sd[((i-1) * NPollsters + 1) : (i * NPollsters)] = (sigma_sdParty[i] / mean(sigma_sdParty)) * sigma_sdPollster;

    for(n in 2:YTOTAL){
      y[i,n]         = y[i,n-1]  + eps[i,n] + nu - eta - eta2;
      eta = eta * theta0 + alpha * theta2 * nu * (1 - theta0) / sqrt(1-square(theta0));
      eta2 = eta2 * theta1 + alpha2 * theta2 * nu * (1 - theta1) / sqrt(1-square(theta1));
      nu = (eps[i,n] + nu) * theta2;
    }
    w[((i-1) * YTOTAL + 1) : (i * YTOTAL)] = y[i] + ElectionMatrix * to_vector(pollError[,i]);
  }
    mu = w[matchedDates] + 
    csr_matrix_times_vector(NTOTAL, NPollsters * NParties, Aw1, Av1, Au1, housebias) + 
    csr_matrix_times_vector(NTOTAL, NPollsters * NParties * NElections, Aw2, Av2, Au2, housebiasEl);
    sigma = sqrt(square(csr_matrix_times_vector(NTOTAL, NPollsters * NParties, Aw1, Av1, Au1, sigma_sd)) + square(roundError));
  }
}
model {
  profile("univariate normals") {
  target += std_normal_lpdf(sigma_shift_raw);
  target += std_normal_lpdf(sigma_pollbias_raw);
  target += std_normal_lpdf(sigma_sdPollster_raw);
  target += normal_lpdf(y_start | -1, 3);
  target += normal_lpdf(government | 0, 0.005);
  target += normal_lpdf(opposition | 0, 0.005);
  target += normal_lpdf(alpha | 0, 0.5);
  target += normal_lpdf(alpha2 | 0, 0.25);
  target += normal_lpdf(theta2 | 0.5, 0.25);
  target += normal_lpdf(tau | 0, 0.05);
  target += normal_lpdf(tau2 | 0, 0.05);
  target += normal_lpdf(mushift | 0.02, 0.01);
  target += normal_lpdf(mupbias | 0.1, 0.075);
  target += normal_lpdf(sdshift | 0, 0.01);
  target += normal_lpdf(sdpbias | 0, 0.03);
  target += normal_lpdf(sigma_sdParty | 1, 0.5);
  target += normal_lpdf(housebias | 0, tau);
  target += normal_lpdf(housebiasEl | 0, tau2);
  target += normal_lpdf(muPollster | 0.1, 0.05);
  target += normal_lpdf(sdPollster | 0.05, 0.025);
  }
  profile("other") {
  u ~ scaled_inv_chi_square(nuT, 1);
  Eps_corr ~ lkj_corr_cholesky(3);
  EpsPoll_corr ~ lkj_corr_cholesky(3);
  }
  profile("multivariate normals") {
  epsilon ~ multi_normal_cholesky(Zero, diag_pre_multiply(sigma_shift, Eps_corr));
  pollError ~ multi_normal_cholesky(Zero2, diag_pre_multiply(sigma_pollbias, EpsPoll_corr));
  }
  profile("likelihood"){
  for(n in 1:NTOTAL){
    target += normal_lpdf(pollData[n] | mu[n] , sigma[n]) * weight[n];
  }
  }
}
generated quantities {
  array[NParties] vector[YTOTAL] yFinal;
  vector[NParties] futurePollError;
  yFinal = y;
  profile("generated quantities") {
  futurePollError = multi_normal_cholesky_rng(rep_vector(0, NParties), diag_pre_multiply(sigma_pollbias, EpsPoll_corr));
  for(i in 1:NParties){
    yFinal[i] = yFinal[i] - to_vector(ElectionVector * futurePollError[i]);
  }
  }
}

