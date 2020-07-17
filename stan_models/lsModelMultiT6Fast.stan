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
  matrix[NParties, YTOTAL] govMatrix;
  matrix[YTOTAL,NElections] ElectionMatrix;
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
  // real<lower=0.5, upper = 0.95> theta;
  real<lower=0, upper = 0.8> theta2;
  real<lower=0, upper = 2> alpha;
  vector<lower=-5, upper = 5>[NPollsters * NParties] housebias;
  vector<lower=-5, upper = 5>[NElections * NPollsters * NParties] housebiasEl;
  real<lower=0, upper = 0.15> tau;
  real<lower=0, upper = 0.15> tau2;
  real<lower=0, upper = 0.2> muPollster;
  real<lower=0, upper = 0.1> sdPollster;
  vector<lower=0, upper = 5>[NParties] sigma_sdParty;
  real<lower=0, upper = 0.05> sdshift;
  real<lower=0, upper = 0.175> sdpbias;
  real<lower=0, upper = 0.1> mushift;
  real<lower=0, upper = 0.3> mupbias;
  real<lower=-0.05, upper = 0.05> opposition;
  real<lower=-0.05, upper = 0.05> government;
  matrix[NParties, YTOTAL] epsilon;
  matrix[NParties, NElections] pollError;
  cholesky_factor_corr[NParties] Eps_corr;
  cholesky_factor_corr[NParties] EpsPoll_corr;
  vector[NParties] sigma_shift_raw;
  vector[NParties] sigma_pollbias_raw;
  vector[NPollsters] sigma_sdPollster_raw;
}
transformed parameters{
  vector<lower=0>[NParties] sigma_shift;
  vector<lower=0>[NParties] sigma_pollbias;
  vector<lower=0>[NPollsters] sigma_sdPollster;
  vector[YTOTAL] y[NParties];
  vector[YTOTAL * NParties] w;
  matrix[NParties, YTOTAL] eps;
  matrix[NParties, NElections] pollError2;
  vector[NTOTAL] mu;
  vector<lower=0>[NTOTAL] sigma;
  vector[NPollsters * NParties] sigma_sd;
  real eta;
  real nu;
  
  sigma_shift = mushift + sdshift * sigma_shift_raw;
  sigma_pollbias = mupbias + sdpbias * sigma_pollbias_raw;
  sigma_sdPollster = muPollster + sdPollster * sigma_sdPollster_raw;

  
  eps = sqrt(1-square(theta2)) / sqrt(1+square(phi)) * 
  ((diag_pre_multiply(sigma_shift, Eps_corr) * epsilon) + 
  opposition + government * govMatrix);
  pollError2 = (diag_pre_multiply(sigma_pollbias, EpsPoll_corr) * pollError);
  for(i in 1:NParties){
    y[i,1] = y_start[i];
    eta = 0;
    nu = 0;

    sigma_sd[((i-1) * NPollsters + 1) : (i * NPollsters)] = sigma_sdParty[i] * sigma_sdPollster;

    for(n in 2:YTOTAL){
      y[i,n]         = y[i,n-1]  + eps[i,n] + nu - eta + phi * eps[i, n-1];
      eta = eta * 0.75 + alpha * theta2 * nu * (1 - 0.75) / sqrt(1-square(0.75));
      nu = (eps[i,n] + nu + phi * eps[i, n-1]) * theta2;
    }
    w[((i-1) * YTOTAL + 1) : (i * YTOTAL)] = y[i] + ElectionMatrix * to_vector(pollError2[i,]);
  }
    mu = w[matchedDates] + 
    csr_matrix_times_vector(NTOTAL, NPollsters * NParties, Aw1, Av1, Au1, housebias * tau) + 
    csr_matrix_times_vector(NTOTAL, NPollsters * NParties * NElections, Aw2, Av2, Au2, housebiasEl * tau2);
    sigma = csr_matrix_times_vector(NTOTAL, NPollsters * NParties, Aw1, Av1, Au1, sigma_sd) + 0.002;
}
model {
  sigma_shift_raw ~ std_normal();
  sigma_pollbias_raw ~ std_normal();
  sigma_sdPollster_raw ~ std_normal();
  housebias ~ std_normal();
  housebiasEl ~ std_normal();
  to_vector(pollError) ~ std_normal();
  to_vector(epsilon) ~ student_t(4, 0, 1);
  y_start ~ normal(-3, 3);
  government ~ normal(0, 0.005);
  opposition ~ normal(0, 0.005);
  alpha ~ normal(0, 0.5);
  // theta ~ normal(0.75, 0.2);
  phi ~ normal(1, 0.5);
  theta2 ~ normal(0.5, 0.25);
  tau ~ normal(0, 0.05);
  tau2 ~ normal(0, 0.05);
  mushift ~ normal(0.015, 0.0075);
  mupbias ~ normal(0.1, 0.075);
  muPollster ~ normal(0.1, 0.05);
  sdshift ~ normal(0, 0.001);
  sdpbias ~ normal(0, 0.05);
  sdPollster ~ normal(0, 0.02);
  Eps_corr ~ lkj_corr_cholesky(3);
  EpsPoll_corr ~ lkj_corr_cholesky(3);

  sigma_sdParty ~ normal(1, 0.5);
  pollData ~ normal(mu, sigma);
  //   for(n in 1:NTOTAL){
  //   target += normal_lpdf(pollData[n] | mu[n] , sigma[n]) * weight[n];
  // }
}  
