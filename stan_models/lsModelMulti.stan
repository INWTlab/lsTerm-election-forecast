data {
  int<lower=0> NTOTAL; // number of poll results (one for each poll and party)
  int<lower=0> NElections; // number of elections (including upcoming)
  int<lower=0> YTOTAL; // number of weeks modeled
  int<lower=0> NPollsters; // number of pollsters
  int<lower=0> NParties; //  number of parties
  int matchedDates[NTOTAL]; // matches polls with week number
  vector[NTOTAL] pollData; // poll results
  matrix[NTOTAL, NPollsters * NParties] IMatrix; // dummy matrix party-pollster interaction for poll assignment
  matrix[NTOTAL, NElections * NPollsters * NParties] IMatrixEl; // dummy matrix party-pollster-election interaction for poll assignment
  vector[YTOTAL] govMatrix[NParties]; // dummy matrix for each party and week combination gov = 1, opposition = 0; for week assignment 
  matrix[YTOTAL, NElections - 1] ElectionMatrix; // dummy matrix election for week assignment 
  vector[YTOTAL] ElectionVector; //dummy variable upcoming election (0/1) for week assignment
  vector[NParties] Zero[YTOTAL]; //matrix of zeros for vote share shift by week expectation
  vector[NParties] Zero2[NElections - 1]; //matrix of zeros for poll error by week expectation
}
transformed data {
  real theta0 = 0.7; // short term decay
  real theta1 = 0.95; // mid term memory decay
  real nuT = 3.5; // degrees of freedom for vote share shift parameter
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
  vector[NParties] y_start; // true vote share in first week
  real<lower=0, upper = 1> theta2; // weekly event spread AR1 parameter
  real<lower=0, upper = 1> alpha; //short term memory loss
  real<lower=0, upper = 0.8> alpha2; //short term memory loss
  vector<lower=-0.5, upper = 0.5>[NPollsters * NParties] housebias; //long term house bias party-pollster-combination
  vector<lower=-0.5, upper = 0.5>[NElections * NPollsters * NParties] housebiasEl; //short term (election) house bias party-pollster-combination
  real<lower=0, upper = 0.15> tau; //house bias sd
  real<lower=0, upper = 0.15> tau2; //house bias election sd
  real<lower=0, upper = 0.2> muPollster; // expectation of pollster sd
  real<lower=0, upper = 0.1> sdPollster; // sd of pollster sd
  vector<lower=0, upper = 5>[NParties] sigma_sdParty; // factor applied to pollster sd by party
  real<lower=0, upper = 0.15> mushift; // expectation of weekly shift sd
  real<lower=0, upper = 0.075> sdshift; // sd of weekly shift sd
  real<lower=0, upper = 0.3> mupbias; // expectation of common pollbias sd
  real<lower=0, upper = 0.175> sdpbias; // sd of common pollbias sd
  real<lower=-0.05, upper = 0.05> opposition; //opposition effect on weekly shift
  real<lower=-0.05, upper = 0.05> government; //government effect on weekly shift
  vector[NParties] epsilon[YTOTAL]; //weekly shift
  vector[NParties] pollError[NElections - 1]; // common pollbias for party-election combination
  cholesky_factor_corr[NParties] Eps_corr; // correlation matrix of weekly shift
  cholesky_factor_corr[NParties] EpsPoll_corr; // correlation matrix of common poll bias
  vector[NPollsters] sigma_sdPollster_raw; // untransformed pollster sd
  vector[NParties] sigma_shift_raw; // untransformed weekly shift sd
  vector[NParties] sigma_pollbias_raw; // untransformed common pollbias sd
  vector<lower=0>[YTOTAL] u; // helper parameter for multivariate t
}
transformed parameters{
  vector[YTOTAL] y[NParties]; // true vote share: MAIN PARAMETER OF INTEREST
  vector[YTOTAL * NParties] w; // true vote share + common poll bias
  vector[YTOTAL] eps[NParties]; // weekly shift before memory effects
  vector[NTOTAL] mu; // poll expectation
  vector<lower=0>[NTOTAL] sigma; // poll sd
  vector[NPollsters * NParties] sigma_sd; // poll sd by party and pollster
  vector<lower=0>[NPollsters] sigma_sdPollster; // transformed common pollster sd
  vector<lower=0>[NParties] sigma_shift; // untransformed weekly shift sd
  vector<lower=0>[NParties] sigma_pollbias; // transformed common pollbias sd
  real eta; // short term memory loss current week
  real eta2; // mid term memory loss current week
  real nu; // spread effect current week
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
      eta = eta * theta0 + alpha * nu * (1 - theta0) / sqrt(1-square(theta0)) * theta2; // short term memory loss (after fraction line: regularization)
      eta2 = eta2 * theta1 + alpha2 * nu * (1 - theta1) / sqrt(1-square(theta1)) * theta2;  // mid term memory loss (after fraction line: regularization)
      nu = (eps[i,n] + nu) * theta2;
    }
    w[((i-1) * YTOTAL + 1) : (i * YTOTAL)] = y[i] + ElectionMatrix * to_vector(pollError[,i]);
  }
    mu = w[matchedDates] + 
    csr_matrix_times_vector(NTOTAL, NPollsters * NParties, Aw1, Av1, Au1, housebias) + 
    csr_matrix_times_vector(NTOTAL, NPollsters * NParties * NElections, Aw2, Av2, Au2, housebiasEl);
    sigma = csr_matrix_times_vector(NTOTAL, NPollsters * NParties, Aw1, Av1, Au1, sigma_sd) + 0.002;
}
model {
  sigma_shift_raw ~ std_normal();
  sigma_pollbias_raw ~ std_normal();
  sigma_sdPollster_raw ~ std_normal();
  y_start ~ normal(-1, 3);
  government ~ normal(0, 0.005);
  opposition ~ normal(0, 0.005);
  alpha ~ normal(0, 0.5);
  alpha2 ~ normal(0, 0.25);
  theta2 ~ normal(0.5, 0.25);
  tau ~ normal(0, 0.05);
  tau2 ~ normal(0, 0.05);
  mushift ~ normal(0.02, 0.01);
  mupbias ~ normal(0.1, 0.075);
  sdshift ~ normal(0, 0.01);
  sdpbias ~ normal(0, 0.03);
  Eps_corr ~ lkj_corr_cholesky(3);
  EpsPoll_corr ~ lkj_corr_cholesky(3);
  muPollster ~ normal(0.1, 0.05);
  sdPollster ~ normal(0.05, 0.025);
  u ~ scaled_inv_chi_square(nuT, 1);
  epsilon ~ multi_normal_cholesky(Zero, diag_pre_multiply(sigma_shift, Eps_corr));
  pollError ~ multi_normal_cholesky(Zero2, diag_pre_multiply(sigma_pollbias, EpsPoll_corr));
  sigma_sdParty ~ normal(1, 0.5);
  housebias ~ normal(0, tau);
  housebiasEl ~ normal(0, tau2);
  pollData ~ normal(mu, sigma);
} 
generated quantities {
  vector[YTOTAL] yFinal[NParties];
  vector[NParties] futurePollError;
  yFinal = y;
  futurePollError = multi_normal_cholesky_rng(rep_vector(0, NParties), diag_pre_multiply(sigma_pollbias, EpsPoll_corr));
  for(i in 1:NParties){
    yFinal[i] = yFinal[i] - to_vector(ElectionVector * futurePollError[i]);
  }
}

