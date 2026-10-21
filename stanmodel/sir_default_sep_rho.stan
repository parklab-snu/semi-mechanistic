data {
  int N;
  array[N] int week;
  array[N] int year;
  int Nyear;
  array[N] int missing;
  array[N] int cases;
  array[N] real brate;
  array[N] real drate;
  array[N] real pop;
  int Ngroup;
  array[N] int group;
  real gamma;
}

parameters {
  real<lower=0, upper=1> S0;
  real<lower=0, upper=1> I0;
  real<lower=0, upper=1> rho0;
  real rhos;
  array[52,Ngroup] real logbeta;
  array[Nyear-1] real z_epsilon;
  real<lower=0> sigma_epsilon;
  real<lower=0> phi;
  real<lower=0> omega;
}

transformed parameters {
  vector<lower=0>[N] S;
  vector<lower=0>[N] I;
  vector<lower=0>[N] C;
  vector<lower=0>[N] beta_eff;
  vector<lower=0>[N] rhovec;
  
  vector[Nyear] epsilon;
  epsilon[1] = 0;
  for (i in 2:Nyear) {
    epsilon[i] = sigma_epsilon * z_epsilon[i-1];
  }
  
  S[1] = S0 * pop[1];
  I[1] = I0 * pop[1];
  C[1] = 0;
  beta_eff[1] = exp(logbeta[week[1],group[1]]+epsilon[year[1]]);
  rhovec[1] = rho0;
  
  for (i in 2:N) {
    beta_eff[i] = exp(logbeta[week[i],group[i]]+epsilon[year[i]]);
    
    rhovec[i] = rho0 * exp(rhos * (i-1)/30000);
    
    real foi = beta_eff[i] * (I[i-1]+omega)/pop[1];
    real Sout = (1-exp(-(foi+drate[i]))) * S[i-1];
    real StoI = foi/(foi+drate[i]) * Sout;
    real Iout = (1-exp(-(gamma+drate[i]))) * I[i-1];
    real ItoR = gamma/(gamma+drate[i]) * Iout;
    
    S[i] = S[i-1] - Sout + brate[i] * pop[i];
    I[i] = I[i-1] + StoI - Iout;
    C[i] = ItoR * rhovec[i];
  }
}

model {
  for (i in 1:Ngroup) {
    for (j in 1:52) {
      logbeta[j,i] ~ normal(0.5, 0.2);
    }
  }
  
  
  z_epsilon ~ normal(0, 1);
  sigma_epsilon ~ normal(0, 0.5);
  
  S0 ~ beta(1, 1);
  I0 ~ normal(0, 0.001);
  rho0 ~ beta(1, 1);
  rhos ~ normal(0, 1);
  
  phi ~ normal(100, 20);
  omega ~ normal(0, 200);
  
  for (i in 2:N) {
    if (missing[i] == 0) {
      cases[i] ~ neg_binomial_2(C[i], phi);
    }
  }
}

generated quantities {
  vector[N] R0;
  for (i in 1:N) {
    R0[i] = exp(logbeta[week[i], group[i]]+epsilon[year[i]])/pop[1]*pop[i]/(1-exp(-gamma));
  }
}