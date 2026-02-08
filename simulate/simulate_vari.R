library(dplyr)
library(cmdstanr)
source("../script/script_data.R")
source("../R/simulate.R")

load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_cmdstanr.rda")

data_filter <- data_final %>%
  filter(year < 1796)

ss <- simulate_sir_vari(week=data_filter$week,
                   year=data_filter$year-1669,
                   brate=data_filter$birth/data_filter$population,
                   drate=data_filter$death/data_filter$population,
                   pop=data_filter$population,
                   gamma=-log(1-1/3),
                   S0=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("S0", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
                   I0=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("I0", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
                   rho=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("rho", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
                   logbeta=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
                   epsilon=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
                   omega=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("omega", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
                   phi=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("phi", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
                   delta=0.002,
                   tmax=nrow(data_filter))

simulate_vari <- list(
  data.frame(
    week=data_filter$week,
    year=data_filter$year-1669,
    brate=data_filter$birth/data_filter$population,
    drate=data_filter$death/data_filter$population,
    pop=data_filter$population,
    S=ss$S,
    I=ss$I,
    cases=ss$cases
  ),
  S0=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("S0", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  I0=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("I0", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  rho=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("rho", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  logbeta=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  epsilon=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  omega=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("omega", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  phi=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("phi", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)]
)

save("simulate_vari", file="simulate_vari.rda")
