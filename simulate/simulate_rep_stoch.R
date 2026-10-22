library(dplyr)
library(cmdstanr)
source("../script/script_data.R")
source("../R/simulate.R")

load("../stanfit_main/stanfit_sir_brw_erw.rda")

data_filter <- data_final %>%
  filter(year < 1796)

nsim <- 10

simulate_rep_stoch <- vector('list', nsim)

for (i in 1:nsim) {
  print(i)
  ss <- simulate_sir_stoch(week=data_filter$week,
                     year=data_filter$year-1669,
                     brate=data_filter$birth/data_filter$population,
                     drate=data_filter$death/data_filter$population,
                     pop=data_filter$population,
                     gamma=-log(1-1/3),
                     S0=summary_sir_brw_erw_laplace$median[grepl("S0", summary_sir_brw_erw_laplace$variable)],
                     I0=summary_sir_brw_erw_laplace$median[grepl("I0", summary_sir_brw_erw_laplace$variable)],
                     rho=summary_sir_brw_erw_laplace$median[grepl("rho", summary_sir_brw_erw_laplace$variable)],
                     logbeta=summary_sir_brw_erw_laplace$median[grepl("logbeta\\[", summary_sir_brw_erw_laplace$variable)],
                     epsilon=summary_sir_brw_erw_laplace$median[grepl("^epsilon\\[", summary_sir_brw_erw_laplace$variable)],
                     omega=summary_sir_brw_erw_laplace$median[grepl("omega", summary_sir_brw_erw_laplace$variable)],
                     phi=summary_sir_brw_erw_laplace$median[grepl("phi", summary_sir_brw_erw_laplace$variable)],
                     tmax=nrow(data_filter),
                     seed=i)
  
  simulate_rep_stoch[[i]] <- list(
    data.frame(
      week=data_filter$week,
      year=data_filter$year-1669,
      brate=ss$birthvec/ss$popvec,
      drate=ss$deathvec/ss$popvec,
      pop=ss$popvec,
      S=ss$S,
      I=ss$I,
      cases=ss$cases
    ),
    S0=summary_sir_brw_erw_laplace$median[grepl("S0", summary_sir_brw_erw_laplace$variable)],
    I0=summary_sir_brw_erw_laplace$median[grepl("I0", summary_sir_brw_erw_laplace$variable)],
    rho=summary_sir_brw_erw_laplace$median[grepl("rho", summary_sir_brw_erw_laplace$variable)],
    logbeta=summary_sir_brw_erw_laplace$median[grepl("logbeta\\[", summary_sir_brw_erw_laplace$variable)],
    epsilon=summary_sir_brw_erw_laplace$median[grepl("^epsilon\\[", summary_sir_brw_erw_laplace$variable)],
    omega=summary_sir_brw_erw_laplace$median[grepl("omega", summary_sir_brw_erw_laplace$variable)],
    phi=summary_sir_brw_erw_laplace$median[grepl("phi", summary_sir_brw_erw_laplace$variable)]
  )
}

save("simulate_rep_stoch", file="simulate_rep_stoch.rda")
