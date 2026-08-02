library(dplyr)
library(cmdstanr)
load("../simulate/simulate_rep.rda")

simulate_base <- simulate_rep[[1]]

model <- cmdstan_model("../stanmodel/sir_perturb_demog_dd.stan")

standata <- list(
  N=nrow(simulate_base[[1]]),
  week=simulate_base[[1]]$week,
  year=simulate_base[[1]]$year-min(simulate_base[[1]]$year)+1,
  Nyear=length(unique(simulate_base[[1]]$year)),
  cases=c(0, simulate_base[[1]]$cases[-1]),
  missing=c(1, rep(0, nrow(simulate_base[[1]])-1)),
  brate=simulate_base[[1]]$brate,
  drate=simulate_base[[1]]$drate,
  pop=simulate_base[[1]]$pop,
  gamma=-log(1-1/3)
)

nfit <- 100

reslist <- vector('list', nfit)
for (i in 1:nfit) {
  print(i)
  
  oo <- model$optimize(data=standata,
                       seed=i,
                       show_messages = FALSE,
                       show_exceptions = FALSE,
                       jacobian = TRUE,
                       iter=1e4)
  
  lp <- try(oo$lp())
  
  if (!inherits(lp, "try-error")) {
    reslist[[i]] <- data.frame(
      seed=i,
      value=oo$lp()
    )
  }
}

stanfit_sir_perturb_demog_dd_cmdstanr_optim_rep1 <- reslist %>%
  bind_rows

plot(stanfit_sir_perturb_demog_dd_cmdstanr_optim_rep1$value)

oo_final <- model$optimize(data=standata,
                           seed=stanfit_sir_perturb_demog_dd_cmdstanr_optim_rep1$seed[which.max(stanfit_sir_perturb_demog_dd_cmdstanr_optim_rep1$value)],
                           jacobian = TRUE,
                           show_messages = FALSE,
                           iter=1e4) 

oo_final$lp()

stanfit_sir_perturb_demog_dd_cmdstanr_laplace_rep1 <- 
  model$laplace(data = standata, mode = oo_final)

summary_sir_perturb_demog_dd_cmdstanr_laplace_rep1 <- 
  stanfit_sir_perturb_demog_dd_cmdstanr_laplace_rep1$summary()

save("stanfit_sir_perturb_demog_dd_cmdstanr_optim_rep1", 
     "stanfit_sir_perturb_demog_dd_cmdstanr_laplace_rep1",
     "summary_sir_perturb_demog_dd_cmdstanr_laplace_rep1",
     file="stanfit_sir_perturb_demog_dd_cmdstanr_rep1.rda")
