library(dplyr)
library(cmdstanr)
load("../simulate/simulate_vari.rda")

model <- cmdstan_model("../stanmodel/sir_perturb_demog_dd_poisson.stan")

standata <- list(
  N=nrow(simulate_vari[[1]]),
  week=simulate_vari[[1]]$week,
  year=simulate_vari[[1]]$year-min(simulate_vari[[1]]$year)+1,
  Nyear=length(unique(simulate_vari[[1]]$year)),
  cases=c(0, simulate_vari[[1]]$cases[-1]),
  missing=c(1, rep(0, nrow(simulate_vari[[1]])-1)),
  brate=simulate_vari[[1]]$brate,
  drate=simulate_vari[[1]]$drate,
  pop=simulate_vari[[1]]$pop,
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
                       iter=1e4)
  
  lp <- try(oo$lp())
  
  if (!inherits(lp, "try-error")) {
    reslist[[i]] <- data.frame(
      seed=i,
      value=oo$lp()
    )
  }
}

stanfit_sir_perturb_demog_dd_cmdstanr_optim_sv <- reslist %>%
  bind_rows

# -25878.99
# -24980.31

plot(stanfit_sir_perturb_demog_dd_cmdstanr_optim_sv$value)
max(stanfit_sir_perturb_demog_dd_cmdstanr_optim_sv$value)

oo_final <- model$optimize(data=standata,
                           seed=stanfit_sir_perturb_demog_dd_cmdstanr_optim_sv$seed[which.max(stanfit_sir_perturb_demog_dd_cmdstanr_optim_sv$value)],
                           jacobian = TRUE,
                           show_messages = FALSE,
                           iter=1e4) 

oo_final$lp()

stanfit_sir_perturb_demog_dd_cmdstanr_laplace_sv <- 
  model$laplace(data = standata, mode = oo_final)

summary_sir_perturb_demog_dd_cmdstanr_laplace_sv <- 
  stanfit_sir_perturb_demog_dd_cmdstanr_laplace_sv$summary()

save("stanfit_sir_perturb_demog_dd_cmdstanr_optim_sv", 
     "stanfit_sir_perturb_demog_dd_cmdstanr_laplace_sv",
     "summary_sir_perturb_demog_dd_cmdstanr_laplace_sv",
     file="stanfit_sir_perturb_demog_dd_cmdstanr_sv.rda")

dd <- oo_final$draws()

plot(simulate_vari[[1]]$cases)
lines(dd[grepl("C\\[", colnames(dd))], col=2)

plot(simulate_vari$logbeta)
lines(dd[grepl("logbeta\\[", colnames(dd))])

plot(simulate_vari$epsilon)
lines(dd[grepl("^epsilon\\[", colnames(dd))])

plot(simulate_vari[[1]]$S)
lines(dd[grepl("S\\[", colnames(dd))], col=2)

simulate_vari$omega
dd[grepl("^omega", colnames(dd))]

simulate_vari$phi
dd[grepl("^phi", colnames(dd))]

simulate_vari$rho
dd[grepl("^rho", colnames(dd))]
