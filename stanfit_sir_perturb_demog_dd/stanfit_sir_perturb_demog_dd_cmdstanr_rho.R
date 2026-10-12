library(dplyr)
library(cmdstanr)
source("../script/script_data.R")

data_filter <- data_final %>%
  filter(year < 1796)

model <- cmdstan_model("../stanmodel/sir_perturb_demog_dd_rho.stan")

standata <- list(
  N=nrow(data_filter),
  week=data_filter$week,
  year=data_filter$year-min(data_filter$year)+1,
  Nyear=length(unique(data_filter$year)),
  cases=data_filter$smpx,
  missing=data_filter$missing,
  brate=data_filter$birth/data_filter$population,
  drate=data_filter$death/data_filter$population,
  pop=data_filter$population,
  gamma=-log(1-1/3)
)

nfit <- 100

t0 <- Sys.time()
reslist <- vector('list', nfit)
for (i in 1:nfit) {
  print(i)
  
  oo <- model$optimize(data=standata,
                       seed=i,
                       jacobian = TRUE,
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

stanfit_sir_perturb_demog_dd_cmdstanr_rho_optim <- reslist %>%
  bind_rows

plot(stanfit_sir_perturb_demog_dd_cmdstanr_rho_optim$value)

oo_final <- model$optimize(data=standata,
                           seed=stanfit_sir_perturb_demog_dd_cmdstanr_rho_optim$seed[which.max(stanfit_sir_perturb_demog_dd_cmdstanr_rho_optim$value)],
                           jacobian = TRUE,
                           show_messages = FALSE) 

stanfit_sir_perturb_demog_dd_cmdstanr_rho_laplace <- 
  model$laplace(data = standata, mode = oo_final)

t1 <- Sys.time()
t1 - t0 ## ~8 minutes for 100 optimization

summary_sir_perturb_demog_dd_cmdstanr_rho_laplace <- 
  stanfit_sir_perturb_demog_dd_cmdstanr_rho_laplace$summary()

save("stanfit_sir_perturb_demog_dd_cmdstanr_rho_optim", 
     "stanfit_sir_perturb_demog_dd_cmdstanr_rho_laplace",
     "summary_sir_perturb_demog_dd_cmdstanr_rho_laplace",
     file="stanfit_sir_perturb_demog_dd_cmdstanr_rho.rda")

dd <- oo_final$draws()

plot(data_filter$smpx)
lines(dd[grepl("C\\[", colnames(dd))], col=2)

plot(exp(dd[grepl("^epsilon\\[", colnames(dd))]), type="l")    

dd[grepl("sigma_epsilon", colnames(dd))]

plot(exp(dd[grepl("beta\\[", colnames(dd))])*3, type="l")    

plot(dd[grepl("R0\\[", colnames(dd))], type="l")    
plot(dd[grepl("S\\[", colnames(dd))]/data_filter$population, type="l")    

dd[grepl("phi", colnames(dd))]

dd[grepl("rho0", colnames(dd))]

dd[grepl("rhos", colnames(dd))]
