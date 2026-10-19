library(dplyr)
library(cmdstanr)
source("../script/script_data.R")

data_filter <- data_final %>%
  filter(year < 1796)

model <- cmdstan_model("../stanmodel/sir_perturb_demog_dd_norw_wide.stan")

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

stanfit_sir_perturb_demog_dd_norw_wide_cmdstanr_optim <- reslist %>%
  bind_rows

plot(stanfit_sir_perturb_demog_dd_norw_wide_cmdstanr_optim$value)

oo_final <- model$optimize(data=standata,
                           seed=stanfit_sir_perturb_demog_dd_norw_wide_cmdstanr_optim$seed[which.max(stanfit_sir_perturb_demog_dd_norw_wide_cmdstanr_optim$value)],
                           jacobian = TRUE,
                           show_messages = FALSE) 

stanfit_sir_perturb_demog_dd_norw_wide_cmdstanr_laplace <- 
  model$laplace(data = standata, mode = oo_final)

t1 <- Sys.time()
t1 - t0 ## ~8 minutes for 100 optimization

summary_sir_perturb_demog_dd_norw_wide_cmdstanr_laplace <- 
  stanfit_sir_perturb_demog_dd_norw_wide_cmdstanr_laplace$summary()

save("stanfit_sir_perturb_demog_dd_norw_wide_cmdstanr_optim", 
     "stanfit_sir_perturb_demog_dd_norw_wide_cmdstanr_laplace",
     "summary_sir_perturb_demog_dd_norw_wide_cmdstanr_laplace",
     file="stanfit_sir_perturb_demog_dd_norw_wide_cmdstanr.rda")

dd_norw_wide <- oo_final$draws()

plot(data_filter$smpx)
lines(dd_norw_wide[grepl("C\\[", colnames(dd_norw_wide))], col=2)

plot(exp(dd_norw_wide[grepl("^epsilon\\[", colnames(dd_norw_wide))]), type="l")    

dd_norw_wide[grepl("sigma_epsilon", colnames(dd_norw_wide))]

plot(exp(dd_norw_wide[grepl("beta\\[", colnames(dd_norw_wide))])*3, type="l")    

plot(dd_norw_wide[grepl("beta_eff\\[", colnames(dd_norw_wide))], type="l")    
plot(dd_norw_wide[grepl("S\\[", colnames(dd_norw_wide))]/data_filter$population, type="l")    

dd_norw_wide[grepl("phi", colnames(dd_norw_wide))]
dd_norw_wide[grepl("rho", colnames(dd_norw_wide))]
