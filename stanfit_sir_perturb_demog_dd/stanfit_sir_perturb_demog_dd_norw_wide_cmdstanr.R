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
ncores <- 8

t0 <- Sys.time()
run_one <- function(i) {
  print(i)
  oo <- try(model$optimize(data=standata,
                           seed=i,
                           jacobian = TRUE,
                           refresh=0,
                           show_messages = FALSE,
                           show_exceptions = FALSE,
                           iter=1e4), silent=TRUE)
  if (inherits(oo, "try-error")) {
    warning(paste("Optimization failed for seed", i))
    return(NULL)
  }

  lp <- try(oo$lp(), silent=TRUE)
  if (inherits(lp, "try-error") || length(lp)!=1 || !is.finite(lp)) {
    warning(paste("No finite log density for seed", i))
    return(NULL)
  }

  data.frame(seed=i, value=lp)
}

reslist <- parallel::mclapply(1:nfit, run_one, mc.cores=ncores, mc.preschedule=FALSE)
reslist <- Filter(Negate(is.null), reslist)
if (!length(reslist)) stop("All optimization attempts failed")

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
t1 - t0

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

plot((dd_norw_wide[grepl("beta\\[", colnames(dd_norw_wide))])*3, type="l")    

plot(dd_norw_wide[grepl("beta_eff\\[", colnames(dd_norw_wide))], type="l")    
plot(dd_norw_wide[grepl("S\\[", colnames(dd_norw_wide))]/data_filter$population, type="l")    

dd_norw_wide[grepl("phi", colnames(dd_norw_wide))]
dd_norw_wide[grepl("rho", colnames(dd_norw_wide))]
