library(dplyr)
library(cmdstanr)
source("../script/script_data.R")

data_filter <- data_final %>%
  filter(year < 1796)

model <- cmdstan_model("../stanmodel/sir_default_sep_rho.stan")

year <- data_filter$year-min(data_filter$year)+1

group <- ceiling(year/20)
group[group==7] <- 6

standata <- list(
  N=nrow(data_filter),
  week=data_filter$week,
  year=year,
  Nyear=length(unique(data_filter$year)),
  cases=data_filter$smpx,
  missing=data_filter$missing,
  brate=data_filter$birth/data_filter$population,
  drate=data_filter$death/data_filter$population,
  pop=data_filter$population,
  gamma=-log(1-1/3),
  Ngroup=6,
  group=group
)

nfit <- 100
ncores <- 8

t0 <- Sys.time()
run_one <- function(i) {
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

stanfit_sir_default_sep_rho_optim <- reslist %>%
  bind_rows

plot(stanfit_sir_default_sep_rho_optim$value)

oo_final <- model$optimize(data=standata,
                           seed=stanfit_sir_default_sep_rho_optim$seed[which.max(stanfit_sir_default_sep_rho_optim$value)],
                           jacobian = TRUE,
                           iter=1e4,
                           show_messages = FALSE) 

stanfit_sir_default_sep_rho_laplace <- 
  model$laplace(data = standata, mode = oo_final)

t1 <- Sys.time()
t1 - t0

summary_sir_default_sep_rho_laplace <- 
  stanfit_sir_default_sep_rho_laplace$summary()

save("stanfit_sir_default_sep_rho_optim", 
     "stanfit_sir_default_sep_rho_laplace",
     "summary_sir_default_sep_rho_laplace",
     file="stanfit_sir_default_sep_rho.rda")

dd <- oo_final$draws()

plot(data_filter$smpx)
lines(dd[grepl("C\\[", colnames(dd))], col=2)

plot(exp(dd[grepl("^epsilon\\[", colnames(dd))]), type="l")    

dd[grepl("sigma", colnames(dd))]
dd[grepl("sigma_epsilon", colnames(dd))]

plot((dd[grepl("logbeta\\[", colnames(dd))]), type="l")    

plot((dd[grepl("rhovec\\[", colnames(dd))]), type="l")    

plot(dd[grepl("beta_eff\\[", colnames(dd))], type="l")    
plot(dd[grepl("S\\[", colnames(dd))]/data_filter$population, type="l")    

dd[grepl("phi", colnames(dd))]
dd[grepl("rho", colnames(dd))]

plot(exp(summary_sir_default_sep_rho_laplace$median[grepl("^epsilon\\[", summary_sir_default_sep_rho_laplace$variable)]))

lines(exp(summary_sir_default_sep_rho_laplace$q5[grepl("^epsilon\\[", summary_sir_default_sep_rho_laplace$variable)]))

lines(exp(summary_sir_default_sep_rho_laplace$q95[grepl("^epsilon\\[", summary_sir_default_sep_rho_laplace$variable)]))
