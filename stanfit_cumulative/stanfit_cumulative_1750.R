library(dplyr)
library(cmdstanr)
source("../script/script_data.R")

data_filter <- data_final %>%
  filter(year < 1750)

model <- cmdstan_model("../stanmodel/sir_perturb_demog_dd-v2.stan")

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

stanfit_cumulative_1750_optim <- reslist %>%
  bind_rows

ww <- which(rank(-stanfit_cumulative_1750_optim$value)==min(rank(-stanfit_cumulative_1750_optim$value)))

oo_final <- model$optimize(data=standata,
                           seed=stanfit_cumulative_1750_optim$seed[ww[2]],
                           jacobian = TRUE,
                           show_messages = FALSE) 

stanfit_cumulative_1750_laplace <- 
  model$laplace(data = standata, mode = oo_final)

t1 <- Sys.time()
t1 - t0 ## ~8 minutes for 100 optimization

summary_cumulative_1750_laplace <- 
  stanfit_cumulative_1750_laplace$summary()

save("stanfit_cumulative_1750_optim", 
     "stanfit_cumulative_1750_laplace",
     "summary_cumulative_1750_laplace",
     file="stanfit_cumulative_1750.rda")

dd <- oo_final$draws()

plot(data_filter$smpx)
lines(dd[grepl("C\\[", colnames(dd))], col=2)

plot(exp(dd[grepl("^epsilon\\[", colnames(dd))]), type="l")    

dd[grepl("sigma_epsilon", colnames(dd))]

plot((dd[grepl("beta\\[", colnames(dd))])*3, type="l")    

plot(dd[grepl("beta_eff\\[", colnames(dd))], type="l")    
plot(dd[grepl("S\\[", colnames(dd))]/data_filter$population, type="l")    

dd[grepl("phi", colnames(dd))]
dd[grepl("rho", colnames(dd))]
