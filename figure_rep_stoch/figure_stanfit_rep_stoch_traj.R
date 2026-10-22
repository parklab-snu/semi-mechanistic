library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(cmdstanr)
library(gridExtra)
load("../simulate/simulate_rep_nochange_stoch.rda")
load("../stanfit_sir_default_rep_nc_stoch/stanfit_sir_default_rep_nc_stoch1.rda")
load("../stanfit_sir_default_rep_nc_stoch/stanfit_sir_default_rep_nc_stoch2.rda")
load("../stanfit_sir_default_rep_nc_stoch/stanfit_sir_default_rep_nc_stoch3.rda")
load("../stanfit_sir_default_rep_nc_stoch/stanfit_sir_default_rep_nc_stoch4.rda")
load("../stanfit_sir_default_rep_nc_stoch/stanfit_sir_default_rep_nc_stoch5.rda")
load("../stanfit_sir_default_rep_nc_stoch/stanfit_sir_default_rep_nc_stoch6.rda")
load("../stanfit_sir_default_rep_nc_stoch/stanfit_sir_default_rep_nc_stoch7.rda")
load("../stanfit_sir_default_rep_nc_stoch/stanfit_sir_default_rep_nc_stoch8.rda")
load("../stanfit_sir_default_rep_nc_stoch/stanfit_sir_default_rep_nc_stoch9.rda")
load("../stanfit_sir_default_rep_nc_stoch/stanfit_sir_default_rep_nc_stoch10.rda")

fitdata <- lapply(1:10, function(x) {
  dd <- get(paste0("summary_sir_default_laplace_rep_nc_stoch", x))
  
  ss <- simulate_rep_nochange_stoch[[x]]
  
  data.frame(
    week=ss[[1]]$week,
    year=ss[[1]]$year,
    cases=ss[[1]]$cases,
    pred=dd$median[grepl("C\\[", dd$variable)],
    lwr=dd$q5[grepl("C\\[", dd$variable)],
    upr=dd$q95[grepl("C\\[", dd$variable)],
    fit=x
  )
}) %>%
  bind_rows %>%
  mutate(
    fit=paste0("Replicate ", fit)
  ) %>%
  mutate(
    fit=factor(fit, levels=paste0("Replicate ", 1:10))
  )

g1 <- ggplot(fitdata) +
  geom_point(aes(year+week/52+1669, cases), shape=".") +
  geom_ribbon(aes(year+week/52+1669, ymin=lwr, ymax=upr), fill="#EF6351", alpha=0.4) +
  geom_line(aes(year+week/52+1669, pred), col="#EF6351") +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly deaths", expand=c(0, 0)) +
  facet_wrap(~fit, ncol=) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    strip.background = element_blank()
  )

ggsave("figure_stanfit_rep_stoch_traj.pdf", g1, width=8, height=6)
