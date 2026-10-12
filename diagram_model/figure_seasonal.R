library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(cmdstanr)
source("../script/script_data.R")

load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_cmdstanr.rda")

data_filter <- data_final %>%
  filter(year < 1796)

betadata <- data.frame(
  week=1:52,
  est=exp(summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)]),
  lwr=exp(summary_sir_perturb_demog_dd_cmdstanr_laplace$q5[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)]),
  upr=exp(summary_sir_perturb_demog_dd_cmdstanr_laplace$q95[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)])
)

g1 <- ggplot(betadata) +
  geom_ribbon(aes(week, ymin=lwr, ymax=upr), fill="#EF6351", alpha=0.4) +
  geom_line(aes(week, est), col="#EF6351", lwd=1) +
  scale_x_continuous("Week of year", expand=c(0, 0),
                     breaks=c(1,  26, 52),
                     labels=c("1", "26", "52")) +
  scale_y_continuous("Transmission rate\n(1/week)") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

epsilondata <- data.frame(
  year=unique(data_filter$year),
  est=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  lwr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q5[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  upr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q95[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)]
)

g2 <- ggplot(epsilondata) +
  geom_ribbon(aes(year, ymin=exp(lwr), ymax=exp(upr)), fill="#EF6351", alpha=0.4) +
  geom_line(aes(year, exp(est)), col="#EF6351") +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Multiplicative\nchange") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

gcomb <- ggarrange(g1, g2, nrow=1)

ggsave("figure_seasonal.pdf", gcomb, width=5, height=1.2)
