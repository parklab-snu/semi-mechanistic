library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(cmdstanr)
source("../script/script_data.R")
source("../R/colors.R")

load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_cmdstanr.rda")
load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_cmdstanr_rho.rda")

data_filter <- data_final %>%
  filter(year < 1796)

epsilondata <- data.frame(
  year=unique(data_filter$year),
  est=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  lwr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q5[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  upr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q95[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  est2=summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$median[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$variable)],
  lwr2=summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$q5[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$variable)],
  upr2=summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$q95[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$variable)]
)

g1 <- ggplot(epsilondata) +
  geom_ribbon(aes(year, ymin=exp(lwr), ymax=exp(upr)), fill=colors_default[3], alpha=0.4) +
  geom_line(aes(year, exp(est)), col=colors_default[3]) +
  geom_ribbon(aes(year, ymin=exp(lwr2), ymax=exp(upr2)), fill=colors_new[1], alpha=0.4) +
  geom_line(aes(year, exp(est2)), col=colors_new[1]) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Multiplicative changes\nin transmission") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

rhodata <- data.frame(
  year=data_filter$year,
  week=data_filter$week,
  est=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("rho", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  lwr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q5[grepl("rho", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  upr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q95[grepl("rho", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  est2=summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$median[grepl("^rhovec\\[", summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$variable)],
  lwr2=summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$q5[grepl("^rhovec\\[", summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$variable)],
  upr2=summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$q95[grepl("^rhovec\\[", summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$variable)]
)

g2 <- ggplot(rhodata) +
  geom_ribbon(aes(year+week/52, ymin=(lwr), ymax=(upr)), fill=colors_default[3], alpha=0.4) +
  geom_line(aes(year+week/52, (est)), col=colors_default[3]) +
  geom_ribbon(aes(year+week/52, ymin=(lwr2), ymax=(upr2)), fill=colors_new[1], alpha=0.4) +
  geom_line(aes(year+week/52, (est2)), col=colors_new[1]) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Reporting rate") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

Sdata <- data.frame(
  year=data_filter$year,
  week=data_filter$week,
  est=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("^S\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  lwr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q5[grepl("^S\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  upr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q95[grepl("^S\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  est2=summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$median[grepl("^S\\[", summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$variable)],
  lwr2=summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$q5[grepl("^S\\[", summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$variable)],
  upr2=summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$q95[grepl("^S\\[", summary_sir_perturb_demog_dd_cmdstanr_rho_laplace$variable)]
)

g3 <- ggplot(Sdata) +
  geom_ribbon(aes(year+week/52, ymin=(lwr), ymax=(upr)), fill=colors_default[3], alpha=0.4) +
  geom_line(aes(year+week/52, (est)), col=colors_default[3]) +
  geom_ribbon(aes(year+week/52, ymin=(lwr2), ymax=(upr2)), fill=colors_new[1], alpha=0.4) +
  geom_line(aes(year+week/52, (est2)), col=colors_new[1]) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Susceptible population") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

gcomb <- ggarrange(g2, g1, g3, ncol=1,
                   labels=c("A", "B", "C"))

ggsave("figure_stanfit_rho.pdf", gcomb, width=6, height=6)
