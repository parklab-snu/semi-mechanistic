library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(ggsci)
library(cmdstanr)
source("../script/script_data.R")

load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_cmdstanr.rda")
load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_norw_cmdstanr.rda")

data_filter <- data_final %>%
  filter(year < 1796)

fitdata <- data.frame(
  year=data_filter$year,
  week=data_filter$week,
  true=data_filter$smpx,
  est=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("C\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  lwr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q5[grepl("C\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  upr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q95[grepl("C\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  est2=summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$median[grepl("C\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)],
  lwr2=summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$q5[grepl("C\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)],
  upr2=summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$q95[grepl("C\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)]
)

g1 <- ggplot(fitdata) +
  annotate("rect", xmin=1721, xmax=1728, ymin=-Inf, ymax=Inf, fill="#D9F2FF", alpha=0.5) +
  annotate("rect", xmin=1728, xmax=1740, ymin=-Inf, ymax=Inf, fill="#AED8EB", alpha=0.5) +
  annotate("rect", xmin=1740, xmax=1768, ymin=-Inf, ymax=Inf, fill="#8FC5DC", alpha=0.5) +
  annotate("rect", xmin=1768, xmax=1790, ymin=-Inf, ymax=Inf, fill="#6EB1CC", alpha=0.5) +
  annotate("rect", xmin=1790, xmax=1796, ymin=-Inf, ymax=Inf, fill="#3F94B6", alpha=0.5) +
  # geom_vline(xintercept=unique(data_filter$year), lty=3, col="gray70") +
  geom_point(data=data_filter, aes(year+week/52, smpx), size=0.3) +
  geom_ribbon(aes(year+week/52, ymin=lwr, ymax=upr, fill="Random walk priors"), alpha=0.4) +
  geom_line(aes(year+week/52, est, col="Random walk priors")) +
  geom_ribbon(aes(year+week/52, ymin=lwr2, ymax=upr2, fill="Independent priors"), alpha=0.4) +
  geom_line(aes(year+week/52, est2, col="Independent priors")) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly deaths", expand=c(0, 0), limits=c(0, 190)) +
  scale_color_manual("", values=ggsci::pal_aaas()(3)[2:3]) +
  scale_fill_manual("", values=ggsci::pal_aaas()(3)[2:3]) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

ggplot(fitdata) +
  geom_point(aes(true+1, est+1), size=0.3) +
  geom_smooth(aes(true+1, est+1), method="lm") +
  geom_abline(intercept=0, slope=1) +
  scale_x_log10("Observed") +
  scale_y_log10("Predicted") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

betadata <- data.frame(
  week=1:52,
  est=exp(summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)]),
  lwr=exp(summary_sir_perturb_demog_dd_cmdstanr_laplace$q5[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)]),
  upr=exp(summary_sir_perturb_demog_dd_cmdstanr_laplace$q95[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)]),
  est2=exp(summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$median[grepl("logbeta\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)]),
  lwr2=exp(summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$q5[grepl("logbeta\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)]),
  upr2=exp(summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$q95[grepl("logbeta\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)])
)

g2 <- ggplot(betadata) +
  geom_ribbon(aes(week, ymin=lwr, ymax=upr, fill="Random walk priors"), alpha=0.4) +
  geom_line(aes(week, est, col="Random walk priors")) +
  geom_ribbon(aes(week, ymin=lwr2, ymax=upr2, fill="Independent priors"), alpha=0.4) +
  geom_line(aes(week, est2, col="Independent priors")) +
  scale_x_continuous("Week of year", expand=c(0, 0),
                     breaks=c(1, 11, 21, 31, 41, 51),
                     labels=c("Wk 1", "Wk 11", "Wk 21", "Wk 31", "Wk 41", "Wk 51")) +
  scale_y_continuous("Transmission rate\n(1/week)") +
  scale_color_manual("", values=ggsci::pal_aaas()(3)[2:3]) +
  scale_fill_manual("", values=ggsci::pal_aaas()(3)[2:3]) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

epsilondata <- data.frame(
  year=unique(data_filter$year),
  est=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  lwr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q5[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  upr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q95[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)],
  est2=summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$median[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)],
  lwr2=summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$q5[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)],
  upr2=summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$q95[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)]
)

g3 <- ggplot(epsilondata) +
  annotate("rect", xmin=1721, xmax=1728, ymin=0.45, ymax=Inf, fill="#D9F2FF", alpha=0.5) +
  annotate("rect", xmin=1728, xmax=1740, ymin=0.45, ymax=Inf, fill="#AED8EB", alpha=0.5) +
  annotate("rect", xmin=1740, xmax=1768, ymin=0.45, ymax=Inf, fill="#8FC5DC", alpha=0.5) +
  annotate("rect", xmin=1768, xmax=1790, ymin=0.45, ymax=Inf, fill="#6EB1CC", alpha=0.5) +
  annotate("rect", xmin=1790, xmax=1796, ymin=0.45, ymax=Inf, fill="#3F94B6", alpha=0.5) +
  geom_ribbon(aes(year, ymin=exp(lwr), ymax=exp(upr), fill="Random walk priors"), alpha=0.4) +
  geom_line(aes(year, exp(est), col="Random walk priors")) +
  geom_ribbon(aes(year, ymin=exp(lwr2), ymax=exp(upr2), fill="Independent priors"), alpha=0.4) +
  geom_line(aes(year, exp(est2), col="Independent priors")) +
  geom_hline(yintercept=1, lty=2) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_log10("Multiplicative\nchanges in\ntransmission",
                breaks=c(0.5, 1, 2, 4), limits=c(0.45, 4.5), expand=c(0, 0),
                labels=c("1/2", "1", "2", "4")) +
  scale_color_manual("", values=ggsci::pal_aaas()(3)[2:3]) +
  scale_fill_manual("", values=ggsci::pal_aaas()(3)[2:3]) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

Sdata <- data.frame(
  year=data_filter$year,
  week=data_filter$week,
  est=summary_sir_perturb_demog_dd_cmdstanr_laplace$median[grepl("^S\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)]/data_filter$population,
  lwr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q5[grepl("^S\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)]/data_filter$population,
  upr=summary_sir_perturb_demog_dd_cmdstanr_laplace$q95[grepl("^S\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace$variable)]/data_filter$population,
  est2=summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$median[grepl("^S\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)]/data_filter$population,
  lwr2=summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$q5[grepl("^S\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)]/data_filter$population,
  upr2=summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$q95[grepl("^S\\[", summary_sir_perturb_demog_dd_norw_cmdstanr_laplace$variable)]/data_filter$population
)

g4 <- ggplot(Sdata) +
  annotate("rect", xmin=1721, xmax=1728, ymin=-Inf, ymax=Inf, fill="#D9F2FF", alpha=0.5) +
  annotate("rect", xmin=1728, xmax=1740, ymin=-Inf, ymax=Inf, fill="#AED8EB", alpha=0.5) +
  annotate("rect", xmin=1740, xmax=1768, ymin=-Inf, ymax=Inf, fill="#8FC5DC", alpha=0.5) +
  annotate("rect", xmin=1768, xmax=1790, ymin=-Inf, ymax=Inf, fill="#6EB1CC", alpha=0.5) +
  annotate("rect", xmin=1790, xmax=1796, ymin=-Inf, ymax=Inf, fill="#3F94B6", alpha=0.5) +
  geom_ribbon(aes(year+week/52, ymin=(lwr), ymax=(upr), fill="Random walk priors"), alpha=0.4) +
  geom_line(aes(year+week/52, (est), col="Random walk priors")) +
  geom_ribbon(aes(year+week/52, ymin=(lwr2), ymax=(upr2), fill="Independent priors"), alpha=0.4) +
  geom_line(aes(year+week/52, (est2), col="Independent priors")) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Proportion\nsusceptible", breaks=c(0, 0.1, 0.2, 0.3), limits=c(0, 0.3),
                     expand=c(0, 0)) +
  scale_color_manual("", values=ggsci::pal_aaas()(3)[2:3]) +
  scale_fill_manual("", values=ggsci::pal_aaas()(3)[2:3]) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

gcomb <- ggarrange(g1, g2, g3, g4,  ncol=1,
                   labels=c("A", "B", "C", "D"))

ggsave("figure_stanfit-v2.pdf", gcomb, width=6, height=8)
