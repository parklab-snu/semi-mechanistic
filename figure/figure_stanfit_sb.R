library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(cmdstanr)
library(gridExtra)
load("../simulate/simulate_base.rda")

load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_cmdstanr_sb.rda")

data_filter <- simulate_base[[1]]

fitdata <- data.frame(
  year=data_filter$year+1669,
  week=data_filter$week,
  est=summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$median[grepl("C\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$variable)],
  lwr=summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$q5[grepl("C\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$variable)],
  upr=summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$q95[grepl("C\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$variable)]
)

g1 <- ggplot(fitdata) +
  geom_vline(xintercept=unique(data_filter$year)+1669, lty=3, col="gray70") +
  geom_point(data=data_filter, aes(year+week/52+1669, cases), size=0.3) +
  geom_ribbon(aes(year+week/52, ymin=lwr, ymax=upr), fill="#EF6351", alpha=0.4) +
  geom_line(aes(year+week/52, est), col="#EF6351") +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly deaths", expand=c(0, 0), limits=c(0, 190)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

betaorig <- data.frame(
  week=1:52,
  true=exp(simulate_base$logbeta)
)

betadata <- data.frame(
  week=1:52,
  est=exp(summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$median[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$variable)]),
  lwr=exp(summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$q5[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$variable)]),
  upr=exp(summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$q95[grepl("logbeta\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$variable)])
)

g2 <- ggplot(betadata) +
  geom_vline(xintercept=1:52, lty=3, col="gray70") +
  geom_ribbon(aes(week, ymin=lwr, ymax=upr), fill="#EF6351", alpha=0.4) +
  geom_line(aes(week, est), col="#EF6351") +
  geom_line(data=betaorig, aes(week, true), lwd=1) +
  scale_x_continuous("Week of year", expand=c(0, 0),
                     breaks=c(1, 11, 21, 31, 41, 51),
                     labels=c("Wk 1", "Wk 11", "Wk 21", "Wk 31", "Wk 41", "Wk 51")) +
  scale_y_continuous("Transmission rate\n(1/week)") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

epsilonorig <- data.frame(
  year=unique(data_filter$year)+1669,
  true=exp(simulate_base$epsilon)
)

epsilondata <- data.frame(
  year=unique(data_filter$year)+1669,
  est=summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$median[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$variable)],
  lwr=summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$q5[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$variable)],
  upr=summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$q95[grepl("^epsilon\\[", summary_sir_perturb_demog_dd_cmdstanr_laplace_sb$variable)]
)

g3 <- ggplot(epsilondata) +
  geom_vline(xintercept=unique(data_filter$year)+1669, lty=3, col="gray70") +
  geom_ribbon(aes(year, ymin=exp(lwr), ymax=exp(upr)), fill="#EF6351", alpha=0.4) +
  geom_line(aes(year, exp(est)), col="#EF6351") +
  geom_line(data=epsilonorig, aes(year, true), lwd=1) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Multiplicative\nchanges in\ntransmission") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

draws_sb <- stanfit_sir_perturb_demog_dd_cmdstanr_laplace_sb$draws()

g4 <- ggplot(as.data.frame(draws_sb[,"S0"])) +
  geom_histogram(aes(S0), col="#EF6351", fill="#EF6351", alpha=0.2) +
  geom_vline(xintercept=simulate_base$S0, lty=2, lwd=1) +
  scale_x_continuous("Initial susceptible",
                     breaks=c(0.17, 0.18, 0.19)) +
  scale_y_continuous("Counts", expand=c(0, 0), limits=c(0, 110)) +
  theme(
    panel.border = element_rect(linewidth=1),
    panel.grid = element_blank()
  )

g5 <- ggplot(as.data.frame(draws_sb[,"I0"])) +
  geom_histogram(aes(I0), col="#EF6351", fill="#EF6351", alpha=0.2) +
  geom_vline(xintercept=simulate_base$I0, lty=2, lwd=1) +
  scale_x_continuous("Initial infected",
                     breaks=c(0.001, 0.002, 0.003),
                     limits=c(0.001, 0.003)) +
  scale_y_continuous("Counts", expand=c(0, 0), limits=c(0, 160)) +
  theme(
    panel.border = element_rect(linewidth=1),
    panel.grid = element_blank()
  )

g6 <- ggplot(as.data.frame(draws_sb[,"rho"])) +
  geom_histogram(aes(rho), col="#EF6351", fill="#EF6351", alpha=0.2) +
  geom_vline(xintercept=simulate_base$rho, lty=2, lwd=1) +
  scale_x_continuous("Fatality rate", breaks=c(0.128, 0.13, 0.132)) +
  scale_y_continuous("Counts", expand=c(0, 0), limits=c(0, 110)) +
  theme(
    panel.border = element_rect(linewidth=1),
    panel.grid = element_blank()
  )

g7 <- ggplot(as.data.frame(draws_sb[,"omega"])) +
  geom_histogram(aes(omega), col="#EF6351", fill="#EF6351", alpha=0.2) +
  geom_vline(xintercept=simulate_base$omega, lty=2, lwd=1) +
  scale_x_continuous("Imported infection") +
  scale_y_continuous("Counts", expand=c(0, 0), limits=c(0, 180)) +
  theme(
    panel.border = element_rect(linewidth=1),
    panel.grid = element_blank()
  )

g8 <- ggplot(as.data.frame(draws_sb[,"phi"])) +
  geom_histogram(aes(phi), col="#EF6351", fill="#EF6351", alpha=0.2) +
  geom_vline(xintercept=simulate_base$phi, lty=2, lwd=1) +
  scale_x_continuous("Overdispersion") +
  scale_y_continuous("Counts", expand=c(0, 0), limits=c(0, 110)) +
  theme(
    panel.border = element_rect(linewidth=1),
    panel.grid = element_blank()
  )

gcomb <- ggarrange(g1, g2, g3, nrow=3,
                   labels=c("A", "B", "C"))
gcomb2 <- ggarrange(g4, g5, g6, g7, g8, nrow=1,
                    labels=c("D", "E", "F", "G", "H"))

gfinal <- arrangeGrob(gcomb, gcomb2, heights=c(3,1))

ggsave("figure_stanfit_sb.pdf", gfinal, width=8, height=8)
