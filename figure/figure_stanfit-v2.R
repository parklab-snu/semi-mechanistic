library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(ggsci)
library(cmdstanr)
source("../script/script_data.R")

load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_cmdstanr.rda")
load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_wide_cmdstanr.rda")
load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_norw_cmdstanr.rda")
load("../stanfit_sir_perturb_demog_dd/stanfit_sir_perturb_demog_dd_norw_wide_cmdstanr.rda")

data_filter <- data_final %>%
  filter(year < 1796)

all_summ <- list(
  rw_n=summary_sir_perturb_demog_dd_cmdstanr_laplace,
  rw_w=summary_sir_perturb_demog_dd_wide_cmdstanr_laplace,
  norw_n=summary_sir_perturb_demog_dd_norw_cmdstanr_laplace,
  norw_w=summary_sir_perturb_demog_dd_norw_wide_cmdstanr_laplace
)

fitdata <- lapply(all_summ, function(x) {
  data.frame(
    year=data_filter$year,
    week=data_filter$week,
    est=x$median[grepl("C\\[", x$variable)],
    lwr=x$q5[grepl("C\\[", x$variable)],
    upr=x$q95[grepl("C\\[", x$variable)]
  )
}) %>%
  bind_rows(
    .id="group"
  ) %>%
  mutate(
    group=factor(group, 
                 levels=c("norw_n", "norw_w", "rw_n", "rw_w"),
                 labels=c("Independent, narrow priors",
                          "Independent, wide priors",
                          "Random walk, narrow priors",
                          "Random walk, wide priors"))
  )

g1 <- ggplot(fitdata) +
  # annotate("rect", xmin=1721, xmax=1728, ymin=-Inf, ymax=Inf, fill="#D9F2FF", alpha=0.5) +
  # annotate("rect", xmin=1728, xmax=1740, ymin=-Inf, ymax=Inf, fill="#AED8EB", alpha=0.5) +
  # annotate("rect", xmin=1740, xmax=1768, ymin=-Inf, ymax=Inf, fill="#8FC5DC", alpha=0.5) +
  # annotate("rect", xmin=1768, xmax=1790, ymin=-Inf, ymax=Inf, fill="#6EB1CC", alpha=0.5) +
  # annotate("rect", xmin=1790, xmax=1796, ymin=-Inf, ymax=Inf, fill="#3F94B6", alpha=0.5) +
  # geom_vline(xintercept=unique(data_filter$year), lty=3, col="gray70") +
  geom_point(data=data_filter, aes(year+week/52, smpx), size=0.3) +
  geom_ribbon(aes(year+week/52, ymin=lwr, ymax=upr, fill=group), alpha=0.4) +
  geom_line(aes(year+week/52, est, col=group)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly deaths", expand=c(0, 0), limits=c(0, 260)) +
  scale_color_manual("", values=ggsci::pal_nejm()(5)[1:4]) +
  scale_fill_manual("", values=ggsci::pal_nejm()(5)[1:4]) +
  guides(color=guide_legend(nrow=2,byrow=TRUE), fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

betadata <- lapply(all_summ, function(x) {
  data.frame(
    week=1:52,
    est=exp(x$median[grepl("logbeta\\[", x$variable)]),
    lwr=exp(x$q5[grepl("logbeta\\[", x$variable)]),
    upr=exp(x$q95[grepl("logbeta\\[", x$variable)])
  )
}) %>%
  bind_rows(
    .id="group"
  ) %>%
  mutate(
    group=factor(group, 
                 levels=c("norw_n", "norw_w", "rw_n", "rw_w"),
                 labels=c("Independent, narrow priors",
                          "Independent, wide priors",
                          "Random walk, narrow priors",
                          "Random walk, wide priors"))
  )

g2 <- ggplot(betadata) +
  geom_ribbon(aes(week, ymin=lwr, ymax=upr, fill=group), alpha=0.4) +
  geom_line(aes(week, est, col=group)) +
  scale_x_continuous("Week of year", expand=c(0, 0),
                     breaks=c(1, 11, 21, 31, 41, 51),
                     labels=c("Wk 1", "Wk 11", "Wk 21", "Wk 31", "Wk 41", "Wk 51")) +
  scale_y_continuous("Transmission rate\n(1/week)") +
  scale_color_manual("", values=ggsci::pal_nejm()(5)[1:4]) +
  scale_fill_manual("", values=ggsci::pal_nejm()(5)[1:4]) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

epsilondata <- lapply(all_summ, function(x) {
  data.frame(
    year=unique(data_filter$year),
    est=exp(x$median[grepl("^epsilon\\[", x$variable)]),
    lwr=exp(x$q5[grepl("^epsilon\\[", x$variable)]),
    upr=exp(x$q95[grepl("^epsilon\\[", x$variable)])
  )
}) %>%
  bind_rows(
    .id="group"
  ) %>%
  mutate(
    group=factor(group, 
                 levels=c("norw_n", "norw_w", "rw_n", "rw_w"),
                 labels=c("Independent, narrow priors",
                          "Independent, wide priors",
                          "Random walk, narrow priors",
                          "Random walk, wide priors"))
  )

g3 <- ggplot(epsilondata) +
  geom_ribbon(aes(year, ymin=lwr, ymax=upr, fill=group), alpha=0.4) +
  geom_line(aes(year, est, col=group)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_log10("Multiplicative\nchanges in\ntransmission",
                breaks=c(0.5, 1, 2, 4), limits=c(0.45, 4.5), expand=c(0, 0),
                labels=c("1/2", "1", "2", "4")) +
  geom_hline(yintercept=1, lty=2) +
  scale_color_manual("", values=ggsci::pal_nejm()(5)[1:4]) +
  scale_fill_manual("", values=ggsci::pal_nejm()(5)[1:4]) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

Sdata <- lapply(all_summ, function(x) {
  data.frame(
    year=data_filter$year,
    week=data_filter$week,
    est=x$median[grepl("S\\[", x$variable)],
    lwr=x$q5[grepl("S\\[", x$variable)],
    upr=x$q95[grepl("S\\[", x$variable)]
  )
}) %>%
  bind_rows(
    .id="group"
  ) %>%
  mutate(
    group=factor(group, 
                 levels=c("norw_n", "norw_w", "rw_n", "rw_w"),
                 labels=c("Independent, narrow priors",
                          "Independent, wide priors",
                          "Random walk, narrow priors",
                          "Random walk, wide priors"))
  )

gcomb <- ggarrange(g1, g2, g3,  ncol=1,
                   labels=c("A", "B", "C"))

ggsave("figure_stanfit-v2.pdf", gcomb, width=6, height=6)
