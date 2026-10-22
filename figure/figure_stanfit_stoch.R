library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(ggsci)
library(cmdstanr)
source("../R/colors.R")

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

data_filter <- data_final %>%
  filter(year < 1796)

all_summ <- list(
  summary_sir_default_laplace_rep_nc_stoch1,
  summary_sir_default_laplace_rep_nc_stoch2,
  summary_sir_default_laplace_rep_nc_stoch3,
  summary_sir_default_laplace_rep_nc_stoch4,
  summary_sir_default_laplace_rep_nc_stoch5,
  summary_sir_default_laplace_rep_nc_stoch6,
  summary_sir_default_laplace_rep_nc_stoch7,
  summary_sir_default_laplace_rep_nc_stoch8,
  summary_sir_default_laplace_rep_nc_stoch9,
  summary_sir_default_laplace_rep_nc_stoch10
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
    .id="fit"
  )

ggplot(fitdata) +
  geom_point(data=data_filter, aes(year+week/52, smpx), size=0.3) +
  geom_ribbon(aes(year+week/52, ymin=lwr, ymax=upr), alpha=0.4, fill=colors_new[4]) +
  geom_line(aes(year+week/52, est), col=colors_new[4]) +
  facet_wrap(~fit)


ggplot(fitdata) +
  geom_point(data=data_filter, aes(year+week/52, smpx), size=0.3) +
  geom_ribbon(aes(year+week/52, ymin=lwr, ymax=upr, fill=group), alpha=0.4) +
  geom_line(aes(year+week/52, est, col=group)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly deaths", expand=c(0, 0), limits=c(0, 260)) +
  scale_color_manual("Prior distribution", values=colors_default) +
  scale_fill_manual("Prior distribution", values=colors_default) +
  guides(color=guide_legend(nrow=2,byrow=TRUE), fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "top"
  )

data_comp <- fitdata %>%
  merge(data_filter)

data_Rsq <- data_comp %>%
  group_by(group) %>%
  summarize(
    Rsq=round(cor(log(est+1), log(smpx+1))^2, 3)
  ) %>%
  mutate(
    text=paste0("R^2==", Rsq)
  )

g2 <- ggplot(data_comp) +
  geom_point(aes(est+1, smpx+1, col=group), alpha=0.2, shape=21) +
  geom_abline(intercept=0, slope=1, lty=2) +
  geom_smooth(aes(est+1, smpx+1, col=group, fill=group), method="lm", fullrange=TRUE) +
  geom_text(data=data_Rsq, aes(4, 100, label=text), parse=TRUE, family="Times") +
  scale_x_log10("Predicted deaths", expand=c(0,0)) +
  scale_y_log10("Observed deaths", expand=c(0,0)) +
  scale_color_manual("Prior: seasonal / long-term", values=colors_default) +
  scale_fill_manual("Prior: seasonal / long-term", values=colors_default) +
  facet_wrap(~group, nrow=1)+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    strip.background = element_blank(),
    legend.position = "none"
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
                 levels=c("default", "erw", "brw", "brw_erw"),
                 labels=c("ID seasonal, ID long-term",
                          "ID seasonal, RW long-term",
                          "RW seasonal, ID long-term",
                          "RW seasonal, RW long-term"))
  )

g3 <- ggplot(betadata) +
  geom_ribbon(aes(week, ymin=lwr, ymax=upr, fill=group), alpha=0.4) +
  geom_line(aes(week, est, col=group)) +
  scale_x_continuous("Week of year", expand=c(0, 0),
                     breaks=c(1, 11, 21, 31, 41, 51),
                     labels=c("Wk 1", "Wk 11", "Wk 21", "Wk 31", "Wk 41", "Wk 51")) +
  scale_y_continuous("Transmission rate\n(1/week)") +
  scale_color_manual("", values=colors_default) +
  scale_fill_manual("", values=colors_default) +
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
                 levels=c("default", "erw", "brw", "brw_erw"),
                 labels=c("ID seasonal, ID long-term",
                          "ID seasonal, RW long-term",
                          "RW seasonal, ID long-term",
                          "RW seasonal, RW long-term"))
  )

g4 <- ggplot(epsilondata) +
  geom_ribbon(aes(year, ymin=lwr, ymax=upr, fill=group), alpha=0.4) +
  geom_line(aes(year, est, col=group)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_log10("Multiplicative\nchanges in\ntransmission",
                breaks=c(0.5, 1, 2, 4), limits=c(0.45, 4.5), expand=c(0, 0),
                labels=c("1/2", "1", "2", "4")) +
  geom_hline(yintercept=1, lty=2) +
  scale_color_manual("", values=colors_default) +
  scale_fill_manual("", values=colors_default) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

gcomb <- ggarrange(g1, g2, g3, g4,  ncol=1,
                   labels=c("A", "B", "C", "D"))

ggsave("figure_stanfit_compare.pdf", gcomb, width=8, height=8)
