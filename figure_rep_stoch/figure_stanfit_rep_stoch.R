library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("../R/colors.R")

simulation_env <- new.env()
load("../simulate/simulate_rep_nochange_stoch.rda", envir=simulation_env)
simulations <- simulation_env$simulate_rep_nochange_stoch

get_summary <- function(dd, variable, time, true, trans=identity) {
  dd <- dd[match(paste0(variable, "[", seq_along(true), "]"), dd$variable), ]
  stopifnot(!anyNA(dd$variable))
  data.frame(
    time, true=true,
    est=trans(dd$median), lwr=trans(dd$q5), upr=trans(dd$q95)
  )
}

fitdata <- lapply(seq_along(simulations), function(i) {
  message("Stochastic simulation: replicate ", i)
  sim <- simulations[[i]]
  dat <- sim[[1]]
  year <- dat$year+1669
  week <- dat$week

  # Retain summaries only, rather than all ten large fit objects.
  fit_env <- new.env()
  load(paste0("../stanfit_sir_default_rep_nc_stoch/",
              "stanfit_sir_default_rep_nc_stoch", i, ".rda"),
       envir=fit_env)
  dd <- fit_env[[paste0("summary_sir_default_laplace_rep_nc_stoch", i)]]
  stopifnot(!is.null(dd))

  bind_rows(
    beta=get_summary(dd, "logbeta", data.frame(x=seq_along(sim$logbeta)),
                     exp(sim$logbeta), exp),
    epsilon=get_summary(dd, "epsilon", data.frame(x=unique(year)),
                        exp(sim$epsilon), exp),
    beff=get_summary(dd, "beta_eff", data.frame(x=year+week/52, year=year),
                     exp(sim$logbeta[week]+sim$epsilon[dat$year])),
    .id="parameter"
  ) %>%
    mutate(fit=i)
}) %>%
  bind_rows()

truedata <- fitdata %>%
  distinct(parameter, x, true)

coverdata <- fitdata %>%
  filter(parameter=="beff") %>%
  mutate(bias=(est-true)/true, covered=lwr <= true & true <= upr) %>%
  group_by(year) %>%
  summarize(
    bias_lwr=t.test(bias)$conf.int[1],
    bias_upr=t.test(bias)$conf.int[2],
    bias=mean(bias),
    coverage=mean(covered),
    n=n(),
    lwr=binom.test(sum(covered), n)$conf.int[1],
    upr=binom.test(sum(covered), n)$conf.int[2],
    .groups="drop"
  )

covermean <- coverdata %>%
  summarize(bias=mean(bias), coverage=mean(coverage))

cols <- c("Independent priors"=colors_default[1], "Simulated truth"="black")

common <- list(
  scale_color_manual("", values=cols),
  theme(
    panel.grid=element_blank(),
    panel.border=element_rect(linewidth=1),
    axis.title.x=element_blank(),
    legend.position="none"
  )
)

plot_fit <- function(parameter_name) {
  ggplot(filter(fitdata, parameter==parameter_name)) +
    geom_line(aes(x, est, group=fit, col="Independent priors"),
              linewidth=0.4, alpha=0.7) +
    geom_line(data=filter(truedata, parameter==parameter_name),
              aes(x, true, col="Simulated truth"), linewidth=0.5) +
    common
}

g1 <- plot_fit("beta") +
  scale_x_continuous("Week of year", expand=c(0, 0),
                     breaks=c(1, 11, 21, 31, 41, 51),
                     labels=c("Wk 1", "Wk 11", "Wk 21", "Wk 31", "Wk 41", "Wk 51")) +
  scale_y_continuous("Seasonal\ntransmission rate\n(1/week)") +
  guides(color=guide_legend(override.aes=list(alpha=1, linewidth=0.8))) +
  theme(legend.position="top")

g2 <- plot_fit("epsilon") +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_log10("Multiplicative\nchanges in\ntransmission",
                breaks=c(0.125, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 2, 4, 8, 16),
                labels=c("1/8", "1/4", "1/2", "3/4", "1", "1.25", "1.5", "2", "4", "8", "16"))

g3 <- plot_fit("beff") +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_log10("Effective\ntransmission rate\n(1/week)")

g4 <- ggplot(coverdata) +
  geom_hline(yintercept=0, lty=2) +
  geom_hline(data=covermean, aes(yintercept=bias, col="Independent priors"), lty=3) +
  geom_errorbar(aes(year, ymin=bias_lwr, ymax=bias_upr, col="Independent priors"), width=0) +
  geom_point(aes(year, bias, col="Independent priors"), size=0.5) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Relative bias") +
  common

g5 <- ggplot(coverdata) +
  geom_hline(yintercept=0.9, lty=2) +
  geom_hline(data=covermean, aes(yintercept=coverage, col="Independent priors"), lty=3) +
  geom_errorbar(aes(year, ymin=lwr, ymax=upr, col="Independent priors"), width=0) +
  geom_point(aes(year, coverage, col="Independent priors"), size=0.5) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Coverage", limits=c(0, 1)) +
  common +
  theme(axis.title.x=element_text())

gcomb <- ggarrange(g1, g2, g3, g4, g5, ncol=1, labels=c("A", "B", "C", "D", "E"))

ggsave("figure_stanfit_rep_stoch.pdf", gcomb, width=5.5, height=9)
