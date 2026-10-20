library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("../R/colors.R")

scenarios <- data.frame(
  scenario=c("Full change", "Half change", "No change"),
  suffix=c("", "_half", "_nc"),
  simulation=c("simulate_rep", "simulate_rep_half", "simulate_rep_nochange")
)

groups <- c("Random walk priors", "Independent priors")
cols <- setNames(colors_default[c(1,3)], sort(groups))

get_summary <- function(dd, variable, time, true, trans=identity) {
  dd <- dd[match(paste0(variable, "[", seq_along(true), "]"), dd$variable), ]
  stopifnot(!anyNA(dd$variable))
  data.frame(
    time, true=true,
    est=trans(dd$median), lwr=trans(dd$q5), upr=trans(dd$q95)
  )
}

read_scenario <- function(suffix, simulation, scenario) {
  env <- new.env()
  load(paste0("../simulate/", simulation, ".rda"), envir=env)
  sim <- env[[simulation]][[1]]
  year <- sim[[1]]$year+1669
  week <- sim[[1]]$week

  lapply(seq_along(groups), function(j) {
    model <- paste0("sir_perturb_demog_dd", c("", "_norw")[j])
    lapply(1:10, function(i) {
      message(scenario, ": ", groups[j], ", replicate ", i)
      # Keep only extracted summaries, rather than retaining all 60 fit objects.
      env <- new.env()
      load(paste0("../stanfit_", model, "_rep", suffix,
                  "/stanfit_", model, "_cmdstanr_rep", suffix, i, ".rda"),
           envir=env)
      dd <- env[[paste0("summary_", model, "_cmdstanr_laplace_rep", suffix, i)]]
      stopifnot(!is.null(dd))

      bind_rows(
        beta=get_summary(dd, "logbeta", data.frame(x=1:52),
                         exp(sim$logbeta), exp),
        epsilon=get_summary(dd, "epsilon", data.frame(x=unique(year)),
                            exp(sim$epsilon), exp),
        beff=get_summary(dd, "beta_eff", data.frame(x=year+week/52, year=year),
                         exp(sim$logbeta[week]+sim$epsilon[sim[[1]]$year])),
        .id="parameter"
      ) %>%
        mutate(scenario=scenario, group=groups[j], fit=i)
    }) %>% bind_rows()
  }) %>% bind_rows()
}

fitdata <- lapply(1:nrow(scenarios), function(i) {
  with(scenarios[i, ], read_scenario(suffix, simulation, scenario))
}) %>%
  bind_rows() %>%
  mutate(scenario=factor(scenario, levels=scenarios$scenario))

truedata <- fitdata %>%
  distinct(scenario, parameter, x, true)

coverdata <- fitdata %>%
  filter(parameter=="beff") %>%
  mutate(bias=(est-true)/true, covered=lwr <= true & true <= upr) %>%
  group_by(scenario, group, year) %>%
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
  group_by(scenario, group) %>%
  summarize(bias=mean(bias), coverage=mean(coverage), .groups="drop")

# Each plot is one row, with the three scenarios as fixed-scale facets.
common <- list(
  facet_wrap(~scenario, nrow=1, scale="free_y"),
  scale_color_manual("", values=cols),
  theme(
    panel.grid=element_blank(),
    panel.border=element_rect(linewidth=1),
    panel.spacing.x=grid::unit(0.4, "in"),
    axis.title.x=element_blank(),
    strip.background=element_blank(),
    strip.text=element_blank(),
    legend.position="none"
  )
)

plot_fit <- function(parameter_name) {
  ggplot(filter(fitdata, parameter==parameter_name)) +
    geom_line(aes(x, est, group=interaction(fit, group), col=group)) +
    geom_line(data=filter(truedata, parameter==parameter_name),
              aes(x, true)) +
    common
}

g1 <- plot_fit("beta") +
  scale_x_continuous("Week of year", expand=c(0, 0),
                     breaks=c(1, 11, 21, 31, 41, 51),
                     labels=c("Wk 1", "Wk 11", "Wk 21", "Wk 31", "Wk 41", "Wk 51")) +
  scale_y_continuous("Transmission rate\n(1/week)", limits=c(1.55, 2)) +
  theme(strip.text=element_text(), legend.position="top")

g2 <- plot_fit("epsilon") +
  geom_hline(yintercept=1, lty=2) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_log10("Multiplicative\nchanges in\ntransmission",
                breaks=c(0.5, 1, 2, 4), limits=c(0.45, 4.5), expand=c(0, 0),
                labels=c("1/2", "1", "2", "4")) 

g3 <- plot_fit("beff") +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_log10("Transmission rate", 
                breaks=c(1, 2, 4, 8), limits=c(1, 8))

g4 <- ggplot(coverdata) +
  geom_point(aes(year, bias, col=group), size=0.5) +
  geom_errorbar(aes(year, ymin=bias_lwr, ymax=bias_upr, col=group), width=0) +
  geom_hline(yintercept=0, lty=2) +
  geom_hline(data=covermean, aes(yintercept=bias, col=group), lty=3) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Relative bias", limits=c(-0.43, 0.04), expand=c(0, 0)) +
  common

g5 <- ggplot(coverdata) +
  geom_point(aes(year, coverage, col=group), size=0.5) +
  geom_hline(yintercept=0.9, lty=2) +
  geom_hline(data=covermean, aes(yintercept=coverage, col=group), lty=3) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Coverage", limits=c(0, 1)) +
  common

gcomb <- ggarrange(g1, g2, g3, g4, g5, ncol=1, labels=c("A", "B", "C", "D", "E"))

ggsave("figure_rep_comb.pdf", gcomb, width=10, height=8)
