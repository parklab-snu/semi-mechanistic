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

read_scenario <- function(suffix, simulation, scenario) {
  env <- new.env()
  load(paste0("../simulate/", simulation, ".rda"), envir=env)
  simulations <- env[[simulation]]

  lapply(seq_along(groups), function(j) {
    model <- paste0("sir_perturb_demog_dd", c("", "_norw")[j])
    lapply(1:10, function(i) {
      message(scenario, ": ", groups[j], ", replicate ", i)
      sim <- simulations[[i]][[1]]
      year <- sim$year+1669
      # Keep only extracted summaries, rather than retaining all 60 fit objects.
      env <- new.env()
      load(paste0("../stanfit_", model, "_rep", suffix,
                  "/stanfit_", model, "_cmdstanr_rep", suffix, i, ".rda"),
           envir=env)
      dd <- env[[paste0("summary_", model, "_cmdstanr_laplace_rep", suffix, i)]]
      stopifnot(!is.null(dd))
      dd <- dd[match(paste0("S[", seq_len(nrow(sim)), "]"), dd$variable), ]
      stopifnot(!anyNA(dd$variable))

      data.frame(
        x=year+sim$week/52, year=year, true=sim$S,
        est=dd$median, lwr=dd$q5, upr=dd$q95
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
  distinct(scenario, x, true)

# Average over weeks first; bias intervals use independent simulation replicates.
repdata <- fitdata %>%
  mutate(bias=(est-true)/true, covered=lwr <= true & true <= upr) %>%
  group_by(scenario, group, fit, year) %>%
  summarize(bias=mean(bias), coverage=mean(covered), .groups="drop")

coverdata <- repdata %>%
  group_by(scenario, group, year) %>%
  summarize(
    bias_lwr=t.test(bias)$conf.int[1],
    bias_upr=t.test(bias)$conf.int[2],
    bias=mean(bias),
    coverage=mean(coverage),
    n=n(),
    .groups="drop"
  )

covermean <- coverdata %>%
  group_by(scenario, group) %>%
  summarize(bias=mean(bias), coverage=mean(coverage), .groups="drop")

# Each plot is one row, with the three scenarios as columns.
common <- list(
  facet_wrap(~scenario, nrow=1, scales="free_y"),
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

g1 <- ggplot(fitdata) +
  geom_line(aes(x, est, group=interaction(fit, group), col=group)) +
  geom_line(data=truedata, aes(x, true)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Susceptible population",
                     labels=function(x) format(x, big.mark=",", scientific=FALSE, trim=TRUE)) +
  common +
  theme(strip.text=element_text(), legend.position="top")

g2 <- ggplot(coverdata) +
  geom_point(aes(year, bias, col=group), size=0.5) +
  geom_errorbar(aes(year, ymin=bias_lwr, ymax=bias_upr, col=group), width=0) +
  geom_hline(yintercept=0, lty=2) +
  geom_hline(data=covermean, aes(yintercept=bias, col=group), lty=3) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Relative bias") +
  common

g3 <- ggplot(coverdata) +
  geom_point(aes(year, coverage, col=group), size=0.5) +
  geom_hline(yintercept=0.9, lty=2) +
  geom_hline(data=covermean, aes(yintercept=coverage, col=group), lty=3) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Coverage", limits=c(0, 1)) +
  common +
  theme(axis.title.x=element_text())

gcomb <- ggarrange(g1, g2, g3, ncol=1, labels=c("A", "B", "C"))

ggsave("figure_rep_S.pdf", gcomb, width=10, height=6)
