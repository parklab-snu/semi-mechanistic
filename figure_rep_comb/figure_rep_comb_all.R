library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("../R/colors.R")

scenarios <- data.frame(
  process=rep(c("Deterministic", "Stochastic"), each=2),
  scenario=rep(c("Full change", "No change"), 2),
  suffix=c("", "_nc", "_stoch", "_nc_stoch"),
  simulation=c("simulate_rep", "simulate_rep_nochange",
               "simulate_rep_stoch", "simulate_rep_nochange_stoch")
)
scenarios$column <- paste(scenarios$process, scenarios$scenario, sep="\n")

models <- c("Random walk priors"="sir_brw_erw",
            "Independent priors"="sir_default")
groups <- names(models)
cols <- setNames(colors_default[c(1,3)], sort(groups))

get_summary <- function(dd, variable, time, true, trans=identity) {
  dd <- dd[match(paste0(variable, "[", seq_along(true), "]"), dd$variable), ]
  stopifnot(!anyNA(dd$variable))
  data.frame(
    time, true=true,
    est=trans(dd$median), lwr=trans(dd$q5), upr=trans(dd$q95)
  )
}

read_scenario <- function(suffix, simulation, column) {
  env <- new.env()
  load(paste0("../simulate/", simulation, ".rda"), envir=env)
  simulations <- env[[simulation]]

  lapply(seq_along(groups), function(j) {
    model <- models[[j]]
    lapply(seq_along(simulations), function(i) {
      message(gsub("\n", " / ", column), ": ", groups[j], ", replicate ", i)
      sim <- simulations[[i]]
      year <- sim[[1]]$year+1669
      week <- sim[[1]]$week
      # Keep only extracted summaries, rather than retaining all fit objects.
      env <- new.env()
      load(paste0("../stanfit_", model, "_rep", suffix,
                  "/stanfit_", model, "_rep", suffix, i, ".rda"),
           envir=env)
      dd <- env[[paste0("summary_", model, "_laplace_rep", suffix, i)]]
      stopifnot(!is.null(dd))

      bind_rows(
        # Only replicate 1 is displayed in the model-fit row.
        cases=if (i==1L) get_summary(dd, "C", data.frame(x=year+week/52),
                                    sim[[1]]$cases),
        beta=get_summary(dd, "logbeta", data.frame(x=1:52),
                         exp(sim$logbeta), exp),
        epsilon=get_summary(dd, "epsilon", data.frame(x=unique(year)),
                            exp(sim$epsilon), exp),
        beff=get_summary(dd, "beta_eff", data.frame(x=year+week/52, year=year),
                         exp(sim$logbeta[week]+sim$epsilon[sim[[1]]$year])),
        .id="parameter"
      ) %>%
        mutate(column=column, group=groups[j], fit=i)
    }) %>% bind_rows()
  }) %>% bind_rows()
}

fitdata <- lapply(1:nrow(scenarios), function(i) {
  with(scenarios[i, ], read_scenario(suffix, simulation, column))
}) %>%
  bind_rows() %>%
  mutate(column=factor(column, levels=scenarios$column))


fitdata <- fitdata %>%
  filter(
    !(fit==5 & column=="Stochastic\nFull change")
  )

truedata <- fitdata %>%
  distinct(column, parameter, x, true)

coverdata <- fitdata %>%
  filter(parameter=="beff") %>%
  mutate(bias=(est-true)/true, covered=lwr <= true & true <= upr) %>%
  group_by(column, group, year) %>%
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
  group_by(column, group) %>%
  summarize(bias=mean(bias), coverage=mean(coverage), .groups="drop")

# Each row has deterministic scenarios first, then stochastic scenarios.
common <- list(
  facet_wrap(~column, nrow=1),
  scale_color_manual("", values=cols),
  theme(
    panel.grid=element_blank(),
    panel.border=element_rect(linewidth=1),
    panel.spacing.x=grid::unit(0.25, "in"),
    axis.title.x=element_blank(),
    strip.background=element_blank(),
    strip.text=element_blank(),
    legend.position="none"
  )
)

g0 <- ggplot(filter(fitdata, parameter=="cases", fit==1L)) +
  geom_point(data=filter(truedata, parameter=="cases"),
             aes(x, true), shape=".", na.rm=TRUE) +
  geom_ribbon(aes(x, ymin=lwr, ymax=upr, fill=group), alpha=0.25) +
  geom_line(aes(x, est, col=group), linewidth=0.3) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly deaths\n(replicate 1)", expand=expansion(mult=c(0, 0.05))) +
  common +
  scale_fill_manual("", values=cols) +
  theme(strip.text=element_text(), legend.position="top")

plot_fit <- function(parameter_name) {
  ggplot(filter(fitdata, parameter==parameter_name)) +
    geom_line(aes(x, est, group=interaction(fit, group), col=group)) +
    geom_line(data=filter(truedata, parameter==parameter_name),
              aes(x, true)) +
    common
}

g1 <- plot_fit("beta") +
  scale_x_continuous("Week of year", expand=expansion(mult=0.02),
                     breaks=c(1, 11, 21, 31, 41, 51),
                     labels=c("Wk 1", "Wk 11", "Wk 21", "Wk 31", "Wk 41", "Wk 51")) +
  scale_y_continuous("Seasonal\ntransmission rate\n(1/week)",
                     expand=expansion(mult=0.05))

g2 <- plot_fit("epsilon") +
  geom_hline(yintercept=1, lty=2) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Annual\ntransmission\nmultiplier")

g4 <- ggplot(coverdata) +
  geom_point(aes(year, bias, col=group), size=0.5) +
  geom_errorbar(aes(year, ymin=bias_lwr, ymax=bias_upr, col=group), width=0) +
  geom_hline(yintercept=0, lty=2) +
  geom_hline(data=covermean, aes(yintercept=bias, col=group), lty=3) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Relative bias,\ncombined transmission") +
  common

g5 <- ggplot(coverdata) +
  geom_point(aes(year, coverage, col=group), size=0.5) +
  geom_hline(yintercept=0.9, lty=2) +
  geom_hline(data=covermean, aes(yintercept=coverage, col=group), lty=3) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Coverage,\ncombined transmission", limits=c(0, 1)) +
  common

gcomb <- ggarrange(g0, g1, g2, g4, g5, ncol=1,
                   labels=c("A", "B", "C", "D", "E"))

ggsave("figure_rep_comb_all.pdf", gcomb, width=10, height=10)
