library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(cmdstanr)
library(gridExtra)
source("../R/colors.R")
load("../simulate/simulate_rep.rda")
load("../stanfit_sir_perturb_demog_dd_rep/stanfit_sir_perturb_demog_dd_cmdstanr_rep1.rda")
load("../stanfit_sir_perturb_demog_dd_rep/stanfit_sir_perturb_demog_dd_cmdstanr_rep2.rda")
load("../stanfit_sir_perturb_demog_dd_rep/stanfit_sir_perturb_demog_dd_cmdstanr_rep3.rda")
load("../stanfit_sir_perturb_demog_dd_rep/stanfit_sir_perturb_demog_dd_cmdstanr_rep4.rda")
load("../stanfit_sir_perturb_demog_dd_rep/stanfit_sir_perturb_demog_dd_cmdstanr_rep5.rda")
load("../stanfit_sir_perturb_demog_dd_rep/stanfit_sir_perturb_demog_dd_cmdstanr_rep6.rda")
load("../stanfit_sir_perturb_demog_dd_rep/stanfit_sir_perturb_demog_dd_cmdstanr_rep7.rda")
load("../stanfit_sir_perturb_demog_dd_rep/stanfit_sir_perturb_demog_dd_cmdstanr_rep8.rda")
load("../stanfit_sir_perturb_demog_dd_rep/stanfit_sir_perturb_demog_dd_cmdstanr_rep9.rda")
load("../stanfit_sir_perturb_demog_dd_rep/stanfit_sir_perturb_demog_dd_cmdstanr_rep10.rda")

load("../stanfit_sir_perturb_demog_dd_norw_rep/stanfit_sir_perturb_demog_dd_norw_cmdstanr_rep1.rda")
load("../stanfit_sir_perturb_demog_dd_norw_rep/stanfit_sir_perturb_demog_dd_norw_cmdstanr_rep2.rda")
load("../stanfit_sir_perturb_demog_dd_norw_rep/stanfit_sir_perturb_demog_dd_norw_cmdstanr_rep3.rda")
load("../stanfit_sir_perturb_demog_dd_norw_rep/stanfit_sir_perturb_demog_dd_norw_cmdstanr_rep4.rda")
load("../stanfit_sir_perturb_demog_dd_norw_rep/stanfit_sir_perturb_demog_dd_norw_cmdstanr_rep5.rda")
load("../stanfit_sir_perturb_demog_dd_norw_rep/stanfit_sir_perturb_demog_dd_norw_cmdstanr_rep6.rda")
load("../stanfit_sir_perturb_demog_dd_norw_rep/stanfit_sir_perturb_demog_dd_norw_cmdstanr_rep7.rda")
load("../stanfit_sir_perturb_demog_dd_norw_rep/stanfit_sir_perturb_demog_dd_norw_cmdstanr_rep8.rda")
load("../stanfit_sir_perturb_demog_dd_norw_rep/stanfit_sir_perturb_demog_dd_norw_cmdstanr_rep9.rda")
load("../stanfit_sir_perturb_demog_dd_norw_rep/stanfit_sir_perturb_demog_dd_norw_cmdstanr_rep10.rda")

betaorig <- data.frame(
  week=1:52,
  true=exp(simulate_rep[[1]]$logbeta)
)

betadata <- lapply(1:10, function(x){
  dd <- get(paste0("summary_sir_perturb_demog_dd_cmdstanr_laplace_rep", x))
  
  data.frame(
    week=1:52,
    fit=x,
    est=exp(dd$median[grepl("logbeta\\[", dd$variable)]),
    lwr=exp(dd$q5[grepl("logbeta\\[", dd$variable)]),
    upr=exp(dd$q95[grepl("logbeta\\[", dd$variable)])
  )
}) %>%
  bind_rows

betadata_norw <- lapply(1:10, function(x){
  dd <- get(paste0("summary_sir_perturb_demog_dd_norw_cmdstanr_laplace_rep", x))
  
  data.frame(
    week=1:52,
    fit=x,
    est=exp(dd$median[grepl("logbeta\\[", dd$variable)]),
    lwr=exp(dd$q5[grepl("logbeta\\[", dd$variable)]),
    upr=exp(dd$q95[grepl("logbeta\\[", dd$variable)])
  )
}) %>%
  bind_rows

betadata_all <- bind_rows(
  betadata %>% mutate(group="Random walk priors"),
  betadata_norw %>% mutate(group="Independent priors")
)

g1 <- ggplot(betadata_all) +
  geom_line(aes(week, est, group=interaction(fit, group), col=group)) +
  geom_line(data=betaorig, aes(week, true), lwd=1) +
  scale_color_manual("", values=colors_default[c(1,3)]) +
  scale_x_continuous("Week of year", expand=c(0, 0),
                     breaks=c(1, 11, 21, 31, 41, 51),
                     labels=c("Wk 1", "Wk 11", "Wk 21", "Wk 31", "Wk 41", "Wk 51")) +
  scale_y_continuous("Transmission rate\n(1/week)") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "top"
  )

epsilonorig <- data.frame(
  year=unique(simulate_rep[[1]][[1]]$year)+1669,
  true=exp(simulate_rep[[1]]$epsilon)
)

epsilondata <- lapply(1:10, function(x){
  dd <- get(paste0("summary_sir_perturb_demog_dd_cmdstanr_laplace_rep", x))
  
  data.frame(
    year=unique(simulate_rep[[1]][[1]]$year)+1669,
    fit=x,
    est=(dd$median[grepl("^epsilon\\[", dd$variable)]),
    lwr=(dd$q5[grepl("^epsilon\\[", dd$variable)]),
    upr=(dd$q95[grepl("^epsilon\\[", dd$variable)])
  )
}) %>%
  bind_rows

epsilondata_norw <- lapply(1:10, function(x){
  dd <- get(paste0("summary_sir_perturb_demog_dd_norw_cmdstanr_laplace_rep", x))
  
  data.frame(
    year=unique(simulate_rep[[1]][[1]]$year)+1669,
    fit=x,
    est=(dd$median[grepl("^epsilon\\[", dd$variable)]),
    lwr=(dd$q5[grepl("^epsilon\\[", dd$variable)]),
    upr=(dd$q95[grepl("^epsilon\\[", dd$variable)])
  )
}) %>%
  bind_rows

epsilondata_all <- bind_rows(
  epsilondata %>% mutate(group="Random walk priors"),
  epsilondata_norw %>% mutate(group="Independent priors")
)

g2 <- ggplot(epsilondata_all) +
  geom_line(aes(year, exp(est), group=interaction(fit, group), col=group)) +
  geom_line(data=epsilonorig, aes(year, true), lwd=1) +
  geom_hline(yintercept=1, lty=2) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_log10("Multiplicative\nchanges in\ntransmission",
                breaks=c(0.5, 1, 2, 4), limits=c(0.45, 4.5), expand=c(0, 0),
                labels=c("1/2", "1", "2", "4")) +
  scale_color_manual("", values=colors_default[c(1,3)]) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

coverdata <- merge(
  epsilonorig,
  epsilondata
) %>%
  mutate(
    bias = (exp(est)-true)/true,
    covered = exp(lwr) <= true & true <= exp(upr)
  ) %>%
  group_by(year) %>%
  summarize(
    bias_lwr=t.test(bias)[[4]][1],
    bias_upr=t.test(bias)[[4]][2],
    bias=mean(bias),
    coverage = mean(covered),
    n = n(),
    lwr = binom.test(sum(covered), n)[[4]][1],
    upr = binom.test(sum(covered), n)[[4]][2]
  )

coverdata_norw <- merge(
  epsilonorig,
  epsilondata_norw
) %>%
  mutate(
    bias = (exp(est)-true)/true,
    covered = exp(lwr) <= true & true <= exp(upr)
  ) %>%
  group_by(year) %>%
  summarize(
    bias_lwr=t.test(bias)[[4]][1],
    bias_upr=t.test(bias)[[4]][2],
    bias=mean(bias),
    coverage = mean(covered),
    n = n(),
    lwr = binom.test(sum(covered), n)[[4]][1],
    upr = binom.test(sum(covered), n)[[4]][2]
  )

coverdata_all <- bind_rows(
  coverdata %>% mutate(group="Random walk priors"),
  coverdata_norw %>% mutate(group="Independent priors")
)

g3 <- ggplot(coverdata_all) +
  geom_point(aes(year, bias, col=group)) +
  geom_errorbar(aes(year, ymin=bias_lwr, ymax=bias_upr, col=group), width=0) +
  geom_hline(yintercept=0, lty=2) +
  # geom_hline(yintercept=mean(coverdata$bias), lty=2, col=ggsci::pal_nejm()(3)[3], linewidth=1) +
  # geom_hline(yintercept=mean(coverdata_norw$bias), lty=2, col=ggsci::pal_nejm()(3)[1], linewidth=1) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Relative bias") +
  scale_color_manual("", values=colors_default[c(1,3)]) +
  scale_fill_manual("", values=colors_default[c(1,3)]) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

g4 <- ggplot(coverdata_all) +
  geom_point(aes(year, coverage, col=group)) +
  geom_hline(yintercept=0.9, col="black", lty=2) +
  # geom_hline(yintercept=mean(coverdata$coverage), lty=2, col=ggsci::pal_nejm()(3)[3], linewidth=1) +
  # geom_hline(yintercept=mean(coverdata_norw$coverage), lty=2, col=ggsci::pal_nejm()(3)[1], linewidth=1) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Coverage") +
  scale_color_manual("", values=colors_default[c(1,3)]) +
  scale_fill_manual("", values=colors_default[c(1,3)]) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

gcomb <- ggarrange(g1, g2, g3, g4, ncol=1, labels=c("A", "B", "C", "D"))

ggsave("figure_stanfit_rep-v2.pdf", gcomb, width=8, height=8)
