library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(cmdstanr)
library(gridExtra)
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

g1 <- ggplot(betadata) +
  geom_vline(xintercept=1:52, lty=3, col="gray70") +
  geom_line(aes(week, est, group=fit), col="#EF6351") +
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

g2 <- ggplot(epsilondata) +
  geom_vline(xintercept=unique(simulate_rep[[1]][[1]]$year)+1669, lty=3, col="gray70") +
  geom_line(aes(year, exp(est), group=fit), col="#EF6351") +
  geom_line(data=epsilonorig, aes(year, true), lwd=1) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Multiplicative changes\nin transmission") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

epsiloncover <- merge(
  epsilonorig,
  epsilondata
) %>%
  mutate(
    bias = (exp(est)-true)/true,
    covered = exp(lwr) <= true & true <= exp(upr)
  ) %>%
  group_by(fit) %>%
  summarize(
    bias=mean(bias),
    coverage = mean(covered),
    n = n(),
    lwr = binom.test(sum(covered), n)[[4]][1],
    upr = binom.test(sum(covered), n)[[4]][2]
  )

g3 <- ggplot(epsiloncover) +
  geom_point(aes(factor(fit), bias), col="#EF6351", size=2) +
  geom_hline(yintercept=mean(epsiloncover$bias), lty=1, col="#EF6351") +
  geom_hline(yintercept=0, lty=2) +
  scale_x_discrete("Replicate") +
  scale_y_continuous("Bias") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

g4 <- ggplot(epsiloncover) +
  geom_point(aes(factor(fit), coverage), col="#EF6351", size=2) +
  geom_errorbar(aes(factor(fit), ymin=lwr, ymax=upr), width=0, col="#EF6351") +
  geom_hline(yintercept=0.9, lty=2) +
  geom_hline(yintercept=mean(epsiloncover$coverage), lty=1, col="#EF6351") +
  scale_x_discrete("Replicate") +
  scale_y_continuous("Coverage probability", limits=c(0, 1)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1)
  )

gcomb <- ggarrange(g2, g3, g4, nrow=1, labels=c("A", "B", "C"),
                   widths=c(3, 1,1))

ggsave("figure_stanfit_rep.pdf", gcomb, width=8, height=2)
