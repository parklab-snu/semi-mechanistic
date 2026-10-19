library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(cmdstanr)
library(gridExtra)
load("../simulate/simulate_rep.rda")
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

fitdata <- lapply(1:10, function(x) {
  dd <- get(paste0("summary_sir_perturb_demog_dd_norw_cmdstanr_laplace_rep", x))
  
  ss <- simulate_rep[[x]]
  
  data.frame(
    week=ss[[1]]$week,
    year=ss[[1]]$year,
    cases=ss[[1]]$cases,
    pred=dd$median[grepl("C\\[", dd$variable)],
    lwr=dd$q5[grepl("C\\[", dd$variable)],
    upr=dd$q95[grepl("C\\[", dd$variable)],
    fit=x
  )
}) %>%
  bind_rows %>%
  mutate(
    fit=paste0("Replicate ", fit)
  )

g1 <- ggplot(fitdata) +
  geom_point(aes(year+week/52+1669, cases), shape=".") +
  geom_ribbon(aes(year+week/52+1669, ymin=lwr, ymax=upr), fill="#EF6351", alpha=0.4) +
  geom_line(aes(year+week/52+1669, pred), col="#EF6351") +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly deaths", expand=c(0, 0)) +
  facet_wrap(~fit, ncol=3) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    strip.background = element_blank()
  )

fitdata_summ <- fitdata %>%
  group_by(fit) %>%
  summarize(
    R2=cor(log(cases+1), log(pred+1), use="complete.obs")^2
  ) %>%
  mutate(
    text=paste0("R^2==",signif(R2,3))
  )

g2 <- ggplot(fitdata) +
  geom_point(aes(cases+1, pred+1), shape=".") +
  geom_smooth(aes(cases+1, pred+1), method="lm", col="#EF6351") +
  geom_text(data=fitdata_summ, aes(1.5, 100, label=text), hjust=0, family="Times", parse=TRUE) +
  scale_x_log10("Observed+1", expand=c(0, 0)) +
  scale_y_log10("Predicted+1", expand=c(0, 0)) +
  facet_wrap(~fit, ncol=3) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    strip.background = element_blank()
  )

ggsave("figure_stanfit_rep_traj_norw_1.pdf", g1, width=8, height=6)
ggsave("figure_stanfit_rep_traj_norw_2.pdf", g2, width=8, height=6)
