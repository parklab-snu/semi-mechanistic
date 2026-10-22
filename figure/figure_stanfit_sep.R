library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
library(ggsci)
library(cmdstanr)
source("../script/script_data.R")
source("../R/colors.R")

load("../stanfit_main/stanfit_sir_default_sep.rda")
load("../stanfit_main/stanfit_sir_default.rda")

data_filter <- data_final %>%
  filter(year < 1796)

year <- data_filter$year-min(data_filter$year)+1

group <- ceiling(year/20)
group[group==7] <- 6

all_summ <- list(
  Separate=summary_sir_default_sep_laplace,
  Joint=summary_sir_default_laplace
)

fitdata <- lapply(all_summ, function(x) {
  data.frame(
    year=data_filter$year,
    week=data_filter$week,
    group_y=group,
    est=x$median[grepl("C\\[", x$variable)],
    lwr=x$q5[grepl("C\\[", x$variable)],
    upr=x$q95[grepl("C\\[", x$variable)]
  )
}) %>%
  bind_rows(
    .id="group"
  )

colors_this <- c(colors_default[1], colors_new[1])

g1 <- ggplot(fitdata) +
  geom_vline(xintercept=c(1690, 1710, 1730, 1750, 1770), lty=3) +
  geom_point(data=data_filter, aes(year+week/52, smpx), size=0.3) +
  geom_ribbon(aes(year+week/52, ymin=lwr, ymax=upr, fill=group), alpha=0.3) +
  geom_line(aes(year+week/52, est, col=group)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly deaths", expand=c(0, 0), limits=c(0, 260)) +
  scale_color_manual("Prior distribution", values=colors_this) +
  scale_fill_manual("Prior distribution", values=colors_this) +
  guides(color=guide_legend(nrow=1,byrow=TRUE), fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

data_comp <- fitdata %>%
  merge(data_filter) %>%
  mutate(
    group_y=factor(group_y,
                   levels=1:6,
                   labels=c("1670-1689",
                            "1690-1709",
                            "1710-1729",
                            "1730-1749",
                            "1750-1769",
                            "1770-1796"))
  )

data_Rsq <- data_comp %>%
  group_by(group, group_y) %>%
  summarize(
    Rsq=signif(cor(log(est+1), log(smpx+1))^2, 3)
  ) %>%
  ungroup %>%
  mutate(
    text=paste0("R^2==", Rsq),
    y=-30+120*as.numeric(as.factor(group)),
    text=ifelse(text=="R^2==0.77", "R^2==0.770", text)
  )

g2 <- ggplot(data_comp) +
  geom_point(aes(est+1, smpx+1, col=group), alpha=0.2, shape=21) +
  geom_abline(intercept=0, slope=1, lty=2) +
  geom_smooth(aes(est+1, smpx+1, col=group, fill=group), method="lm", fullrange=TRUE) +
  geom_text(data=data_Rsq, aes(5.5, y, label=text, col=group),
            parse=TRUE, family="Times") +
  scale_x_log10("Predicted deaths", expand=c(0,0)) +
  scale_y_log10("Observed deaths", expand=c(0,0)) +
  scale_color_manual("Prior distribution", values=colors_this) +
  scale_fill_manual("Prior distribution", values=colors_this) +
  facet_wrap(~group_y, nrow=1)+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    strip.background = element_blank(),
    legend.position = "none"
  )

betadata <- lapply(all_summ, function(x) {
  data.frame(
    week=rep(1:52, 6),
    group_y=rep(1:6, each=52),
    est=exp(x$median[grepl("logbeta\\[", x$variable)]),
    lwr=exp(x$q5[grepl("logbeta\\[", x$variable)]),
    upr=exp(x$q95[grepl("logbeta\\[", x$variable)])
  )
}) %>%
  bind_rows(
    .id="group"
  ) %>%
  mutate(
    group_y=factor(group_y,
                 levels=1:6,
                 labels=c("1670-1689",
                          "1690-1709",
                          "1710-1729",
                          "1730-1749",
                          "1750-1769",
                          "1770-1796"))
  )

betadata2 <- bind_rows(
  betadata %>% mutate(week=week-52),
  betadata,
  betadata %>% mutate(week=week+52)
) %>%
  group_by(group, group_y) %>%
  arrange(group, group_y, week) %>%
  mutate(
    est_roll=zoo::rollapply(est, 7, "mean", partial=TRUE)
  ) %>%
  filter(week %in% 1:52)

g3 <- ggplot(betadata) +
  geom_errorbar(aes(week, ymin=lwr, ymax=upr, col=group), width=0, linewidth=0.2,
                position = position_dodge(width=1), alpha=0.5) +
  geom_point(aes(week, est, col=group), size=0.5,
             position = position_dodge(width=1)) +
  geom_line(data=betadata2, aes(week, est_roll, col=group), lwd=1) +
  scale_x_continuous("Week of year", expand=c(0, 0),
                     breaks=c(1,11, 21, 31, 41, 51)) +
  scale_y_continuous("Smoothed\ntransmission rate\n(1/week)", expand=c(0, 0)) +
  coord_cartesian(ylim=c(1, 2.5)) +
  scale_color_manual("Prior distribution", values=colors_this) +
  scale_fill_manual("Prior distribution", values=colors_this) +
  facet_wrap(~group_y, nrow=1) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    legend.position = "none",
    strip.background = element_blank()
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
  )

g4 <- ggplot(epsilondata) +
  geom_ribbon(aes(year, ymin=lwr, ymax=upr, fill=group), alpha=0.3) +
  geom_line(aes(year, est, col=group)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_log10("Multiplicative\nchanges in\ntransmission",
                breaks=c(0.5, 1, 2, 4), limits=c(0.45, 4.5), expand=c(0, 0),
                labels=c("1/2", "1", "2", "4")) +
  geom_hline(yintercept=1, lty=2) +
  scale_color_manual("Prior distribution", values=colors_this) +
  scale_fill_manual("Prior distribution", values=colors_this) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

gcomb <- ggarrange(g1, g2, g3, g4, ncol=1,
                   labels=c("A", "B", "C", "D"),
                   draw=FALSE)

ggsave("figure_stanfit_sep.pdf", gcomb, width=8, height=8)
