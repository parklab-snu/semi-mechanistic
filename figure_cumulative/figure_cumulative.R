library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("../script/script_data.R")

files <- list.files("../stanfit_cumulative_default", pattern="^stanfit_cumulative_[0-9]+\\.rda$",
                    full.names=TRUE)
years <- as.integer(sub("^stanfit_cumulative_([0-9]+)\\.rda$", "\\1", basename(files)))
files <- files[order(years)]
years <- sort(years)
stopifnot(length(years) > 0)

# The cutoff is exclusive: the 1690 fit uses observations through 1689.
fit_labels <- paste0(min(data_final$year), "-", years-1)
data_filter <- data_final %>% filter(year < max(years))

all_summ <- lapply(seq_along(years), function(i) {
  env <- new.env()
  load(files[i], envir=env)
  dd <- env[[paste0("summary_cumulative_", years[i], "_laplace")]]
  stopifnot(!is.null(dd))
  dd
})

# Match each summary to its own fitting period, keeping parameters in time order.
get_data <- function(variable, trans=identity) {
  lapply(seq_along(years), function(i) {
    data_fit <- data_filter %>% filter(year < years[i])
    time <- if (variable=="logbeta") {
      data.frame(week=1:52)
    } else if (variable=="epsilon") {
      data.frame(year=unique(data_fit$year))
    } else {
      data_fit %>% select(year, week)
    }
    dd <- all_summ[[i]]
    dd <- dd[match(paste0(variable, "[", seq_len(nrow(time)), "]"), dd$variable), ]
    stopifnot(!anyNA(dd$variable))

    data.frame(
      time,
      est=trans(dd$median), lwr=trans(dd$q5), upr=trans(dd$q95),
      group=factor(years[i], levels=years, labels=fit_labels)
    )
  }) %>% bind_rows()
}

fitdata <- get_data("C")
betadata <- get_data("logbeta", exp)
epsilondata <- get_data("epsilon", exp)
Sdata <- get_data("S")

common <- list(
  scale_color_viridis_d("Data used", option="D", end=0.85),
  scale_fill_viridis_d("Data used", option="D", end=0.85),
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank(),
    legend.position = "none"
  )
)

g1 <- ggplot(fitdata) +
  geom_point(data=filter(data_filter, missing==0),
             aes(year+week/52, smpx), size=0.3) +
  geom_ribbon(aes(year+week/52, ymin=lwr, ymax=upr, fill=group), alpha=0.15) +
  geom_line(aes(year+week/52, est, col=group)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly deaths", expand=expansion(mult=c(0, 0.05))) +
  common +
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE)) +
  theme(legend.position="top")

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
  common +
  facet_wrap(~group, nrow=1) +
  theme(
    axis.title.x = element_text(),
    strip.background = element_blank()
  )

g3 <- ggplot(betadata) +
  geom_ribbon(aes(week, ymin=lwr, ymax=upr, fill=group), alpha=0.15) +
  geom_line(aes(week, est, col=group)) +
  scale_x_continuous("Week of year", expand=c(0, 0),
                     breaks=c(1,  21, 41),
                     labels=c("Wk 1", "Wk 21", "Wk 41")) +
  scale_y_continuous("Transmission rate\n(1/week)") +
  facet_wrap(~group, nrow=1) +
  common +
  theme(
    strip.background = element_blank()
  )

g4 <- ggplot(epsilondata) +
  geom_ribbon(aes(year, ymin=lwr, ymax=upr, fill=group), alpha=0.15) +
  geom_line(aes(year, est, col=group)) +
  geom_hline(yintercept=1, lty=2) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_log10("Multiplicative\nchanges in\ntransmission",
                breaks=c(0.5, 1, 2, 4), labels=c("1/2", "1", "2", "4")) +
  common

gcomb <- ggarrange(g1, g2, g3, g4, ncol=1, labels=c("A", "B", "C", "D"))

ggsave("figure_cumulative_default.pdf", gcomb, width=8, height=6)
