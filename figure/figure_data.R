library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times"))
library(egg)
source("../script/script_data.R")

data_filter <- data_final %>%
  filter(year < 1796)

g1 <- ggplot(data_filter) +
  annotate("rect", xmin=1721, xmax=1728, ymin=-Inf, ymax=Inf, fill="#D9F2FF") +
  annotate("rect", xmin=1728, xmax=1740, ymin=-Inf, ymax=Inf, fill="#AED8EB") +
  annotate("rect", xmin=1740, xmax=1768, ymin=-Inf, ymax=Inf, fill="#8FC5DC") +
  annotate("rect", xmin=1768, xmax=1790, ymin=-Inf, ymax=Inf, fill="#6EB1CC") +
  annotate("rect", xmin=1790, xmax=1796, ymin=-Inf, ymax=Inf, fill="#3F94B6") +
  geom_vline(xintercept=unique(data_filter$year), lty=3, col="gray70") +
  geom_line(aes(year+week/52, smpx)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly deaths", expand=c(0, 0), limits=c(0, 180)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

g2 <- ggplot(data_filter) +
  geom_vline(xintercept=unique(data_filter$year), lty=3, col="gray70") +
  geom_line(aes(year+week/52, birth)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Weekly births") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

g3 <- ggplot(data_filter) +
  geom_vline(xintercept=unique(data_filter$year), lty=3, col="gray70") +
  geom_line(aes(year+week/52, population/1e6)) +
  scale_x_continuous("Year", expand=c(0, 0)) +
  scale_y_continuous("Population size\n(millions)",
                     breaks=c(0.4, 0.6, 0.8, 1), limits=c(0.4, 1)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=1),
    axis.title.x = element_blank()
  )

gcomb <- ggarrange(g1, g2, g3, nrow=3,
                   labels=c("A", "B", "C"))

ggsave("figure_data.pdf", gcomb, 
       width=6, height=8*3/5)
