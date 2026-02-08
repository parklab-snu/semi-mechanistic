library(dplyr)
data <- read.table("../Data/London_smallpox.csv", header=TRUE, sep = ",")
pop.data <- read.table("../Data/LondonPopulation1550_1931annual.csv", 
                       header=TRUE, sep = ",")

data_weekly_52 <- data %>%
  rename(
    year=year.from
  ) %>%
  group_by(year) %>%
  arrange(numdate) %>%
  mutate(
    week=1:n()
  ) %>%
  mutate(
    week=ifelse(week==53, 52, week)
  ) %>%
  group_by(year, week) %>%
  summarize(
    smpx=round(mean(smpx)),
    birth=mean(birth.trend)
  ) %>%
  filter(year>= 1670) %>%
  mutate(
    missing=as.numeric(is.na(smpx)),
    smpx=ifelse(is.na(smpx), 0, smpx)
  ) %>%
  arrange(year, week)

popfun <- splinefun(pop.data$Year, pop.data$Total)

population <- popfun(data_weekly_52$year+data_weekly_52$week/52)

death <- diff(population[1]+cumsum(data_weekly_52$birth)-population)

data_final <- data.frame(
  year=data_weekly_52$year,
  week=data_weekly_52$week,
  smpx=data_weekly_52$smpx,
  missing=data_weekly_52$missing,
  birth=data_weekly_52$birth,
  death=c(death, NA),
  population=popfun(data_weekly_52$year+data_weekly_52$week/52)
) %>%
  filter(year < 1840) %>%
  arrange(year, week)
