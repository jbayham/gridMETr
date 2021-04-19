
library(fixest)
library(ggthemes)


# ---------------------------------
# daily
# ---------------------------------

dfd <- read_rds("../../../../RSTOR/pierce_pm/weighteddata2_census2018.rds")

fd0 <- feols(weightedpm25 ~ weightedhms, data = dfd)
fd1 <- feols(weightedpm25 ~ weightedhms | cbsa, data = dfd)

pd1 <- ggplot(dfd %>% 
               filter(weightedhms > 0) %>% 
               sample_n(1000), aes(x = weightedhms, y = weightedpm25)) +
  geom_point() +
  labs(title = "Daily PM and HMS",
       subtitle = "Scatter of Random Sample of Obs. with HMS > 0",
       caption = "HMS = 1 compared to HMS = 0 is conditionally correlated with an increase of PM between 3.542-3.647",
       y = "Weighted PM",
       x = "Weighted HMS") +
  theme_tufte()

pd1

ggsave("../../../../RSTOR/pierce_pm/pm_hms_comp_daily.png", width = 8, height = 6)



# ---------------------------------
# quarterly
# ---------------------------------

dfq1 <- fread(file = "../../../../RSTOR/pierce_pm/pm_weather_quarterly_quartiles_18.csv")
dfq2 <- fread(file = "../../../../RSTOR/pierce_pm/pm_weather_quarterly_mean_18.csv") %>% 
  mutate(Color = ifelse(weightedhms == 0, "1", "0"))

fq0 <- feols(weightedpm25 ~ weightedhms, data = dfq2)
fq1 <- feols(weightedpm25 ~ weightedhms | cbsa + quarter, data = dfq2)


pq1 <- ggplot(dfq2 %>% 
               # filter(weightedhms > 0) %>%
               sample_n(1000), aes(x = weightedhms, y = weightedpm25)) +
  geom_point(aes(color = Color)) +
  scale_color_manual(values = c("#7765E3","#C8ADC0"), guide = F) +
  labs(title = "Quarterly PM and HMS",
       subtitle = "Scatter of Random Sample of Obs.",
       caption = "HMS = 1 compared to HMS = 0 is conditionally correlated with an increase of PM between 0.034-0.058",
       y = "Weighted PM",
       x = "Weighted HMS") +
  theme_tufte()

pq1

ggsave("../../../../RSTOR/pierce_pm/pm_hms_comp_quarterly.png", width = 8, height = 6)




