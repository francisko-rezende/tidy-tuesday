library(tidyverse)
library(here)
library(ggbeeswarm)
library(lubridate)

theme_set(theme_minimal())


christmas_songs <- read_csv(here::here("2019", "week-52", "data", "raw", "christmas_songs.csv")) %>% 
  mutate(weekid = mdy(weekid))
  

weeks_on_list <- christmas_songs %>%
  distinct(songid, weeks_on_chart, .keep_all = T)

m1 <- glm(weeks_on_chart ~ weekid, data = weeks_on_list, family = "poisson")
summary(m1)

exp(coef(m1)[2])
  
weeks_on_list %>% ggplot(aes(x = weekid, y = weeks_on_chart)) +
  geom_quasirandom(size = 2.5,
              alpha = .5,
              color = "#B3000C",
              groupOnX = T,
              width = 1) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"),
              color = "#165b33",
              se = F)+
  # scale_x_date(labels = c(seq(1960, 2010, 10))) +
  labs(y = "Number of weeks on billboard charts",
       x = "",
       title = "So this is christmas... And have you done?",
       subtitle = "I, for one, decided to join #TidyTuesday and make this plot showing that the number of\nweeks chritmas songs stay on billboard charts seems to be decreasing with time.\nThe trend line is the product of a GLM w/ Poisson error distribution.",
       caption = "Data: Billboard hot 100-list/Kaggle/data.world\nPlot: @francisko_r") +
  theme(panel.grid = element_line(color = "#729575", size = .2),
        axis.text = element_text(color = "#366640"),
        axis.title = element_text(color = "#366640"),
        title = element_text(color =  "#366640"),
        text = element_text(size = 13,
                            family = "Palatino"),
        panel.background = element_rect(color = "#eeeac5"),
        plot.background = element_rect(colour = "#eeeac5"),
        plot.subtitle = element_text(size = 9.9))


ggsave(filename = "week_num.png", path = here::here("2019", "week-52", "products"), dpi = 1000)
