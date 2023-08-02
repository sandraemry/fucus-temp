library(tidyverse)
library(here)
library(lubridate)

my_theme <- theme(axis.title.x = element_text(size = 14), 
                  axis.text.y = element_text(size = 12), 
                  axis.title.y = element_text(size = 14), 
                  axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14),
                  panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


################################ Shirley ################################

shirley_salinity <- read_csv("./fucus-temp/data/raw_data/salinity/Race_Rocks/Race_Rocks_-_Daily_Sea_Surface_Temperature_and_Salinity_1921-2021.csv", skip = 1)

shirley_salinity <- shirley_salinity %>% 
  rename(salinity = `SALINITY (PSU)`, temperature = `TEMPERATURE ( C )`, date = `DATE (YYYY-MM-DD)`) %>% 
  select(date, salinity, temperature) %>% 
  mutate(date = ymd(date)) %>% 
  filter(salinity != 999.9) %>% 
  separate(date, into = c("year", "month", "day"), sep = "-", remove = F) %>% 
  unite("month_day", month:day, sep = "-", remove = F) %>% 
  group_by(month) %>% 
  # mutate(min_salinity = min(salinity), mean_salinity = mean(salinity)) %>% 
  ungroup() %>% 
  mutate(site = "shirley") %>% 
  filter(month %in% c("05", "06", "07", "08"))

# mean and min salinity from 2009-2018
shirley_salinity %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% c(1990:2018)) %>% 
  summarise(avg_sal = mean(salinity), sd_sal = sd(salinity), min_sal = min(salinity))


################################ Nanaimo ################################

nanaimo_salinity <- read_csv("./fucus-temp/data/raw_data/salinity/Departure_Bay_PBS/Departure_Bay_PBS_-_Daily_Sea_Surface_Temperature_and_Salinity_1914-2021.csv", skip = 1)

nanaimo_salinity <- nanaimo_salinity %>% 
  rename(salinity = `SALINITY (PSU)`, temperature = `TEMPERATURE ( C )`, date = `DATE (YYYY-MM-DD)`) %>% 
  select(date, salinity, temperature) %>% 
  mutate(date = ymd(date)) %>% 
  filter(salinity != 999.9) %>% 
  separate(date, into = c("year", "month", "day"), sep = "-", remove = F) %>% 
  unite("month_day", month:day, sep = "-", remove = F) %>% 
  group_by(year, month) %>% 
  # mutate(min_salinity = min(salinity), mean_salinity = mean(salinity)) %>% 
  ungroup() %>% 
  mutate(site = "nanaimo") %>% 
  filter(month %in% c("05", "06", "07", "08"))

# mean and min salinity from 2009-2018
nanaimo_salinity %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% c(1990:2018)) %>% 
  summarise(avg_sal = mean(salinity), sd_sal = sd(salinity), min_sal = min(salinity))


############################## COMBINE SITES ############################## 
salinity <- rbind(nanaimo_salinity, shirley_salinity)

salinity_summary <- salinity %>% 
  mutate(year = as.integer(year)) %>% 
  filter(year %in% 1990:2018) %>% 
  group_by(site, month_day) %>% 
  mutate(mean_salinity = mean(salinity), sd_salinity = sd(salinity))


# ggplot(data = salinity %>% filter(year %in% c(2015, 2016, 2017, 2018))) + 
#   geom_line(aes(x = as.Date(month_day, format = "%m-%d"), y = salinity, color = site, linetype = year)) + 
#   scale_color_manual(values = c("#D94801", "#0570B0")) + 
#   theme_classic() + 
#   theme(legend.position = "top")  + 
#   labs(x = "Date", y = "Salinity (psu)") + 
#   scale_x_date(date_breaks = "2 weeks", labels = date_format("%B %d")) + 
#   theme(axis.title.x = element_text(size = 10), 
#         axis.text.y = element_text(size = 8), 
#         axis.title.y = element_text(size = 10), 
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         legend.text = element_text(size = 10),
#         legend.title = element_text(size = 8),
#         panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# salinity_1SD_plot <- ggplot(data = salinity_summary) + 
#   geom_line(aes(x = as.Date(month_day, format = "%m-%d"), y = mean_salinity, color = site)) + 
#   geom_ribbon(aes(x = as.Date(month_day, format = "%m-%d"), 
#                   y = mean_salinity, ymin = mean_salinity - sd_salinity, 
#                   ymax = mean_salinity + sd_salinity, fill = site), 
#               alpha = 0.5) +
#   scale_color_manual(values = c("#D94801", "#0570B0")) + 
#   scale_fill_manual(values = c("#D94801", "#0570B0")) + 
#   theme_classic() + 
#   theme(legend.position = "top")  + 
#   labs(x = "Date", y = "Mean daily salinity from 2009-2018 (psu)") + 
#   scale_x_date(date_breaks = "2 weeks", labels = date_format("%B %d")) + 
#   theme(axis.title.x = element_text(size = 10), 
#         axis.text.y = element_text(size = 8), 
#         axis.title.y = element_text(size = 10), 
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
#         legend.text = element_text(size = 10),
#         legend.title = element_text(size = 8),
#         panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# 
# ggsave(here("fucus-temp", "figures", "salinity_1SD_plot.png"), plot = salinity_1SD_plot)

salinity_2SD_plot <- ggplot(data = salinity_summary) + 
  geom_line(aes(x = as.Date(month_day, format = "%m-%d"), y = mean_salinity, color = site)) + 
  geom_ribbon(aes(x = as.Date(month_day, format = "%m-%d"), 
                  y = mean_salinity, ymin = mean_salinity - sd_salinity, 
                  ymax = mean_salinity + sd_salinity, fill = site), 
              alpha = 0.5, show.legend = F) +
  geom_ribbon(aes(x = as.Date(month_day, format = "%m-%d"), 
                  y = mean_salinity, ymin = mean_salinity - 2*sd_salinity, 
                  ymax = mean_salinity + 2*sd_salinity, fill = site), 
              alpha = 0.25, show.legend = F) + 
  scale_color_manual(values = c("#D94801", "#0570B0"), labels = c("Nanaimo", "Shirley")) + 
  scale_fill_manual(values = c("#D94801", "#0570B0")) + 
  theme_classic() + 
  theme(legend.position = "top")  + 
  labs(x = "", y = "Mean daily salinity from 1990-2018 (psu)", color = "Source Region") + 
  scale_x_date(date_breaks = "2 weeks", labels = date_format("%B %d")) + 
  theme(axis.title.x = element_text(size = 14), 
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(here("fucus-temp", "figures", "salinity_2SD_plot.png"), plot = salinity_2SD_plot)
saveRDS(salinity_2SD_plot, file = "fucus-temp/figures/salinity_2SD_plot")


