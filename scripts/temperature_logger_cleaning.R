## ibutton data from 2019 - Nanaimo and Shirley 
## February 16th, 2022

library(tidyverse)
library(purrr)
library(here)
library(lubridate)
library(skimr)

# create list of all file paths for csv files in the ibutton folder
cell_files <- c(list.files("./2021/field/data/temperature_fielddata_2021", full.names = TRUE))

# give names to each filepath
names(cell_files) <- cell_files %>% 
  gsub(pattern = ".csv$", replacement = "")

all_cells <- map_df(cell_files, read_csv, col_names = TRUE, skip = 20,
                    .id = "file_name")

all_cells <- all_cells %>% 
  rename(temperature = temp, date_time = time) %>% 
  separate(file_name, into = c("extra", "code"),
           sep = 57, remove = T, fill = "warn") %>% 
  separate(code, into = c("outplant_site", "block"), sep = "_") %>% 
  dplyr::select(outplant_site, block, date_time, temperature ) %>% 
  separate(date_time, into = c("date", "time"), 
           sep = " ", remove = F) %>% 
  mutate(date = ymd(date), 
         time = hms(time),
         outplant_site = factor(outplant_site)) %>% 
  mutate(heat_trmt = factor(case_when(
    outplant_site == "dunbar" & block %in% c("1", "2", "3", "4", "7", "9", "11", "12") ~ "heat_dome_only",
    outplant_site == "dunbar" & block %in% c("5", "6", "8", "10") ~ "previously_heated",
    outplant_site == "deep" & block %in% c("2", "3", "5", "6", "7", "8", "9", "10", "12") ~ "heat_dome_only",
    outplant_site == "deep" & block %in% c("1", "4", "11") ~ "previously_heated"# I'm not including 6 as a heated because the heater broke, so there are only 3 heated blocks 
  ))) 

all_cells <- all_cells %>% 
  mutate(outplant_site = factor(outplant_site, levels = c("dunbar", "deep")))

my_theme <- theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 12), 
                  legend.text = element_text(size = 14),
                  panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# New facet label names for outplant site variable
facet_labs <- c("Dunbar", "Deep Cove")
names(facet_labs) <- c("dunbar", "deep")

#### Only June 16th - simulated heatwave
ggplot(data = all_cells %>% 
         filter( date == "2021-06-16",
                time >= "10H 00M 0S")) + 
  geom_point(aes(x = date_time, y = temperature, color = heat_trmt)) + 
  facet_wrap(~outplant_site, labeller = labeller(outplant_site = facet_labs)) + 
  scale_color_manual(values = c("darkgoldenrod3", "darkcyan"), labels = c("Control", "Heatwave")) + 
  labs(x = "time", y = "Rock Temperature", color = "Heat treatment") + 
  my_theme + 
  theme_classic()

## Fugureing out which 'heated' blocks are cooler than the fucus temp says they should be....
all_cells %>% 
  filter(heat_trmt == "previously_heated", outplant_site == "deep") %>% 
  filter(date == "2021-06-16",
          time >= "10H 00M 0S") %>% 
  group_by(block) %>% 
  summarise(max_temp = max(temperature))
## Block 11 only reached 23.7 but fucus temp was: mean = 24.9, max 44.3

# 
# all_cells %>% 
#   filter(date == "2021-06-16",
#          time >= "10H 00M 0S") %>% 
#   group_by(outplant_site, heat_trmt, block) %>% 
#   summarise(mean_temp = mean(temperature),
#             max_temp = max(temperature))

all_cells %>% group_by(date, outplant_site, heat_trmt, block) %>% summarise(max_temp = max(temperature, na.rm = T)) %>% 
  ggplot(data = .) + 
  geom_point(aes(x = date, y = max_temp, color = heat_trmt)) + 
  facet_wrap(~outplant_site) + 
  geom_smooth(aes(x = date, y = max_temp, color = heat_trmt), method = "gam") + 
  scale_color_manual(values = c("darkgoldenrod3", "darkcyan"), labels = c("Heatwave", "Control")) + 
  my_theme





