## survival data for modelling TPCs 

library(tidyverse)

# Sooke -------------------------------------------------------------------

sooke <- read_csv("./data_raw/sooke_survival.csv")

# separate 'plate' into site, temp, salinity, replicate

sooke_tidy <- sooke %>% 
  separate(plate, into = c("site", "temp", "salinity","replicate"), by = "_") %>% 
  mutate(temp = as.numeric(temp), salinity = as.factor(salinity)) %>% 
  mutate(total = (germinated + apical_hairs + ungerminated + dead)) %>% 
  mutate(proportion_alive = (germinated + apical_hairs)/total) %>% 
  arrange(temp, salinity, replicate)

write_csv(sooke_tidy, "./data/sooke_tidy.csv")

ggplot(sooke_tidy, aes(x = temp, y = proportion_alive)) + 
  geom_point(aes(colour = salinity), alpha = 1/2, position = position_jitter(width = 0, height = 0.05)) + 
  scale_x_continuous(limits = c(2, 32), breaks = c(2, 8, 12, 16, 20, 24, 28, 32)) + 
  scale_color_brewer(palette = "Set1") + 
  labs(x = "temperature", y = "proportion alive", subtitle = "Sooke") + 
  theme(
    panel.background = element_blank(), 
    strip.background = element_rect(colour="black",
                                    fill = "lightgrey"), 
    panel.border     = element_blank(), 
    panel.grid.minor = element_line(colour="grey95"),
    panel.grid.major = element_blank(), 
    axis.text.x = element_text(size = 10,
                               hjust = 1,
                               face = "plain"),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12),
    legend.key = element_blank(),
    legend.position  = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

ggsave("./figures/sooke_survival.pdf")

# Nanaimo -----------------------------------------------------------------

nanaimo <- read_csv("./data_raw/nanaimo_survival.csv")

# separate 'plate' into site, temp, salinity, replicate

nanaimo_tidy <- nanaimo %>% 
  separate(plate, into = c("site", "temp", "salinity","replicate"), by = "_") %>% 
  mutate(temp = as.numeric(temp), salinity = as.factor(salinity)) %>% 
  mutate(total = (germinated + apical_hairs + ungerminated + dead)) %>% 
  mutate(proportion_alive = (germinated + apical_hairs)/total) %>% 
  arrange(temp, salinity, replicate)

write_csv(nanaimo_tidy, "./data/nanaimo_tidy.csv")

ggplot(nanaimo_tidy, aes(x = temp, y = proportion_alive)) + 
  geom_point(aes(colour = salinity), alpha = 1/2, position = position_jitter(width = 0, height = 0.05)) + 
  scale_x_continuous(limits = c(2, 32), breaks = c(2, 8, 12, 16, 20, 24, 28, 32)) + 
  scale_color_brewer(palette = "Set1") + 
  labs(x = "temperature", y = "proportion alive", subtitle = "Nanaimo") + 
  theme(
    panel.background = element_blank(), 
    strip.background = element_rect(colour="black",
                                    fill = "lightgrey"), 
    panel.border     = element_blank(), 
    panel.grid.minor = element_line(colour="grey95"),
    panel.grid.major = element_blank(), 
    axis.ticks = element_line(colour = "black"),
    axis.text.x = element_text(size = 10,
                               hjust = 1,
                               face = "plain"),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12),
    legend.key = element_blank(),
    legend.position  = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )



