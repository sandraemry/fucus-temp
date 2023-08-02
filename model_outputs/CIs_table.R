library(tidyverse)
library(kableExtra)

# Germination -------------------------------------------------------------

param_with_ci_germ <- readRDS("./fucus-temp/model_outputs/param_with_ci_germ.rds")

param_with_ci_germ$parameter_value[param_with_ci_germ$param == "rmax"] <- param_with_ci_germ$parameter_value[param_with_ci_germ$param == "rmax"] * 100
param_with_ci_germ$conf_lower[param_with_ci_germ$param == "rmax"] <- param_with_ci_germ$conf_lower[param_with_ci_germ$param == "rmax"] * 100
param_with_ci_germ$conf_upper[param_with_ci_germ$param == "rmax"] <- param_with_ci_germ$conf_upper[param_with_ci_germ$param == "rmax"] * 100

param_with_ci_germ$salinity[param_with_ci_germ$salinity == "05"] <- "5"

param_with_ci_germ <- param_with_ci_germ %>% 
  select(site, salinity, param, parameter_value, conf_lower, conf_upper) %>% 
  mutate(across(where(is.numeric), round, 1)) %>% 
  mutate(salinity = factor(salinity, levels = c("5", "15", "30"))) %>% 
  arrange(param, site, salinity) 

param_with_ci_germ$site[param_with_ci_germ$site == "jr"] <- "Shirley"
param_with_ci_germ$site[param_with_ci_germ$site == "np"] <- "Nanaimo"

levels(param_with_ci_germ$param) <- c("Pmax", "Topt", "CTmax")

param_with_ci_germ <- param_with_ci_germ %>% 
  rename(`Parameter value` = parameter_value, 
         `Parameter` = param, 
         Site = site,
         `Salinity (psu)` = salinity,
         `lower 95% CI` = conf_lower,
         `upper 95% CI` = conf_upper)

param_with_ci_germ %>% 
  kableExtra::kable(align = "lccccl") %>%
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = T,
                            position = "left",
                            font_size = 16) %>% 
  kableExtra::row_spec(0, bold = T, hline_after = T) %>% 
  kableExtra::save_kable("./fucus-temp/model_outputs/table1a.jpg")



# growth ------------------------------------------------------------------

param_with_ci_growth <- readRDS("./fucus-temp/model_outputs/param_with_ci_growth.rds")

param_with_ci_growth$salinity[param_with_ci_growth$salinity == "05"] <- "5"

param_with_ci_growth <- param_with_ci_growth %>% 
  select(site, salinity, param, parameter_value, conf_lower, conf_upper) %>% 
  mutate(across(where(is.numeric), round, 1)) %>% 
  mutate(salinity = factor(salinity, levels = c("5", "15", "30"))) %>% 
  arrange(param, site, salinity) 

param_with_ci_growth$site[param_with_ci_growth$site == "jr"] <- "Shirley"
param_with_ci_growth$site[param_with_ci_growth$site == "np"] <- "Nanaimo"

levels(param_with_ci_growth$param) <- c("Pmax", "Topt", "CTmax")

param_with_ci_growth <- param_with_ci_growth %>% 
  rename(`Parameter value` = parameter_value, 
         `Parameter` = param, 
         Site = site,
         `Salinity (psu)` = salinity,
         `lower 95% CI` = conf_lower,
         `upper 95% CI` = conf_upper)

param_with_ci_growth %>% 
  kableExtra::kable(align = "lccccl") %>%
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = T,
                            position = "left",
                            font_size = 16) %>% 
  kableExtra::row_spec(0, bold = T, hline_after = T) %>% 
  kableExtra::save_kable("./fucus-temp/model_outputs/table1b.jpg")

