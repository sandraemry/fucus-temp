library(kableExtra)
library(tidyverse)

growth <- readRDS(file =  "./fucus-temp/model_outputs/growth_anova_output.rds")
germ <- readRDS(file =  "./fucus-temp/model_outputs/germ_anova_output.rds")

table1 <- rbind(germ, growth)

table1$`Source of variation` <- str_replace(table1$`Source of variation`, "Site", "Population")

table1 %>% 
  kableExtra::kable(align = "lccl") %>%
  kableExtra::kable_classic() %>% 
  kableExtra::kable_styling(full_width = T,
                            position = "left",
                            font_size = 16) %>% 
  kableExtra::row_spec(0, bold = T, hline_after = T) %>% 
  kableExtra::pack_rows("Germination", 1, 8) %>% 
  kableExtra::pack_rows("Growth", 9, 16) %>% 
  kableExtra::save_kable("./fucus-temp/model_outputs/table2.jpg")

