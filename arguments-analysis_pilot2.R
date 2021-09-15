library(tidyverse)
library(haven)

norms_codes <- read_csv("raw-data/norms-codes.csv") %>% 
  drop_na(keep) %>% 
  mutate(qult_n = paste0("^", qult_n, "$"))
  
pilot2 <- read_csv("raw-data/Modern_justifications_pilot2.csv") %>% 
  filter(Status != "Survey Preview")


ref_list <- as.data.frame(as.character(pilot2[1, ]))
ref_list <- ref_list %>% 
  rename(desc = `as.character(pilot2[1, ])`) %>% 
  mutate(var_names = names(pilot2)) %>% 
  select(var_names, desc)


pilot2_long <- pilot2 %>% 
  rename(year_born = `year_born#1_1`,
         consent = Q31,
         attn_check = Q9) %>% 
  filter(attn_check != "Strongly agree") %>%
  slice(-1, -2) %>% 
  pivot_longer(matches("^\\d+"),
               names_to = c("item", ".value"),
               names_pattern = "(^\\d+)_(.+)") %>% 
  drop_na(position) %>% 
  pivot_longer(matches("arguments"),
               names_to = "args",
               values_to = "values")

pilot2_long <- pilot2_long %>% 
  mutate(item = str_replace_all(item, set_names(norms_codes$norm, norms_codes$qult_n)),
         args = str_replace_all(args,
                                c("arguments_1" = "It is important and valuable for those who do it.",
                                  "arguments_2" = "It is likely to bring joy to others.",
                                  "arguments_3" = "It goes against the purpose of the situation.",
                                  "arguments_4" = "It is likely to disturb others.",
                                  "arguments_5" = "It is selfish.",
                                  "arguments_6" = "It is likely to hurt someone's feelings.",
                                  "arguments_7" = "It is likely to seem indecent to others.",
                                  "arguments_8" = "It does not conform to how we traditionally behave.",
                                  "arguments_9" = "It shows a lack of respect for authority.")),
         args_shrt = str_replace_all(args,
                                     c("It is important and valuable for those who do it." = "important",
                                       "It is likely to bring joy to others." = "joy",
                                       "It goes against the purpose of the situation." = "purpose",
                                       "It is likely to disturb others." = "disturb",
                                       "It is selfish." = "selfish",
                                       "It is likely to hurt someone's feelings." = "hurt",
                                       "It is likely to seem indecent to others." = "indecent",
                                       "It does not conform to how we traditionally behave." = "conform",
                                       "It shows a lack of respect for authority." = "authority")),
         values = str_replace_all(values,
                                  c("Click to write Scale Point 1" = "Strongly disagree",
                                    "Click to write Scale Point 2" = "Disagree",
                                    "Click to write Scale Point 3" = "Neutral",
                                    "Click to write Scale Point 4" = "Agree",
                                    "Click to write Scale Point 5" = "Strongly agree")))

write_rds(pilot2_long, "clean-data/norms_argument_data_pilot2.rds")






  
