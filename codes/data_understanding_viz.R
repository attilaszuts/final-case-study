library(tidyverse)
library(readxl)
library(skimr)
library(GGally)
library(ggsci)
library(ggpubr)

df <- readRDS('data/clean/west-coast-protein_clean.rds')


glimpse(df)
skim(df)

# is there a difference in sales between states? - pretty much no
df %>% 
  ggplot(aes(state, log(gm_this_year), fill = state)) + 
  geom_boxplot() 

# what is the distribution of gm?
df %>% 
  ggplot(aes(gm_this_year, fill = gm_this_year)) + 
  geom_histogram() 

df %>% 
  ggplot(aes(log(gm_this_year), fill = gm_this_year)) + 
  geom_histogram() 

# is there a difference in sales performance between salesmen? - no
df %>% 
  ggplot(aes(sales_person, log(sales_this_year))) + 
  geom_boxplot()


## correlation between sales this year and last year

df %>% ggplot(aes(sales_this_year, last_year_sales)) + geom_smooth() + geom_point() + scale_y_log10() + scale_x_log10()

# by state
df %>% ggplot(aes(sales_this_year, last_year_sales, color = state)) + geom_smooth() + geom_point() + scale_y_log10() + scale_x_log10()

# by key accounts
df %>% ggplot(aes(sales_this_year, last_year_sales, color = key_accounts)) + geom_smooth() + geom_point() + scale_y_log10() + scale_x_log10()

# by share of wallet
df %>% ggplot(aes(sales_this_year, last_year_sales, color = ifelse(company_p_in_shops < 0.5, "small", "large"))) + geom_smooth() + geom_point() + scale_y_log10() + scale_x_log10()

# by stand alone shop (or multiple)
df %>% ggplot(aes(sales_this_year, last_year_sales, color = stand_alone_shops)) + geom_smooth() + geom_point() + scale_y_log10() + scale_x_log10()

# by relationship quality
df %>% ggplot(aes(sales_this_year, last_year_sales, color = ifelse(relationship_quality %in% c(1, 2, 3), "bad", "good"))) + geom_smooth() + geom_point() + scale_y_log10() + scale_x_log10()


# by partner profile
w <- df %>% filter(partner_profil_oracle == "Webshop") %>% ggplot(aes(sales_this_year, last_year_sales)) + geom_smooth() + geom_point() + scale_y_log10() + scale_x_log10()

s <- df %>% filter(partner_profil_oracle == "Shop") %>% ggplot(aes(sales_this_year, last_year_sales)) + geom_smooth() + geom_point() + scale_y_log10() + scale_x_log10()

g <- df %>% filter(partner_profil_oracle == "Gym") %>% ggplot(aes(sales_this_year, last_year_sales)) + geom_smooth() + geom_point() + scale_y_log10() + scale_x_log10()

d <- df %>% filter(partner_profil_oracle == "Distributor") %>% ggplot(aes(sales_this_year, last_year_sales)) + geom_smooth() + geom_point() + scale_y_log10() + scale_x_log10()

ggarrange(w, s, g, d)

# percentage of key accounts
df %>% group_by(state) %>% summarise(ka_count = sum(as.numeric(key_accounts)) / sum(as.numeric(df$key_accounts))) %>% ggplot(aes(state, ka_count)) + geom_bar(stat="identity") + scale_y_continuous(limits = c(0,1))
