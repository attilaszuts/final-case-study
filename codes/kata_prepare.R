
# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(skimr)


# import data -------------------------------------------------------------

data <- read_excel('data/raw/west-coast-protein_orig.xlsx', sheet = 2)


# clean data --------------------------------------------------------------

# fix variable names
data <- clean_names(data)

# filter for active partners
data <- data %>% dplyr::filter(active_partner == 1)

# skim the data
skim(data)

# recode missing values to NA
data <- data %>% dplyr::mutate( population = ifelse(population == 0, NA, population),
                         size_of_the_shop_m2 = ifelse(size_of_the_shop_m2 == 0, NA, size_of_the_shop_m2),
                         relationship_quality = ifelse(relationship_quality == 0, NA, relationship_quality))

# fix variable types
data <- data %>% dplyr::mutate(sales_channel_p_gym = as.numeric(sales_channel_p_gym),
                        state = as.factor(state),
                        partner_profil_oracle = as.factor(partner_profil_oracle),
                        sales_person = as.factor(sales_person),
                        city_village = as.factor(city_village),
                        region = as.factor(region),
                        relationship_quality = as.factor(relationship_quality),
                        key_accounts = as.factor(key_accounts),
                        stand_alone_shops = as.factor(stand_alone_shops),
                        amazon_web = as.factor(amazon_web),
                        active_partner = NULL
)

# check missing values
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]
# missings in population, size of the shop m2, sales channel p gym and amazon web

# check number of observations from the 2 states
table(data$state)

# save clean data
saveRDS(data, 'data/clean/west-coast-protein_clean.rds')
