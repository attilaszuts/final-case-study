
# setup -------------------------------------------------------------------

library(tidyverse)
library(janitor)


# import data -------------------------------------------------------------

data <- read_excel('data/raw/west-coast-protein_orig.xlsx', sheet = 2)

# fix variable names
data <- clean_names(data)

# filter for active partners
data <- data %>% dplyr::filter(active_partner == 1)

# check number of observations from the 2 states
table(data$state)

# check missing values
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]
# missings in population, size of the shop m2 and amazon web

#