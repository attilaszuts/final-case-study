library(tidyverse)
library(readxl)
library(skimr)
library(GGally)
library(ggsci)
library(ggpubr)
library(scales)

df <- readRDS('data/clean/west-coast-protein_clean.rds')


glimpse(df)
skim(df)

# is there a difference in sales between states? - pretty much no
df %>% 
  ggplot(aes(state, log(gm_this_year), fill = state)) + 
  geom_boxplot(show.legend = F) + 
  theme_classic() + 
  labs(y = "Gross Margin, this year, in log", 
         x = NULL,
       title = "Is there a difference in sales between sales?") + 
  scale_fill_manual(values = pal_futurama("planetexpress")(12)[c(1,4)])

# what is the distribution of gm?
df %>% 
  ggplot(aes(gm_this_year, fill = gm_this_year)) + 
  geom_histogram(aes(y = stat(count) / sum(count)), binwidth = 20000, fill = "#5A9599FF", color = "black", alpha = 0.7) + 
  scale_y_continuous(labels = scales::percent) + 
  theme_classic() + 
  labs(x = "Gross Margin, this year",
       y = "Frequency",
       title = "Distribution of gross margin among partners")

df <- df %>% 
  mutate(ln_gm_this_year = log(gm_this_year)) 

# with normal distribution
df %>% 
  ggplot(aes(ln_gm_this_year)) + 
  geom_histogram(aes(y = stat(count) / sum(count)), bins = 30, fill = "#5A9599FF", color = "black", alpha = 0.7) + 
  scale_y_continuous(labels = scales::percent) +
  stat_function(fun = dnorm, args = list(mean = mean(df$ln_gm_this_year, na.rm = T), sd = sd(df$ln_gm_this_year, na.rm = T)), alpha = 0.2, linetype = "dashed") +
  labs(x = "Gross Margin, this year, in log",
       y = "Frequency",
       title = "Distribution of gross margin, log transfromed",
       subtitle = "Dashed line represents normal distribution curve") + 
  theme_classic()

# is there a difference in sales performance between salesmen? - no
df %>% 
  ggplot(aes(sales_person, log(sales_this_year))) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(
    x = "Sales Person",
    y = "Sales this year, in log($)",
    title = "Is there a difference in sales performance between salesmen?"
  )


## correlation between sales this year and last year

df %>% 
  filter(last_year_sales != 0) %>% 
  ggplot(aes(sales_this_year, last_year_sales)) + 
  geom_smooth(color = "#FF6F00FF", alpha = 0.2) + 
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  labs(x = "Sales this year, USD", y = "Sales last year, USD",
       title = "Correlation between sales this year and last year") + 
  theme_classic()

# by state
df %>%
  filter(last_year_sales != 0) %>% 
  ggplot(aes(sales_this_year, last_year_sales, color = state)) + 
  geom_smooth(alpha = 0.2, show.legend = F) + 
  geom_point(alpha = 0.6, show.legend = T) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  scale_color_manual(values = pal_futurama("planetexpress")(12)[c(1,4)], name = NULL) + 
  labs(x = "Sales this year, USD", y = "Sales last year, USD",
       title = "Correlation between sales this year and last year",
       subtitle = "Conditional on state") + 
  theme_classic() + 
  theme(legend.position = c(0.1, 0.9), legend.title = NULL, legend.background = element_rect(fill = "transparent", color = "transparent"))

# by key accounts
df %>% 
  filter(last_year_sales != 0) %>% 
  ggplot(aes(sales_this_year, last_year_sales, color = key_accounts)) + 
  geom_smooth(alpha = 0.2, show.legend = F) + 
  geom_point(alpha = 0.6, show.legend = T) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  scale_color_manual(values = pal_futurama("planetexpress")(12)[c(1,4)], name = NULL, labels = c("Not Key Account", "Key Account")) + 
  labs(x = "Sales this year, USD", y = "Sales last year, USD",
       title = "Correlation between sales this year and last year",
       subtitle = "Conditional on Key Accounts") + 
  theme_classic() + 
  theme(legend.position = c(0.1, 0.9), legend.title = NULL, legend.background = element_rect(fill = "transparent", color = "transparent"))


# by share of wallet
df %>% 
  filter(last_year_sales != 0) %>% 
  ggplot(aes(sales_this_year, last_year_sales, color = ifelse(company_p_in_shops < 0.5, "<50%", ">=50%"))) + 
  geom_smooth(alpha = 0.2, show.legend = F) + 
  geom_point(alpha = 0.6, show.legend = T) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  scale_color_manual(values = pal_futurama("planetexpress")(12)[c(1,4)], name = NULL) + 
  labs(x = "Sales this year, USD", y = "Sales last year, USD",
       title = "Correlation between sales this year and last year",
       subtitle = "Conditional on Share of Wallet") + 
  theme_classic() + 
  theme(legend.position = c(0.1, 0.9), legend.title = NULL, legend.background = element_rect(fill = "transparent", color = "transparent"))

# share of wallet distribution
df  %>% ggplot(aes(company_p_in_shops)) + 
  geom_histogram(aes(y = stat(count) / sum(count)), bins = 30, fill = "#5A9599FF", color = "black", alpha = 0.7) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_classic() + 
  labs(x = "Share of wallet",
       y = "Frequency",
       title = "Partner share of wallet as estimated by employees")

# by stand alone shop (or multiple)
df %>% 
  filter(last_year_sales != 0) %>% 
  ggplot(aes(sales_this_year, last_year_sales, color = stand_alone_shops)) + 
  geom_smooth(alpha = 0.2, show.legend = F) + 
  geom_point(alpha = 0.6, show.legend = T) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  scale_color_manual(values = pal_futurama("planetexpress")(12)[c(1,4)], name = NULL, labels = c("Multiple shops/webshop(s)", "Only one shop")) + 
  labs(x = "Sales this year, USD", y = "Sales last year, USD",
       title = "Correlation between sales this year and last year",
       subtitle = "Conditional on number of shops") + 
  theme_classic() + 
  theme(legend.position = c(0.2, 0.9), legend.title = NULL, legend.background = element_rect(fill = "transparent", color = "transparent"))


# by relationship quality
df %>% 
  filter(last_year_sales != 0) %>% 
  ggplot(aes(sales_this_year, last_year_sales, color = ifelse(relationship_quality %in% c(1, 2, 3), "Bad", "Good"))) + 
  geom_smooth(alpha = 0.2, show.legend = F) + 
  geom_point(alpha = 0.6, show.legend = T) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  scale_color_manual(values = pal_futurama("planetexpress")(12)[c(1,4)], name = NULL) + 
  labs(x = "Sales this year, USD", y = "Sales last year, USD",
       title = "Correlation between sales this year and last year",
       subtitle = "Conditional on relatiionship quality") + 
  theme_classic() + 
  theme(legend.position = c(0.1, 0.9), legend.title = NULL, legend.background = element_rect(fill = "transparent", color = "transparent"))


df %>% 
  ggplot(aes(relationship_quality)) + 
  geom_bar(aes(y = stat(count) / sum(count)), stat="count", fill = "#5A9599FF", alpha = 0.7, color = "black") + 
  theme_classic() +
  labs(x = "Relationship quality",
       y = "Frequency", 
       title = "Distribution of Relationship quality among partners") + 
  scale_y_continuous(labels = scales::percent) 

# by partner profile
w <- df %>% 
  filter(last_year_sales != 0) %>% 
  filter(partner_profil_oracle == "Webshop") %>% 
  ggplot(aes(sales_this_year, last_year_sales)) + 
  geom_smooth(color = "#FF6F00FF", alpha = 0.2) + 
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  labs(x = NULL, y = NULL) + 
  theme_classic()

s <- df %>% 
  filter(last_year_sales != 0) %>% 
  filter(partner_profil_oracle == "Shop") %>% 
  ggplot(aes(sales_this_year, last_year_sales)) + 
  geom_smooth(color = "#FF6F00FF", alpha = 0.2) + 
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  labs(x = NULL, y = NULL) + 
  theme_classic()

g <- df %>% 
  filter(last_year_sales != 0) %>% 
  filter(partner_profil_oracle == "Gym") %>% 
  ggplot(aes(sales_this_year, last_year_sales)) + 
  geom_smooth(color = "#FF6F00FF", alpha = 0.2) + 
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  labs(x = NULL, y = NULL) + 
  theme_classic()

d <- df %>% 
  filter(last_year_sales != 0) %>% 
  filter(partner_profil_oracle == "Distributor") %>% 
  ggplot(aes(sales_this_year, last_year_sales)) + 
  geom_smooth(color = "#FF6F00FF", alpha = 0.2) + 
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  labs(x = NULL, y = NULL) + 
  theme_classic()

annotate_figure(
  ggarrange(w, s, g, d, 
            labels = c("Webshop", "Shop", "Gym", "Distributor"),
            label.x = 0.05,
            label.y = 1-0.05,
            hjust = c(-0.5, -1, -1, -0.5)), 
  bottom = "Sales this year, in USD", 
  left = "Sales last year, in USD", 
  top = "Correlation between sales this year and last year")

# df %>% ggplot(aes(sales_this_year, last_year_sales)) + 
#   geom_smooth() + 
#   geom_point() + 
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(1, 10^7)) + 
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^2+450, 10^6+5000000)) + 
#   facet_grid(rows = vars(partner_profil_oracle))


# percentage of key accounts
df %>% 
  group_by(state) %>% 
  summarise(ka_count = sum(as.numeric(key_accounts)) / sum(as.numeric(df$key_accounts))) %>% 
  ggplot(aes(state, ka_count)) + 
  geom_bar(stat="identity", fill = "#5A9599FF", alpha = 0.7, color = "black") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme_classic() + 
  geom_text(aes(label=percent(ka_count)), vjust=-1) +
  labs(x = NULL,
       y = NULL, 
       title = "Percentage of key accounts per state")


# profit
df %>% ggplot(aes(log(sales_this_year), log(gm_this_year))) + 
  geom_smooth(color = "#FF6F00FF", alpha = 0.2) + 
  geom_point() + 
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
  #               labels = trans_format("log10", math_format(10^.x)),
  #               limits = c(1, 10^7)) + 
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
  #               labels = trans_format("log10", math_format(10^.x)),
  #               limits = c(10^2+450, 10^6+5000000))  + 
  labs(x = "Sales this year, USD", y = "Sales last year, USD",
       title = "") + 
  theme_classic()

# strong correlation between financial metrics
ggcorr(df)


df %>% 
  ggplot(aes(key_accounts, protein_gm_this_year)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = NULL,
       y = "GM, in USD",
       title = "Gross Margin of protein sales this year") + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_discrete(labels = c("Not Key Account", "Key Account"))
  

# those accounts that fall in the 95% range of key account protein sales should be considered as potential KAs. --> good for validation
sd(df[df$key_accounts == 1, ]$protein_gm_this_year)
