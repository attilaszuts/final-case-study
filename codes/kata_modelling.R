
# setup -------------------------------------------------------------------

library(tidyverse)
library(factoextra)
library(NbClust)
library(ggalt)
library(ggsci)
library(scales)
source("codes/helper.R")

# import data -------------------------------------------------------------

data <- readRDS('data/clean/west-coast-protein_clean.rds')

# drop columns with a lot of missing values
data <- data %>% select(-c(amazon_web, size_of_the_shop_m2))

# drop observations where there are missing values
data <- data %>% filter(!is.na(sales_channel_p_gym) & !is.na(population))

# filter numerical variables only
num_data <- data %>% select(where(is.numeric))

# filter key accounts
data2 <- data %>% dplyr::filter(key_accounts == 0)
num_data2 <- data2 %>% select(where(is.numeric))

# PCA ------------------------------------------------------

# this part is without key accounts

# get pc-s
pca_result <- prcomp(num_data2, scale. = TRUE)
print(pca_result)

# calculate variances
variances <- pca_result$sdev^2
variances

# calculate total variance
total_variance <- sum(variances)
total_variance

# get share of variance by component
share_variance_by_component <- variances / total_variance
df_variance <- tibble(
  component = 1:length(variances),
  share_variance = share_variance_by_component
) %>%
  mutate(`Cumulative share of variance` = cumsum(share_variance)) %>% 
  rename(`Share of variance` = share_variance)

# plot the share of variance
ggplot(data = pivot_longer(df_variance, -component)) +
  geom_line(aes(x = component, y = value, color = name), show.legend = F) +
  facet_wrap(~ name, scales = "free_y") +
  theme_classic() + 
  scale_color_manual(values = pal_futurama("planetexpress")(12)[c(1,4)])

# plot contributions of variables in PC1
pl_cont_1 <- fviz_contrib(pca_result, "var", axes = 1)
pl_cont_1
# variables related to sales and profit contribute the most

# plot contributions of variables in PC2
pl_cont_2 <- fviz_contrib(pca_result, "var", axes = 2)
pl_cont_2
# -sales_channel_p_shop, +sales_channel_p_gym, +company_p_in_shops
# variables related to sales channels contribute the most

# plot contributions of variables in PC3
pl_cont_3 <- fviz_contrib(pca_result, "var", axes = 3)
pl_cont_3
# +webshop, -share of wallet, -num of months ordered, -distributor 

# plot contributions of variables in PC4
pl_cont_4 <- fviz_contrib(pca_result, "var", axes = 4)
pl_cont_4
# +sales channel other, -population, -gym

# loadings on PC2 (whats the difference between cluster 2 and 3)
pc2 <- pca_result$rotation[,2]
pc2_loadings <- data.frame(pc2_vars = names(pc2), loadings = pc2)
row.names(pc2_loadings) <- NULL

pc2_loadings %>% arrange(desc(loadings))

# loadings on PC3 (whats the difference between cluster 2 and 3)
pc3 <- pca_result$rotation[,3]
pc3_loadings <- data.frame(pc3_vars = names(pc3), loadings = pc3)
row.names(pc3_loadings) <- NULL

pc3_loadings %>% arrange(desc(loadings))

# loadings on PC4 (whats the difference between cluster 2 and 3)
pc4 <- pca_result$rotation[,4]
pc4_loadings <- data.frame(pc4_vars = names(pc4), loadings = pc4)
row.names(pc4_loadings) <- NULL

pc4_loadings %>% arrange(desc(loadings))



# visualize partners based on PC1 and PC2
fviz_pca_ind(pca_result, axes = c(1,2))
# they are more separated along PC2

# k-means for PCA
first_two_pc <- as_tibble(pca_result$x[, 1:2])
# create clusters for PCA
km_pca <- kmeans(first_two_pc, centers = 3, nstart = 20)

# add cluster labels to df
data_w_clusters_pca_2 <- mutate(first_two_pc, cluster = factor(km_pca$cluster))

# plot clusters
ggplot(data_w_clusters_pca_2, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  theme_bw() +
  labs( x='\n PC1', y='PC2 \n', title = 'Data split into three clusters (PCA)') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) )
# there are 3 visible clusters

# assign cluster labels to original df (if run with KAs then change to df to data)
data2 <- data2 %>% mutate(cluster_pca = data_w_clusters_pca_2$cluster)

# k-means -----------------------------------------------------------------

# this part is without key accounts

# standardize observations
data_stand <- as.data.frame(scale(num_data2))

# look at the elbow point
fviz_nbclust(data_stand, kmeans, method = "wss")

# use NbCLust to determine K
nb <- NbClust(data_stand, method = "kmeans", min.nc = 2, max.nc = 10, index = "all")
# the best number of clusters is 3

# create 3 clusters
km_3 <- kmeans(data_stand, centers = 3, nstart = 20)

# add cluster labels to df
data_w_clusters_3 <- mutate(data_stand, cluster = factor(km_3$cluster))

# plot observations coloured by clusters
ggplot(data_w_clusters_3, aes(x = sales_channel_p_shop, y = sales_this_year, color = cluster)) +
  geom_point() +
  theme_bw() +
  labs( x='\n Sales channel shop', y='Sales this year \n', title = 'Data split into three clusters (kmeans)') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) )
  #scale_y_log10() +
  #scale_x_log10()

# assign cluster labels to original df (if run with KAs then change to df to data)
data2 <- data2 %>% mutate(cluster_kmeans = data_w_clusters_3$cluster)

# only one observation is put in another cluster according to pca the rest are the same for
# both methods
sum(data2$cluster_kmeans == data2$cluster_pca)

# interpretation ----------------------------------------------------------

pl_sc_sales <- data2 %>% 
  filter(last_year_sales != 0) %>% 
  ggplot(aes(sales_this_year, last_year_sales, color = cluster_pca)) + 
  geom_point(alpha = 0.6, size = 2, show.legend = F) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  labs(x = "Sales this year, USD", y = "Sales last year, USD",
       title = "Correlation between sales this year and last year") + 
  theme_classic() + 
  scale_color_manual(values = pal_futurama()(12)[c(1,4,9)])
pl_sc_sales

# create helper df-s for annotations
clust1 <- data_w_clusters_pca_2 %>% filter(cluster == 1)
clust2 <- data_w_clusters_pca_2 %>% filter(cluster == 2)
clust3 <- data_w_clusters_pca_2 %>% filter(cluster == 3)


# Orange cluster is pretty much the current key accounts, or partners, that have a very high revenue
# I think black should be the other potential partner group worth considering for additional services, 
# as they are the most similar to the current key accounts. (in terms of shop numbers, and types)
pl_cl_12 <- data_w_clusters_pca_2 %>% 
  ggplot(aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(show.legend = F, size = 2) +
  theme_classic() +
  labs( x='\n PC1', y='PC2 \n', title = 'Data split into three clusters (PCA)') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) + 
  scale_color_manual(values = pal_futurama()(12)[c(1,4,9)]) + 
  geom_encircle(aes(PC1, PC2), 
                data=clust1, 
                color="#FF6F00FF", 
                size=2, 
                expand=0.03) +
  geom_encircle(aes(PC1, PC2), 
                data=clust2, 
                color="#8A4198FF", 
                size=2, 
                expand=0.02) +
  geom_encircle(aes(PC1, PC2), 
                data=clust3, 
                color="#3D3B25FF", 
                size=2, 
                expand=0.02) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed")
pl_cl_12 


# join all observations together
data3 <- data2 %>% select(partner_code, cluster_pca) %>%  right_join(data, by = "partner_code")

# cluster 1 shouold be key account
pl_sc_potKA <- data3 %>% 
  dplyr::filter(!(cluster_pca %in% c(3,2))) %>% 
  mutate(cluster_pca = ifelse(is.na(cluster_pca), "Existing KA", "Potential KA")) %>% 
  ggplot(aes(sales_this_year, last_year_sales, color = cluster_pca)) + 
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(1, 10^7)) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2+450, 10^6+5000000))  + 
  theme_classic() + 
  scale_color_manual(values = pal_futurama()(12)[c(7,1)], name = NULL) +
  labs(x = "Sales this year, USD", y = "Sales last year, USD",
       title = "Existing and Potential Key Accounts", 
       subtitle = "") + 
  theme(legend.position = c(0.1, 0.9), legend.title = NULL, legend.background = element_rect(fill = "transparent", color = "transparent"))
pl_sc_potKA  







# table + boxplot + relationship (support our findings with their relationship quality)
options(digits = 4)
data3 %>% 
  group_by(cluster_pca) %>% 
  dplyr::filter(cluster_pca %in% c(2, 3)) %>% 
  mutate(cluster_pca = ifelse(cluster_pca == "2", "Cluster 2", "Cluster 3")) %>% 
  rename(Cluster = cluster_pca) %>% 
  summarise(
    `Share of wallet (%)` = mean(company_p_in_shops) * 100,
    `Estimated sales in gym (%)` = mean(sales_channel_p_gym) * 100,
    `Estimated sales in shop (%)` = mean(sales_channel_p_shop) * 100,
    `Relationship (1-6)` = mean(as.numeric(relationship_quality))
  ) %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "out/cluster2vs3.html")

# differences between 2, 3 in terms of factors


# representative sales channels for key accounts
data3 %>% 
  mutate(cluster_pca = ifelse(is.na(cluster_pca), "4", cluster_pca)) %>% 
  filter(cluster_pca != "1") %>% 
  group_by(key_accounts) %>% 
  summarise(
    mean_shop = mean(sales_channel_p_shop),
    mean_gym = mean(sales_channel_p_gym),
    mean_distributor = mean(sales_channel_p_distributor),
    mean_webshop = mean(sales_channel_p_webshop),
    mean_other = mean(sales_channel_p_other),
    mean_protein = mean(protein_sales_this_year),
    mean_months = mean(number_of_months_when_ordered),
    mean_wallet = mean(company_p_in_shops),
    mean_relationship = mean(as.numeric(relationship_quality)),
    sd_relationship = sd(as.numeric(relationship_quality))
  )


# visualize partners based on PC1 and PC2
fviz_pca_ind(pca_result, axes = c(1,2))
# they are more separated along PC2

# k-means for PCA
first_4_pc <- as_tibble(pca_result$x[, 1:4])
# create clusters for PCA
km_pca_4 <- kmeans(first_4_pc, centers = 3, nstart = 20)

# add cluster labels to df
data_w_clusters_4 <- mutate(first_4_pc, cluster = factor(km_pca_4$cluster))


data_w_clusters_4 %>% 
  ggplot(aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(show.legend = F, size = 2) +
  theme_classic() +
  labs( x='\n PC1', y='PC2 \n', title = 'Data split into three clusters (PCA)') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) + 
  scale_color_manual(values = pal_futurama()(12)[c(4,1 , 9)]) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed")


pl_cl_32 <- data_w_clusters_4 %>% 
  ggplot(aes(x = PC3, y = PC2, color = cluster)) +
  geom_point(show.legend = F, size = 2) +
  theme_classic() +
  labs( x='\n PC3', y='PC2 \n', title = 'Data split into three clusters (PCA)') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) + 
  scale_color_manual(values = pal_futurama()(12)[c(4, 1, 9)]) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed")

pl_cl_42 <- data_w_clusters_4 %>% 
  ggplot(aes(x = PC4, y = PC2, color = cluster)) +
  geom_point(show.legend = F, size = 2) +
  theme_classic() +
  labs( x='\n PC4', y='PC2 \n', title = 'Data split into three clusters (PCA)') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) + 
  scale_color_manual(values = pal_futurama()(12)[c(4, 1, 9)]) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed")

# pl_cl_43 <- 
data_w_clusters_4 %>% 
  ggplot(aes(x = PC4, y = PC3, color = cluster)) +
  geom_point(show.legend = F, size = 2) +
  theme_classic() +
  labs( x='\n PC4', y='PC3 \n', title = 'Data split into three clusters (PCA)') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) + 
  scale_color_manual(values = pal_futurama()(12)[c(1, 4, 9)]) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed")





plot_export()
