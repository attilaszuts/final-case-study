
# setup -------------------------------------------------------------------

library(tidyverse)
library(factoextra)
library(NbClust)
library(ggalt)
library(ggsci)
library(scales)

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
fviz_contrib(pca_result, "var", axes = 1)
# variables related to sales and profit contribute the most

# plot contributions of variables in PC2
fviz_contrib(pca_result, "var", axes = 2)
# variables related to sales channels contribute the most

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

data2 %>% 
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


# create helper df-s for annotations
clust1 <- data_w_clusters_pca_2 %>% filter(cluster == 1)
clust2 <- data_w_clusters_pca_2 %>% filter(cluster == 2)
clust3 <- data_w_clusters_pca_2 %>% filter(cluster == 3)


# Orange cluster is pretty much the current key accounts, or partners, that have a very high revenue
# I think black should be the other potential partner group worth considering for additional services, 
# as they are the most similar to the current key accounts. (in terms of shop numbers, and types)
data_w_clusters_pca_2 %>% 
  ggplot(aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(show.legend = F) +
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
