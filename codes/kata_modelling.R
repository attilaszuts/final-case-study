
# setup -------------------------------------------------------------------

library(tidyverse)
library(factoextra)
library(NbClust)

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
  mutate(cum_share_variance = cumsum(share_variance))

# plot the share of variance
ggplot(data = pivot_longer(df_variance, -component)) +
  geom_line(aes(x = component, y = value, color = name)) +
  facet_wrap(~ name, scales = "free_y") +
  theme(legend.position = "bottom")

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
