# load pre-requisite libraries
library(tidyverse)
library(broom)

# the theme for graphics
# just for asthetic reasons
# does not matter for analysis
graph_theme <- theme_bw() +
  theme(aspect.ratio = 1,
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0))


insurance <- read_csv("insurance.csv")
View(insurance)

nrows <- nrow(insurance)
nrows

ggplot(data = insurance) +
  geom_point(mapping = aes(x = agents, y = saving)) +
  xlim(1, 10) + ylim(1, 10) +
  graph_theme

ggplot(data = insurance) +
  geom_jitter(mapping = aes(x = agents, y = saving)) +
  xlim(1, 10) + ylim(1, 10) +
  graph_theme

# run k-means with 3 segments
k <- 3
(kmeans_res <- kmeans(insurance, centers = k))

(kmeans_fitted <- fitted(kmeans_res))

(kmeans_fitted <- tidy(fitted(kmeans_res)) %>% rename(segment = .rownames))

(insurance_segmented <- insurance %>%
  mutate(segment = factor(kmeans_fitted$segment)))

ggplot(insurance_segmented) +
  geom_jitter(aes(x = agents, y = saving, color = segment)) +
  xlim(1, 10) + ylim(1, 10) +
  graph_theme

#Profile each segment
profile <- kmeans_fitted %>%
  group_by(segment, agents, saving) %>% summarize(size = n() / nrows)
profile

ggplot(insurance_segmented) +
  geom_point(data = profile, mapping = aes(x = agents, y = saving, size = size)) + 
  geom_jitter(aes(x = agents, y = saving, color = segment)) +
  xlim(1, 10) + ylim(1, 10) +
  graph_theme

