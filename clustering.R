# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle", "NbClust"))
library(cluster)
library(rattle)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# rattle is annoying til I figure out quartz
write.table(wine, file = "data/wine.csv", sep = ",", row.names = FALSE)

wine <- read.csv("data/wine.csv")
head(wine)
str(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

winescale <- wine
winescale <- as.data.frame(round(scale(wine), digits = 4))
winescale$Type <- NULL

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: 
# A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",
       main = "Total WSS vs. Number of Clusters (15)")
}

par(mar = c(8, 8, 8, 8), family = "HersheySans")
wssplot(winescale)

# I'd like to try this with a different number of clusters to see what happens.

wssplot02 <- function(data, nc = 64, seed = 64) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "within-groups sum of squares",
       main = "Total WSS vs. Number of Clusters (64)")
}

par(mar = c(8, 8, 8, 8), family = "HersheySans")
wssplot02(winescale)

# The steep decline; the leveling-off; the flattening.

wssplot03 <- function(data, nc = 6, seed = 144) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "within-groups sum of squares",
       main = "Total WSS vs. Number of Clusters (6)")
}

par(mar = c(8, 8, 8, 8), family = "HersheySans")
wssplot03(winescale)


# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works




# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.


library(NbClust)
set.seed(1234)
nc <- NbClust(winescale, min.nc=2, max.nc=15, method="kmeans")

par(mfrow = c(1, 1), mar = c(4, 4, 4, 4), family = "Times")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")



# Exercise 3: How many clusters does this method suggest?


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

set.seed(24)
fit.km <- kmeans(winescale, centers = 3, iter.max = 10, nstart = 3)


# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

clustTab <- table(fit.km$cluster, wine$Type)
clustTab

#      1  2  3
#   1 59  2  0
#   2  0 68  0
#   3  0  1 48

clustDF <- as.data.frame(clustTab)

# Exercise 6:
# * Visualize these clusters using function clusplot() from the cluster library
# * Would you consider this a good clustering?

par(mfrow = c(1, 1), mar = c(8, 8, 8, 8), family = "HersheySans")
clusplot(winescale, clus = fit.km$cluster)

# add clusters to winescale df
wineclust <- winescale
wineclust$cluster <- as.factor(fit.km$cluster)

# create new df with centers of clusters for each variable
centers <- as.data.frame(fit.km$centers)


# check and see how the variables are correlated
library(corrplot)
winecor <- cor(winescale)
par(mfrow = c(2, 2), mar = c(8, 8, 8, 8), family = "Arial Rounded MT Bold")
corrplot(winecor, method = "circle", tl.srt = 45)



# Do more strongly-correlated variables fall more cleanly into clusters?
library(ggplot2)

wcplot <- ggplot(wineclust, aes(x = Alcohol, y = Malic, color = cluster)) +
  geom_point() +
  geom_point(data = centers, aes(x = Alcohol, y = Malic, color = 'Center')) +
  geom_point(data = centers, aes(x = Alcohol, y = Malic, color = 'Center'),
             size = 24, alpha = 0.25) +
  theme_minimal(base_size = 12, base_family = "HersheySans")

wcplot

wcplot02 <- ggplot(wineclust, aes(x = Alcohol, y = Proline, color = cluster)) +
  geom_point() +
  geom_point(data = centers, aes(x = Alcohol, y = Proline, color = 'Center')) +
  geom_point(data = centers, aes(x = Alcohol, y = Proline, color = 'Center'),
             size = 24, alpha = 0.25) +
  theme_minimal(base_size = 12, base_family = "HersheySans") +
  geom_segment(data = vtess$delsgs, aes(x = x1, y = y1, xend = x2, yend = y2),
               color = "red4", alpha = 0.50)

wcplot02

# Voronoi Cells -------------------------------------------

library(deldir)
vtess <- deldir(winescale$Alcohol, winescale$Proline)
vtess_tile <- tile.list(vtess)

library(RColorBrewer)
winepal <- brewer.pal(3, "Reds")

par(mfrow = c(1, 1), mar = c(6, 6, 6, 6), family = "HersheySans")
plot.tile.list(vtess_tile, type = "triang", asp = 1, axes = F, 
               fillcol = winepal)

# ggplot w/ delaunay triangulation
wcplot03 <- ggplot(wineclust, aes(x = Alcohol, y = Proline, color = cluster)) +
  geom_point(size = 2.75) +
  geom_point(data = centers, aes(x = Alcohol, y = Proline, color = 'Center')) +
  geom_point(data = centers, aes(x = Alcohol, y = Proline, color = 'Center'),
             size = 24, alpha = 0.25) +
  theme_minimal(base_size = 12, base_family = "HersheySans") +
  geom_segment(data = vtess$delsgs, aes(x = x1, y = y1, xend = x2, yend = y2),
               color = "red4", alpha = 0.35) +
  labs(title = "Delaunay Triangulation over Alcohol and Proline points")

wcplot03

wcplot04 <- ggplot(wineclust, aes(x = Alcohol, y = Proline, color = cluster)) +
  geom_point(size = 2.75) +
  geom_point(data = centers, aes(x = Alcohol, y = Proline, color = 'Center')) +
  geom_point(data = centers, aes(x = Alcohol, y = Proline, color = 'Center'),
             size = 24, alpha = 0.25) +
  theme_minimal(base_size = 12, base_family = "HersheySans") +
  geom_segment(data = vtess$dirsgs, aes(x = x1, y = y1, xend = x2, yend = y2),
               color = "red4", alpha = 0.35) +
  labs(title = "Voronoi tessellation over Alcohol and Proline points")

wcplot04





