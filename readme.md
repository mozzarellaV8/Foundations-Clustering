# K-means - clustering exercise

_student work in R_

This exercise is adapted from the book [R in Action](http://www.r-bloggers.com/k-means-clustering-from-r-in-action/) and takes a look at a `wine` dataset.

- [Load and Scale the data](#load-and-scale)
- [Selecting a Number of Clusters: Method 01](#method-01)
- [Selecting a Number of Clusters: Method 02](#method-02)

## Load and Scale

``` r
library(cluster)
library(rattle)
library(NbClust)

data(wine, package = "rattle")
head(wine)
```

We've got 178 rows of observations across 14 variables. A glimpse at the first 24:

![wine data](plots/winedata.png)

First off we'll remove the first column of and then normalizing the data using `scale()`. This method takes the mean and standard deviation of each value, and then normalizes each value by 

1. substrating the mean of all values from each value.
2. dividing each value by the standard deviation of all values. 

I believe this essentially creates [z-score](http://www.r-bloggers.com/r-tutorial-series-centering-variables-and-generating-z-scores-with-the-scale-function/). 

``` r
wine$Type <- NULL
winescale <- as.data.frame(round(scale(wine), digits = 4))
```

## Selecting a Number of Clusters

### Method 01

_Looking for an elbow:_ 

"A plot of the total within-groups sums of squares against the number of clusters in a K-means solution can be helpful. A bend in the graph can suggest the appropriate amount of clusters."

![wssplot](plots/SSW-nc15.png)

**How many clusters does this method suggest?**

My initial thought is that it would suggest 3-5 clusters - but much closer to 3. It feels like 5 if I strain to see them. I'm looking at the 'joints' or 'elbows' of the plot - distinct changes in value, forming into a feature until the next 'bend' in the graph. My thinking is that clusters would form where there's an interval of points that exhibits something resembling a linear relationship - until it reaches the next 'elbow'. 

Essentially: observing a 'steep decline' in WSS (nc 1-3); a 'leveling-off'(nc 4-12, approximately); and a 'flattening out' (nc 12-15, approximately).

**Why does this method work? What is the intuition behind it?**

The _sum of squares within_ is the _distance of each sample_ from _it's groups' mean._ Group in this case is a column, with each value being a member. With the `wine` data, groups would be `Alcohol`, `Color`, `Diluation`, and such. The SSW is found by:

		0. calculate the mean of the group.
		1. find the difference between each sample and the mean of it's group.
		2. square each of these differences.
		3. take the sum of these squares.

So it's the _sum of squared differences_ between _each sample_ and _the mean of it's group._ 

The larger the value for the WSS, the larger the distance would be between each element and it's group's mean. The smaller the WSS, the closer each point would be to it's groups' mean. 

**Look at the code for wssplot() and figure out how it works.**

``` r
wssplot <- function(data, nc=15, seed=1234){
	            wss <- (nrow(data)-1)*sum(apply(data,2,var))
				for (i in 2:nc){
		        	set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}

	                plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                     ylab="Within groups sum of squares",
	                     main = "Total WSS vs. Number of Clusters (15)")
	   }
```

I decided to take the function apart to really see what was going on. To distinguish between my code and the exercise code, I switched `wss` to `ssw`


Here we have the grand sum
``` r
ssw <- (nrow(winescale)-1)*sum(apply(winescale, 2, var)) # 2477.996
```

If we set a number of clusters to 15 for a potential K-means solution, this for loop returns the sum of squares within for each 'number of clusters' from two clusters to 15. It skips the first (nc=1) as we initialized that in the last line of code.

``` r
for (i in 2:15) {
  set.seed(123)
  ssw[i] <- sum(kmeans(winescale, centers = i)$withinss)
}
```

After running through this loop, we have values for the sum of squares of `n` number of clusters from 1 to 15. 

``` r
ssw[2] # 1718.964
```

And to see them all:

```
sswDF <- as.data.frame(ssw = ssw, nc = 1:15)

Just to check:

![wss plot function](plots/wssplot-deconstruct-02.png)




### Method 02

This method uses the function `NbClust()` to analyze criteria, and returns a distribution of the potential number of clusters. 

``` r
library(NbClust)
set.seed(1234)
nc <- NbClust(winescale, min.nc = 2, max.nc = 15, method = "kmeans")
barplot(table(nc$Best.n[1,]),
				xlab = "Number of Clusters", ylab = "Number of Criteria",
				main = "Number of Clusters Chosen by 26 Criteria")
```

**How many clusters does this method suggest?**

This method literally suggests 3 clusters, as shown in the output and barplot:

![NbClust barplot](plots/NbClust-01.png)









