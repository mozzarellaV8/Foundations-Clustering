# The Original wssplot() function:
#
# wssplot <- function(data, nc=15, seed=1234){
#    wss <- (nrow(data)-1)*sum(apply(data,2,var))
#    for (i in 2:nc){
#    set.seed(seed)
#    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
#       plot(1:nc, wss, type="b", xlab="Number of Clusters",
#       ylab="Within groups sum of squares",
#       main = "Total WSS vs. Number of Clusters (15)")
# }


ssw <- (nrow(winescale)-1)*sum(apply(winescale, 2, var))
# 2477.996

for (i in 2:15) {
  set.seed(123)
  ssw[i] <- sum(kmeans(winescale, centers = i)$withinss)}

plot(1:15, ssw, type = "b", xlab = "Number of Clusters",
       ylab = "within-groups sum of squares")

ssw[2] # 1718.96
ssw[3] # 1278.34
ssw[4] # 1211.149

# put into data frame to view ssw - nc relationship
sswDF <- data.frame(nc = 1:15, ssw = ssw)

# plot each
par(mfcol = c(1, 2), mar = c(4, 4, 4, 4), las = 1, family = "HersheySans")
plot(1:15, ssw, type = "b", xlab = "Number of Clusters",
     ylab = "within-groups sum of squares",
     main = "1:15 (nc) - ssw")
plot(sswDF$nc, sswDF$ssw, type = "b", xlab = "Number of Clusters",
     ylab = "within-groups sum of squares",
     main = "sswDF$nc - sswDF$ssw")
