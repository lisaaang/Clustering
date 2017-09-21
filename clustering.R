# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))
library(cluster)
library(rattle.data)
library(NbClust)
# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

wineDF <- scale(wine[-1]) 
head(wineDF)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	for (i in 2:nc){
		              set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)
	                }
	              plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
	   }

wssplot(wineDF)

# Exercise 2:
#   * How many clusters does this method suggest?
#     Ans: This method suggests 3 clusters
#   * Why does this method work? What's the intuition behind it?
#     Ans: k-means clustering aims to minimize the within-group dispersion and 
#          maximize the between-group dispersion. The sum of squares is the sum of 
#          squared distance between each member of the cluster and the cluster centroid.
#          As number of clusters increases, the sum of squares should decrease because
#          clusters are smaller and should theoretically have reduced dispersion.
#          The "bend" indicates when further increasing the number of clusters dramatically 
#          "slows" the reduction in sum of squares. ie does not improve the clustering as much.
#          
#   * Look at the code for wssplot() and figure out how it works
#     Ans: wssplot starts with 15 clusters and specific seed(1234). 
#          Using a for loop, it runs a k-means clustering for 2-15 clusters, storing the sum of 
#          within-groups sum of squares in wss[i] for each iteration.
#          This is then plotted out.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

set.seed(1234)
nc <- NbClust(wineDF, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# Ans: This method also suggests 3

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
set.seed(1234)
fit.km <- kmeans(wineDF, 3, nstart=25)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
# Ans: Very good clustering, with only 6 errors (out of 178), giving 96.6% accuracy.

wineFit <- table(wine$Type, fit.km$cluster)
wineFit
  
# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
#   Ans: Looks great!

clusplot(wineDF, fit.km$cluster, main='Wine Type Clusters',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


