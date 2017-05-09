#' ---	
#' title: "Clustering and Association Rules"	
#' output:	
#'   pdf_document: default	
#'   html_document: default	
#' ---	
#' 	
#' 	
#' ## Data Mining 2, IC2	
#' 	
#' 	
#' 	
#' Loading Data	
#' 	
#' 	
#' 	
setwd("C:/Data Mining II/Home Works")	
dat1 = read.csv("food_4_association_15s.csv")	
dat2 = read.csv("qry_Food_by_Month.csv")	
#' 	
#' 	
#' 	
#' Iris Data	
#' 	
#' 	
iris1 = iris	
set.seed(10676565)	
	
	
index <- sample(1:nrow(iris1),round(0.9*nrow(iris1)))	
iris1 <- iris1[index,]	
	
	
iris1 = cbind(scale(iris1[,sapply(iris,is.numeric)]),iris[,!sapply(iris,is.numeric)])	
fit <- kmeans(iris1[,1:4], 5) #5 cluster solution	
#Display number of clusters in each cluster	
table(fit$cluster)	
library(fpc)	
plotcluster(iris1[,1:4], fit$cluster)	
fit$centers	
prediction.strength(iris[,1:4], Gmin=2, Gmax=15, M=10,cutoff=0.8)	
#' 	
#' 	
#' 	
#' 	
iris2=iris1[,1:4]	
wss <- (nrow(iris2)-1)*sum(apply(iris2,2,var))	
for (i in 2:12) wss[i] <- sum(kmeans(iris2,	
                                     centers=i)$withinss)	
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")	
#' 	
#' 	
#' 	
d = dist(iris2, method = "euclidean")	
result = matrix(nrow = 14, ncol = 3)	
for (i in 2:15){	
  cluster_result = kmeans(iris2, i)	
  clusterstat=cluster.stats(d, cluster_result$cluster)	
  result[i-1,1]=i	
  result[i-1,2]=clusterstat$avg.silwidth	
  result[i-1,3]=clusterstat$dunn   	
}	
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')	
	
plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')	
#' 	
#' 	
#' Hierarchical clustering	
#' 	
#' 	
#Wards Method or Hierarchical clustering	
#Calculate the distance matrix	
iris2.dist=dist(iris2)	
#Obtain clusters using the Wards method	
seed.hclust=hclust(iris2.dist, method="ward")	
plot(seed.hclust)	
seed.3clust = cutree(seed.hclust,k=2)	
plot(seed.3clust)	
#' 	
#' 	
#' 	
#Centroid Plot against 1st 2 discriminant functions	
#Load the fpc library needed for plotcluster function	
library(fpc)	
#plotcluster(ZooFood, fit$cluster)	
plotcluster(iris2, seed.3clust)	
#' 	
#' 	
#' Cincinnati Zoo Data	
#' 	
#' 	
	
dat2 = scale(dat2)	
fit <- kmeans(dat2, 2) #5 cluster solution	
#Display number of clusters in each cluster	
table(fit$cluster)	
library(fpc)	
plotcluster(dat2, fit$cluster)	
	
#' 	
#' 	
#' 	
	
wss <- (nrow(dat2-1)*sum(apply(dat2,2,var)))	
for (i in 2:12) wss[i] <- sum(kmeans(dat2,	
                                     centers=i)$withinss)	
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")	
#' 	
#' 	
d = dist(dat2, method = "euclidean")	
result = matrix(nrow = 14, ncol = 3)	
for (i in 2:15){	
  cluster_result = kmeans(dat2, i)	
  clusterstat=cluster.stats(d, cluster_result$cluster)	
  result[i-1,1]=i	
  result[i-1,2]=clusterstat$avg.silwidth	
  result[i-1,3]=clusterstat$dunn   	
}	
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')	
	
plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')	
#' 	
#' ```	
#' 	
#' 	
#Wards Method or Hierarchical clustering	
#Calculate the distance matrix	
iris2.dist=dist(dat2)	
#Obtain clusters using the Wards method	
seed.hclust=hclust(iris2.dist, method="ward")	
plot(seed.hclust)	
seed.3clust = cutree(seed.hclust,k=4)	
plot(seed.3clust)	
#' 	
#' 	
#' 	
#Centroid Plot against 1st 2 discriminant functions	
#Load the fpc library needed for plotcluster function	
library(fpc)	
#plotcluster(ZooFood, fit$cluster)	
plotcluster(dat2, seed.3clust)	
#' 	
#' 	
#' Association RUles	
#' 	
#' 	
library(arules)	
TransFood <- read.csv('http://homepages.uc.edu/~maifg/DataMining/data/food_4_association.csv')	
TransFood <- TransFood[, -1]	
dat1 <- as(as.matrix(TransFood), "transactions")	
itemFrequencyPlot(dat1,topN=10)	
	
basket_rules <- apriori(dat1,parameter = list(sup = 0.003, conf = 0.5,target="rules"))	
summary(basket_rules)	
	
#' 	
#' 	
#' 	
inspect(head(basket_rules))	
inspect(subset(basket_rules, size(basket_rules)>3))	
inspect(subset(basket_rules, lift>10))	
	
``	
`	
#' 	
library('arulesViz')	
plot(basket_rules)	
#' 	
#' 	
#' 	
plot(head(sort(basket_rules, by="lift"), 10), method = "graph")	
plot(basket_rules, method="grouped")	
#' 	
#' 	
#' 	
