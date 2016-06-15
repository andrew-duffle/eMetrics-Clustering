#Load up a few packages we need. You will have to uncomment the first time you install
#install.packages('ggplot2')
#install.packages('cluster')
#install.packages('useful')
#install.packages('NbClust')
#install.packages('rgl')
#install.packages('dbscan')

library(ggplot2)    
library(cluster)   
library(useful)     
library(NbClust)   
library(rgl) 
library(dbscan)


#Helpful function to evaluate the optimal number of clusters
wssplot <- function(data, nc=15){                    
  
  par(mfrow=c(1,2))
  
  wss <- NULL  
  pctExp <-NULL
  
  for (k in 1:nc)
  {
    kclus <- kmeans(data, centers=k)
    wss[k] <- kclus$tot.withinss      #store the total within SSE for given k
    pctExp[k] <- 1-wss[k]/kclus$totss
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  plot(1:nc, pctExp, type="b", xlab="Number of Clusters",
       ylab="Pct Explained")
  
  par(mfrow=c(1,1))
}

#Great way to browse and load files instead of setting work directories.
#From here browse to find the provided sample data. 
f<-file.choose()
Product_data<-read.csv(f,header = TRUE, sep = ",")
Product_data[,2:9]<-scale(Product_data[,2:9])

# Try the function 
wssplot(Product_data[,2:9],nc=15)

# I also really like this package. It will run multiple test to determine the optimal clusters.
#It might take a few minutes to run. 
NbClust(Product_data[,2:9],method="kmeans")

#While the majority rule says three on very large data sets this will most likely not be the case. 
#Below let's evaluate one of the larger numbers of k using Hartigan's rule.  General rule of thumb 
#is to stop at n-1 clusters after the value of the cluster added drops below the threshold. You can 
#see here why Hartigan's rule suggests 10 clusters. This works great for large data sets. 

clusFit1<-FitKMeans(Product_data[,2:9],max.clusters=30,nstart=20)   
clusFit1
PlotHartigan(clusFit1)

#For our first clustering example we will use the most basic of clustering partitioned clustering 
#or more specifically kmeans. Kmeans is focused on minimizing the R2 distance of data points.  
#We will stick with 3 clusters for simplicity.

productKM <- kmeans(Product_data[,2:9],3, nstart=10) 
KMProduct<-Product_data
KMProduct$cluster <- as.factor(productKM$cluster)

#Quick view of how many are in each cluster
table(KMProduct$cluster)

#Fast and simple way to pull a sample of the data to paste to Excel and evaluate
write.table((KMProduct[sample(1:nrow(KMProduct),100,replace=FALSE),]),"clipboard",sep="\t",row.names = FALSE)

dev.off()
#Next we will try some hierarchical clustering using euclidean distance and the ward method. This is
#the one based on experience that is the easiest to communicate yet still works very well. 
dist <- dist(Product_data[,2:9], method="euclidean") 
hc <- hclust(dist, method="ward")
plot(hc, labels=FALSE)

#Based on the majority rule 2 is the optimal number of clusters. This is where domain knowledge comes
#in and you have to make some decisions. I feel that 2 is too obvious of a cluster and does not give 
#enough flexibility. I suggest creating your split on the next highest cluster recommendations which is 9.    
NbClust(Product_data[,2:9],method="ward.D")

dev.off()
plot(hc, labels=FALSE)
rect.hclust(hc, k=9, border="red")  
HProduct<-Product_data
HProduct$hcluster <- as.factor(cutree(hc, k=9))

table(HProduct$hcluster)

#Paste to Excel to review
write.table((HProduct[sample(1:nrow(KMProduct),100,replace=FALSE),]),"clipboard",sep="\t",row.names = FALSE)

#And finally one more example of a very advanced model using density based clustering and the optics 
#package. Density based clustering is very useful for extremely noise data but is very difficult to 
#explain and visualize. Not highly used in industry but can be good to have in our tool belt. 

dev.off()

db<-optics(Product_data[,2:9],eps=1,minPts=20)
db <- optics_cut(db, eps =.65)
plot(db)
dbProduct<-Product_data
dbProduct$cluster<-as.factor(db$cluster)
table(dbProduct$cluster)
DSample<-as.data.frame(dbProduct[sample(1:nrow(dbProduct),100,replace=FALSE),])


#Paste to Excel to review
write.table((dbProduct[sample(1:nrow(KMProduct),100,replace=FALSE),]),"clipboard",sep="\t",row.names = FALSE)

