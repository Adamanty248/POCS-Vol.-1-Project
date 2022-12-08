##packages
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)


#read in data
sentiment<-read.csv("/Users/cailin/Desktop/Pocs/norm_sentiments.csv")
sentiment1<-sentiment
sentiment2<-sentiment
sentiment4<-sentiment
sentiment5<-sentiment
##kmeans clustering

##standardize the data
# create standardization function 
standardize <- function(x) {
  return ((x - mean(x))/sd(x))
}

# create a z-score standardized data frame for easier interpretation
sentiment_z <- as.data.frame(lapply(sentiment, standardize))

RNGversion('3.5.3')
set.seed(2345)
#look at different ks
sent_clusters <- kmeans(sentiment_z, centers = 3)
RNGversion(getRversion())

##evaluate clusters
#look at sent_clusters
table(sent_clusters$cluster)

#look at centers
sent_clusters$centers

center1<-as.data.frame(sent_clusters$centers[1,])
center2<-as.data.frame(sent_clusters$centers[2,])
center3<-as.data.frame(sent_clusters$centers[3,])
names(center1)[1]<-"Sentiment"
names(center2)[1]<-"Sentiment"
names(center3)[1]<-"Sentiment"
center1$prop<-seq.int(nrow(center1)) 
center2$prop<-seq.int(nrow(center2)) 
center3$prop<-seq.int(nrow(center3)) 

#add clusters into data
sentiment$cluster<-factor(sent_clusters$cluster, labels = c(1, 2,3))

##clus 1: got worse over time
##clus2: got better over time
##clus3: got bad, stayed bad



clus1<-sentiment %>% filter(cluster==1)
clus2<-sentiment %>% filter(cluster==2)
clus3<-sentiment %>% filter(cluster==3)

clus1plot<-center1 %>% ggplot(aes(y=Sentiment, x=prop))+
  geom_line()+
  labs( y="Sentiment", title="Cluster 1 Center")
clus2plot<-center2 %>% ggplot(aes(y=Sentiment, x=prop))+
  geom_line()+
  labs( y="Sentiment", title="Cluster 2 Center")
clus3plot<-center3 %>% ggplot(aes(y=Sentiment, x=prop))+
  geom_line()+
  labs( y="Sentiment", title="Cluster 3 Center")


##feed clusters back into classification alg to learn more about model
library(C50)
sentmodel <- C5.0(cluster ~ ., data = sentiment)
summary(sentmodel)

pca1<-fviz_cluster(sent_clusters, data = sentiment)


#########looking at k=2
## cluster1= gets worse, cluster 2= gets better
#uses only x80 for model

RNGversion('3.5.3')
set.seed(2345)
#look at different ks
sent_clusters2 <- kmeans(sentiment_z, centers = 2)
RNGversion(getRversion())

sentiment1$cluster<-factor(sent_clusters2$cluster, labels = c(1, 2))

clus1_2<-sentiment1 %>% filter(cluster==1)
clus2_2<-sentiment1 %>% filter(cluster==2)

center1_2<-as.data.frame(sent_clusters2$centers[1,])
center2_2<-as.data.frame(sent_clusters2$centers[2,])
names(center1_2)[1]<-"Sentiment"
names(center2_2)[1]<-"Sentiment"
center1_2$prop<-seq.int(nrow(center1_2)) 
center2_2$prop<-seq.int(nrow(center2_2)) 

clus1plot_2<-center1_2 %>% ggplot(aes(y=Sentiment, x=prop))+
  geom_line()+
  labs( y="Sentiment", title="Cluster 1 Center")
clus2plot_2<-center2_2 %>% ggplot(aes(y=Sentiment, x=prop))+
  geom_line()+
  labs( y="Sentiment", title="Cluster 2 Center")

sentmodel2 <- C5.0(cluster ~ ., data = sentiment1)
summary(sentmodel2)




#########looking at k=4
#clus1: gradually gets worse
#clus2: gradually gets better
#clus3: gets bad, stays bad
#clus 4: gets good, stays good

#model uses mainly x80, then x92, then x37
RNGversion('3.5.3')
set.seed(2345)
#look at different ks
sent_clusters4 <- kmeans(sentiment_z, centers = 4)
RNGversion(getRversion())

sentiment2$cluster<-factor(sent_clusters4$cluster, labels = c(1, 2, 3, 4))

clus1_4<-sentiment2 %>% filter(cluster==1)
clus2_4<-sentiment2 %>% filter(cluster==2)
clus3_4<-sentiment2 %>% filter(cluster==3)
clus4_4<-sentiment2 %>% filter(cluster==4)

center1_4<-as.data.frame(sent_clusters4$centers[1,])
center2_4<-as.data.frame(sent_clusters4$centers[2,])
center3_4<-as.data.frame(sent_clusters4$centers[3,])
center4_4<-as.data.frame(sent_clusters4$centers[4,])
names(center1_4)[1]<-"Sentiment"
names(center2_4)[1]<-"Sentiment"
names(center3_4)[1]<-"Sentiment"
names(center4_4)[1]<-"Sentiment"

center1_4$prop<-seq.int(nrow(center1_4)) 
center2_4$prop<-seq.int(nrow(center2_4)) 
center3_4$prop<-seq.int(nrow(center3_4)) 
center4_4$prop<-seq.int(nrow(center4_4)) 

clus1plot_4<-center1_4 %>% ggplot(aes(y=Sentiment, x=prop))+
  geom_line()+
  labs( y="Sentiment", title="Cluster 1 Center")
clus2plot_4<-center2_4 %>% ggplot(aes(y=Sentiment, x=prop))+
  geom_line()+
  labs( y="Sentiment", title="Cluster 2 Center")
clus3plot_4<-center3_4 %>% ggplot(aes(y=Sentiment, x=prop))+
  geom_line()+
  labs( y="Sentiment", title="Cluster 3 Center")
clus4plot_4<-center4_4 %>% ggplot(aes(y=Sentiment, x=prop))+
  geom_line()+
  labs( y="Sentiment", title="Cluster 4 Center")

sentmodel2 <- C5.0(cluster ~ ., data = sentiment2)
summary(sentmodel2)


clusterdist<-fviz_cluster(sent_clusters4, data = sentiment2)
