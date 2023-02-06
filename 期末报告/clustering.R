#install.packages('corrplot')
#install.packages('factoextra')
#install.packages('ggplot2')
library(corrplot)
library(cluster)
library(factoextra)
data<-read.csv('cleaned.csv',sep=',')
data=data[,-1]
scal.data=scale(data[,-1])
kappa(cor(scal.data))
corr=round(cor(data),1)
corrplot(corr,)
data.pr<-princomp(scal.data,cor=T)
summary(data.pr)
screeplot(data.pr,type="lines")
data.pr

cluster=data[,c(1:4,23:27,5:15)]
cluster=scale(cluster)
corr=round(cor(cluster),1)
corrplot(corr,tl.col='black',
         tl.srt=30 )
kappa(cor(cluster))
options(scipen = 100)
fviz_nbclust(cluster,kmeans,method="wss")+geom_vline(xintercept=3,linetype=2)
set.seed(90)
km<-kmeans(cluster,iter.max=100,center=3,nstart=20)
fviz_cluster(km, cluster, geom = "point",ellipse.type = "convex",
             repel = TRUE,
             labelsize=7,palette = "jco",
             ggtheme = theme_minimal())
cluster1<-round(aggregate(cluster,by=list(km$cluster),FUN=mean),2)
?fviz_cluster
write.csv(cluster1,file='cluser.csv')

