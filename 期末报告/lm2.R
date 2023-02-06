#install.packages('fastDummies')
#install.packages('ggplot2')
#install.packages('randomForest')
library(rpart)
library(rpart.plot)
library(fastDummies)
library(randomForest)
library(ggplot2)
library('pROC')

data<-read.csv('cleaned.csv',sep=',')
lmdata=data[,-1]

#GLM
set.seed(110)
train.rows <- sample(rownames(lmdata), nrow( lmdata )*0.7)
train.data <- lmdata[train.rows , ]
valid.rows <- setdiff(rownames( lmdata ), train.rows)
valid.data <- lmdata[valid.rows , ]
#glm1
#cor(lmdata)
result=glm(Response~.,data=train.data,family='binomial')
summary(result)
AIC(result)
result=step(result,direction="backward")
p=predict(result,newdata=valid.data,type='response')
confusionMatrix(factor(ifelse(p >= 0.5, 1, 0)), factor(valid.data$Response), positive = "1")
#
c <- as.data.frame(result$coefficients)
c$name <- rownames(c)
colnames(c)[1] <- "coef"
c$odds <- exp(c$coef)
c=c[order(c$odds),]
c=c[-1,]
ggplot(c,aes(x=odds,y=name))+
  geom_bar(stat='identity')+
  geom_vline(aes(xintercept=1),size=.25,linetype='dashed')+
  theme(panel.grid.minor=element_blank())+
  ylab("")+xlab("odds ratio")
  

#default tree
customer.default.tree = rpart(Response ~ ., 
                               data = train.data, 
                               method = "class")
prp( customer.default.tree, 
     type = 1, extra = 1, varlen = -10, 
     box.col = ifelse(customer.default.tree$frame$var == "<leaf>", '#f3f0f1', '#bac3d5'))
customer.default.tree.pred <- predict(customer.default.tree, valid.data, type = "class")
confusionMatrix(customer.default.tree.pred, as.factor(valid.data$Response), positive = "1")

#random forest
customer.rf <- randomForest(as.factor(Response) ~ ., 
                            data = train.data, 
                            ntree = 500,
                            mtry = 4, 
                            nodesize = 5, 
                            importance = TRUE)
summary(customer.rf)
varImpPlot(customer.rf, type = 1,main='variable importance',col='#486090')
customer.rf.pred <- predict(customer.rf, valid.data)
confusionMatrix(customer.rf.pred, as.factor(valid.data$Response), positive = "1")
#roc=roc(valid.data$Response,as.numeric(p))
#plot(roc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),max.auc.polygon=TRUE,print.thres=TRUE)

