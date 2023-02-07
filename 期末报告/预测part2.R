
data<-read.csv('cleaned.csv',sep=',')
lmdata=cbind(data[,2:5],data[,12:16],data[,25:31])
#wine
lmwine=cbind(lmdata,data[,6])
names(lmwine)[17]='wine'
set.seed(1120)
train.rows <- sample(rownames(lmwine), nrow( lmdata )*0.7)
train.data <- lmwine[train.rows , ]
valid.rows <- setdiff(rownames( lmwine ), train.rows)
valid.data <- lmwine[valid.rows , ]
result1=lm(wine~.,data=train.data)
summary(result1)
AIC(result1)
result=step(result1,direction = 'back')
summary(result)
AIC(result)
p= predict(result,newdata=valid.data,type='response')
valid.resid = valid.data$wine - p
plot(valid.resid)
accuracy(p,actual)
par(mfrow=c(2,2))
plot(result,1:4,col='grey')
res=data.frame( "Predicted" = p[1:10],
            "Actual" = valid.data$wine[1:10],
            "Residual" = valid.resid[1:10])
write.csv(res,file='re.csv')
train.data['1816',]

#meat
lmmeat=cbind(lmdata,data[,8])
names(lmmeat)[17]='meat'
set.seed(1120)
train.rows <- sample(rownames(lmmeat), nrow( lmdata )*0.7)
train.data <- lmmeat[train.rows , ]
valid.rows <- setdiff(rownames( lmmeat ), train.rows)
valid.data <- lmmeat[valid.rows , ]
result1=lm(meat~.,data=train.data)
summary(result1)
AIC(result1)
result=step(result1,direction = 'back')
summary(result)
p= predict(result,newdata=valid.data,type='response')
valid.resid = valid.data$meat- p
#plot(valid.resid)
#accuracy(p,actual)
par(mfrow=c(2,2))
plot(result,1:4,col='grey')
res=data.frame( "Predicted" = p[1:10],
                "Actual" = valid.data$meat[1:10],
                "Residual" = valid.resid[1:10])
#fish
lmfish=cbind(lmdata,data[,9])
names(lmfish)[16]='fish'
set.seed(1120)
train.rows <- sample(rownames(lmfish), nrow( lmdata )*0.7)
train.data <- lmfish[train.rows , ]
valid.rows <- setdiff(rownames( lmfish ), train.rows)
valid.data <- lmfish[valid.rows , ]
result1=lm(fish~.,data=train.data)
summary(result1)
AIC(result1)
result=step(result1,direction = 'back')
summary(result)

#sweet
lmsweet=cbind(lmdata,data[,10])
names(lmsweet)[16]='sweet'
set.seed(120)
train.rows <- sample(rownames(lmfish), nrow( lmdata )*0.7)
train.data <- lmsweet[train.rows , ]
valid.rows <- setdiff(rownames( lmfish ), train.rows)
valid.data <- lmsweet[valid.rows , ]
result1=lm(sweet~.,data=train.data)
summary(result1)
AIC(result1)
result=step(result1,direction = 'back')
summary(result)

#gold
lmgold=cbind(lmdata,data[,10])
names(lmgold)[16]='gold'
set.seed(110)
train.rows <- sample(rownames(lmfish), nrow( lmdata )*0.7)
train.data <- lmgold[train.rows , ]
valid.rows <- setdiff(rownames( lmfish ), train.rows)
valid.data <- lmgold[valid.rows , ]
result1=lm(gold~.,data=train.data)
summary(result1)
AIC(result)
result=step(result1,direction = 'back')
summary(result)

#ttlspned
lmttl=cbind(lmdata,data[,24])
names(lmttl)[17]='ttlspend'
set.seed(200)
train.rows <- sample(rownames(lmttl), nrow( lmdata )*0.7)
train.data <- lmttl[train.rows , ]
valid.rows <- setdiff(rownames( lmttl ), train.rows)
valid.data <- lmttl[valid.rows , ]
result=lm(ttlspend~.,data=train.data)
summary(result)
result1=step(result,direction='backward')
summary(result1)
result2=lm(ttlspend ~ Income + Dt_Customer + NumWebPurchases + 
             NumCatalogPurchases + NumStorePurchases + NumWebVisitsMonth + 
             age + child, data = train.data)
summary(result2)
AIC(result2)
p= predict(result2,newdata=valid.data,type='response')
valid.resid = valid.data$ttlspend- p
par(mfrow=c(2,2))
plot(result2,1:4,col='grey')
res=data.frame( "Predicted" = p[1:10],
                "Actual" = valid.data$ttlspend[1:10],
                "Residual" = valid.resid[1:10])

#
normal=cbind(lmdata,data[,7:11])
normal=normal[,-17]
fruit=lmdata
fruit$fruit=0
for(i in 1:2198){
  if(max(normal[i,16:19])==normal[i,16])fruit[i,]$fruit=1
}
set.seed(100)
train.rows <- sample(rownames(fruit), nrow( lmdata )*0.7)
train.data <- fruit[train.rows , ]
valid.rows <- setdiff(rownames(fruit), train.rows)
valid.data <- fruit[valid.rows , ]
re=glm(fruit~.,data=train.data,family='binomial')
pf=predict(re,newdata=valid.data,type='response')
confusionMatrix(factor(ifelse(p >= 0.5, 1, 0)), factor(valid.data$fruit), positive = "1")

summary(data$MntWines)
summary(data$MntMeatProducts)
summary(data$ttlspend)
