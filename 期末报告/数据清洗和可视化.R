library(ggplot2)
#install.packages('waterfall')
library(waterfall)
library(skimr)
library(fastDummies)

##DATA READING
oridata<-read.csv('marketing_campaign.csv',sep='\t')
dim(oridata)
str(oridata)

##DATA CLEANING
clndata=na.omit(oridata)
dim(clndata) #2216   29

#add variables
clndata$ttlspend= clndata$MntWines+clndata$MntFruits+clndata$MntMeatProducts+clndata$MntFishProducts+clndata$MntSweetProducts+clndata$MntGoldProds
clndata$ttlnum= clndata$NumWebPurchases+clndata$NumCatalogPurchases+clndata$NumStorePurchases
clndata$age=2002-clndata$Year_Birth
clndata=clndata[,-2] #remove year birth
clndata$child=clndata$Kidhome+clndata$Teenhome
clndata=clndata[,-5] #remove kid home teen home
clndata=clndata[,-5]
#clndata$age=clndata$age-20
#resign date
clndata$Dt_Customer=as.Date(clndata$Dt_Customer,"%d-%m-%Y")
clndata$Dt_Customer=as.Date("2022-06-20")-clndata$Dt_Customer
clndata$Dt_Customer=as.numeric(clndata$Dt_Customer,units='days')
#clndata$Dt_Customer=round(clndata$Dt_Customer/365,digits=2)
# education
for(i in 1:length(clndata$Education)){
  if(clndata[i,]$Education=='2n Cycle')clndata[i,]$Education='undergraduate'
  if(clndata[i,]$Education=='Basic')clndata[i,]$Education='undergraduate'
}
# marriage
table(clndata$Marital_Status)
for(i in 1:length(clndata$Marital_Status)){
  if(clndata[i,]$Marital_Status=='Absurd')clndata[i,]$Marital_Status=1
  if(clndata[i,]$Marital_Status=='Alone')clndata[i,]$Marital_Status=1
  if(clndata[i,]$Marital_Status=='Divorced')clndata[i,]$Marital_Status=1
  if(clndata[i,]$Marital_Status=='Married')clndata[i,]$Marital_Status=2
  if(clndata[i,]$Marital_Status=='Single')clndata[i,]$Marital_Status=1
  if(clndata[i,]$Marital_Status=='Together')clndata[i,]$Marital_Status=2
  if(clndata[i,]$Marital_Status=='Widow')clndata[i,]$Marital_Status=1
  if(clndata[i,]$Marital_Status=='YOLO')clndata[i,]$Marital_Status=1
}
clndata$Marital_Status=as.numeric(clndata$Marital_Status)
clndata$familymember=clndata$Marital_Status+clndata$child
#clndata$accept=clndata$AcceptedCmp1+clndata$AcceptedCmp2+clndata$AcceptedCmp3+clndata$AcceptedCmp4+clndata$AcceptedCmp5+clndata$Response
clndata=clndata[,-1]#remove ID
#clndata$Income=round(clndata$Income/10000,digits=2)
clndata=clndata[,-23] #remove 2Z 
clndata=clndata[,-23]
clndata <- dummy_cols(clndata ,select_columns = c("Education"),remove_first_dummy = TRUE,remove_selected_columns = TRUE )
dim(clndata)
str(clndata)
skim(clndata)


#options(scipen = 100)
#age outliner remove
boxplot(clndata$age,main='age-before',col='#bac3d5')
clndata=clndata[clndata$age<100,] 
boxplot(clndata$age,main='age-after',col='#d6d8d8')
#income outliner remove
boxplot(oridata$Income,main='income-before',col='#bac3d5')
clndata=clndata[clndata$Income<150000,] #2208,30
boxplot(clndata$Income,main='income-after',col='#d6d8d8')
#meat outliner remove
boxplot(clndata$MntMeatProducts,main='meat-before',col='#bac3d5')
clndata=clndata[clndata$MntMeatProducts<1000,]
boxplot(clndata$MntMeatProducts,main='meat-after',col='#d6d8d8')
#gold outliner remove
boxplot(clndata$MntGoldProds,main='gold-before',col='#bac3d5')
clndata=clndata[clndata$MntGoldProds<250,]
boxplot(clndata$MntGoldProds,main='gold-after',col='#d6d8d8')
#sweet outliner remove
boxplot(clndata$MntSweetProducts,main='sweet-before',col='#bac3d5')
clndata=clndata[clndata$MntSweetProducts<200,]
boxplot(clndata$MntSweetProducts,main='sweet-after',col='#d6d8d8')

write.csv(clndata,file='cleaned.csv')

#descriptive statistics
par(mfrow=c(3,2))
hist(clndata$age, 
     xlab = "age", 
     main = "Histogram of age", 
     col = "#bac3d5",
     breaks = 10,
     labels = TRUE)
hist(clndata$Income, 
     xlab = "income", 
     main = "Histogram of income", 
     col = "#d6d8d8",
     breaks = 10,
     labels = TRUE)
hist(clndata$Dt_Customer, 
     xlab = "days", 
     main = "Histogram of days", 
     col = "#f3f0f1",
     breaks = 4,
     labels = TRUE)
hist(clndata$Recency, 
     xlab = "recency(days)", 
     main = "Histogram of recency", 
     col = "#b1a8ac",
     breaks = 5,
     labels = TRUE)
hist(clndata$Marital_Status, 
     xlab = "marital status", 
     main = "Histogram of marital status: 1 for single 2 for couple", 
     col = "#b1a8ac",
     breaks = 2,
     labels = TRUE)
hist(clndata$child, 
     xlab = "child", 
     main = "Histogram of child", 
     col = "#8a99af",
     breaks = 3,
     labels = TRUE)
#product
summary(clndata[,6:11])
spend=data.frame(Item=as.factor(c('wines','fruits','meat','fish','sweet',
                                   'gold')),
                  data=c(675860,58365,363357,83367,59883,97392))
#ace product:wines,meat
waterfallchart(Item~data,data=spend,col=c('#bac3d5','#d6d8d8'),main='product')
sum(clndata$MntFruits)
#promotion
sum(clndata$AcceptedCmp1) #142
sum(clndata$AcceptedCmp2) #30
sum(clndata$AcceptedCmp3) #163
sum(clndata$AcceptedCmp4) #164
sum(clndata$Response) #333
Campaign_column = c("1st","2nd","3rd","4th","5th","last")
Result_column = c(142,30,163,164,162,333)
Percentage_column = c("142 (6.4%)","30 (1.4%)","163 (7.4%)","164 (7.4%)","162 (7.3%)","333 (15.0%)")
CampaignResult = data.frame(Campaign_column,Result_column,Percentage_column)
plot =barplot( height = CampaignResult$Result_column,
                 names.arg = CampaignResult$Campaign_column,
                 ylim = c(0,350),
                 ylab = "Number of people accepted the offer",
                 main = "effectiveness of promotion campaign",
                 col = "#d6d8d8")
text( x = plot,
      y = CampaignResult$Result_column,
      label = CampaignResult$Percentage_column,
      pos = 3, cex = 0.8, col = "#8a99af")
table(clndata$NumDealsPurchases)
hist(clndata$NumDealsPurchases)
dealsname=as.character(c(0:10))
dealsname=c(dealsname,"above10")
plot= barplot(names.arg=dealsname,
              height=c(34,956,493,293,187,94,60,39,14,8,5,15),
              ylab="number of people",
              main="number of deals made with a discount",
              col='#bac2d5')
#distribution
purchase=data.frame(Item=as.factor(c('web','catalog','store')),
                 data=c(9049,5812,12849))
waterfallchart(Item~data,data=purchase,col=c('#b1a8ac','#bac3d5'),main='distribution')
#most distribution:store





