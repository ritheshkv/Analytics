climate = read.csv("climate_change.csv")

head(climate)

climate_train = subset(climate,Year < 2007 )

climate_test =subset(climate,Year >= 2007)

temp_model = lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data = climate_train)

summary(temp_model)

print(cor(climate_train),digits=1)


temp_model2 = lm(Temp~MEI+TSI+Aerosols+N2O,data = climate_train)

summary(temp_model2)


impr_model = step(temp_model)

summary(impr_model)

test_model = predict(impr_model,newdata = climate_test)

SSE = sum((test_model-climate_test$Temp)^2)

SST = sum((mean(climate_train$Temp)-climate_test$Temp)^2)

R = 1 - SSE/SST

pisaTrain = read.csv("pisa2009train.csv")

pisaTest = read.csv("pisa2009test.csv")

head(pisaTrain)

tapply(pisaTrain$readingScore,pisaTrain$male,mean)

summary(pisaTrain)

table(pisaTrain$raceeth)

str(pisaTrain)

pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)

table(pisaTrain$grade)

summary(pisaTrain$raceeth)

pisaTrain$raceeth = relevel(pisaTrain$raceeth,"White")

summary(pisaTest$raceeth)

pisaTest$raceeth = relevel(pisaTest$raceeth,"White")

lmScore = lm(readingScore~.,data=pisaTrain)

summary(lmScore)

sqrt(sum(lmScore$residuals^2)/nrow(pisaTrain))

vif(lmscore)

predTest <- predict(lmScore,newdata = pisaTest)

summary(predTest)

SSE = sum((pisaTest$readingScore-predTest)^2)

RMSE = sqrt(SSE/nrow(pisaTest))

mean(pisaTrain$readingScore)

SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)


#out of sample R-squared 

R = 1-SSE/SST

FluTrain = read.csv("FluTrain.csv")

head(FluTrain)

which.max(FluTrain$ILI)

FluTrain$Week[303]

which.max(FluTrain$Queries)

hist(FluTrain$ILI)

#since data is skewed use log of dependent variable

plot(FluTrain$Queries,log(FluTrain$ILI))

plot(FluTrain$Queries,FluTrain$ILI)

FluTrend1 = lm(log(FluTrain$ILI)~FluTrain$Queries,data=FluTrain)

summary(FluTrend1)

SSE = sum(FluTrend1$residuals^2)

SST = sum((mean(log(FluTrain$ILI))-log(FluTrain$ILI))^2)

1-SSE/SST

FluTest <- read.csv("FluTest.csv")



FluTest2 = data.frame(FluTest,ILI = log(FluTest$ILI))

keeps = c("Week","ILI.1","Queries")

FluTest2 = FluTest2[keeps]

colnames(FluTest2)[2] = "ILI"

names(FluTest2)



PredTest1 = exp(predict(FluTrend1,newdata =FluTest2))

head(PredTest1)

which(FluTest$Week == "2012-03-11 - 2012-03-17")

PredTest1[11]

#adding variable with time lag using zoo library for irregular time series and 
#getting back the data using coredata

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)

summary(FluTrain$ILILag2)

class(ILILag2)

plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

FluTrend2 = lm(log(FluTrain$ILI)~FluTrain$Queries+log(FluTrain$ILILag2),data = FluTrain)

summary(FluTrend2)

ILILag2 = lag(zoo(FluTest$ILI),-2,na.pad = TRUE)

FluTest$ILILag2 = coredata(ILILag2)

summary(FluTest$ILILag2)

FluTest$ILILag2[1]=FluTrain$ILI[416]

FluTest$ILILag2[2]=FluTrain$ILI[417]


# error:'newdata' had 52 rows but variables found have 417 rows

PredTest2 = exp(predict(FluTrend2,newdata =data.frame(FluTest$Week,log(FluTest$ILI),FluTest$Queries,log(FluTest$ILILag2))))


Elantra = read.csv("Elantra.csv")

ElantraTrain = subset(Elantra,Elantra$Year <= 2012)

ElantraTest = subset(Elantra,Elantra$Year > 2012)

model1 = lm(ElantraSales~Unemployment+CPI_all+CPI_energy+Queries,data=ElantraTrain)

summary(model1)

model2 = lm(ElantraSales~Month+Unemployment+CPI_all+CPI_energy+Queries,data = ElantraTrain)

summary(model2)

str(ElantraTrain)

ElantraTest$MonthFactor = as.factor(ElantraTest$Month)

ElantraTrain$MonthFactor = as.factor(ElantraTrain$Month)

ElantraLM = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFactor, data=ElantraTrain)

summary(ElantraLM)

cor(ElantraTrain)

modelsal = predict(ElantraLM,newdata = ElantraTest)

sum((modelsal-ElantraTest$ElantraSales)^2)

SST = sum((mean(ElantraTrain$ElantraSales)-ElantraTest$ElantraSales)^2)

SSE = sum((ElantraTest$ElantraSales-modelsal)^2)

sqrt(SST/14)

1-SSE/SST

which.max(abs(modelsal - ElantraTest$ElantraSales))

quality = read.csv("quality.csv")

str(quality)

table(quality$PoorCare)

#for logistic regression baseline is the one with highest dependent variable value(mode)

install.packages("caTools")
#package for splitting data

library(caTools)

set.seed(88)

#seed makes sure that split makes exact row "true" or "false" for that seed on any machine

split = sample.split(quality$PoorCare,SplitRatio = 0.75)

summary(split)

class(split)

str(split)

str(quality)

qualityTrain = subset(quality,split==TRUE)

table(qualityTrain$PoorCare)

qualityTest = subset(quality,split==FALSE)

nrow(qualityTest)

QualityLog = glm(PoorCare~OfficeVisits + Narcotics,data =qualityTrain,family= "binomial")


summary(QualityLog)

#AIC is similar to adjusted r-squared it accounts for n and p
  
predictTrain = predict(QualityLog,type = "response")

summary(predictTrain)

tapply(predictTrain,qualityTrain$PoorCare,mean)

QualityLog2 = glm(PoorCare~StartedOnCombination+ProviderCount,data=qualityTrain,family="binomial")

summary(QualityLog2)

predictTrain2 = predict(QualityLog2,type="response")

summary(predictTrain2)

tapply(predictTrain2,qualityTrain$PoorCare,mean)

table(qualityTrain$PoorCare,predictTrain > .5)

install.packages("ROCR")

library(ROCR)

ROCRpred = prediction(predictTrain,qualityTrain$PoorCare)

ROCRperf = performance(ROCRpred,"tpr","fpr")

plot(ROCRperf,colorize=TRUE,print.cutoffs.at = seq(0,1,.1),text.adj = c(-.2,1.7))

predictTest = predict(QualityLog,type = "response",newdata = qualityTest)

table(qualityTest$PoorCare,predictTest > .3)

tapply(predictTest,qualityTest$PoorCare,mean)

ROCRtest = prediction(predictTest,qualityTest$PoorCare)

ROCRperftest = performance(ROCRtest,"tpr","fpr")

plot(ROCRperftest,colorize=TRUE,print.cutoffs.at = seq(0,1,.1),text.adj = c(-2,1.7))

typeof(ROCRperftest)
  
  class(ROCRperftest)
  
 #to calculate AUC 
  
  auc = as.numeric(performance(ROCRtest,"auc")@y.values)
  
  
framingham = read.csv("framingham.csv")
 
library(caTools)

set.seed(1000)

split = sample.split(framingham$TenYearCHD,SplitRatio = .65)

train =subset(framingham,split==TRUE)

test = subset(framingham,split==FALSE)

#dot indicates all variables

framinghamLog = glm(TenYearCHD~.,data=train,family = "binomial")

summary(framinghamLog)

predictTest = predict(framinghamLog,type="response",newdata =test )

table(test$TenYearCHD,predictTest > .5)

library(ROCR)

ROCRpred = prediction(predictTest,test$TenYearCHD)

as.numeric( performance(ROCRpred,"auc")@y.values)

tapply(predictTest,test$TenYearCHD,mean,na.rm =TRUE)
  
polling = read.csv("PollingData.csv")

str(polling)

summary(polling)

install.packages("mice")

library(mice)

simple = polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]

summary(simple)

set.seed(144)

imputed = complete(mice(simple))

summary(imputed) 

polling$Rasmussen = imputed$Rasmussen

polling$SurveyUSA = imputed$SurveyUSA

summary(polling)

polling = read.csv("PollingData_Imputed.csv")

Train = subset(polling,Year == 2004 | Year == 2008 )

Test = subset(polling,Year ==2012)

table(Train$Republican)

table(sign(Train$Rasmussen))

table(Train$Republican,sign(Train$Rasmussen))


cor(Train)

str(Train)

cor(Train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])

mod1 = glm(Republican~PropR,data = Train,family = "binomial")

summary(mod1)

pred1 = predict(mod1,type = "response")

table(Train$Republican,pred1 > .5)

mod2 = glm(Republican~SurveyUSA+DiffCount,data = Train,family="binomial")

pred2 = predict(mod2,type = "response")

table(Train$Republican,pred2 > .5)

summary(mod2)


table(Test$Republican,sign(Test$Rasmussen))


TestPrediction = predict(mod2,type = "response",newdata = Test)

table(Test$Republican,TestPrediction >= .5)

#here we dont care about ROC,because we want accuracy of the mode,not whether we 
#predict one for another(one sort of error)


MichaelJack = subset(songs,artistname == "Michael Jackson")

table(MichaelJack$Top10)

table(songs$timesignature)

songs$songtitle[which.max(songs$tempo)]


songTrain = subset(songs,year <= 2009)

songTest = subset(songs,year %in% 2010)

#exclude the below variables

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

songTrain = songTrain[,!(names(songs) %in% nonvars)]

songTest = songTest[,!(names(songs) %in% nonvars)]

SongsLog1 = glm(Top10 ~ ., data=songTrain, family=binomial)

summary(SongsLog1)

cor(songTrain$loudness,songTrain$energy)

#for numeric variables,we can exclude them using '-'  operator

SongsLog2 = glm(Top10 ~ . - loudness, data=songTrain, family=binomial)

summary(SongsLog2)


SongsLog3 = glm(Top10 ~ . - energy, data=songTrain, family=binomial)

summary(SongsLog3)

predtest = predict(SongsLog3,newdata = songTest,type ="response" )

table(songTest$Top10,predtest > .45)

parole = read.csv("parole.csv")

table(parole$violator)


parole$state = as.factor(parole$state)

parole$crime <- as.factor(parole$crime)

summary(parole$male)

str(parole$state)

table(parole$state)

set.seed(144)

library(caTools)

split = sample.split(parole$violator, SplitRatio = 0.7)

train = subset(parole, split == TRUE)

test = subset(parole, split == FALSE)

sum(train1!=train)

nrow(train)

install.packages("compare")

library(compare)

compa = compare(train,train1,allowAll = TRUE)

compa$tM

remove(train1)

paroletrain  = glm(violator~.,data = train,family=binomial)

summary(paroletrain)

predparol = predict(paroletrain,type = "response",newdata = test)

summary(predparol)

table(test$violator,predparol >=.5)

tapply(predparol,test$violator,mean)

library(ROCR)

lk = prediction(predparol,test$violator)

as = performance(lk,"auc")@y.values

plot(as,print.cutoffs.at = seq(0,1,.1),text.adj =c(-1.7,.4))

loans = read.csv("loans.csv")

summary(loans)

table(loans$not.fully.paid)

#create dataframe of missing values

missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))


loansim = read.csv("loans_imputed.csv")

summary(loansim)

summary(loans)

library(mice)

set.seed(144)

#except dependent variable all other variables are used 

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed

summary(loans)

library(caTools)

set.seed(144)

split = sample.split(loansim$not.fully.paid,SplitRatio = .70)

Train = subset(loansim,split==TRUE)

Test = subset(loansim,split==FALSE)


modl1 = glm(not.fully.paid~.,data = Train,family = "binomial")

summary(modl1)


predicted.risk = predict(modl1,newdata = Test,type="response")

Test$predicted.risk = predicted.risk

table(Test$not.fully.paid,Test$predicted.risk >=.5)

library(ROCR)

pred = prediction(Test$predicted.risk,Test$not.fully.paid)

auc = as.numeric((performance(pred,"auc"))@y.values)

#bivariate logistic regression means only single independent variable

modl2 = glm(not.fully.paid~int.rate,data = Train,family="binomial")

summary(modl2)


class(predicted.risk2) = predict(modl2,newdata = Test,type="response")

max(predicted.risk2)

table(predicted.risk2>=.5)

pred2 = prediction(predicted.risk2,Test$not.fully.paid)

perf3 = performance(pred2,"auc")@y.values

Test$profit = exp(Test$int.rate*3)-1

Test$profit[Test$not.fully.paid==1] = -1

mean(Test$profit)

table(Test$not.fully.paid)

summary(Test$int.rate)

highinterest = subset(Test,int.rate >= .15)

  
mean(highinterest$profit)

summary(highinterest$not.fully.paid)

cutoff = sort(highinterest$predicted.risk, decreasing=FALSE)[100]

selectedLoans =subset(highinterest,predicted.risk <=cutoff)

sum(selectedLoans$profit)

table(selectedLoans$not.fully.paid)

baseball = read.csv("baseball.csv")

length(table(baseball$Year))

baseball = subset(baseball,Playoffs == 1)

table(table(baseball$Year))

PlayoffTable = table(baseball$Year)

names(PlayoffTable)

str(baseball)

#how to access variable from a table

baseball$NumCompetitors=PlayoffTable[as.character(baseball$Year)]

table(baseball$NumCompetitors)

baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)

str(baseball)

table(baseball$WorldSeries)

bas1 = glm(WorldSeries~League,data = baseball,family="binomial")


summary(bas1)

bas2 = glm(WorldSeries~Year+RA+RankSeason+NumCompetitors,data = baseball,family="binomial")

summary(bas2)

#correlation between more than 2 variables

cor(baseball[c("Year","RA","RankSeason","NumCompetitors")])


bas3 = glm(WorldSeries~Year,data =baseball,family="binomial")

bas4 = glm(WorldSeries~RA,data =baseball,family="binomial")

bas5 = glm(WorldSeries~RankSeason,data =baseball,family="binomial")

bas6 = glm(WorldSeries~NumCompetitors,data =baseball,family="binomial")

bas7 = glm(WorldSeries~Year+RA,data =baseball,family="binomial")

bas8 = glm(WorldSeries~Year+RankSeason,data =baseball,family="binomial")

bas9 = glm(WorldSeries~Year+NumCompetitors,data =baseball,family="binomial")

bas10 = glm(WorldSeries~RA+RankSeason,data =baseball,family="binomial")

bas11 = glm(WorldSeries~RA+NumCompetitors,data =baseball,family="binomial")

bas12 = glm(WorldSeries~RankSeason+NumCompetitors,data =baseball,family="binomial")

summary(bas12)

stevens <- read.csv("stevens.csv")

library("caTools")

set.seed(3000)

spl <- sample.split(stevens$Reverse,SplitRatio = .7)

Train = subset(stevens,spl==TRUE)

Test = subset(stevens,spl==FALSE)

install.packages("rpart")

library("rpart")

install.packages("rpart.plot")

library("rpart.plot")

StevensTree = rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=Train,method="class",minbucket=25 )

prp(StevensTree)

predictCART = predict(StevensTree,newdata = Test,type = "class")

table(Test$Reverse,predictCART)

library("ROCR")

#this gives probability of getting 1 and 0

predictROC = predict(StevensTree,newdata = Test)

pred = prediction(predictROC[,2],Test$Reverse)

perf = performance(pred,"tpr","fpr")

plot(perf)


StevensTree2 = rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=Train,method="class",minbucket=5)

prp(StevensTree2)

StevensTree3 = rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=Train,method="class",minbucket=100)

prp(StevensTree3)

#minbucket is minimum number of observations in a subset

install.packages("randomForest")

library(randomForest)

StevensForest = randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = Train,nodesize=25,ntree=200)

#convert dependent variable to factor to not get the error
Train$Reverse = as.factor(Train$Reverse)

Test$Reverse = as.factor(Test$Reverse)


StevensForest = randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = Train,nodesize=25,ntree=200)

predictForest = predict(StevensForest,newdata = Test)

table(Test$Reverse,predictForest)

set.seed(100)

StevensForest1 = randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = Train,nodesize=25,ntree=200)

predictForest1 = predict(StevensForest1,newdata= Test)


table(Test$Reverse,predictForest1)

set.seed(200)

StevensForest2 = randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = Train,nodesize=25,ntree=200)

predictForest2 = predict(StevensForest2,newdata= Test)


table(Test$Reverse,predictForest2)

#K-fold cross validation using caret and e1071

install.packages("caret")

library("caret")

install.packages("e1071")

library("e1071")

numFolds = trainControl(method = "cv",number = 10)

cpGrid = expand.grid(.cp=seq(.01,.5,.01))
  
train(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = Train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

StevensTreeCV = rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = Train,method = "class",cp=.19)

PredictCV = predict(StevensTreeCV,newdata = Test,type = "class")

table(Test$Reverse,PredictCV)

library(rpart.plot)

prp(StevensTreeCV)

#times series example


kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)

kingstimeseries <- ts(kings)

kingstimeseries

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

birthstimeseries <- ts(births, frequency=12, start=c(1946,1))

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")

souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))


plot.ts(kingstimeseries)

install.packages("TTR")

library("TTR")

kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)

plot.ts(kingstimeseriesSMA3)

kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)

remove(kingstimeseriesSMA3)

birthstimeseriescomponents <- decompose(birthstimeseries)

typeof(birthstimeseriescomponents)

plot(birthstimeseriescomponents)


remove(ftrain)

Claims = read.csv("ClaimsData.csv")

str(Claims)

table(Claims$bucket2009)/nrow(Claims)

library("caTools")

set.seed(88)

spl=sample.split(Claims$bucket2009,SplitRatio = .6)

ClaimsTrain <- subset(Claims,spl == TRUE)

ClaimsTest = subset(Claims,spl == FALSE)

mean(ClaimsTrain$age)

summary(ClaimsTrain$diabetes)

table(ClaimsTest$bucket2009,ClaimsTest$bucket2008)

PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow = TRUE,nrow = 5)

#penalty error for baseline model which predicts same for 2009 as 2008

sum(as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)


table(ClaimsTest$bucket2009)[1]/nrow(ClaimsTest)


#-----------------------------------------------------------------------------


head(c,2)


names(c)[7]="MRP"

names(c)


head(new)

newq<-group_by(new,Order.Channel)

summarise(newq,mrpm=mean(MRP))

levels(new$Order.Channel)

str(newq)

library("dplyr")


f<- function(num){
hello<-"Hello, world!"
for(i in seq_len(num)){
  cat(hello)
}
chars<-nchar(hello)*num

}
  
print(f(6))

y<- 10

f<- function(x){
  y<-4
  y^2+g(x)
}

g<-function(x){
                x*y
              }

f(9)

#---------------------------------------------------------------------------------------

ClaimsTree = rpart(bucket2009~age+alzheimers+arthritis+cancer+copd+depression+diabetes+heart.failure+ihd+kidney+osteoporosis+stroke+reimbursement2008+bucket2008,data = ClaimsTrain,method ="class",cp=0.00005)

prp(ClaimsTree)

PredictTest=predict(ClaimsTree,newdata = ClaimsTest,type = "class")

table(ClaimsTest$bucket2009,PredictTest)

ClaimsTree = rpart(bucket2009~age+alzheimers+arthritis+cancer+copd+depression+diabetes+heart.failure+ihd+kidney+osteoporosis+stroke+reimbursement2008+bucket2008,data = ClaimsTrain,method ="class",cp=0.00005,parms = list(loss=PenaltyMatrix))

boston= read.csv("boston.csv")

plot(boston$LON,boston$LAT)

#pch=19 indicates solid dots

points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col="blue",pch=19)

points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red",pch=19)

points(boston$LON[boston$NOX>=.55],boston$LAT[boston$NOX>=.55],col="green",pch=19)

summary(boston$MEDV)


points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)

#the plot shows non-linear relationship between house prices and Lat/Lon

plot(boston$LAT,boston$MEDV)

plot(boston$LON,boston$MEDV)

latlonlm = lm(MEDV~ LAT + LON,data = boston)

summary(latlonlm)

latlonlm$fitted.values

points(boston$LAT[latlonlm$fitted.values>=21.2],boston$LON[latlonlm$fitted.values>=21.2],col="blue",pch="19")

library("rpart")

library("rpart.plot")

latlontree = rpart(MEDV~ LAT + LON,data = boston)

prp(latlontree)

fittedvalues = predict(latlontree)

points(boston$LON[fittedvalues>=21.2],boston$LAT[fittedvalues>=21.2],col="blue",pch="$")

latlontree = rpart(MEDV ~ LAT + LON,data = boston,minbucket =50)

plot(latlontree)

text(latlontree)

abline(v=-71.07)

abline(h=42.17)

abline(h=42.21)

library("caTools")

set.seed(123)

split = sample.split(boston$MEDV,SplitRatio = .7)

train = subset(boston,split==T)

test = subset(boston,split==F)

linreg = lm(MEDV~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM +AGE + DIS +RAD + TAX + PTRATIO,data =  train)

summary(linreg)

linreg.pred = predict(linreg,newdata = test)

linreg.sse = sum((linreg.pred-test$MEDV)^2)

tree = rpart(MEDV~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM +AGE + DIS +RAD + TAX + PTRATIO,data = train)

prp(tree)

tree.pred = predict(tree,newdata = test)

tree.sse = sum((tree.pred-test$MEDV)^2)

library("caret")

library("e1071")

tr.control = trainControl(method = "cv",number = 10)

cp.grid = expand.grid(.cp = (1:10)*.001)

tr = train(MEDV~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,data = train,method = "rpart",trControl = tr.control,tuneGrid = cp.grid)

best.tree = tr$finalModel

prp(best.tree)

best.tree.pred = predict(best.tree,newdata = test)

best.tree.sse = sum((best.tree.pred-test$MEDV)^2)

linreg.sse

vote<-read.csv("gerber.csv")

summary(vote)

tapply(vote$civicduty,vote$voting, mean)

tapply(vote$hawthorne,vote$voting,mean)

tapply(vote$neighbors,vote$voting,mean)

tapply(vote$self,vote$voting,mean)

votlm<- glm(voting~hawthorne+civicduty+neighbors+self,data = vote,family="binomial")

summary(votlm)

predvote<-predict(votlm,type = "response")

table(vote$voting,predvote > .3)

table(vote$voting)

library("ROCR")

ROCRpred <-prediction(predvote,vote$voting)


votearea<-performance(ROCRpred,"auc")

library("rpart")

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors,method="class", data=vote)

library("rpart.plot")

prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors,data=vote, cp=0.0)

prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=vote, cp=0.0)

prp(CARTmodel3)

CARTmodel4 = rpart(voting ~ control + civicduty + hawthorne + self + neighbors + sex, data=vote, cp=0.0)

prp(CARTmodel4)

CARTmodel5 = rpart(voting~ control,data = vote,cp=0.0)

prp(CARTmodel5,digits = 6)

CARTmodel6 = rpart(voting~ control + sex,data = vote,cp=0.0)

prp(CARTmodel6,digits=6)

votlm2 = glm(voting~sex + control,data = vote,family = "binomial")


summary(votlm2)

table(vote$sex)

votlm3 = glm(voting ~ sex + control + sex:control, data=vote, family="binomial")

summary(votlm3)

poss = data.frame(sex = c(0,0,1,1),control = c(0,1,0,1))

predict(votlm2,newdata = poss,type = "response")

predict(votlm3, newdata=poss, type="response")

letterrec =  read.csv("letters_ABPR.csv")

letterrec$isB = as.factor(letterrec$letter == "B")

require("caTools")

set.seed(1000)

split = sample.split(letterrec$isB,SplitRatio = .5)

train = letterrec[split==TRUE,]

test = letterrec[split==FALSE,]

383/sum(table(test$letter))

CARTb = rpart(isB ~ . - letter, data=train, method="class")

predictb=predict(CARTb,newdata = test,type = "class")

table(test$isB,predictb)

install.packages("randomForest")

library("randomForest")

set.seed(1000)

rforestb = randomForest(isB ~ . - letter, data = train)

#random forest will do classification or regression based on class of independent variable

class(train$isB)

predictrf = predict(rforestb,newdata = test)


table(test$isB,predictrf)


letterrec$letter = as.factor(letterrec$letter)

set.seed(2000)

split = sample.split(letterrec$letter,SplitRatio = .5)

train = subset(letterrec,split==T)

test = subset(letterrec,split==F)

table(test$letter)

predb1 = rpart(letter ~ .-isB,data =train,method = "class")

predres = predict(predb1,newdata = test,type = "class")


table(test$letter,predres)

set.seed(1000)

predb2 = randomForest(letter~.-isB,data = train)

predres1 = predict(predb2,newdata = test)

table(test$letter,predres1)


census = read.csv("census.csv")


library("caTools")

set.seed(2000)

split = sample.split(census$over50k,SplitRatio = .6)

train = subset(census,split==T)

test = subset(census,split==F)

censuspred = glm(over50k~.,data = train,family = "binomial")

summary(censuspred)

censuspred1 = predict(censuspred,newdata = test,type = "response")

table(test$over50k,censuspred1>.5)

library("ROCR")

ROCRpred1 = prediction(censuspred1,test$over50k)

ROCRperf1 = performance(ROCRpred,"tpr","fpr")

library("rpart")

library("rpart.plot")

censuspred2 = rpart(over50k~.,data = train,method = "class")


prp(censuspred2)


censuspred3 = predict(censuspred2,newdata = test)


table(test$over50k,censuspred3)

library("ROCR")

ROCRpred = prediction(censuspred3[,2],test$over50k)

ROCRperf = performance(ROCRpred,"tpr","fpr")

plot(ROCRperf)

set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]

library("randomForest")

set.seed(1)

rfcensus = randomForest(over50k~.,data = trainSmall)

rfpredict = predict(rfcensus,newdata = test)

table(test$over50k,rfpredict)

#see which variables are important in a random forest

vu = varUsed(rfcensus, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(rfcensus$forest$xlevels[vusorted$ix]))

#measure the impurity in the model

varImpPlot(rfcensus)

cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

library("caret")

library("e1071")

set.seed(2)

tr.control = trainControl(method = "cv",number = 10)

tr = train(over50k~.,data = train,method = "rpart",trControl = tr.control,tuneGrid = cartGrid)

best.tree = tr$finalModel

CART2 = rpart(over50k~.,data = train,cp=.002)

CARTpred = predict(CART2,newdata = test,type="class")

table(test$over50k,CARTpred)

prp(CART2)


statedata = read.csv("statedataSimple.csv")

statelm = lm(Life.Exp~.,data = statedata)

summary(statelm)

SSE = sum((statelm$fitted.values-statedata$Life.Exp)^2)

statelm2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad,data = statedata)

summary(statelm2)

SSE = sum((statelm2$residuals)^2)

CARTstate = rpart(Life.Exp ~.,data = statedata)

prp(CARTstate)

CARTpred = predict(CARTstate)

SSEcart = sum((CARTpred-statedata$Life.Exp)^2)


CARTstate = rpart(Life.Exp ~.,data = statedata,minbucket = 5)

prp(CARTstate)

CARTpred2 = predict(CARTstate)

SSEcart2 = sum((CARTpred2-statedata$Life.Exp)^2)


CARTstate2 = rpart(Life.Exp ~ Area,data = statedata,minbucket = 1)

CARTpred3 = predict(CARTstate2)

prp(CARTstate2)

SSEcart3 = sum((CARTpred3-statedata$Life.Exp)^2)

set.seed(111)

cartGrid = expand.grid( .cp = seq(0.01,0.5,0.01))

tr.control = trainControl(method = "cv",number = 10)

tr = train(Life.Exp~.,data = statedata,method = "rpart",trControl = tr.control,tuneGrid = cartGrid)

CARTstate3 = rpart(Life.Exp ~ .,data = statedata,cp=.12)

prp(CARTstate3)

predCART = predict(CARTstate3)

SSE4 = sum((predCART-statedata$Life.Exp)^2)

set.seed(111)

cartGrid = expand.grid( .cp = seq(0.01,0.5,0.01))

tr.control = trainControl(method = "cv",number = 10)

tr = train(Life.Exp ~ Area,data = statedata,method = "rpart",trControl = tr.control,tuneGrid = cartGrid)

CARTpred4 = rpart(Life.Exp ~ Area,data=statedata,cp=.02)

prp(CARTpred4)

CARTfit = predict(CARTpred4)

SSEcart5 = sum((CARTfit-statedata$Life.Exp)^2)

####### End of CART,Regression #####

tweets = read.csv("tweets.csv",stringsAsFactors = FALSE)

str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)

library(tm)

library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords,c("apple", stopwords("english")))

corpus = tm_map(corpus, stemDocument)

stopwords("english")[1:10]

length(stopwords("english"))

#use below command for using bag of words approach if there is a problem (for english)

Sys.setlocale("LC_ALL", "C")


frequencies = DocumentTermMatrix(corpus[1])

#look at the matrix created,sparse indicates number of zeroes

inspect(frequencies[1,505:515])

findFreqTerms(frequencies,lowfreq = 20)

#.98 indicates only terms which appear in 2% of terms are retained

sparse = removeSparseTerms(frequencies,.995)

tweetsSparse = as.data.frame(as.matrix(sparse))

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

tweetsSparse$Negative = tweets$Negative

library(CaTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative,SplitRatio = 0.7)

trainSparse = subset(tweetsSparse,split == TRUE)

testSparse = subset(tweetsSparse,split == FALSE)

library(rpart)

library(rpart.plot)

tweetCART = rpart(Negative ~ .,data = train$Sparse,method = "class")

prp(tweetCART)

predictCART = predict(tweetCART,newdata = testSparse,type = "class")

table(testSparse$Negative, predictCART)

library(randomForest)

set.seed(123)

tweetRF = randomForest(Negative ~ .,data = trainSparse)

predictRF = predict(tweetRF,newdata = testSparse)

table(testSparse$Negative,predictRF)

tweetLog = glm(Negative ~ .,data = trainSparse,family = "binomial")

predictions = predict(tweetLog, newdata=testSparse, type="response")

#recitation

emails = read.csv("energy_bids.csv",stringsAsFactors = FALSE)

str(emails)

strwrap(emails$email[1])

library("tm")

corpus = Corpus(VectorSource(emails$email))

strwrap(corpus[[1]])

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords,stopwords("english"))

corpus = tm_map(corpus,stemDocument)

strwrap(corpus[[1]])

dtm =DocumentTermMatrix(corpus)

dtm = removeSparseTerms(dtm,.97)

labeledTerms = as.data.frame(as.matrix(dtm))

labeledTerms$responsive = emails$responsive

str(labeledTerms)

library(caTools)

set.seed(144)

spl = sample.split(labeledTerms$responsive,.7)

train = subset(labeledTerms, spl==TRUE)

test = subset(labeledTerms, spl==FALSE)

library(rpart)

library(rpart.plot)

emailCART = rpart(responsive ~ .,data = train,method = "class")

prp(emailCART)

pred = predict(emailCART,newdata = test)

pred[1:10,]

pred.prob = pred[,2]

table(test$responsive,pred.prob>=.5)

library(ROCR)

predROCR = prediction(pred.prob,test$responsive)

perfROCR = performance(predROCR,"tpr","fpr")

plot(perfROCR,colorize = TRUE)

performance(predROCR,"auc")@y.values


wiki = read.csv("wiki.csv",stringsAsFactors = FALSE)

wiki$Vandal = as.factor(wiki$Vandal)

summary(wiki)

corpusAdded = Corpus(VectorSource(wiki$Added))

corpusAdded  = tm_map(corpusAdded,removeWords,stopwords("english"))

corpusAdded = tm_map(corpusAdded,stemDocument) 

dtmAdded = DocumentTermMatrix(corpusAdded)

dtmAdded = removeSparseTerms(dtmAdded,.997)


wordsAdded = as.data.frame(as.matrix(dtmAdded))

colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))

corpusRemoved = tm_map(corpusRemoved,removeWords,stopwords("english"))

corpusRemoved = tm_map(corpusRemoved,stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)

wordsRemoved = removeSparseTerms(dtmRemoved,.997)

wordsRemoved = as.data.frame(as.matrix(wordsRemoved))

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords = cbind(wordsAdded, wordsRemoved)

wikiWords$Vandal = wiki$Vandal

library(caTools)

set.seed(123)

spl = sample.split(wikiWords$Vandal,.7)

train = subset(wikiWords,spl==T)

test = subset(wikiWords,spl == F)

table(test$Vandal)

wikiCART = rpart(Vandal ~ .,data = train,method = "class")

prp(wikiCART)

wikipred = predict(wikiCART,newdata = test,type ="class")

table(wikipred,test$Vandal)

#since bag of words didn't work, we hypothize presence of website links causes vandalism

wikiWords2 = wikiWords

wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
 
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2,spl==T)

wikiTest2 = subset(wikiWords2,spl==F)

wikiCART2 = rpart(Vandal ~ .,data = wikiTrain2)

prp(wikiCART2)

wikipred2 = predict(wikiCART2,newdata = wikiTest2,type = "class")

table(wikiTest2$Vandal,wikipred2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3,spl==TRUE)

wikiTest3 = subset(wikiWords3,spl==FALSE)

wikiCART3 = rpart(Vandal ~ .,data = wikiTrain3)

prp(wikiCART3)

wikipred3 = predict(wikiCART3,newdata = wikiTest3,type = "class")

table(wikiTest3$Vandal,wikipred3)

trials = read.csv("clinical_trial.csv",stringsAsFactors = FALSE)

max(nchar(trials$abstract))

table(nchar(trials$abstract)==0)

trials$title[which.min(nchar(trials$title))]

library(tm)

corpusTitle = Corpus(VectorSource(trials$title))

corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, PlainTextDocument)

corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle,tolower)

corpusTitle = tm_map(corpusTitle,removePunctuation)

corpusTitle = tm_map(corpusTitle,removeWords,stopwords("english"))

corpusTitle = tm_map(corpusTitle,stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)

dtmTitle = removeSparseTerms(dtmTitle,.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))

ncol(dtmTitle)

#do the same for abstract

corpusAbstract = tm_map(corpusAbstract,tolower)

corpusAbstract = tm_map(corpusAbstract,removePunctuation)

corpusAbstract = tm_map(corpusAbstract,removeWords,stopwords("english"))

corpusAbstract = tm_map(corpusAbstract,stemDocument)

dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmAbstract = removeSparseTerms(dtmAbstract,.95)

dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

ncol(dtmAbstract)

which.max(colSums(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)

dtm$trial = trials$trial

library(caTools)

set.seed(144)

spl = sample.split(dtm$trial,.7)

train = subset(dtm,spl==T)

test = subset(dtm,spl==F)

table(train$trial)

trialCART = rpart(trial ~.,data = train,method = "class")

prp(trialCART)

predictCART = predict(trialCART,type = "class")

max(predictCART[,2])

table(train$trial,predictCART)

predictCART2 = predict(trialCART,newdata = test)

table(test$trial,predictCART2[,2]>=.5)

library(ROCR)

predROCR = prediction(predictCART2[,2],test$trial)

perfROCR = performance(predROCR,"auc")@y.values

emails = read.csv("emails.csv",stringsAsFactors = F)

table(emails$spam)

emails$text[2]

which.min(nchar(emails$text))

corpus = Corpus(VectorSource(emails))

corpus = tm_map(corpus,tolower)

corpus = tm_map(corpus,PlainTextDocument)

corpus = tm_map(corpus,removePunctuation)

corpus = tm_map(corpus,removeWords,stopwords("english"))

corpus = tm_map(corpus,stemDocument)

dtm = DocumentTermMatrix(corpus)

spdtm = removeSparseTerms(dtm,0.95)

inspect(dtm)

#try using n-grams to build a more robust model using libraries "RTextTools", "tau", "RWeka", and "textcat" 

##end of chapter 5###

movies = read.table("movieLens.txt",header = FALSE,sep = "|",quote = "\"")

head(movies)

str(movies)

colnames(movies) = c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")

movies$ID = NULL
movies$ReleaseDate =NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)

distances = dist(movies[2:20],method = "euclidean")

clusterMovies = hclust(distances,method = "ward.D")

plot(clusterMovies)

clusterGroups = cutree(clusterMovies,k=10)

tapply(movies$Action,clusterGroups,mean)

subset(movies,Title=="Men in Black (1997)")

clusterGroups[257]

cluster2 = subset(movies,clusterGroups==2)

cluster2$Title[1:10]

clusterGroups2 = cutree(clusterMovies,k=2)

colMeans(subset(movies[2:20], clusterGroups2 == 2))

#alternate method
splf = split(movies[2:20],clusterGroups2)

sapply(splf,colMeans)

flower = read.csv("flower.csv",header = FALSE)

flowerMatrix = as.matrix.data.frame(flower)

str(flowerMatrix)

flowerVector = as.vector(flowerMatrix)

str(flowerVector)

#cannot convert dataframe directly to vector since it sees as 50 observations by 50 rows

distance = dist(flowerVector,method = "euclidean")

clusterIntensity = hclust(distance,method = "ward.D")

#ward's method is minimum variance method,i.e.minimizes variance within clusters and distance between clusters

plot(clusterIntensity)

rect.hclust(clusterIntensity,k=3,border = "red")

flowerClusters = cutree(clusterIntensity,k=3)

tapply(flowerVector,flowerClusters,mean)

dim(flowerClusters) = c(50,50)

 image(flowerClusters,axes=FALSE,col = terrain.colors(12))
 
 image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length.out = 256)))

 healthy = read.csv("healthy.csv",header = FALSE)
 
 healthMatrix = as.matrix(healthy)
 
 image(healthMatrix,axes=F,col=grey(seq(0,1,length.out = 256)))

 healthyVector = as.vector(healthMatrix)
 
 distance = dist(healthyVector,method = "euclidean")
 
 #cannot use hierarchical since euclidean distance gives too many vectors
 
 k=5
 
 set.seed(1)
 
 KMC = kmeans(healthyVector,centers = k,iter.max = 1000)

str(KMC)

healthyClusters = KMC$cluster

#mean intensity of clusters

KMC$centers

dim(healthyClusters) = c(nrow(healthMatrix),ncol(healthMatrix))

image(healthyClusters,axes=FALSE,col =rainbow(k))

tumor = read.csv("tumor.csv",header = F)

tumorMatrix = as.matrix(tumor)

tumorVector = as.vector(tumorMatrix)

install.packages("flexclust")

library(flexclust)

#KCCA k-centroids cluster analysis

KMC.kcca = as.kcca(KMC,healthyVector)

tumorClusters = predict(KMC.kcca,newdata = tumorVector)

dim(tumorClusters) = c(nrow(tumorMatrix),ncol(tumorMatrix))

image(tumorClusters,axes = FALSE,col = rainbow(k))

dailykos = read.csv("dailykos.csv")

dist = dist(dailykos,method = "euclidean")

dkos = hclust(dist,method = "ward.D")

plot(dkos)

dkosclust = cutree(dkos,k=7)

dkosclust7 = subset(dailykos,dkosclust==7)
