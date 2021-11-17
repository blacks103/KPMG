# KPMG interjú feladat: Titanic Túlélési modell
getwd()
setwd("C:/Users/Sütõ László/Desktop/kpmg")
#Az alábbi könyvtárakra lesz szükségünk a modell felépítése során.
library(tidyverse)
library(cowplot)
library(corrplot)
library(AER)
library(ISLR)
#Olvassuk be a train és a test adatsort.
train <- read_csv("titanictrain.csv")
test <- read_csv("titanictest.csv")
str(train)
summary(train)
#Alakítsuk az adatsorunkat, hogy Logisztikus regresszióval tudjunk modellezni.
train$death <- ifelse(train$Survived==1,0,1)
train$death <- as.factor(train$death)
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
table(train$Parch)
table(train$SibSp)
#Néhány kategóriában a Parch és a SibSp esetén is kevés van, de legalább nincs üres kategória.
train$Parch <- as.factor(train$Parch)
train$SibSp <- as.factor(train$SibSp)
train$Embarked <- as.factor(train$Embarked)
#A fenti faktor típusú adatokkal már tudunk dolgoni, mivel nincs olyan köztük amelyben sok N/A szerepelne.
#Különbözõ korcsoportokra fogjuk bontani az utasokat, figyelve, 
#arra hogy mindegyik korcsoportba megfelelõ számú utas kerüljön.

#Elsõ lépésként a newtrain-be válogassuk azon utasokat ahol szerepel életkor az adatai között.
newtrain <- train[!is.na(train$Age), ] 

#Vegül egy for ciklussal 6 különbözõ csoportba osztjuk be az utasokat az alábbi módon:

for(i in 1:length(newtrain$Age))
{
  if(0<=newtrain$Age[i] & newtrain$Age[i]<10) {
    newtrain$Age[i]<-1
  } else if(10<=newtrain$Age[i] & newtrain$Age[i]<20) {
    newtrain$Age[i]<-2
  } else if(20<=newtrain$Age[i] & newtrain$Age[i]<30) {
    newtrain$Age[i]<-3
  } else if(30<=newtrain$Age[i] & newtrain$Age[i]<40) {
    newtrain$Age[i]<-4
  } else if(40<=newtrain$Age[i] & newtrain$Age[i]<50) {
    newtrain$Age[i]<-5
  } else
    newtrain$Age[i]<-6
}
#A table paranccsal meg tudjuk nézni, hogy melyik csoportba mennyi utas tartozik.
table(newtrain$Age)
newtrain$Age <- as.factor(newtrain$Age)

#A következõben 2 modellt fogunk felépíteni, az elsõ az egész train adatsor alapján, de a korcsoportok figyelembevétele nélkül jön létre, 
#míg a második csak azokra a train-bõl származó adatokra fog épülni ahol a kor meg van adva.


                                                          #Modell
#Logisztikus regresszió a GLM (Generalized Linear Models) függvény használatával.
Model <- glm(death ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = train, family = 'binomial')
summary(Model)
#A summary alapján a Pclass, Sex és a SibSp statisztikailag szignifikáns magyarázó változó. 
#Jegyezzük meg, hogy a SibSp esetén csak a SibSp3 szignifikáns de ilyenkor ha 1 is az, akkor benne hagyjuk a modellben.
newModel <- glm(death ~ Pclass + Sex + SibSp, data = train, family = 'binomial')
summary(newModel)
#Az új modellt már nem lehet tovább egyszerûsíteni.


                                                          #ModelAge
#Logisztikus regresszió korcsoportokkal
ModelAge <- glm(death ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Age, data = newtrain, family = 'binomial')
summary(ModelAge)
#A summary alapján a Pclass, Sex, SibSp és az Age statisztikailag szignifikáns magyarázó változó. 
#Jegyezzük meg, hogy a SibSp esetén csak a SibSp3 és SibSp4 szignifikáns de ilyenkor ha 1 is az, akkor benne hagyjuk a modellben.
newModelAge <- glm(death ~ Pclass + Sex + SibSp + Age, data = newtrain, family = 'binomial')
summary(newModelAge)
#Az új modellt már nem lehet tovább egyszerûsíteni.

#A newModel esetén az AIC (Akaike information criterion) 556.34.
#A NewModelAge esetén az AIC (Akaike information criterion) 428.51.
#Az információs kritérium alapján az a modell a jobb amelyik az életkorcsoportokat is tartalmazza.


#A modellek építésével készen vagyunk, most formázzuk a teszt adatsort úgy, mint a train adatsort.
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Parch <- as.factor(test$Parch)
test$SibSp <- as.factor(test$SibSp)
test$Embarked <- as.factor(test$Embarked)
newtest <- test[!is.na(test$Age), ] 
for(i in 1:length(newtest$Age))
{
  if(0<=newtest$Age[i] & newtest$Age[i]<10) {
    newtest$Age[i]<-1
  } else if(10<=newtest$Age[i] & newtest$Age[i]<20) {
    newtest$Age[i]<-2
  } else if(20<=newtest$Age[i] & newtest$Age[i]<30) {
    newtest$Age[i]<-3
  } else if(30<=newtest$Age[i] & newtest$Age[i]<40) {
    newtest$Age[i]<-4
  } else if(40<=newtest$Age[i] & newtest$Age[i]<50) {
    newtest$Age[i]<-5
  } else
    newtest$Age[i]<-6
}
#A table paranccsal meg tudjuk nézni, hogy melyik csoportba mennyi utas tartozik.
table(newtest$Age)
newtest$Age <- as.factor(newtest$Age)

#Tesztelés az elsõ modellel:
pred1 <- predict(newModel, test, type= "response")
pred1

#Tesztelé a második modellel:
pred2 <- predict(newModelAge, newtest, type= "response")
pred2

#Az ellenõrzés szemléltetésére a confusion matrix-ot szokták alkalmazni, ezt nem tehetjük most meg, 
#mivel a test adatsor nem tartalmazza a tényleges adatokat.

#Ha lenne, akkor a kód a következõképpen nézne ki:
#p1 <- ifelse(pred1>0.5,1,0)
#tab1 <- table(Predicted=pred1, Actual=test$death)
