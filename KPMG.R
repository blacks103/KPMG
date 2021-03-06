# KPMG interj� feladat: Titanic T�l�l�si modell
getwd()
setwd("C:/Users/S�t� L�szl�/Desktop/kpmg")
#Az al�bbi k�nyvt�rakra lesz sz�ks�g�nk a modell fel�p�t�se sor�n.
library(tidyverse)
library(cowplot)
library(corrplot)
library(AER)
library(ISLR)
#Olvassuk be a train �s a test adatsort.
train <- read_csv("titanictrain.csv")
test <- read_csv("titanictest.csv")
str(train)
summary(train)
#Alak�tsuk az adatsorunkat, hogy Logisztikus regresszi�val tudjunk modellezni.
train$death <- ifelse(train$Survived==1,0,1)
train$death <- as.factor(train$death)
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
table(train$Parch)
table(train$SibSp)
#N�h�ny kateg�ri�ban a Parch �s a SibSp eset�n is kev�s van, de legal�bb nincs �res kateg�ria.
train$Parch <- as.factor(train$Parch)
train$SibSp <- as.factor(train$SibSp)
train$Embarked <- as.factor(train$Embarked)
#A fenti faktor t�pus� adatokkal m�r tudunk dolgoni, mivel nincs olyan k�zt�k amelyben sok N/A szerepelne.
#K�l�nb�z� korcsoportokra fogjuk bontani az utasokat, figyelve, 
#arra hogy mindegyik korcsoportba megfelel� sz�m� utas ker�lj�n.

#Els� l�p�sk�nt a newtrain-be v�logassuk azon utasokat ahol szerepel �letkor az adatai k�z�tt.
newtrain <- train[!is.na(train$Age), ] 

#Veg�l egy for ciklussal 6 k�l�nb�z� csoportba osztjuk be az utasokat az al�bbi m�don:

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
#A table paranccsal meg tudjuk n�zni, hogy melyik csoportba mennyi utas tartozik.
table(newtrain$Age)
newtrain$Age <- as.factor(newtrain$Age)

#A k�vetkez�ben 2 modellt fogunk fel�p�teni, az els� az eg�sz train adatsor alapj�n, de a korcsoportok figyelembev�tele n�lk�l j�n l�tre, 
#m�g a m�sodik csak azokra a train-b�l sz�rmaz� adatokra fog �p�lni ahol a kor meg van adva.


                                                          #Modell
#Logisztikus regresszi� a GLM (Generalized Linear Models) f�ggv�ny haszn�lat�val.
Model <- glm(death ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = train, family = 'binomial')
summary(Model)
#A summary alapj�n a Pclass, Sex �s a SibSp statisztikailag szignifik�ns magyar�z� v�ltoz�. 
#Jegyezz�k meg, hogy a SibSp eset�n csak a SibSp3 szignifik�ns de ilyenkor ha 1 is az, akkor benne hagyjuk a modellben.
newModel <- glm(death ~ Pclass + Sex + SibSp, data = train, family = 'binomial')
summary(newModel)
#Az �j modellt m�r nem lehet tov�bb egyszer�s�teni.


                                                          #ModelAge
#Logisztikus regresszi� korcsoportokkal
ModelAge <- glm(death ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Age, data = newtrain, family = 'binomial')
summary(ModelAge)
#A summary alapj�n a Pclass, Sex, SibSp �s az Age statisztikailag szignifik�ns magyar�z� v�ltoz�. 
#Jegyezz�k meg, hogy a SibSp eset�n csak a SibSp3 �s SibSp4 szignifik�ns de ilyenkor ha 1 is az, akkor benne hagyjuk a modellben.
newModelAge <- glm(death ~ Pclass + Sex + SibSp + Age, data = newtrain, family = 'binomial')
summary(newModelAge)
#Az �j modellt m�r nem lehet tov�bb egyszer�s�teni.

#A newModel eset�n az AIC (Akaike information criterion) 556.34.
#A NewModelAge eset�n az AIC (Akaike information criterion) 428.51.
#Az inform�ci�s krit�rium alapj�n az a modell a jobb amelyik az �letkorcsoportokat is tartalmazza.


#A modellek �p�t�s�vel k�szen vagyunk, most form�zzuk a teszt adatsort �gy, mint a train adatsort.
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
#A table paranccsal meg tudjuk n�zni, hogy melyik csoportba mennyi utas tartozik.
table(newtest$Age)
newtest$Age <- as.factor(newtest$Age)

#Tesztel�s az els� modellel:
pred1 <- predict(newModel, test, type= "response")
pred1

#Tesztel� a m�sodik modellel:
pred2 <- predict(newModelAge, newtest, type= "response")
pred2

#Az ellen�rz�s szeml�ltet�s�re a confusion matrix-ot szokt�k alkalmazni, ezt nem tehetj�k most meg, 
#mivel a test adatsor nem tartalmazza a t�nyleges adatokat.

#Ha lenne, akkor a k�d a k�vetkez�k�ppen n�zne ki:
#p1 <- ifelse(pred1>0.5,1,0)
#tab1 <- table(Predicted=pred1, Actual=test$death)
