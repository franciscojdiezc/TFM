#1. Cargamos las librerias

if(!require("plyr")){
  install.packages("plyr")
  library("plyr")
}

if(!require("caTools")){
  install.packages("caTools")
  library("caTools")
}

if(!require("ROCR")){
  install.packages("ROCR")
  library("ROCR")
}


if (!require("e1071")){
  install.packages("e1071")
  library("e1071")
}

if (!require("glmnet")){
  install.packages("glmnet") 
  library("glmnet")
}
#2. Cargamos el fichero

football=read.csv2("Data/european_football.csv")

#3. Observamos el fichero y formateamos las variables 

str(football)

football$Mes=as.factor(football$Mes)
football$LocalVisitante=as.factor(football$LocalVisitante)
football$JuegaEuropa=as.factor(football$JuegaEuropa)
football$MundialOEurocopa=as.factor(football$MundialOEurocopa)
football$GanaFavorito=as.factor(football$GanaFavorito)

#4. Modelo

modeloLogit=glm(GanaFavorito~Probabilidad+Mes+LocalVisitante+JuegaEuropa+MundialOEurocopa, data=football,family=binomial(link="logit"))
summary(modeloLogit)

modeloLogitFinal=step(modeloLogit,direction="both",trace=1)
summary(modeloLogitFinal)
anova(modeloLogitFinal,modeloLogit)

coef(modeloLogitFinal)

exp(coef(modeloLogitFinal))

exp(cbind(coef(modeloLogitFinal), confint(modeloLogitFinal,level=0.95)))

football$prediccion=predict(modeloLogit,type="response")

football$prediccion2=predict(modeloLogitFinal,type="response")

#5. Test y Entrenamiento

SAMPLE = sample.split(football$GanaFavorito, SplitRatio = .75) #Para hacer un modelo de entrenamiento y de test
footballTrain = subset(football, SAMPLE == TRUE)
footballTest = subset(football, SAMPLE == FALSE)

modeloLogitTrain=glm(GanaFavorito~Div+Probabilidad+Mes+LocalVisitante+JuegaEuropa+MundialOEurocopa, data=footballTrain,family=binomial(link="logit"))

summary(modeloLogitTrain)

footballTrain$prediccion=predict(modeloLogitTrain,type="response")
Predauxiliar= prediction(footballTrain$prediccion, footballTrain$GanaFavorito, label.ordering = NULL)
auc.tmp = performance(Predauxiliar, "auc");
aucModeloLogittrain = as.numeric(auc.tmp@y.values)
aucModeloLogittrain

CurvaRocModeloLogitTrain <- performance(Predauxiliar,"tpr","fpr")
plot(CurvaRocModeloLogitTrain,colorize=TRUE) #Todo lo que pinte encima de la recta hemos ganado comparando modelos.
abline(a=0,b=1)

## Indice de GINI
GINItrain=2*aucModeloLogittrain-1

#Test

footballTest$prediccion=predict(modeloLogitTrain, newdata=footballTest,type="response")
Predauxiliar = prediction(footballTest$prediccion, footballTest$GanaFavorito, label.ordering = NULL)
auc.tmp = performance(Predauxiliar, "auc");
aucModeloLogittest = as.numeric(auc.tmp@y.values)
aucModeloLogittest

CurvaRocModeloLogitTest <- performance(Predauxiliar,"tpr","fpr")
plot(CurvaRocModeloLogitTest,colorize=TRUE)
abline(a=0,b=1)

## Capacidad del Modelo

mean(as.numeric(footballTest$GanaFavorito)-1)
aggregate(footballTest$prediccion~footballTest$GanaFavorito,FUN=mean)


#Probit

modeloProbit=glm(GanaFavorito~Div+Mes+Probabilidad+LocalVisitante+JuegaEuropa+MundialOEurocopa, data=football,family=binomial(link="probit"))
summary(modeloProbit)

modeloProbitFinal=step(modeloProbit,direction="both",trace=1)
summary(modeloProbitFinal)
anova(modeloProbitFinal,modeloProbit)

football$prediccion3 = predict.glm(modeloProbit, data=football,type="response")
football$prediccion4 = predict(modeloProbitFinal, data=football,type="response")

#Test & Entrenamiento

modeloProbitTrain=glm(GanaFavorito~Div+Probabilidad+Mes+LocalVisitante+JuegaEuropa+MundialOEurocopa, data=footballTrain,family=binomial(link="probit"))

summary(modeloProbitTrain)

footballTrain$prediccion2=predict(modeloProbitTrain,type="response")
Predauxiliar2= prediction(footballTrain$prediccion2, footballTrain$GanaFavorito, label.ordering = NULL)
auc.tmp2 = performance(Predauxiliar2, "auc");
aucModeloProbittrain = as.numeric(auc.tmp2@y.values)
aucModeloProbittrain

CurvaRocModeloProbitTrain <- performance(Predauxiliar2,"tpr","fpr")
plot(CurvaRocModeloProbitTrain,colorize=TRUE) #Todo lo que pinte encima de la recta hemos ganado comparando modelos.
abline(a=0,b=1)

#Test

footballTest$prediccion2=predict(modeloProbitTrain, newdata=footballTest,type="response")
Predauxiliar2 = prediction(footballTest$prediccion2, footballTest$GanaFavorito, label.ordering = NULL)
auc.tmp2 = performance(Predauxiliar2, "auc");
aucModeloProbittest = as.numeric(auc.tmp2@y.values)
aucModeloProbittest

CurvaRocModeloProbitTest <- performance(Predauxiliar2,"tpr","fpr")
plot(CurvaRocModeloProbitTest,colorize=TRUE)
abline(a=0,b=1)

## Capacidad del Modelo

mean(as.numeric(footballTest$GanaFavorito)-1)
aggregate(footballTest$prediccion2~footballTest$GanaFavorito,FUN=mean)

#Tanto Logit como Probit tienen un accuracy muy parecido

#-----
  #PUESTA EN VALOR
#-----
  
#1.Threshold
  
ALPHA = 0.5
ConfusionTest=table(footballTest$GanaFavorito,footballTest$prediccion>=ALPHA)
AccuracyTest = (sum(footballTest$GanaFavorito==1 & footballTest$prediccion>=ALPHA)+sum(footballTest$GanaFavorito==0 & footballTest$prediccion<ALPHA))/length(footballTest$GanaFavorito)
PrecisionTest=sum(footballTest$GanaFavorito==1 & footballTest$prediccion>=ALPHA)/sum(footballTest$prediccion>=ALPHA)
CoberturaTest=sum(footballTest$GanaFavorito==1 & footballTest$prediccion>=ALPHA)/sum(footballTest$GanaFavorito==1)
ConfusionTest
AccuracyTest
PrecisionTest
CoberturaTest

#2.KS y punto de maxima separación

footballKS=footballTest[order(footballTest$prediccion, decreasing=TRUE),c("GanaFavorito","prediccion")]
footballKS$N=1:length(footballKS$GanaFavorito)
footballKS$EXITOSACUM=cumsum(as.numeric(footballKS$GanaFavorito)-1)
footballKS$FRACASOSACUM=footballKS$N-footballKS$EXITOSACUM
footballKS$EXITOSTOT=sum(footballKS$GanaFavorito==1)
footballKS$FRACASOSTOT=sum(footballKS$GanaFavorito==0)
footballKS$TOTAL=footballKS$EXITOSTOT+footballKS$FRACASOSTOT
footballKS$TPR=footballKS$EXITOSACUM/footballKS$EXITOSTOT
footballKS$FPR=footballKS$FRACASOSACUM/footballKS$FRACASOSTOT
footballKS$DIFF=footballKS$TPR-footballKS$FPR
plot(footballKS$DIFF)
max(footballKS$DIFF)
which(footballKS$DIFF==max(footballKS$DIFF))
footballKS[4310,]

plot(footballKS$prediccion*1000,1-footballKS$TPR,xlab="SCORE",ylab="Porcentaje acumulado",main="Distribuciones por Score (rojo malos, azul buenos)",type="l",col="blue")
lines(footballKS$prediccion*1000,1-footballKS$FPR,col="red")

#3.F1Score y punto optimo estadistico

footballKS$Accuracy=(footballKS$EXITOSACUM+footballKS$FRACASOSTOT-footballKS$FRACASOSACUM)/footballKS$TOTAL
footballKS$Precision=footballKS$EXITOSACUM/footballKS$N
footballKS$Cobertura=footballKS$EXITOSACUM/footballKS$EXITOSTOT
footballKS$F1Score=2*(footballKS$Precision*footballKS$Cobertura)/(footballKS$Precision+footballKS$Cobertura)
plot(footballKS$F1Score)
max(footballKS$F1Score,na.rm=TRUE)
which(footballKS$F1Score==max(footballKS$F1Score,na.rm=TRUE))
footballKS[4310,]

ALPHAS=seq(0,1,0.05)
Accuracy=c()
Precision=c()
Cobertura=c()
F1Score=c()
for (i in 1:length(ALPHAS)){
  ALPHA=ALPHAS[i]
  Confusion=table(footballKS$GanaFavorito,footballKS$prediccion>=ALPHA)
  Accuracy=c(Accuracy,(sum(footballKS$GanaFavorito==1 & footballKS$prediccion>=ALPHA)+sum(footballKS$GanaFavorito==0 & footballKS$prediccion<ALPHA))/length(footballKS$GanaFavorito))
  Precision=c(Precision,sum(footballKS$GanaFavorito==1 & footballKS$prediccion>=ALPHA)/sum(footballKS$prediccion>=ALPHA))
  Cobertura=c(Cobertura,sum(footballKS$GanaFavorito==1 & footballKS$prediccion>=ALPHA)/sum(footballKS$GanaFavorito==1))
}
F1Score=2*(Precision*Cobertura)/(Precision+Cobertura)
DFF1=data.frame(ALPHAS,Accuracy,Precision,Cobertura,F1Score)

DFF1

#4. Beneficio

indicadores = read.csv2("Data/indicadores.csv", sep = ',')

indicadores$Indicador = as.numeric(levels(indicadores$Indicador)[indicadores$Indicador])

indicadores$Indicador = indicadores$Indicador/ 10

footballConIndicadores = merge(x = football, y = indicadores, by.x = 'Probabilidad', by.y = 'CuotaFavorito')

ALPHA=0.74

footballConIndicadores$Beneficio <- ifelse(footballConIndicadores$GanaFavorito == 1, footballConIndicadores$Indicador*footballConIndicadores$Probabilidad-footballConIndicadores$Indicador, -footballConIndicadores$Indicador)

footballALPHA <- footballConIndicadores[footballConIndicadores$prediccion>=ALPHA,]

BeneficioNeto = sum(footballALPHA$Beneficio)
BeneficioNeto

# ---

  #VARIABLES DUMMIES

# ---

if(!require("dummies")){
  install.packages("dummies")
  library("dummies")
}

football2=read.csv2("Data/european_football.csv")

football2$Mes=as.factor(football2$Mes)
football2$LocalVisitante=as.factor(football2$LocalVisitante)
football2$JuegaEuropa=as.factor(football2$JuegaEuropa)
football2$MundialOEurocopa=as.factor(football2$MundialOEurocopa)

footballDummies <- dummy.data.frame(football2)

footballDummies$GanaFavorito = as.factor(footballDummies$GanaFavorito)

#Aplicamos Logit

modeloLogitD=glm(GanaFavorito~., data=footballDummies,family=binomial(link="logit"))
summary(modeloLogitD)

modeloLogitDFinal=step(modeloLogitD,direction="both",trace=1)
summary(modeloLogitDFinal)

anova(modeloLogitDFinal,modeloLogitD)

footballDummies$prediccion=predict(modeloLogitD,type="response")

footballDummies$prediccion2=predict(modeloLogitDFinal,type="response")

#Entrenamiento & Test

SAMPLE2 = sample.split(footballDummies$GanaFavorito, SplitRatio = .75) #Para hacer un modelo de entrenamiento y de test
footballDTrain = subset(footballDummies, SAMPLE2 == TRUE)
footballDTest = subset(footballDummies, SAMPLE2 == FALSE)

modeloLogitDTrain=glm(GanaFavorito~., data=footballDTrain,family=binomial(link="logit"))

summary(modeloLogitDTrain)

footballDTrain$prediccion=predict(modeloLogitDTrain,type="response")
PredauxiliarD= prediction(footballDTrain$prediccion, footballDTrain$GanaFavorito, label.ordering = NULL)
auc.tmpD = performance(PredauxiliarD, "auc");
aucModeloLogitDtrain = as.numeric(auc.tmpD@y.values)
aucModeloLogitDtrain

CurvaRocModeloLogitDTrain <- performance(PredauxiliarD,"tpr","fpr")
plot(CurvaRocModeloLogitDTrain,colorize=TRUE)
abline(a=0,b=1)

#Test

footballDTest$prediccion=predict(modeloLogitDTrain, newdata=footballDTest,type="response")
PredauxiliarD = prediction(footballDTest$prediccion, footballDTest$GanaFavorito, label.ordering = NULL)
auc.tmpD = performance(PredauxiliarD, "auc");
aucModeloLogitDtest = as.numeric(auc.tmpD@y.values)
aucModeloLogitDtest

CurvaRocModeloLogitDTest <- performance(PredauxiliarD,"tpr","fpr")
plot(CurvaRocModeloLogitDTest,colorize=TRUE)
abline(a=0,b=1)
