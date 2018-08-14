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

#2. Cargamos el fichero

football=read.csv2("Data/european_football2.csv")

#3. Observamos el fichero y formateamos las variables 

str(football)

football$Mes=as.factor(football$Mes)
football$LocalVisitante=as.factor(football$LocalVisitant)
football$JuegaEuropa=as.factor(football$JuegaEuropa)
football$AñoMundialOEurocopa=as.factor(football$AñoMundialOEurocopa)
football$GanaFavorito=as.factor(football$GanaFavorito)

#4. Modelo

modeloLogit=glm(GanaFavorito~Div+Mes+LocalVisitante+JuegaEuropa+AñoMundialOEurocopa, data=football,family=binomial(link="logit"))
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

modeloLogitTrain=glm(GanaFavorito~Mes+LocalVisitante+JuegaEuropa+AñoMundialOEurocopa, data=footballTrain,family=binomial(link="logit"))

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

mean(as.numeric(footballTest$GanaFavorito)-1)
aggregate(footballTest$prediccion~footballTest$GanaFavorito,FUN=mean)


#Probit

modeloProbit=glm(GanaFavorito~Div+Mes+Probabilidad+LocalVisitante+JuegaEuropa+AñoMundialOEurocopa, data=football,family=binomial(link="probit"))
summary(modeloProbit)

modeloProbitFinal=step(modeloProbit,direction="both",trace=1)
summary(modeloProbitFinal)
anova(modeloProbitFinal,modeloProbit)

football$predict = predict.glm(modeloProbit, data=football,type="response")
football$predict2 = predict(modeloProbit, data=football,type="response")

#Prueba Mes

football=read.csv2("Data/european_football.csv")


football$Mes=as.factor(football$Mes)
football$LocalVisitante=as.factor(football$LocalVisitant)
football$JuegaEuropa=as.factor(football$JuegaEuropa)
football$AñoMundialOEurocopa=as.factor(football$AñoMundialOEurocopa)
football$GanaFavorito=as.factor(football$GanaFavorito)

mes = subset(football, football$Div =='F1')

modeloLogitMes=glm(GanaFavorito~Mes+Probabilidad+LocalVisitante+JuegaEuropa+AñoMundialOEurocopa, data=mes,family=binomial(link="logit"))
summary(modeloLogitMes)

mes$prediccion=predict(modeloLogitMes,type="response")


mean(as.numeric(mes$GanaFavorito)-1)
aggregate(mes$prediccion~mes$GanaFavorito,FUN=mean)
