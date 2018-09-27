library(dplyr)
library(readr)

football <- read_csv2('european_football.csv')


football$Div=as.factor(football$Div)
football$Mes=as.factor(football$Mes)
football$LocalVisitante=as.factor(football$LocalVisitante)
football$JuegaEuropa=as.factor(football$JuegaEuropa)
football$MundialOEurocopa=as.factor(football$MundialOEurocopa)
football$GanaFavorito=as.factor(football$GanaFavorito)

modeloLogit=glm(GanaFavorito~Probabilidad+Mes+LocalVisitante+
                  JuegaEuropa+MundialOEurocopa, data=football,family=binomial(link="logit"))

football$prediccion=predict(modeloLogit,type="response")

x2 = football[1, c(1:6)]

x <- data.frame("Div" = 'D1', "Mes" = 1, 'Probabilidad' = 1.65, 'LocalVisitante' = 0,
                'JuegaEuropa' = 0, 'MundialOEurocopa' = 0)

x$Div=as.factor(x$Div)
x$Mes=as.factor(x$Mes)
x$LocalVisitante=as.factor(x$LocalVisitante)
x$JuegaEuropa=as.factor(x$JuegaEuropa)
x$MundialOEurocopa=as.factor(x$MundialOEurocopa)

predict.glm(modeloLogit,type="response", newdata = x)

