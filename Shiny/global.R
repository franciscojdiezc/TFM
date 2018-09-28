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

