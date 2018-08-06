#Primero cargamos librerias.

library(ggplot2)

#Cargamos el fichero.

visualizar <- read.csv('Data/visualizar_rstudio.csv')

#Observamos el contenido y cambiamos el formato.

summary(visualizar)

visualizar$Date = as.Date(visualizar$Date)

visualizar$GanaFavorito = as.factor(visualizar$GanaFavorito)

#Creamos la columna mes

visualizar$Mes = as.factor(as.numeric(format(visualizar$Date, "%m")))

dir.create('graficos')

pdf("graficos/grafico.pdf")

ggplot(visualizar, aes(x = Mes, y=GanaFavorito)) +
  geom_bar(stat='identity')+
    facet_grid(Sport ~ GanaFavorito)

dev.off()

