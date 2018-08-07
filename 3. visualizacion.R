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

#pdf("graficos/grafico.pdf")

#Mes y deporte donde más gana el favorito

ggplot(visualizar, aes(x = Mes, y=GanaFavorito, fill = GanaFavorito)) +
  geom_bar(stat='identity')+
      facet_grid(~Sport) +
        ggtitle('Gana favorito por meses y deporte') +
          theme(legend.position = 'bottom')

#Comparación de cuotas

ggplot(visualizar, aes(x = CuotaFavorito, col = GanaFavorito)) +
  geom_density(kernel='gaussian') +
  ggtitle('Cuotas') +
  theme(legend.position = 'bottom') + xlim(1,3) +
  facet_grid(Sport~., scales='free') + labs(y="") +theme_bw()

football = visualizar[visualizar$Sport == 'Football',]
 
ggplot(football, aes(x = Favorito, y=GanaFavorito, fill = GanaFavorito)) +
  geom_bar(stat='identity')+
  facet_grid(~Mes) +
  ggtitle('Local vs Visitante') +
  theme(legend.position = 'bottom')

#Gana Favorito
#Fútbol Local y Visitante


#dev.off()
library(scales)

ggplot(visualizar, aes(x = GanaFavorito, y= ..prop.., fill = GanaFavorito)) +
  geom_bar(stat='count', position = 'fill') +
  ggtitle('Gana favorito por meses y deporte') +
  theme(legend.position = 'bottom')+ 
  scale_y_continuous(labels=percent)
