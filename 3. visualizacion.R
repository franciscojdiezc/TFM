#Primero cargamos librerias.

library(ggplot2)
library(scales)

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

#1.Mes y deporte donde más gana el favorito

ggplot(visualizar, aes(x = Mes, y=GanaFavorito, fill = GanaFavorito)) +
  geom_bar(stat='identity')+
      facet_grid(~Sport) +
        ggtitle('Gana favorito por meses y deporte') +
          theme(legend.position = 'bottom')


#1.2Deporte por meses
ggplot(visualizar, aes(x = GanaFavorito, y= ..prop.., group = 1)) +
  geom_bar(stat='count', position = 'dodge') +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5)+
  ggtitle('Gana favorito por deporte') + 
  scale_y_continuous(labels=percent) + facet_grid(Sport~Mes)


#2.Comparación de cuotas

ggplot(visualizar, aes(x = CuotaFavorito, y=..count.., col = GanaFavorito)) +
  geom_density(kernel='gaussian') +
  ggtitle('Cuotas') + xlim(1,3) +
  facet_grid(Sport~., scales='free') + labs(y="") +theme_bw()  +
  theme(legend.position = 'bottom')

#3.Fútbol Local vs Visitante siendo favorito por meses

football = visualizar[visualizar$Sport == 'Football',]
 
ggplot(football, aes(x = GanaFavorito, y=..prop.., group=1,fill = GanaFavorito )) +
  geom_bar(stat='count', position = 'dodge') +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5)+
  ggtitle('Local vs Visitante') + 
  scale_y_continuous(labels=percent) +
  facet_grid(Favorito~Mes)


#4. Gana Favorito

ggplot(visualizar, aes(x = GanaFavorito, y= ..prop.., group = 1)) +
  geom_bar(stat='count', position = 'dodge') +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5)+
  ggtitle('Gana favorito por deporte') + 
  scale_y_continuous(labels=percent)


ggplot(visualizar, aes(x = GanaFavorito, y= ..prop.., group = 1)) +
  geom_bar(stat='count', position = 'dodge') +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5)+
  ggtitle('Gana favorito por deporte') + 
  scale_y_continuous(labels=percent) + facet_grid(~Mes)

##4.3 General por deporte
ggplot(visualizar, aes(x = GanaFavorito, y= ..prop.., group = 1)) +
  geom_bar(stat='count', position = 'dodge',fill="lightblue",colour="black") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5)+
  ggtitle('Gana favorito por deporte') + 
  scale_y_continuous(labels=percent) + facet_grid(~Sport)


##Hucha por deportes y meses

ggplot(visualizar, aes(x = Sport, y = HuchaC)) +
  stat_summary(fun.y = sum,geom="bar", fill="lightblue",colour="black", size = 1)+
  ggtitle('Hucha')

ggplot(visualizar, aes(x = Mes, y = HuchaC)) +
  stat_summary(fun.y = sum,geom="bar", colour = "red", size = 1)+
  ggtitle('Hucha') + facet_grid(Sport~. , scales = 'free')

#dev.off()

