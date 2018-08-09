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

#Creamos una carpeta para guardar los gráficos en pdf

dir.create('graficos')

#GRÁFICO 1

pdf("graficos/grafico1.pdf")

ggplot(visualizar, aes(x = Mes, y=GanaFavorito, fill = GanaFavorito)) +
  geom_bar(stat='identity')+
      facet_grid(~Sport) +
  theme_light()+
  labs(subtitle="Meses donde hay más eventos deportivos", y="", x="Meses", title="Deportes") +
  theme(legend.position = 'bottom')

dev.off()

#GRÁFICO2

pdf("graficos/grafico2.pdf", width = 10)

ggplot(visualizar, aes(x = GanaFavorito, y= ..prop.., group = 1)) +
  geom_bar(stat='count', position = 'dodge') +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -0.6 , size = 2 , colour = 'black') + 
  labs(subtitle="% Victoria del favorito por mes y deporte", y="Porcentaje", x="Meses", title="Gana Favorito") +
  scale_y_continuous(labels=percent) + facet_grid(Sport~Mes, scales = 'free')

dev.off()

#GRÁFICO 3

pdf("graficos/grafico3.pdf")

ggplot(visualizar, aes(x = CuotaFavorito, y=..count.., col = GanaFavorito)) +
  geom_density(kernel='gaussian') + xlim(1,3) +
  facet_grid(Sport~., scales='free') + labs(y="") +theme_bw()  + theme(legend.position = 'bottom') +
  labs(subtitle="Eventos con más victorias del favorito por deporte", 
       y="Total Eventos", x="Cuotas", title="Cuotas de los favoritos")

dev.off()

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

#¿Hucha por cuotas? Evolución para con que cuota ganaría más
