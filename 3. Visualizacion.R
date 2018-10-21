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

#GRAFICO1

pdf("graficos/grafico1.pdf")

ggplot(visualizar, aes(x = Mes, y=GanaFavorito, fill = GanaFavorito)) +
  geom_bar(stat='identity')+
      facet_grid(~Sport) +
  theme_light()+
  labs(subtitle="Meses donde hay mas eventos deportivos", y="", x="Meses", title="Deportes") +
  theme(legend.position = 'bottom')

dev.off()

#GRAFICO2

pdf("graficos/grafico2.pdf", width = 10)

ggplot(visualizar, aes(x = GanaFavorito, y= ..prop.., group = 1)) +
  geom_bar(stat='count', position = 'dodge') +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -0.6 , size = 2 , colour = 'black') + 
  labs(subtitle="% Victoria del favorito por mes y deporte", y="Porcentaje", x="Meses", title="Gana Favorito") +
  scale_y_continuous(labels=percent) + facet_grid(Sport~Mes, scales = 'free')

dev.off()

#GRAFICO 3

pdf("graficos/grafico3.pdf")

ggplot(visualizar, aes(x = CuotaFavorito, y=..count.., col = GanaFavorito)) +
  geom_density(kernel='gaussian') + xlim(1,3) +
  facet_grid(Sport~., scales='free') + labs(y="") +theme_bw()  + theme(legend.position = 'bottom') +
  labs(subtitle="Victorias y derrotas del favorito por cuota y deporte", 
       y="Total Eventos", x="Cuotas", title="Cuotas de los favoritos")

dev.off()

#GRAFICO 4

football = visualizar[visualizar$Sport == 'Football',]

pdf("graficos/grafico4.pdf", width = 10)
 
ggplot(football, aes(x = GanaFavorito, y=..prop.., group=1,fill = GanaFavorito )) +
  geom_bar(stat='count', position = 'dodge',fill="lightblue",colour="black") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -0.6, size = 2) + 
  scale_y_continuous(labels=percent) +
  facet_grid(Favorito~Mes) + 
  labs(subtitle="% Victorias cuando el equipo favorito es local o visitante", 
       y="Porcentaje", x="Gana Gavorito", title="Futbol: Local vs Visitante") +
  theme_get()

dev.off()

#GRAFICO 5

pdf("graficos/grafico5.pdf")

ggplot(visualizar, aes(x = GanaFavorito, y= ..prop.., group = 1)) +
  geom_bar(stat='count', position = 'dodge', fill="#0072B2",colour="red") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) + 
  scale_y_continuous(labels=percent)+ 
  labs(subtitle="Global", 
       y="Porcentaje", x="Gana Gavorito", title="Victoria del favorito") + 
  theme(plot.title = element_text(color="darkgreen", size=20, face="bold.italic"),
                axis.title.x = element_text(color="blue", size=14, face="bold"),
                axis.title.y = element_text(color="#993333", size=14, face="bold"))

dev.off()

#GRAFICO 6

pdf("graficos/grafico6.pdf",width = 10)

ggplot(visualizar, aes(x = GanaFavorito, y= ..prop.., group = 1)) +
  geom_bar(stat='count', position = 'dodge' ,fill="lightblue",colour="white") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5, size = 2)+ 
  scale_y_continuous(labels=percent) + facet_grid(~Mes) +labs(subtitle="Mes", 
  y="Porcentaje", x="Gana Gavorito", title="% Victoria del favorito")

dev.off()

#GRAFICO 7

pdf("graficos/grafico7.pdf",width = 10)

ggplot(visualizar, aes(x = GanaFavorito, y= ..prop.., group = 1)) +
  geom_bar(stat='count', position = 'dodge',fill="lightblue",colour="black") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) + 
  scale_y_continuous(labels=percent) + facet_grid(~Sport)+labs(subtitle="Deportes", 
  y="Porcentaje", x="Gana Gavorito", title="% Victoria del favorito")

dev.off()

#GRAFICO 8

pdf("graficos/grafico8.pdf")

ggplot(visualizar, aes(x = Sport, y = HuchaC)) +
  stat_summary(fun.y = sum,geom="bar", fill="lightblue",colour="black", size = 1)+
  stat_summary(aes(label = ..y..), fun.y = 'sum', geom = 'text', col = 'Black', vjust = -0.5, size = 3)+
  labs(subtitle="Deportes", 
  y="", x="Deportes", title="Hucha (Euros)") +
  theme(axis.text.y=element_blank())

dev.off()

#GRAFICO 9

pdf("graficos/grafico9.pdf",width = 10)

ggplot(visualizar, aes(x = Mes, y = HuchaC, group=1)) +
  stat_summary(fun.y = sum,geom="bar", colour = "red", size = 1) +
  stat_summary(fun.y = sum,geom="line", colour = "darkblue", size = 1)+
  facet_grid(Sport~. , scales = 'free')+
  labs(subtitle="Meses y Deportes", 
  y="", x="Meses", title="Hucha (Euros)") +
  theme(axis.text.y=element_blank())

dev.off()

#GRAFICO 10

pdf("graficos/grafico10.pdf",width = 10)

ggplot(visualizar, aes(x = CuotaFavorito, y = HuchaC, group=1)) +
  stat_summary_bin(fun.y = sum, geom = 'bar',colour = "darkgreen", size = 1)+
  facet_grid(Sport~. , scales = 'free')+
  labs(subtitle="Cuotas y Deportes", 
       y="", x="Cuotas", title="Hucha (Euros)") +
  theme(axis.text.y=element_blank()) +xlim(0.9,3)

dev.off()


