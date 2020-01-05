#INSTALAR O PACKAGE TIDYVERSE

#install.packages("tidyverse")

#CONHECENDO O TIDYVERSE (devtools = developer tools)

devtools::session_info(c("tidyverse"))

#require(gridExtra) # possibilita visualizacao de varios graficos numa tela

#libraries

library(tidyverse)
library(readr) # fornece uma maneira rapida e amigavel de ler dados (como 'csv','tsv'e'fwf')
library(stringr) # trabalha com caracteres de forma bem consistente
library(lubridate) #trabalha com datas 
library(ggplot2) # graficos
library(dplyr) # manipulacao de dados de forma consistente. Trabalha o df como objeto.
library(tidyr) #os dados podem ser gerenciados 
library(gridExtra)


# IMPORTANDO O ARQUIVO: airbnb.csv (dados ate jul.2019)

airbnb <- read.csv("airbnb.csv")

# VERIFICANDO AS VARIAVEIS 

nrow(airbnb)
str(airbnb)
View(airbnb)


# 1. note que last esta como factor, mas deveria ser data.
# 2. a variavel last poderia se chamar "data".
# 3. temos varias linhas sem dados


#1, AJUSTANDO O DATA FRAME 

# 1. Transformando em formato Date

airbnb$last<- as.Date(airbnb$last,format="%Y-%m-%d")
str(airbnb)

#  2. Renomeando a variavel

colnames(airbnb)[colnames(airbnb) == 'last'] <- 'date'
str(airbnb)

# 3. Limpando as linhas vazias (observacoes vazias)

airbnb <- filter(airbnb, !(area=="")) #funcao do dplyer

#Alternativamente com o %>% (pipe)
#airbnb <- airbnb %>% 
#       filter(!(area==""))

# Observe o numero de linhas. As observacoes vazias foram
# excluidas

nrow(airbnb)

#View(airbnb)


# TIBBLE, UMA FORMA ELEGANTE DE SE TRABALHAR COM DF

#ex-ante

airbnb

# O elegante tibble

library(tibble) 
airbnb <- as_tibble(airbnb)

#ex-post

airbnb
str(airbnb)

# SELECIONANDO AS VARIAVEIS QUE TRABALHAREMOS


airbnb <- select(airbnb,'price', 'area','location','latitude', 
                 'longitude','room',
                 'nights','reviews','mrev',
                 'host_listing', 'date', 'availability')

#View(airbnb)

summary(airbnb)

# UMA PRIMEIRA VARREDURA DA DF airbnb


# 1. price possui valor bem acima do terceiro quartil. Sabemos que nas pesquisas anteriores, 
#    pode haver alguns valores registrados por mes.

# 2. nights tambem possui possui valores bem acima do terceiro quartil.

# 3. reviews e mrev idem (mrev tambem possui NA's)

# 4. host_listing idem. 


###################################################################################

# Visualizando a variavel price

g1 <- ggplot(data=airbnb)+
  geom_boxplot(aes(x=area, y=price))

g2 <- ggplot(data=airbnb, aes(x=price))+
  geom_histogram()

grid.arrange(g1, g2, ncol=2)

# SUMARIO DO VARIAVEL PRICE

summary(airbnb$price)
# o terceiro quantil (75%) tem price = US$ 178 

# VERIFICANDO OS QUANTIS. A CRITERIO DO DATA SCIENCE ANALYST
# TRABALHEIR COM 5% 


qlower <- quantile(airbnb$price, probs = 0.05, na.rm= TRUE)
qlower
qupper <- quantile(airbnb$price, probs = 0.95, na.rm = TRUE)
qupper

# Com um quantil de 5% e 95% os precos estao em um intervalo
# de $ 40 e $ 365.


# CRIANDO UMA NOVA COLUNA PRICE_DAY - CONDICAO E IFELSE ():

#  Se preço maior que o percentil de 95% entao price_day = price dividido por 30
#  Caso contrario: price

airbnb$price_day <- ifelse(airbnb$price>365,airbnb$price/30,airbnb$price)

# View(airbnb)

# apenas um check Minnimo e Maximo

max(airbnb$price_day)

min(airbnb$price_day)


#Filtro de do intervalo do percentil de price_day

qlower <- quantile(airbnb$price_day, probs = 0.05, na.rm= TRUE)
qlower
qupper <- quantile(airbnb$price_day, probs = 0.95, na.rm = TRUE)
qupper

# Os quantis de price day estao no intervalor de 29 a 29. Pura escolha,
# Optei por criar um intervalo entre 29 e 365. Fica a seu criterio.

airbnb <- filter(airbnb, price_day>= 29 & price_day <= 365)

#View(airbnb)

min(airbnb$price_day)
max(airbnb$price_day)


########################################################################################

#  VISUALIZACAO DO PRICE_DAY:

# Visualizando price_day em relacao a area (distritos de NYC)
# ao tipo de acomodacao em geral tem todos os distritos
# o histograma e;
# datas em relacao a mediana de precos


g1 <- ggplot(data=airbnb)+
  geom_boxplot(aes(x=area, y=price_day))


g2 <- ggplot(data=airbnb, aes(x=price_day))+
  geom_histogram()
  

g3 <- ggplot(data =airbnb, aes(x= room, y = price_day))+ 
  geom_boxplot()


grid.arrange(g1,g2,g3)

##########################################################################################

# Usando o tidayverse mais a fundo:

# select() selecionar variaveis
# gather() organizar dados
# group_by() agrupar 
# summarise() sumarizar


# CALCULANDO A MEDIANA DOS PRECOS POR DATA

# Criando uma df que recebera airbnb

df <- airbnb
df <- select(df, date, price_day)
df <- group_by(df, date)
df <- summarise(df, median = median(price_day, na.rm=TRUE))

#ALTERNATIVAMENTE PELO PIPE
#df <- airbnb
#df%>%
#  select(date, price_day) %>%
#        group_by(date) %>%
#             summarise(median = median(price_day, na.rm=TRUE))

              
df # virou um data frame de data e precos medianos/ data

#preços medianos de 01.jan.2015 ate 08.jul.2019! 


g4 <- ggplot(data=df, aes(x=date, y= median))+
      geom_line()+
      xlim(as.Date(c("2015-01-01", "2019-07-08"))) # uma senhora ts


grid.arrange(g1, g2, g3, g4, ncol=2)

summary(airbnb$price_day)

########################################################################################

# ANALISE DE PRECOS PARA O DISTRITO DE MANHATTAN

df_man <- airbnb

df_man <- filter(df_man, area == 'Manhattan')
#View(df_man)

# Um check basico

ggplot(data= df_man, aes(area))+
             geom_bar()


# OBSERVEM A SUTILEZA
# ALTEREI DA DF_MAN PARA DF_MAN1 
# IMPORTANTE PELO DATA FRAME APOS O SUMMARIZE

df_man1 <- group_by(df_man, date)

df_man1 <- summarise(df_man1, median = median(price_day, na.rm=TRUE))

# SCRIPTS DOS GRAFICOS

m1 <- ggplot(data=df_man)+  # note que a data = df_man
  geom_boxplot(aes(x=area, y=price_day))


m2 <- ggplot(data=df_man, aes(x=price_day))+
  geom_histogram()


m3 <- ggplot(data =df_man, aes(x= room, y = price_day))+ 
  geom_boxplot()


m4 <- ggplot(data=df_man1, aes(x=date, y= median))+ #<==== data = df_man1
  geom_line()+
  xlim(as.Date(c("2015-01-01", "2019-07-08"))) 

grid.arrange(m1, m2, m3, m4, ncol=2)

summary(df_man$price_day)


########################################################################################

#ANALISE DE PRECOS PARA O DISTRITO DE HELLS HITCHEN


df_hk <- filter(airbnb, location == "Hell's Kitchen")
View(df_hk)

# Um check basico
ggplot(hk, aes(location))+
  geom_bar()
  
#Note que alterei df_hk para df_hk1 <==== IMPORTANTE

df_hk1 <- group_by(df_hk, date)

df_hk1 <- summarise(df_hk1, median = median(price_day, na.rm=TRUE))


#PLOTANDO OS GRAFICOS

hk1 <- ggplot(data=df_hk)+  # note que a data = df_hk
            geom_boxplot(aes(x=area, y=price_day))


hk2 <- ggplot(data=df_hk, aes(x=price_day))+
            geom_histogram()


hk3 <- ggplot(data =df_hk, aes(x= room, y = price_day))+ 
            geom_boxplot()

# O plot abaixo e com a mediana, logo:


hk4 <- ggplot(data=df_hk1, aes(x=date, y= median))+ #<==== data = df3
           geom_line()+
              xlim(as.Date(c("2015-01-01", "2019-07-08"))) 


grid.arrange(hk1, hk2, hk3, hk4, ncol=2)


summary(df_hk$price_day)

########################################################################################

#AGRUPANDO MANHATTAN: BAIRROS POR PRICE_DAY

df_man <- filter(airbnb, area == 'Manhattan')

View(df_man)

#Note que alterei df_man para df_man1 <==== IMPORTANTE

df_man1 <- group_by(df_man, location)

df_man1 <- summarise(df_man1, median = median(price_day, na.rm=TRUE))

df_man1

m1 <- ggplot(data=df_man1, aes(x = reorder(location, median), y = median,
             fill=median))+  # note que a data = df2
        geom_bar(stat = "identity", fill ="red")+
        coord_flip()
        


##################################################################################

#  AVALIANDO VARIAVEL:  NIGHTS

# Plotando histograma

df_n <- airbnb

max(df_n$nights, na.rm = TRUE)

df_n

summary(df_n$nights)

ggplot(data = df_n , aes(x=nights))+
                        geom_histogram()

# Avaliando os quantis 

qlower <- quantile(df_n$nights, probs = 0.05, na.rm= TRUE)
qlower
qupper <- quantile(df_n$nights, probs = 0.95, na.rm = TRUE)
qupper



# FILTRAR NO INTERVALO DOS PERCENTIS

df_n <- filter(df_n, nights >= 1 & nights <= 30)

min(df_n$nights)
max(df_n$nights)

#Plotando 

n1 <- ggplot(data=df_n, aes(x=nights))+
         geom_histogram()

n2 <- ggplot(data=df_n)+
        geom_boxplot(aes(x=area, y=nights))


grid.arrange(n1, n2,  ncol=2)

# Ao que parece o pessoal fica mais tempo em Manhattan
# Vejamos Manhattan

#########################################################

# Revendo o script efetuado, temos:

df_n1 <- filter(df_n, area == 'Manhattan')

summary(df_n1$nights)


n1 <- ggplot(data= df_n1, aes(x= area, y = nights))+
            geom_boxplot()

n2 <- ggplot(data=df_n1, aes(nights))+
            geom_histogram()

grid.arrange(n1, n2, ncol = 2)


###################################################

#AVALIANDO REVIEWS

# Reviews

ggplot(data=airbnb, aes(x=price_day, y=reviews))+
            geom_point(color='gray50') +
              geom_smooth(method = "lm", se = FALSE)

cor(airbnb$price_day,airbnb$reviews)

# aparentemente a correlacao e quase nula

ggplot(data=airbnb)+
  geom_boxplot(aes(x=area, y= reviews))


ggplot( data= airbnb)+
  geom_histogram(aes(x=airbnb$reviews))


summary(airbnb$reviews)

qlower <- quantile(airbnb$reviews, probs = 0.05, na.rm= TRUE)
qlower
qupper <- quantile(airbnb$reviews, probs = 0.95, na.rm = TRUE)
qupper


# Expurando os outliers

head(airbnb)

df_r <- filter(airbnb, reviews>= 0 & reviews <= 50)


summary(df_r$reviews)

ggplot(data=df_r, aes(reviews))+
  geom_histogram(col="blue")

#Um boxplot incrementado

r2 <- ggplot(data=df_r,aes(x=area, y= reviews))+
  geom_boxplot()+
  stat_summary(fun.y = median, geom = "line", aes(group=5), linetype = 3)+
  stat_summary(fun.y = median, geom="point", colour="red")



r3 <- ggplot(data= airbnb, aes(x= area, y = price_day))+
  geom_boxplot()+
  stat_summary(fun.y = median, geom = "line", aes(group=5), linetype = 3)+
  stat_summary(fun.y = median, geom="point", colour="red")

grid.arrange(r2,r3,ncol=2)

#############################################################

# Temos o script. Fizemos antes! Vamos para Manhattan e depois para o
# Harlem

df_r1 <- filter(airbnb, location == 'NoHo')

df_r1

ggplot(data=df_r1, aes(x=price_day, y=reviews))+
  geom_point(color='gray50') +
  geom_smooth(method = "lm", se = FALSE)

n1 <- ggplot(data= df_r1, aes(x= location, y = nights))+
  geom_boxplot()

n2 <- ggplot(data=df_n1, aes(nights))+
  geom_histogram()

grid.arrange(n1, n2, ncol = 2)


###############################################################

# ANALISANDO A VARIAVEL AVAILABILITY

# Avaliabity

a1 <- ggplot(data=airbnb, aes(x=availability))+
  geom_histogram()

a2 <- ggplot(data = airbnb, aes(x = area, y = availability))+
  geom_boxplot()+
  stat_summary(fun.y = mean, geom = "line", aes(group=5), linetype = 3)+
  stat_summary(fun.y = mean, geom="point", colour="red")

a1
a2


# summary( airbnb %>% filter(area == "Brooklyn") %>% 
#         group_by(availability))

# Verificando os plots revisitanto o numero de reviews


grid.arrange(a1, a2, r3, ncol=3)

# Pede-se comparar 

# Manhattan: avaliabilily e price_day
# Compara o mesmo exercicio para Bronx e Queens!!!!


#################################################
# GEOLOCALIZACAO


g1 <- ggplot(data = airbnb)+
  geom_histogram(aes(latitude))

g2 <- ggplot(data = airbnb)+
  geom_histogram(aes(longitude))

grid.arrange(g1, g2, ncol =2)

summary(airbnb$latitude)

summary(airbnb$longitude)

lat_median = median(airbnb$latitude, na.rm = TRUE)
lon_median = median(airbnb$longitude, na.rm = TRUE)


############################################################

# EFETUANDO A CORRELACAO COM O PERFORMANCE ANALYTICS


View(airbnb)

library(PerformanceAnalytics)

chart.Correlation(airbnb[,c(7:10,12:12)],histogram=TRUE, pch=3)


# Interessante filtrar por Area. Homework.
# Além disso, efetuar a analise de host_listing e mrev.

############################################################

#  AT LAST BUT NOT LEAST!

#  GEOLOCALIZACAO

ggplot(data = airbnb)+
  geom_point(aes(x = longitude, y = latitude, col='blue'))

# Dois novos packages

install.packages("magrittr")
install.packages("leaflet")
library(magrittr)
library(leaflet)

geo <- leaflet() %>%
  addTiles() %>%  # usa o mapa  default 
  addMarkers(lng=lon_median, lat=lat_median,
             popup="Mediana")
geo



