#limpeza de dados
#install.packages("tidyverse")

library(dplyr)
library(tidyr)

bike <- read.csv("raw_data.csv")

# View(bike)

# Primeira observacao dos dados do dataframe

str(bike)
glimpse(bike)#uma outra forma de verificar as variaveis
dim(bike) # tamanho da matriz: 17379 observacoes para 13 variaveis
head(bike) # seis primeiras observacoes
tail(bike) # seis ultimas observacoes


#install.packages("stringr")

library(stringr)

str_detect(bike, "NA")

table(is.na(bike$sources))

summary(bike)# revela o sumario das variaveis e ao final de cada coluna
             # apresenta se existe NAs e quantos NAs

colSums(is.na(bike)) # um caminho alternativo. 

# Note que os 554 NAs estão localizados na coluna sources o que 
# representa 3,18% do total de observacoes.

# Dois caminhos: 

#   a) omitir o "NAs"

#   a) renomear os NAs, a exemplo de: unknown

#         a) omitir os NAs

#           bike <- na.omit(bike)

#             colSums(is.na(bike)) 


#   b) renomear o NAs. Optando por manter para nao perdemos dados
#      Observe que teremos que trata-la como categorica e depois 
#      voltatemos para factor!

bike$sources <- as.character(bike$sources)

bike$sources[is.na(bike$sources)] <- "Unkown"

bike$sources <- as.factor(bike$sources)


colSums(is.na(bike))

#View(bike)

str(bike)

# Valores erroneos

#variavel humidity

bike$humidity <- as.numeric(bike$humidity) # tranformando em numerico


# Renomeando as observacoes em seus niveis (levels)

bike$holiday <- factor(bike$holiday, levels = c(0,1),
                         labels=c("no", "yes"))
  
  
bike$workingday <- factor(bike$workingday , levels = c(0,1),
                            labels=c("no", "yes"))
  
  
bike$season <- factor(bike$season, levels = c(1, 2, 3, 4),
                        labels = c("spring", "summer",
                                   "fall", "winter"))
  
  
bike$weather <- factor(bike$weather, levels = c(1, 2, 3, 4),
                         labels = c("cloud",
                                    "mist",
                                    "rain",
                                    "storm"))
  
str(bike)

$View(bike)

###############################

# Ajustando datetime

#install.packages("lubridate")

library(lubridate)
bike$datetime <- mdy_hm(bike$datetime)

str(bike)

bike_1 <- separate(bike,datetime, into = c("date"," hour"), sep = " ")

#Aternativamente pelo pipe (leia-se: then)

#bike_1 <- bike %>%
#       separate(datetime, into=c("date","hour"), sep=" ")

# Tranformando date em formato DATE

bike_1$date <- as.Date(bike_1$date)

# Tranformando HMS (hora, minuto, segundo)

# install.packages("hms") # format to hour, minute, second

library(hms)

bike_1$hour <- as_hms(bike_1$hour)



str(bike_1)

#View(bike_1)


# Um exemplo com o tidyverse
#######################################################

df <- select(bike_1, date, temp, humidity, casual, count)

df <- group_by(df, date)

df <- summarise(df, median = median(count, na.rm = TRUE))

df

######################################################
library(ggplot2)

ggplot(df, aes(date, median)) +
   geom_line(col="steelblue") +
  labs(x = "Date", y = "Median Count", 
       title = "Rental Bike")+
  scale_x_date(limits = as.Date(c("2011-01-01","2012-12-31")))

# E aqui esta a sua serie temporal para o seu curso
# time series (ts) para os intimos.

########################################################

#  O poder do 7 mais ou menos 2

#  Temos nas variaveis source observacoes AD campains, Facebook page, etc

table(bike_1$sources)

levels(bike_1$sources)[levels(bike_1$sources)=="ad campaign"]     <- "AD campaign"

levels(bike_1$sources)[levels(bike_1$sources)=="Ad Campaign"]     <- "AD campaign"

levels(bike_1$sources)[levels(bike_1$sources)=="blog"]            <- "Blog"

levels(bike_1$sources)[levels(bike_1$sources)=="direct"]            <- "direct"

levels(bike_1$sources)[levels(bike_1$sources)=="facebook page"]   <- "Facebook"

levels(bike_1$sources)[levels(bike_1$sources)=="Twitter    "]     <- "Twitter"

levels(bike_1$sources)[levels(bike_1$sources)=="www.bing.com"]     <- "Bing"

levels(bike_1$sources)[levels(bike_1$sources)=="www.google.co.uk"] <- "Google_UK"

levels(bike_1$sources)[levels(bike_1$sources)=="www.google.fi"]    <- "Google_FI"

levels(bike_1$sources)[levels(bike_1$sources)=="www.google.com"]   <- "Google"

levels(bike_1$sources)[levels(bike_1$sources)=="www.yahoo.com"]    <- "Yahoo"

table(bike_1$sources)

# O seu arquivo esta pronto para uma analise exploratoria de dados. 
 
write.csv(bike_1, "clean_data.csv")
  
  
  