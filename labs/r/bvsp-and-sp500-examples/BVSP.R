install.packages(c("quantmod", "ggplot2", "BatchGetSymbols",
                   "e1071"))

library(quantmod)
library(ggplot2)
library(BatchGetSymbols)
library(e1071)
library(zoo)
library(xts)
start <- as.Date("2007-01-02")
end  <- as.Date("2019-11-08")

#BVSP bolsa de SP (BVSP)
#^GSPC Sp500 (GSPC)

getSymbols("^BVSP", src = "yahoo", from = start, to = end)

#View(BVSP$BVSP.Close)
head(BVSP)

GSPC <- na.omit(BVSP)

retorno <- diff(log(Cl(GSPC$BVSP.Close)))
retorno <- na.omit(retorno)
View(retorno)

hist(retorno$BVSP.Clos)

ggplot(retorno, aes(x=BVSP.Close)) +
  geom_histogram()


mean_return <- mean(retorno$BVSP.Close, na.rm=TRUE) 
mean_return

#a media esta em logaritmo, precisa transformar usando a formula abaixo

#SP500 deu 6,25% ao ano, para quem operou neste período
((exp(mean_return)^252)-1)*100

#252 é o calendario financeiro considera 252 dias (meses com 21 dias úteis)


#calculamos a taxa de retorno. Vamos calcular o desvio padrao
sd(retorno$BVSP.Close, na.rm = TRUE) #antilog -1

exp((sd(retorno$BVSP.Close, na.rm = TRUE)^252)-1)*100
#a vloatilidade (desvio) é a volatividade de 36%a.a