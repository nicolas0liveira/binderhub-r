#====================================================================
#SCRIPT DE EXEMPLOS DA AULA DE ANÁLISE EXPLORATÓRIA 2019

#para rodar os exemplos desse script, voce precisara
#dos arquivos de dados de caixeta, egrandis, esaliga e aves
#que estao disponiveis no site do curso:
#http://ecologia.ib.usp.br/bie5782/doku.php?id=dados:start
#Voce tambem vai precisar dos dados dos pardais, que estao
#no material suplementar do artigo de Zuur et al. 2010
#https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2009.00001.x

#====================================================================
#ajustando o diretório de trabalho
#ajuste o diretorio adequado no seu computador usando setwd()
getwd()

#conferindo os arquivos da pasta
dir()

#limpando a area de trabalho
rm(list=ls())
#--------------------------------------------------------------------
#BLOCO 1 - CONFERENCIA, NAs, zeros 

#leitura dos dados de aves
aves = read.table("aves_cerrado.csv", sep=";", header=TRUE, row.names = 1)

#dando aquela primeira conferida

#str, head, tail
str(aves)
head(aves)
tail(aves)
#localizanto NAs com which() e is.na()
is.na(aves$urubu)
which(is.na(aves$urubu))

#eliminando ou substituindo NAs com na.omit()

aves$urubu[is.na(aves$urubu)] = 0
which(is.na(aves$urubu)) #nao sobrou nenhum NA

x = c(1,2,3,56,78,9,0,NA, NA) #criando um vetor com alguns NAs
na.omit(x) #usando na.omit()
#--------------------------------------------------------------------
#BLOCO 2 - explorando dados quantitativos/continuos

#lendo os dados de esaligna
esa = read.table("esaligna.csv", sep=",", header=TRUE)

#conferindo
str(esa)
head(esa)

#medidas descritivas do conjunto de dados

#media e desvio, esa$folha
mean(esa$dap)

#range
range(esa$dap)

#resumo dos cinco numeros
summary(esa$ht)
summary(esa)

#GRAFICOS UNIVARIADOS

#histograma
hist(esa$folha, col="grey")

#boxplot
boxplot(esa$dap, col="grey")

#plot(density())
density(esa$folha)
plot(density(esa$folha))

#dotchart e dotchart genérico
dotchart(aves$urubu)
plot(x = aves$urubu, y=1:nrow(aves))


#--------------------------------------------------------------------
#BLOCO 3 - normalidade

#histograma e curva com esa$ht
hist(esa$ht)

#diminuir o numero de breaks
hist(esa$ht, breaks=5, col="grey", freq = FALSE)

#adicionando a curva esperada segundo a distribuicao normal
curve(dnorm(x=x, mean = mean(esa$ht), 
            sd = sd(esa$ht)), col="purple",
            add=TRUE)

#qqnorm() e qqline()
qqnorm(esa$ht)
qqline(esa$ht)

#qqnorm e qqline com dados simulados

#simulando dados de mesmo comprimento, media e desvio
hts = rnorm(nrow(esa), mean(esa$ht), sd(esa$ht))

#desenhando o grafico com qqnorm() e qqline()
qqnorm(hts, pch=16)
qqline(hts)

#density + curva
plot(density(esa$ht))
curve(dnorm(x=x,mean(esa$ht), sd(esa$ht)),
      col="red", add=TRUE)
#--------------------------------------------------------------------
#BLOCO 4 - dados qualitativos/categoricos

#lendo os dados de caixeta
cax = read.table("caixeta.csv", sep=",", header=TRUE)
head(cax)

#table por espécie
table(cax$especie)

#table por espécie e local
table(cax$especie, cax$local)

#tableception
table(table(cax$especie))

#barplot de table(cax$local)
table(cax$local)
barplot(table(cax$local))
barplot(table(cax$especie))

#xtabs com TitanicDF
data(Titanic)
class(Titanic)

TitanicDF = data.frame(Titanic)
head(TitanicDF)

xtabs(TitanicDF$Freq~TitanicDF$Survived+TitanicDF$Age)
xtabs(Freq~Survived+Age, data=TitanicDF)

#--------------------------------------------------------------------
#BLOCO 5 - dados bivariados

#inventando dados para o plot de florzinhas
flor = cbind(rep(1:10, 1:10), rep(1:10, 1:10))
flor
plot(flor)
sunflowerplot(flor)

#lendo os dados de egrandis
egrandis = read.table("egrandis.csv", sep=";", header=TRUE)
head(egrandis)

#plot de dap e ht

#scatter.smooth de esa ht e dap
scatter.smooth(esa$ht~esa$dap)

#para embonecar a linha, tem p argumento lpars
scatter.smooth(esa$ht~esa$dap, lpars = list(lwd=3, col="red"))

#funcao cor
cor(esa[,4:8])

#sunflowerplot, egrandis dap e ht
sunflowerplot(dap~ht, data=egrandis)

#boxplot - dap por regiao em egrandis
boxplot(dap~regiao, data=egrandis)

#apply com anscombe e esa[,4:8]

data("anscombe")
anscombe
apply(anscombe, 2, mean) #media de todas as colunas anscombe
apply(anscombe[,1:4], 1, mean) #media so das colunas 1-4

#tapply e aggregate
tapply(cax$h, 
       list(cax$local),
       mean)

tapply(cax$h, 
       list(cax$local, cax$parcela),
       mean)

tapply(cax$h, 
       list(cax$local, cax$parcela, cax$especie),
       mean)

aggregate(cax$h, 
       list(cax$local, cax$parcela),
       sd)

#pacote lattice
library(lattice)

#xyplot de egrandis dap por ht por regiao
xyplot(dap~ht|regiao, data=egrandis)

#histogram de egrandis vol por regiao
histogram(~vol|regiao, 
          data=egrandis[egrandis$regiao %in% c("Salto", "Botucatu"),])

#--------------------------------------------------------------------
#BLOCO 6 - dados multivariados

pardal = read.table("SparrowsElphick.txt", sep="\t", dec=".", head=TRUE)

#o retorno das funcoes plot e cor
plot(pardal[,1:7])
cor_pardal = cor(pardal[,1:7])
cor_pardal

#correlacoes com corrplot
#se tiver que instalar o pacote, rode o seguinte comando
#install.packages(corrplot)

#depois eh so carregar o pacote e usar
library(corrplot)
corrplot(cor_pardal)

#matriz de similaridade, dados de barro colorado
library(vegan)
data(BCI)
BCI = as.matrix(BCI)
str(BCI)

#funcao vegdist, method="jaccard", linhas 1:10
BCI_dist = vegdist(BCI[1:10,], method = "jaccard")
corrplot(as.matrix(BCI_dist))


#correlacoes e pca das colunas de 1:7
pca = princomp(pardal[,1:7])
biplot(pca)

str(pca)
str(pca$scores)

#plot dos componentes 1 e 2
plot(x = pca$scores[,1], y = pca$scores[,2],
     col = pardal$SpeciesCode, pch=19)

#plot do objeto do pca
plot(pca)
#====================================================================