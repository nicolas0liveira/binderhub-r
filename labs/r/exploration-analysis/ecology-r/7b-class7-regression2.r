##########################
#options(help_type="html")
### Regressao multipla ###
##Leitura dos dados, obtidos de http://www.bio.ic.ac.uk/research/mjcraw/statcomp/data/
## Arquivo texto separado por tabulacao (argumento sep="\t")
poluicao <- read.table("Pollute.txt", header=T, sep="\t")
str(poluicao)
################################################################################
## 1. Primeiro modelo: efeito do numero de industrias
################################################################################
## 1.1.Graficos Exploratorios
par(mfrow=c(2,2))
plot(Pollution~Industry, data=poluicao)
plot(Pollution~Population, data=poluicao)
plot(Pollution~Temp, data=poluicao)
plot(Pollution~Wind, data=poluicao)
par(mfrow=c(1,1))
plot(poluicao)
###################
## 1.2. Arvore dos dados
###################
#install.packages("tree")
library(tree)
mtree<-tree(Pollution~.,data=poluicao)
plot(mtree)
text(mtree)
##########################################
## 2. Simplificação e avaliacao do modelo
#########################################
pol.all <- lm(Pollution~Industry * Population * Temp, data=poluicao)
summary(pol.all)
##acrescentando a linha no grafico
pol.1 <- lm(Pollution ~ Industry + Population + Temp + Industry:Population    + Industry:Temp + Population:Temp  , data=poluicao)
summary(pol.1)
anova(pol.1, pol.all)
pol.2 <- lm(Pollution ~ Industry + Population + Temp + Industry:Population  + Industry:Temp, data=poluicao)
summary(pol.2)
anova(pol.1,pol.2)
pol.3 <- lm(Pollution ~ Industry + Population + Temp + Industry:Population, data=poluicao)
summary(pol.3)
#####################
## comparar modelos
#####################
anova(pol.2, pol.3)
pol.4 <- lm(Pollution ~ Industry + Population + Temp, data=poluicao)
summary(pol.4)
anova(pol.3, pol.4)
pol.5 <- lm(Pollution ~ Industry + Population, data=poluicao)
summary(pol.5)
anova(pol.4, pol.5)
pol.6 <- lm(Pollution ~ Industry, data=poluicao)
summary(pol.6)
anova(pol.5, pol.6)
pol.7 <- lm(Pollution ~ Population, data=poluicao)
summary(pol.7)
anova(pol.5, pol.7)
##########################
summary(pol.5)
anova(pol.5)
summary(pol.7)
summary(pol.6)
##########################
pop.1 <- lm(Population ~ Industry, data=poluicao)
anova(pop.1)
summary(pop.1)
#############################
# o que acontece  afinal?  ##
#############################
x11()
par(mfrow=c(2,1))
plot(Population~Industry, data=poluicao)
abline(pop.1, col="blue")
plot(Pollution~Industry, data=poluicao)
abline(pol.6, col="blue")

################################################################################
## O FANTASMA DA COLINEARIDADE
################################################################################

#######################
## Iniciar novamente 
#######################
pol.all0 <- lm(Pollution~Industry * Temp, data=poluicao)
summary(pol.all0)
##acrescentando a linha no grafico
pol.01 <- lm(Pollution ~ Industry  + Temp, data=poluicao)
summary(pol.01)
anova(pol.all0, pol.01)
pol.02 <- lm(Pollution ~ Industry, data=poluicao)
anova(pol.02, pol.01)
pol.04 <- lm(Pollution ~ Temp, data=poluicao)
summary(pol.04)
anova(pol.01, pol.04)
pol.00 <- lm(Pollution ~ 1, data=poluicao)
summary(pol.00)
anova(pol.00, pol.01)
summary(pol.01)
plot(pol.01)
###IMPORTANTE###
## Essencial compreender a diferenca dos testes no resumo e em
##O comando ''anova'' sempre compara modelos. Se o argumneto é um único modelo, ele retorna o resultado de um **teste sequencial**, isto é, o aumento de variação explicada por um fator em relação a um modelo com os fatores **acima dele na tabela**, que segue a ordem da fórmula do modelo. Por isso, os dois comandos abaixo não retornam exatamente os mesmos resultados:
anova(lm(Pollution~Temp+Wind+Industry, data=poluicao))
anova(lm(Pollution~Industry+Wind+Temp, data=poluicao))
##O teste para Industry no resumo equivale a
anova(lm(Pollution~Temp+Wind, data=poluicao),lm(Pollution~Temp+Wind+Industry, data=poluicao))
####### FIM ##############

