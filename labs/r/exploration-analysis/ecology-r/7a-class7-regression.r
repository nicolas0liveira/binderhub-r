options(help_type="html")
?lm

##############################
## GRAFICOS MINIMOS QUADRADOS
##############################
## SIMULANDO DADOS
#########################
## function min quadrados
##########################
source("SSsim.r")
simSS()

######################################################################
####Demonstracao da linha de minimos quadrados
######################################################################
## Gerando os pontos: valor esperado de y = 2 + 3*x, com desvio=padrao de 2.5
## Oito valores de x tomados de uma distribuicao uniforme com minimo=0 e maximo=10
set.seed(42)

(x1 <- runif(8,0,10))
## Valores de y tem media = 2+3*x e distribuicao normal
## em torno desta media, com desvio-padrao = 2.5
(ym=2+3*x1)
plot(x1,ym)
abline(2,3, col="red")
set.seed(42)
y1 <- rnorm(8,mean=ym,sd=2.5)
points(x1,y1, pch=16, col="red")
y1 <- (2 + 3 * x1) + rnorm(8, 0, 2.5)
points(x1,y1, pch=1, col="blue")

## Medias de x e y
(my1 <- mean(y1))
(mx1 <- mean(x1))
points(mx1, my1, pch=1, col="blue", cex=2)
abline(lm(y1~x1), col="green", lty=2)
############################################
### Aqui feche todas as janelas graficas ###
############################################
##Primeiro Grafico: regressao com ponto na intersecao das medias
## "fulcro" da alavanca
## Abrindo a primeira janela para colocar este grafico
x11(height=4.5, width=5)
plot(y1~x1, col="blue", pch=19)
points(mx1,my1,cex=2, col="red"); points(mx1,my1,pch=19,cex=1/3,col="red")
## reta da regressao
abline(lm(y1~x1), col="red", lwd=2)
################################################
## Dados necessarios 
## Sequencia de valores de inclinacao a avaliar
intervalo <- seq(-6,15,by=0.5) 
##Valores maximos e minimos de SSE, para o intervalo de valores de inclinacoes
## Estes valores sao necessarios para definir a escala do eixo y
##Valor maximo de sse: o mÃ¡ximo dos extremos da sequencia
b.max <- max(intervalo)
a.max <- my1-b.max*mx1
b.min <- min(intervalo)
a.min <- my1-b.min*mx1
pred.max <- a.max+b.max*x1
pred.min <- a.min+b.min*x1
sse.max <- max(c(sum((y1-pred.max)^2),sum((y1-pred.min)^2)))
## Valor minimo
sse.min <- sum((y1-fitted(lmy1~x1))^2)
##Peparando os dois graficos
## Graficos com os pontos e "apoio da alavanca"
dev.set(2)
plot(y1~x1, col="blue", pch=19)
points(mx1,my1,cex=2, col="red")
points(mx1,my1,pch=19,cex=1/3,col="red")
## Preparando o grafico do SSE por valor da inclinacao
## Abrindo a segunda janela grafica
x11(height=3.5, width=4)
#X11()
## O grafico
dev.set(3)
plot(0,0,xlim=range(intervalo),ylim=c(sse.min,sse.max),xlab="Inlinacao", ylab="SSE", type="n")
###Aqui comeca a animacao! ###
## A interacao: os SSEs sao calculados para valores de b definidos no intervalo
## Dentro de um loop. Para cada valor, os dois graficos sao atualizados
## Todos os valores
sse <- c()
i <- 0
dev.set(2)
devAskNewPage(TRUE)
for(b in intervalo){
  i <- i+1
  pred.y1 <- (my1-b*mx1)+b*x1
  sse[i] <- sum((y1-pred.y1)^2)
  dev.set(2)
  plot(y1~x1, col="blue", pch=19, ylim=c(-10,50))
  points(mx1,my1,cex=2, col="red"); points(mx1,my1,pch=19,cex=1/3,col="red")
  abline(my1-b*mx1,b, col="red", lty=2)
  segments(x1,pred.y1,x1,y1,col="blue")
  dev.set(3)
  points(b,sse[i],pch=19, cex=0.5)
}
devAskNewPage(FALSE)
###Fim da animacao ###
##Qual o valor minimo?
## Pelo locator
locator(1)
## Guardadndo em um objeto
b.est.g <- locator(1)$x
##Pelo minimo dos valores
b.est <- intervalo[sse==min(sse,na.rm=TRUE)]
## Comparando
b.est.g
b.est
#intervalo
## Plotando no grafico
x11()

## Grafico com o "fulcro"
plot(y1~x1, col="blue", pch=19)
points(mx1,my1,cex=2, col="red"); points(mx1,my1,pch=19,cex=1/3,col="red")
##Linha da regressao ajustada pelo lm
abline(lm(y1~x1), lty=2, col="red")
## Linha com o valor de inclinacao obtido do grafico
abline(my1-b.est*mx1,b.est, lty=2)
#######################################
##### APRESENTAÇÃO
#######################################
## Modelo Linear
graphics.off()
###############################
##### lm() ANOVA
############################
are=c(6,10,8,6,14,17,9,11,7,11)
arg=c(17,15,3,11,14,12,12,8,10,13)
hum=c(13,16,9,12,15,16,17,13,18,14)
planta=c(are,arg,hum)
#### Calculos base ###
fator.solo=as.factor(rep(c("are", "arg", "hum"),each=10))
############################
aov.solo<-aov(planta~fator.solo)
anova(aov.solo)

lm.solo<-lm(planta~fator.solo)
anova(lm.solo)
##################################
babies <- read.table("babies.txt",header=T,na.strings=999)
str(babies)
summary(babies)
###############################################
## Corrigindo os outros dados com código de NA 
##(9 ou 99, dependendo da variavel)
###############################################
babies$age[babies$age==99] <- NA
babies$height[babies$height==99] <- NA
babies$smoke[babies$smoke==9] <- NA
babies$smoke <- as.logical(babies$smoke)
## Eliminando linhas com algum dado faltante
table(apply(is.na(babies),1,sum))
babies <- babies[apply(is.na(babies),1,sum)==0,]
summary(babies)
str(babies)
############
##Figura ###
plot(bwt~gestation, data=babies, cex=0.5)
abline(h=mean(babies$bwt), lty=2, col="red")
##Regressao em funcao do tempo de gestacao
babies.m1 <- lm(bwt~gestation, data=babies)
##Adicionando a linha da regressao ao grafico
abline(babies.m1, col="blue", lwd=2)
##Tabela de ANOVA da regressao
anova(babies.m1)
###################################
### Calculos da ANOVA da Regressao
###################################
##Conteudo do objeto de regressao
names(babies.m1)
str(babies.m1)
babies.m1$coef
coef(babies.m1)#coeficientes
## Calculo dos previstos e dos residuos
## Guardando os coeficientes em um objeto numerico
cf.m1 <- as.numeric(coef(babies.m1))
## Valores previstos
babies$pred.m1 <- cf.m1[1]+cf.m1[2]*babies$gestation
head(babies)
## Residuos
babies$res.m1 <- babies$bwt - babies$pred.m1
## Mostrando todos juntos
print(head(babies[,c(2,1,8,9)]),digits=3)
head(babies[,c(2,1,8,9)])
## A funcao residuals ja calcula os residuos
babies$res.m1[1:4]
residuals(babies.m1)[1:4]
## E tambem a funcao fitted para os previstos
babies$pred.m1[1:4]
fitted(babies.m1)[1:4]
## Soma dos quadrados total
sum((babies$res.m1)^2)
babies.m1.SSE <- sum(residuals(babies.m1)^2)
babies.m1.SSE
## Desvios quadraticos total
babies.SST <- sum((babies$bwt - mean(babies$bwt))^2, na.rm=TRUE)
## Soma dos quadrados da regressao
babies.m1.SSR <- babies.SST - babies.m1.SSE
babies.m1.SSR
## Calculo do MSE
babies.m1.MSE <- babies.m1.SSE/(length(babies$bwt)-2)
## Valor de F
babies.m1.F <- babies.m1.SSR/babies.m1.MSE
babies.m1.F
## Conferindo com o resultado da anova
## Nossos calculos
##MSR (= SSR /1)
babies.m1.SSR
##MSE
babies.m1.MSE
##Valor de F
babies.m1.F
## Significancia do F
pf(babies.m1.F,df1=1, df2=(length(babies$bwt)-2), lower.tail=FALSE)
## E agora a anova
anova(babies.m1)
## Coeficiente de determinacaoo: explicado / total
babies.m1.SSR/babies.SST
summary(babies.m1)
##########################
#### modelo sem preditora
babies.m0<-lm(bwt~1, data=babies)
anova(babies.m0)
babies.SST
######## Anova 2 modelos ###########
anova(babies.m0, babies.m1)
anova(babies.m1)
## Coeficiente de determinacao: explicado / total
babies.m1.SSR/babies.SST
summary(babies.m1)
###################################################
##Sintese: Objeto lm e seus extratores mais comuns:
## O objeto
#################################################
str(babies.m1)
class(babies.m1)
names(babies.m1)
babies.m1$coefficients
babies.m1$residuals[1:4]
babies.m1$fitted.values[1:4]
babies.m1$call
## Funcoes extratoras
names(babies.m1)
coef(babies.m1)
confint(babies.m1)
residuals(babies.m1)[1:4]
fitted(babies.m1)[1:4]
logLik(babies.m1) ## pacote MASS
AIC(babies.m1)
summary(babies.m1)
###########################
#### APRESENTAÇÃO  ########
###########################
## Analise de covariancia
############################
## Conjunto de dados "babies"
## Qual o efeito do tempo de gestacao (continua) e do fato da mae fumar (fator) sobre o peso do bebe?
## relembrando do nosso primeiro modelo
plot(bwt~gestation,data=babies)
abline(babies.m1,col="blue")
x11()
plot(bwt~gestation, data=babies, col=as.factor(babies$smoke))
##########################
## Modelo sem interacao ##
babies.m2 <- lm(bwt~gestation+smoke, data=babies)
## Resumo
summary(babies.m2)
anova(babies.m2)
anova(babies.m1, babies.m2)

## Coeficientes
(cf.bm2 <- coef(babies.m2))
head(model.matrix(babies.m2))
head(babies[,c(2,7)])
## O previsto para o primeiro valor: primeiro fazemos o produto de cada coeficiente
## com a primeira linha da matriz do modelo
model.matrix(babies.m2)[1,]
cf.bm2
model.matrix(babies.m2)[1,]*cf.bm2
sum(model.matrix(babies.m2)[1,]*cf.bm2)
## Conferindo
fitted(babies.m2)[1] ## ok!
## O mesmo para a terceira linha
model.matrix(babies.m2)[3,]
cf.bm2
model.matrix(babies.m2)[3,]*cf.bm2
sum(model.matrix(babies.m2)[3,]*cf.bm2)
## Conferindo
fitted(babies.m2)[3] ## ok!
## Logo, temos duas retas que diferem quanto ao intercepto
## Um grafico
plot(bwt~gestation, data=babies, subset=smoke==F, xlim=range(babies$gestation),
     ylim=range(babies$bwt), col="blue", cex=0.5)
abline(cf.bm2[1],cf.bm2[2],lwd=2, col="blue")
points(bwt~gestation, data=babies, subset=smoke==T, col="red", cex=0.5)
abline(cf.bm2[1]+cf.bm2[3],cf.bm2[2],lwd=2, col="red")
## Volte ao resumo do modelo para se certificar que entendeu:
summary(babies.m2)
summary(babies.m1)
anova(babies.m1, babies.m2)
anova(babies.m2)
####################################
## Modelo com interacao          ###
### E o que significa Interacao? ###
####################################
babies.m3 <- lm(bwt~gestation + smoke + gestation:smoke, data=babies)
babies.m3a <- lm(bwt~gestation * smoke , data=babies)

head(model.matrix(babies.m3))
(cf.bm3 <- coef(babies.m3))
## Qual e o valor previsto para a primeira observacao agora?
model.matrix(babies.m3)[1,]
model.matrix(babies.m3)[1,]*cf.bm3
sum(model.matrix(babies.m3)[1,]*cf.bm3)
## Conferindo
fitted(babies.m3)[1] ## ok!
## E para a terceira?
cf.bm3
model.matrix(babies.m3)[3,]
model.matrix(babies.m3)[3,]*cf.bm3
sum(model.matrix(babies.m3)[3,]*cf.bm3)
## Conferindo
fitted(babies.m3)[3] ## ok!
x11()

anova(babies.m3, babies.m2)
## Ou seja, agora temos duas retas com inclinacoes diferentes!
plot(bwt~gestation, data=babies, subset=smoke==F, xlim=range(babies$gestation),
     ylim=range(babies$bwt), col="blue", cex=0.5)
abline(cf.bm3[1],cf.bm3[2],lwd=2, col="blue")
points(bwt~gestation, data=babies, subset=smoke==T, col="red", cex=0.5)
abline(cf.bm3[1]+cf.bm3[3],cf.bm3[2]+cf.bm3[4],lwd=2, col="red")
legend(locator(1), c("fumante", "ñ fumante"), col=c("red", "blue"), lty=1, bty="n")
#savePlot("ancova.jpeg", type="jpeg")
## O mesmo, em dois graficos separados
require(lattice)
xyplot(bwt~gestation|smoke, data=babies)
xyplot(bwt~gestation|smoke,data=babies,
       panel=function(x,y,...){
         panel.xyplot(x,y,...)
         panel.abline(lm(y~x),...)
     })
###usando layout
layout(matrix(c(1,2),ncol=2))
layout.show(2)
plot(bwt~gestation, subset=smoke==0, data= babies, col="blue", xlim=c(130,370), ylim=c(50,180))
abline(lm(bwt~gestation, subset=smoke==0, data=babies), col="blue")
plot(bwt~gestation, subset=smoke==1, data= babies, col="red", xlim=c(130,370), ylim=c(50,180))
abline(lm(bwt~gestation, subset=smoke==1, data=babies), col="red")

## Falta verificar se a interacao e significativa
anova(babies.m2,babies.m3) ## Sim!
## Diagnosticos do modelo
par(mfrow=c(2,2))
plot(babies.m3)
par(mfrow=c(1,1))
## resumo do modelo
summary(babies.m3)
## Intervalos de confianca dos coeficientes
coef(babies.m3)
confint(babies.m3)
###################################################################

