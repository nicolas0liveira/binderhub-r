#options(help_type="html")
#########################
### Dispositivos Graficos
#########################
X11() ## abre um dispositivo de tela
plot(1:10) 
## fechar o dispositivo x11()
plot(1:10, col="red")## função de nível alto abre o dispositivo de tela automaticamente
### fechar o dispositivo x11()
rect(xleft=1, ybottom=5, xright=3, ytop=7, col="red") ## nível baixo  não funciona sem a de nível alto
###
plot(1:10)
rect(xleft=1, ybottom=5, xright=3, ytop=7, col="red")
rect(xleft=1:5,ybottom=2:6, xright=(1:5)+2, ytop=(2:6)+2, col="blue") 
#
x11()
plot(1:10)
rect(xleft=1, ybottom=5, xright=3, ytop=7, col="red")
rect(xleft=1:5,ybottom=2:6, xright=(1:5)+4, ytop=(2:6)+2, col=1:5) 
########################
### OUTROS DISPOSITIVOS
########################
?png
getwd()
dir()
png(file="myplot2017_%03d.png",bg = "transparent", width=1024, height=768) # abre o dispositivo de arquivo png )
dir()
dir( pattern="myplot")
plot(1:30)
rect(xleft=1, ybottom=5, xright=3, ytop=7, col="red")
rect(xleft=1:5,ybottom=2:6, xright=(1:5)+2, ytop=(2:6)+2, col=1:5)
plot(1:20, col="red")
rect(xleft=1, ybottom=5, xright=3, ytop=7, col="red")
rect(xleft=1:5,ybottom=2:6, xright=(1:5)+2, ytop=(2:6)+2, col="blue")
dev.cur()
dev.list()
dev.set(3)
points(1:10, 1:10, col="red") 
dev.cur()
dev.cur
dev.list()
#npng <- dev.list()[which(names(dev.list())=="png")]
dev.set(4)
dev.off() ## grava o gráfico no dispositivo
dir()
dir(pattern = "myplot")
graphics.off()

#### Gerenciando Dispositivos
png(file="myplot%02d.png", width=1024, height=768)
tiff(file="myplot%02d.tif")
dev.list() # lista dispositivos abertos
plot(1:10)
x11()
dev.cur() # o dispositivo ativo
dev.set(2) # muda o dispositivo ativo
plot(2:20, col="red")
dev.list()
dev.cur() 
dev.set(4)
dev.cur()
dev.off()
dev.list()
dev.cur()
dir(pattern="myplot")
## fechar a janela gráfica
dev.off()
dev.list()
graphics.off() # fecha todas os graphicos abertos

########################
#### Apresentação #####
#######################

#################################
######## FUNCÕES NÍVEL ALTO
#################################
###################
# Função plot ( ) #
###################
getwd()
dir()
aves.c <- read.table("aves_cerrado.csv", head=TRUE, as.is=TRUE, sep=";")
str(aves.c)
aves.c[is.na(aves.c)] <- 0
names(aves.c)[1]
names(aves.c)[1] <- "amostra"
#### Agora o plot
plot(aves.c$urubu, aves.c$carcara) # ou
plot(carcara ~ urubu, data=aves.c, col="red") # ou
plot(aves.c[,3:4], col = "blue") # matriz de duas colunas
## Um fator
aves.c$fisionomia<-as.factor(aves.c$fisionomia)
plot(aves.c[,2:3]) # box-plot
aves.c[aves.c=="ce"]<-"Ce"
table(aves.c$fisionomia)
plot(aves.c[,2:3]) # box-plot
unique(aves.c$fisionomia)
## retirando um nivel inexistente
aves.c$fisionomia<-factor(aves.c$fisionomia, levels=c("Ce", "CC", "CL"))
str(aves.c)
attributes(aves.c$fisionomia)
####
colnames(aves.c)
plot(aves.c[,3:5])
plot(aves.c[,2:5]) # matriz de dispersão: todas as variáveis
plot(~ urubu + carcara + seriema, data=aves.c) # variáveis da fórmula
#plot(seriema ~ urubu + carcara, data=aves.c) # 'seriema' vs. as demais
#####
#Objeto da classe serie temporal!
x<-ts(rpois(30, 5)) # cria 30 valores de uma serie temporal com dados de Poisson
str(x)
plot(x) # gráfico de série temporal
#############################################
## funções de nível alto: abrem o dispositivo de tela caso não haja algum aberto
example(plot)
example(contour)
example(hclust)
example(image)

## example(image)
## example(persp)
demo(image)
graphics.off()
#demo(persp)
########################
### Apresentação
#########################
## funçoes de nivel baixo
example(points)
example(segments)

example(rect)
example(axis)
example(colors)
############
## RGALLERY
############
http://rgraphgallery.blogspot.com.br/
####################
# Grafico coelhinho
####################
#install.packages("onion")
library(onion)
data(bunny)
p3d(bunny,theta=3,phi=104,box=FALSE)
str(bunny)
###################
## Fractal animado
##################
#install.packages("caTools")
library(caTools)
jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")) 
m = 600
C = complex( real=rep(seq(-1.8,0.6, length.out=m), each=m ), 
             imag=rep(seq(-1.2,1.2, length.out=m), m ) ) 
C = matrix(C,m,m)
Z = 0 
X = array(0, c(m,m,20))
for (k in 1:20) { 
  Z = Z^2+C 
  X[,,k] = exp(-abs(Z)) 
} 
write.gif(X, "Mandelbrot.gif", col=jet.colors, delay=20)
#######################
## Parametros Graficos
#######################
par()
?par
par()$mfrow
par()$mar
oldpar<-par(mfrow=c(2,2), mar=c(4,4,1,1))
oldpar

par()$mfrow
plot(1:10, col=1, pch=1, cex=2)
plot(1:10, col=2, pch=2, cex=2)
plot(1:10, col=3, pch=3, cex=2)
plot(1:10, col=4, pch="*", cex=2)
par(oldpar)
par()$mfrow
par()$mar
ls()
#######################
## criando um gráfico mais elaborado
## Cris Banks
#######################
riqueza =c(15,18,22,24,25,30,31,34,37,39,41,45)
area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
par(mfrow = c(2,2))
plot(riqueza~area)
#############################
## mudando legenda e margens
plot(riqueza~area, xlab="Área (ha)", ylab="Riqueza de espécies de\n aves de sub-bosque")
par()$mar
par(mar=c(5,6.7,2,1))
plot(riqueza~area, xlab="Área (ha)", ylab="Riqueza de espécies de\n aves de sub-bosque")
#####################################
## mudando fonte e tamanhos de eixo e 
#####################################
par(cex.axis=1.5, ## aumenta os caracteres dos eixos em 50% 
    cex.lab=2, ## aumenta as legendas dos eixos em 100%
    family="serif") ## muda a fonte
plot(riqueza~area, xlab="Área (ha)", ylab="Riqueza de espécies de\n aves de sub-bosque", cex=2, pch=16)
########################################################
# mudando as marcas dos eixos e a disposição dos valores
########################################################
## dev.list()
## dev.cur()
## dev.set(3)
## dev.off()
## graphics.off()
x11()
### tem que mudar os parâmetros anteriores
par(mar=c(5,6.7,2,1), # muda o tamanho das margens
    cex.axis=1.5, ## aumenta os caracteres dos eixos em 50% 
    cex.lab=2, ## aumenta as legendas dos eixos em 100%
    family="serif") ## muda a fonte
### novos comandos
par(las=1, #legenda do eixo(1=horizontal)
    tcl=0.3, # marca da escala para dentro do eixo
    mgp=c(3,0.3,0), # distancia  (titulo, legenda e linha)
    cex.axis=1.5, # incremento de tamanho dos eixos
    cex.lab=2 ) # incremento das legendas

plot(riqueza~area, xlab="Área (ha)", ylab="Riqueza de espécies de\n aves de sub-bosque", cex=2, pch=16)

###################################
# Inserindo mais informação:
z <- c(50,42,33,29,25,19,17,15,10,11,8,9)

par(mar=c(5,6.7,2,4), # aumento da margem direita
    cex.axis=1.5,  
    cex.lab=2, 
    family="serif",
    las=1,
    tcl=0.3,
    mgp=c(3,0.3,0), 
    cex.axis=1.5, 
    cex.lab=2 )
## primeiro gráfico
plot(riqueza~area, xlab="Área (ha)", ylab="Riqueza de espécies de\n aves de sub-bosque", cex=2, pch=16, bty="u", xaxp=c(0,100,5), ylim= c(10,50))
## mudando parametro para inserir plot no anterior
par(new=TRUE)
plot(z~area, yaxt="n", ylab="", xlab= "",  axes=TRUE, cex=2, pch=17) # não anota titulos e eixos
axis(4) # incluir eixo na posicão 4
###############################################
### inserindo mais + mais informação
###Abri uma nova  janela gráfica!!! Perdi todo o par anterior
###############################################
box <- c(10,13,12,14,15,12,14,15,20,23,22,21,26,27,28,25)
samples <- rep(1:4,each=4)
x11()
par(mfrow=c(2,1)) # separa o dispositivo em colunas e linhas de tamanhos iguais
par(mar=c(3,5,2,5), cex.axis=1.3, cex.lab=1.5, mgp=c(2,0.3,0),	family="serif",las=1, tcl=0.3)
plot(riqueza~area, xlab="Área (ha)", ylab="Riqueza de espécies de\n aves de sub-bosque", cex=1.5, pch=16,  xaxp=c(0,100,4), ylim= c(10,50), bty="u")
## mudando parametro para inserir plot no anterior
par(new=TRUE)
plot(z~area, yaxt="n", ylab="", xlab="", axes=FALSE, cex=1.5, pch=17) 
axis(4)
### outro gráfico no mesmo dispositivo
par(mar=c(3,5,0.5,4))
par(bty="n")
boxplot(box~samples)
#######################
### Inserindo mgptexto
#######################
par(mar=c(5,5,2,5), mfrow=c(2,1))
plot(riqueza~area, xlab="Área (ha)", ylab="Riqueza de espécies de\n aves de sub-bosque", cex=1.5, pch=16,  xaxp=c(0,100,4), ylim= c(0,50), xlim=c(0,100),bty="u")
text(10,50, "a", cex=1.8)
###
par(new=TRUE)
plot(z~area, ann=FALSE, axes=FALSE, cex=1.5, pch=17) 
axis(4)
mtext("Riqueza de espécies\nda matriz", side=4, cex=1.5, line=3.2, las=0)
###
par(mar=c(3,5,0.5,4))
par(bty="l")
boxplot(box~samples, names= c("", "", "", ""))
#boxplot(box~samples, names=c("Tipo 1", "Tipo 2", "Tipo 3", "Tipo 4")) ## funciona tb!
mtext(c("Tipo 1", "Tipo 2", "Tipo 3", "Tipo 4"),side= 1,cex=1.3,line=0.3,at=c(1,2,3,4))
mtext("Diversidade genética", side=2, cex=1.5, line=2.5, las=0)
mtext("Morfotipo", side=1, cex=1.7, line=2)
text(0.87,28, "b", cex=1.8)
################################
### Inserindo linhas de tendência, legenda com cor e simbolos
# e cores: código definitivo! 
## legenda commgp interação de tela!
##############################
model <- lm(riqueza~area)
model1 <- glm(z~area, poisson)
par(mfrow=c(2,1), # divide o dispositivo gráfico em duas linhas e uma coluna
    mar=c(3,5,2,2), # tamanho das margens (lns)
    cex.axis=1.3, # aumenta os eixos em 30%
    cex.lab=1.5, # aumenta as legendas em 50%
    family="serif", # muda a familia de fontes
    las=1, # legenda dos eixos paralelo aos eixos
    tcl=0.3, # marcação das escalas nos eixos interna e tam 0.3
    mgp=c(2,0.3,0), # distância do titulo, legenda e eixo no grafico
    bty="u") # tipo de fechamento da caixa do gráfico
plot(riqueza~area, xlab="Área (ha)", ylab="Riqueza de espécies\n de aves", cex=1.5, pch=16,  ylim=c(8, 50), xaxp=c(0,100,4), col="firebrick3")
text(10,50, "a", cex=1.8)
abline(model, lwd=1.5,col="firebrick3")
#par(new=TRUE)
points(z~area, cex=1.5, pch=17, col="mediumblue")  
axis(4)
#mtext("Riqueza de espécies \nde matrix", side=4, cex=1.5, line=3.2, las=0, col="mediumblue")
xv<-seq(0,100,0.2)
yv<-exp(predict(model1,list(area=xv)))
lines(xv,yv, lwd=1.5, lty=2, col="mediumblue")
#############################
#### FUNÇÃO INTERATIVA!!
locator(1)
legend(locator(1), legend=c("sub-bosque", "matriz"), pch=c(16, 17), col=c("firebrick3","mediumblue") , bty="n")
##############mgp##########
par(mar=c(5,5,0.5,2),bty="l")
boxplot(box~samples,names= c("", "", "", ""),col="grey")
mtext(c("Tipo 1", "Tipo 2", "Tipo 3", "Tipo 4"),side= 1,cex=1.3,line=0.3,at=c(1,2,3,4))
mtext("Diversidade genética", side=2, cex=1.5, line=2.5, las=0)
mtext("Morfotipo", side=1, cex=1.7, line=3)
text(0.7,28, "b", cex=1.8)
savePlot("cris_graf.jpeg", type="jpeg")
##########################
##### Grafico Cris LIMPO
##########################
## riqueza <- c(15,18,22,24,25,30,31,34,37,39,41,45)
## area <- c(2,4.5,6,10,30,34,50,56,60,77.5,80,85)
## z <- c(50,42,33,29,25,19,17,15,10,11,8,9)
## box <- c(10,13,12,14,15,12,14,15,20,23,22,21,26,27,28,25)
## samples <- rep(1:4,each=4)
## model <- lm(riqueza~area)
## model1 <- glm(z~area, poisson)
## par(mfrow=c(2,1), 
##     mar=c(3,5,2,2), # tamanho das margens (lns)
##     cex.axis=1.3, # aumenta os eixos em 30%
##     cex.lab=1.5, # aumenta as legendas em 50%
##     family="serif", # muda a familia de fontes
##     las=1, # legenda dos eixos paralelo aos eixos
##     tcl=0.3, # marcação das escalas nos eixos interna e tam 0.3
##     mgp=c(2,0.3,0), # distância do titulo, legenda e eixo no grafico
##     bty="u")  # tipo de fechamento da caixa do gráfico
## plot(riqueza~area, xlab="Área (ha)", ylab="Riqueza de espécies\n de aves", cex=1.5, pch=16,  ylim=c(8, 50), xaxp=c(0,100,4), col="firebrick3")
## text(10,50, "a", cex=1.8)
## abline(model, lwd=1.5,col="firebrick3")
## #par(new=TRUE)
## points(z~area, cex=1.5, pch=17, col="mediumblue")  
## axis(4)
## #mtext("Riqueza de espécies \nde matrix", side=4, cex=1.5, line=3.2, las=0, col="mediumblue")
## xv<-seq(0,100,0.2)
## yv<-exp(predict(model1,list(area=xv)))
## lines(xv,yv, lwd=1.5, lty=2, col="mediumblue")
## legend(35, 48, legend=c("sub-bosque", "matriz"), pch=c(16, 17), col=c("firebrick3","mediumblue") , bty="n")
## ##############mgp##########
## par(mar=c(5,5,0.5,2), bty="l")
## boxplot(box~samples,names= c("", "", "", ""),col="grey")
## mtext(c("Tipo 1", "Tipo 2", "Tipo 3", "Tipo 4"),side= 1,cex=1.3,line=0.3,at=c(1,2,3,4))
## mtext("Diversidade genética", side=2, cex=1.5, line=2.5, las=0)
## mtext("Morfotipo", side=1, cex=1.7, line=3)
## text(0.7,28, "b", cex=1.8)
## ##########################


##################################
##### UM GRÁFICO DE METANALISE
### Dados modificados de um estudo real (Castanho & Prado 2015) 
###################################################
##Graficos meta-analysis## march 2011
#SURVIVAL
layout(matrix(c(1,2),ncol=2, nrow=1), width=c(8,2))
layout.show(2)
#####
par (mar=c(5,4,4,3.5))#controla tamanhos das margens

plot(x=NULL,y=NULL, xlim=c(-1.5,2.5), ylim=c(0.5,7.5),type="n", yaxt="n", xlab="Effect Size (lnOR)", ylab="", main="SURVIVAL")
abline (v=0, lty=2) ## desenha linhas de regressão (a+bx) ou v=vertical, h=horizontal
abline (h=c(3,6))
axis(side=4, at=c(1,2,4,5,7), labels=c("adult (2)", "young (28)", "temperate (28)", "tropical (2)", "overall (30)"),las=2 ) # desenha o eixo
#locator(1)
#ADULT
points (x=-0.577,y=1, pch=19) # pch: tipo de simbolo
points (x=-1.2,y=1, pch="|", cex=1.2)
points (x= 0.05,y=1, pch="|", cex=1.2)
segments(x0=-1.2, y0=1, x1= 0.05, y1=1) # um segmento
#YOUNG
points (x=0.87,y=2, pch=19)
points (x=-0.05,y=2, pch="|")
points (x=1.1,y=2, pch="|")
segments(x=1.1, y0=2, x1=-0.05, y1=2)
#TEMPERATE
points (x=0.01,y=4, pch=19)
points (x=-0.07,y=4, pch="|")
points (x=0.5,y=4, pch="|")
segments(x=-0.07, y0=4, x1=0.5, y1=4)
#TROPICAL
points (x=1.06,y=5, pch=19)
points (x=0.946,y=5, pch="|")
points (x=2.073,y=5, pch="|")
segments(x=2.073, y0=5, x1=0.946, y1=5)
#OVERALL
points (x=0.457,y=7, pch=19)
points (x=0.025,y=7, pch="|")
points (x=0.847,y=7, pch="|")
segments(x=0.025, y0=7, x1=0.847, y1=7)
#SEGUNDA PARTE
par (mar=c(5,4,4,2.9))#controla tamanhos das margens
plot(x=NULL,y=NULL, xlim=c(0,2), ylim=c(0.5,7.5),type="n", xaxt="n", yaxt="n",xlab="", ylab="", bty="n") # bty: tipo de caixa no limite do gráfico; xaxt: tipo de eixo x 
points(x=0.5, y=0.4, pch="-")
points (x=0.5, y=2.6, pch="-")
segments(x0=0.5, y0=0.4, x1=0.5, y1=2.6)
axis(side=4, at=1.5, labels= "Target life stage", lwd.ticks=0)
points(x=0.5, y=3.4, pch="-")
points (x=0.5, y=5.6, pch="-")
segments(x0=0.5, y0=3.4, x1=0.5, y1=5.6)
axis(side=4, at=4.5, labels= "Geographic region", lwd.ticks=0)
#para salvar em JPEG ou outras extensões
#X11(type="cairo")
#refazer o gráfico
#savePlot("Plot_survival.jpeg", type="jpeg")
#savePlot("Plot_survival.tiff", type="tiff")
#savePlot("Plot_survival.png", type="png")
#################################
# Usando vetores para encurtar o código
#################################
layout (matrix(c(1,2),ncol=2, nrow=1), width=c(8,2))
layout.show(2)
par (mar=c(5,4,4,3.5))#controla tamanhos das margens
plot(x=NULL,y=NULL, xlim=c(-1.5,2.5), ylim=c(0.5,7.5),type="n", yaxt="n", xlab="Effect Size (lnOR)", ylab="", main="SURVIVAL")
abline (v=0, lty=2) ## desenha linhas de regressão (a+bx) ou v=vertical, h=horizontal
abline (h=c(3,6))
axis(side=4, at=c(1,2,4,5,7), labels=c("adult (2)", "young (28)", "temperate (28)", "tropical (2)", "overall (30)"),las=2 ) # desenha o eixo
points (x=c(-0.577, 0.87, 0.01, 1.06,0.457), y=c(1,2, 4, 5, 7), pch=19 ) #medias
points (x=c(-1.2, 0.05, 0.05, 1.1, -0.07, 0.5, .946,2.073, 0.025,0.847), y=rep(c(1,2, 4, 5, 7),each=2), pch= "|" )# limites do IC 95% (pch= tipo de simbolo)
segments(x=c(-1.2,  0.05, -0.07, .946, 0.025), y0=c(1,2, 4, 5, 7), x1=c( 0.05, 1.1, 0.5,2.073,0.847), y1=c(1,2, 4, 5, 7)) # os segmentos do IC
#SEGUNDA PARTE
par (mar=c(5,4,4,2.9))#controla tamanhos das margens
plot(x=NULL,y=NULL, xlim=c(0,2), ylim=c(0.5,7.5),type="n", xaxt="n", yaxt="n",xlab="", ylab="", bty="n") # bty: tipo de caixa no limite do gráfico; xaxt: tipo de eixo x 
points(x=rep(c(0.5),4), y=c(0.4, 2.6, 3.4, 5.6), pch="-")
segments(x0=c(0.5, 0.5), y0=c(0.4, 3.4), x1=c(0.5,0.5), y1=c(2.6, 5.6))
axis(side=4, at=1.5, labels= "Target life stage", lwd.ticks=0)
axis(side=4, at=4.5, labels= "Geographic region", lwd.ticks=0)
#para salvar em JPEG ou outras extensões
savePlot("metagraf.png", type="png")
###################################

