#################################
######### AULA ##################
################################
### simulando amostras
source("simula.amostra.aula.r")
load("./palmcb.RData")
#save(palmcb, file="palmcb.RData")

simula.amostra() # n= 10 simula=100
simula.amostra() # n=100 simula= 100
simula.amostra() # n= 10 simula=1000
simula.amostra() # n=100 simula= 1000
simula.amostra() # n= 200 simula= 1000
################################################
#### simulando a partir de uma conjunto de dados
## Perimetro do tronco de Palmitos em uma parcela
## de 10 ha no Parque Estadual de Carlos Botelho
##############################################
simula.amostra(dados=palmcb) # n= 10 simula=100
simula.amostra(dados=palmcb) # n=100 simula= 100
simula.amostra(dados=palmcb) # n= 10 simula=1000
simula.amostra(dados=palmcb) # n=100 simula= 1000
simula.amostra(dados=palmcb) # n= 200 simula= 100

############################################
### VOLTA PARA A APRESENTACAO #############
############################################

### Dados de mandibula de chacal dourado ####

macho=c(120,107,110,116, 114, 111, 113,117,114,112)
femea=c(110,111,107, 108,110,105,107,106,111,111)

media.m=mean(macho)
media.m
media.f=mean(femea)
mean(femea)
tam.mand=c(macho,femea)
sexo=factor(rep(c("macho","femea"),each=10), levels=c("macho", "femea"))
chacal=data.frame(sexo=sexo,tam.mand=tam.mand)
tapply(chacal$tam.mand, chacal$sexo, mean)
### Um grafico para ver os dados #########
example(boxplot)
oldpar<-par(bty="n")
boxplot(tam.mand~sexo, chacal, main="Tamanho da mandibula de chacal", xaxt="n")
mtext(c("Machos", "Fêmeas"),1,at=1:2)
par(oldpar)
#text(locator(1),"n=20")
#savePlot("chacalboxplot.jpeg", "jpeg")
##########################
######## Grafico dos #####
####### desvios ##########
##########################
plot(1:20,chacal$tam.mand, main="Tamanho da mandibula de chacal",pch=rep(c(15,16),each=10),col=rep(1:2,each=10), xlab="Observacoes", ylab="mandibula (mm)")
lines(c(1,1),c(chacal$tam.mand[1],media.m),col=1)
lines(c(2,2),c(chacal$tam.mand[2],media.m),col=1)
lines(c(3,3),c(chacal$tam.mand[3],media.m),col=1)
lines(c(4,4),c(chacal$tam.mand[4],media.m),col=1)
lines(c(5,5),c(chacal$tam.mand[5],media.m),col=1)
lines(c(6,6),c(chacal$tam.mand[6],media.m),col=1)
lines(c(7,7),c(chacal$tam.mand[7],media.m),col=1)
lines(c(8,8),c(chacal$tam.mand[8],media.m),col=1)
lines(c(9,9),c(chacal$tam.mand[9],media.m),col=1)
lines(c(10,10),c(chacal$tam.mand[10],media.m),col=1)
lines(c(11,11),c(chacal$tam.mand[11],media.f),col=2)
lines(c(12,12),c(chacal$tam.mand[12],media.f),col=2)
lines(c(13,13),c(chacal$tam.mand[13],media.f),col=2)
lines(c(14,14),c(chacal$tam.mand[14],media.f),col=2)
lines(c(15,15),c(chacal$tam.mand[15],media.f),col=2)
lines(c(16,16),c(chacal$tam.mand[16],media.f),col=2)
lines(c(17,17),c(chacal$tam.mand[17],media.f),col=2)
lines(c(18,18),c(chacal$tam.mand[18],media.f),col=2)
lines(c(19,19),c(chacal$tam.mand[19],media.f),col=2)
lines(c(20,20),c(chacal$tam.mand[20],media.f),col=2)
lines(c(1,10),c(media.m,media.m),col=1)
lines(c(11,20),c(media.f,media.f),col=2)
legend(locator(1),c("machos", "femeas"), pch=c(15,16),col=c(1,2), bty="n")
############################################
### VOLTA PARA A APRESENTACAO #############
############################################

dif=mean(macho)-mean(femea)
dif
mean(chacal$tam.mand)
sd(chacal$tam.mand)
mean(femea)-mean(macho)
mean(macho)-mean(femea)
dif=round(abs(mean(femea)-mean(macho)),1)
dif

### Perguntas:
## Essa diferenca pode ser gerada ao acaso?


hist(chacal$tam.mand, freq=FALSE,xlim=c(95,125), main="Distribuição de tamanho de mandibulas de Chacal", xlab= "tamanho de mandibula (mm)", ylab="densidade")
curve(exp=dnorm(x, mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand)),from=95,to=125, col="red", add=T)

rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand))
round(rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand)))
abs(round(rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand))))
abs(round (mean(rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand)))-mean(rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand)))))
abs(round (mean(rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand)))-mean(rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand)))))
abs(round (mean(rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand)))-mean(rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand)))))
abs(round (mean(rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand)))-mean(rnorm(10,mean=mean(chacal$tam.mand),sd=sd(chacal$tam.mand)))))

##### Criando Ciclos #########

for(i in 1:10)
{
#cat("\n\t", i)
cat(i)
}

for(i in c("cascão", "cebolinha", "monica"))
{
cat("\n\t", i)
#cat(i)
}
######
resulta=rep(NA,1000)
resulta
mc<-mean(chacal$tam.mand)
sdc<-sd(chacal$tam.mand)
for (i in 1:1000)
	{
	resulta[i]=
	round(abs(mean(rnorm(10,mean=mc,sd=sdc))-	
	mean(rnorm(10,mean=mc,sd=sdc))),1)
	}
resulta
hist(resulta)

### Qual cenário estamos simulando??? ###

###Vamos fazer isso 1000 vezes??

source("simula.r")

dif.chacal.bi=simula(macho,femea)
hist(dif.chacal.bi)
dif.chacal.uni=simula(macho,femea,nsim=2000)

table(dif.chacal)
n.maior=sum(Mod(dif.chacal)>=dif)
n.maior


### Qual a probabilidade da diferenca observada ter sido gerada pelo acaso?

n.maior/length(dif.chacal)


####Qual a diferenca entre um teste bicaudal e unicaudal?? #########
x11(width=11)
par(mfrow=c(1,2))
dif.chacal.bi=simula(macho,femea, teste= "bi")
dif.chacal.uni=simula(macho,femea, teste= "uni")


### Qual a probabilidade de encontrar diferencas entre machos e femeas com essa magnitude ou maiores ao acaso?
### teste bicaudal: nao importa qual grupo e maior ou menor, apenas se sao diferentes

n.maior=sum(round(dif.chacal.uni,1)>=round(dif,1))
n.menor=sum(round(dif.chacal.uni,1)<=round((dif*-1),1))
n.maior
n.menor
n.sim=1000
p.bi=(n.maior+n.menor)/1000
p.bi
### Os machos apresentam mandibulas maiores que as femeas
### Qual a probabilidade dessa diferenca para valores maiores ter sido gerada ao acaso ??
p.uni=n.maior/n.sim
p.uni
###################################################
############## Simulando o teste T ################
###################################################
## calculando a estatasticas t

v1=var(macho)
v1
v2=var(femea)
v2
n1=length(macho)
n1
n2=length(femea)
n2
## desvio padrao das diferencas
s12=sqrt((v1/n1)+(v2/n2))
s12
dif<-mean

dif
tvalor=dif/s12
tvalor
x11()
res.t= simula(macho,femea,nsim=2000, teste="t")
curve(200*dt(x,19), add=T, col="blue")

maior.menor.t=sum(res.t>=tvalor | res.t<=-tvalor)
maior.menor.t
prob.t=maior.menor.t/2000
prob.t
t.test(macho,femea)

######### ANOVA ################
## VOLTA APRESENTAcaO
### Interpretando uma tabela de ANOVA ######
######################################


### Para o slide ######

are=c(6,10,8,6,14,17,9,11,7,11)
are
arg=c(17,15,3,11,14,12,12,8,10,13)
arg
hum=c(13,16,9,12,15,16,17,13,18,14)
hum
solos=data.frame(are,arg,hum)
solos
str(solos)
boxplot(solos)
vetor.dados=c(are,arg,hum)
vetor.dados
#### Calculos base ###

media.geral=mean(vetor.dados)
media.geral
media.solos=apply(solos,2, mean)
vetor.obs=1:30
vetor.cor=rep(1:3,each=10)
vetor.medias=rep(media.solos, each=10)


############################
#### Calculo da SS total ###
		###Grafico ###
plot(vetor.obs,vetor.dados,ylim=c(0,20),pch=(rep(c(15,16,17),each=10)),col=vetor.cor,ylab="Variável Resposta", xlab="Observações")
xy=locator(1)
legend(xy, legend=c("arenoso","argiloso", "humico"), pch=c(15,16,17), col=1:3, bty="n")

	for(i in 1:30)
	{
	lines(c(i,i),c(vetor.dados[i],mean(vetor.dados)),col=vetor.cor[i])
	}
	abline(h=media.geral)


media.geral
dif.geral=solos-media.geral
dif.geral
sum(dif.geral)
round(sum(dif.geral),10)

ss.solos=dif.geral^2
ss.solos
ss.total=sum(ss.solos)
ss.total

#########################
##### SS intra grupo ##### 
	##Gráfico
plot(vetor.obs,vetor.dados,ylim=c(0,20),pch=(rep(c(15,16,17),each=10)),col=vetor.cor,main="Variação Intra Grupos",ylab="Variável Resposta", xlab="Observações")
	for(i in 1:30)
	{
	lines(c(i,i),c(vetor.medias[i],vetor.dados[i]),col=vetor.cor[i])
	}
	lines(c(1,10),c(media.solos[1],media.solos[1]),col=1)
	lines(c(11,20),c(media.solos[2],media.solos[2]),col=2)
	lines(c(21,30),c(media.solos[3],media.solos[3]),col=3)

legend(locator(1), legend=c("arenoso","argiloso", "humico"), pch=c(15,16,17), col=1:3, bty="n")
solos
media.solos
ss.are=sum((are-media.solos["are"])^2)
ss.are
ss.arg=sum((arg-media.solos["arg"])^2)
ss.arg
ss.hum=sum((hum-media.solos["hum"])^2)
ss.hum
ss.intra=ss.are+ss.arg+ss.hum
ss.intra

sum((solos-vetor.medias)^2)

###############################
####### SS entre grupos #######
	## Gráfico
	plot(vetor.obs,vetor.medias,ylim=c(5,16),pch=(rep(c(15,16,17),each=10)),col=vetor.cor,main="Variação Entre Grupos",ylab="Variável Resposta", xlab="Observações")
		for(i in 1:30)
		{
		lines(c(i,i),c(vetor.medias[i],mean(vetor.medias)),col=vetor.cor[i])
		}
	abline(h=media.geral)
	points(vetor.obs,vetor.dados,ylim=c(0,20),pch=(rep(c(0,1,2),each=10)),col=vetor.cor,cex=0.5)
legend(locator(1), legend=c("arenoso","argiloso", "humico"), pch=c(15,16,17), col=1:3, bty="n")

media.solos=apply(solos,2,mean)
media.solos
media.geral
ss.entre=10*sum((media.solos-media.geral)^2)
ss.entre
 
ss.intra+ss.entre
ss.total
#########################
#### Calculo do F #######
#########################
ms.entre=ss.entre/2
ms.intra=ss.intra/27
ms.entre
ms.intra
F.solos=ms.entre/ms.intra
F.solos

pf(F.solos, 2, 27, lower.tail=FALSE)

########################################
#### Gráfico de densidade probabilistica
######## Distribuição F ################
###################################### 
curve(expr=df(x, 2,27),main="Distribuição F de Fisher (df=2,27)", xlab="Valor F",ylab="Densidade Probabilística (df)",xlim=c(0,10))
abline(v=F.solos,col="red")
abline(h=0, lty=2)
xf=seq(F.solos,10,0.01)
ydf=df(xf,2,27)
polygon(c(F.solos,xf),c(0,ydf),col="red")
text(locator(1),paste("pf(x) =",round(pf(F.solos,2,27,lower.tail=F),4)),cex=0.8, col="red")
#savePlot("densPF.jpeg", "jpeg")
####### Graficos finais ####
par(mfrow=c(2,2))
plot(vetor.obs,vetor.dados,ylim=c(0,20),pch=(rep(c(15,16,17),each=10)),col=vetor.cor,main="Efeito do Solo",ylab="Variável Resposta", xlab="Observações")
  ## Variação total
plot(vetor.obs,vetor.dados,ylim=c(0,20),pch=(rep(c(15,16,17),each=10)),col=vetor.cor,main="Variação Total",ylab="Variável Resposta", xlab="Observações")
	for(i in 1:30)
	{
	lines(c(i,i),c(vetor.dados[i],mean(vetor.dados)),col=vetor.cor[i])
	}
	abline(h=media.geral)

  ### Variacao intra grupo
plot(vetor.obs,vetor.dados,ylim=c(0,20),pch=(rep(c(15,16,17),each=10)),col=vetor.cor,main="Variação Intra Grupos",ylab="Variável Resposta", xlab="Observações")
	for(i in 1:30)
	{
	lines(c(i,i),c(vetor.medias[i],vetor.dados[i]),col=vetor.cor[i])
	}
	lines(c(1,10),c(media.solos[1],media.solos[1]),col=1)
	lines(c(11,20),c(media.solos[2],media.solos[2]),col=2)
	lines(c(21,30),c(media.solos[3],media.solos[3]),col=3)
## Variacao entre grupos

plot(vetor.obs,vetor.medias,ylim=c(5,16),pch=(rep(c(15,16,17),each=10)),col=vetor.cor,main="Variação Entre Grupos",ylab="Variável Resposta", xlab="Observações")
	for(i in 1:30)
	{
	lines(c(i,i),c(vetor.medias[i],mean(vetor.medias)),col=vetor.cor[i])
	}
abline(h=media.geral)
points(vetor.obs,vetor.dados,ylim=c(0,20),pch=(rep(c(0,1,2),each=10)),col=vetor.cor,cex=0.5)

par(mfrow=c(1,1))

##### Anova no R ###############
str(solos)
var.resp=c(solos$are,solos$arg,solos$hum)
var.resp
solos.f=factor(rep(c("are", "arg","hum"),each=10))
solos.f
dados.solos=data.frame(var.resp,solos.f)
res.anova=aov(var.resp~solos.f)
res.anova1=aov(var.resp~solos.f, data=dados.solos)
res.anova
summary(res.anova)
summary(res.anova1)
lm1<-lm(var.resp~solos.f, data=dados.solos)
summary(lm1)
anova(lm1)


#########################################################
############# FUNCOES ######################
#########################################################
#########################################################
############# FUNCAO SIMULA.AMOSTRA######################
#########################################################
simula.amostra=function(dados=rnorm(10000))
{
x11()
nomessage=""
namost=paste("Qual o tamanho da amostra: ")
message=readline(nomessage)
amostra=readline(namost)
amostra<-as.numeric(amostra)
sim.n=paste("Quantas vezes deseja simular a amostra: ")
nsim=as.numeric(readline(sim.n))
arco=rainbow(nsim)
result=rep(NA,nsim)
max.dado=max(dados)
min.dado=min(dados) - 0.1* min(dados)
	for(i in 1:nsim)
	{
	result[i]<-mean(sample(dados,amostra),1)
	titulo= paste("Simulação de Amostra = ", amostra, "\t")
        subt= paste("simulação no. ", i) 
	hist(result,col=arco[i], main=titulo, sub=subt,xlim=c(min.dado,max.dado),xlab="média da amostra")
	}
abline(v=mean(dados),lty=2)
invisible(result)
}
