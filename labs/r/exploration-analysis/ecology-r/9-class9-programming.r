options(help_type="html")
##############################
## Codigo Aula Programação ###
##############################
rm(list=ls())
ls()
dir()
source("simula.r") 
ls()
args(simula)
macho=c(120,107,110,116, 114, 111, 113,117,114,112)
femea=c(110,111,107, 108,110,105,107,106,111,111)
mf=c(macho,femea)
simula(macho,femea)
simula
############################# 
## mostrar código simula ####
#############################
##     SLIDE
######################
source("eda.shape.r")
ls()
eda.shape
# mostrar codigo no editor
rm(list=ls())
ls()
eda.shape<-function(x)
	{
	par(mfrow = c(2,2)) 
	hist(x)             
	boxplot(x)          
	iqd <- summary(x)[5] -	summary(x)[2]
	plot(density(x,width=2*iqd),xlab= "x",ylab="",type="l")
	qqnorm(x)
	qqline(x)
	par(mfrow=c(1,1))
    }
ls()
eda.shape(mf)
summary(mf)
eda.shape(x=mf)
### argumento ###
############################
### modificando eda.shape ##
############################
eda.shape1 <- function(x)
	{
	x11()  # abre um dispositivo grafico
	par(mfrow = c(2,2))
	hist(x,main="Histograma de x") # titulo
	boxplot(x, main="BoxPlot de x") #titulo
	iqd <- summary(x)[5] -	summary(x)[2]
	plot(density(x,width=2*iqd),xlab="x",ylab="",type="l", main="Distribuição de densidade de x") # titulo
	qqnorm(x,col="red",main="Gráfico Quantil x Quantil",xlab="Quantil Teórico",ylab="Quantil da Amostra") # titulo
	qqline(x)
	par(mfrow=c(1,1))
	}
##########################################
### executando eda.shape & eda.shape1 ####
##########################################
set.seed(42)
eda.shape(rnorm(500,6))
x11()
eda.shape1(rnorm(500,6))
##########################################
eda.shape2 <- function(x, nome.var="x")
	{
	x11()  # abre um dispositivo grafico
	par(mfrow = c(2,2))
	hist(x,main=paste("Histograma de", nome.var)) # titulo
	boxplot(x, main=paste("BoxPlot de", nome.var)) #titulo
	iqd <- summary(x)[5] -	summary(x)[2]
	plot(density(x,width=2*iqd),xlab="x",ylab="",type="l", main="Distribuição de densidade") # titulo
	qqnorm(x,col="red",main="Gráfico Quantil x Quantil",xlab="Quantil Teórico",ylab="Quantil da Amostra") # titulo
	qqline(x)
	par(mfrow=c(1,1))
	}
#############################
eda.shape2(mf, "")
#############################
####### VOLTA APRESENTAÇÃO ##
#############################
### FAZENDO VERSÕES PIORADAS DE FUNçÕES EXISTENTES!!!!
### funções simples
media <-function(x)
	{
	soma=sum(x)
	nobs=length(x)
	md=soma/nobs
	return(md)
	}
#############################
ls()
media
media()
dados=rnorm(20,2,1)
dados
media(dados)
dados1=rnorm(200,2,1)
media(dados1)
dados2=(rnorm(10000,2,1))
media(dados2)
sd(dados)
sd(dados2)
dados3=rnorm(20,2,0.01)

media(dados3)
dados4=rnorm(200,2,0.01)
media(dados4)
dados[2]<-NA
dados
media(dados)
mean(dados)
mean(dados,na.rm=T)
media(dados,na.rm=T)
args(media)
args(mean)
#############################
# volta para a apresentação
#############################
##Uma função mais elaborada, considerando a
##presença e excluíndo NA por padrão, e lançando mensagem na tela 
##sobre o número de NAs removidos. Note que é uma função com dois argumentos
##que permite ao usuário tomar a decisão de remover ou não NAs, diferente da função
##mean() em que o padrão é invertido. Note que vamos sobrepor o objeto anterior. 
########################################################
media<-function(x,rmNA=TRUE)  
	{
	if(rmNA==TRUE)
		{
		dados1=(na.omit(x))
		n.NA=length(x)-length(dados1)
		cat("\t", n.NA," valores NA excluídos\n")
		}
		else
		{
		dados1=x
		}
	eda.shape1(dados1)
	soma=sum(dados1)
	nobs=length(dados1)
	media=soma/nobs
	return(media)
	}
?"if"
###calcular a média do vetor dados.dens
dados
res.med<-media(dados)
res.med
minha.media<-media(dados)
###################################
###função para calcular variância
##################################
variancia<-function(x)
	{
	source("media.r")
	med=media(x)
	dados=na.omit(x)	
	disvquad=(dados-med)^2
	vari=sum(disvquad)/(length(dados)-1)
	return(vari)
	}

###Calcular a variância de dados e comparando com a função do R!
variancia(dados)
var(dados)
var(dados,na.rm=T)
#################################################################
## O índices de dispersão nos ajuda a verificar se os dados são 
###  distribuidos aleatório, agregado ou uniforme 
### Gráfico #######
amost1= c(4,6,4,4,5,6,4,5,7)
amost2=c(1,7,2,8,9,7,1,8,2)
plot(jitter(rep(c(1,2),each=9)),c(amost1,amost2), col=rep(c(1,2),each=9), xlim=(c(0,3)), pch=16,bty="l", cex=1.5, xaxt="n",xlab= "Amostras", ylab="Número de Indivíduos",cex.lab=1.2, bg="transparent")
axis(1, c("Local A", "Local B"), at=c(1,2))
abline(h=mean(amost1), lty=2, lwd=2)
mean(amost1)
mean(amost2)
var(amost1)
var(amost2)
savePlot("disp.png", type="png" )
##########################
# volta apresentação  ####
##########################
##########################
ID.curso<-function(x)
	{
	id=variancia(x)/media(x)
	return(id)
	}
##################################
ls()
ID.curso
###Tomando dados simulado de  contagem de uma espécie em uma amostra de 20 parcelas de 20x20m, 
###podemos verificar o padrão de dispersão dessa espécie, utilizando o índice de Dispersão (razão variância / média)
###Antes vamos simular dados com parânetros conhecidos:
## 1. Simulando Aleatório
aleat=rpois(200,5)
aleat
## 2. Uniforme
unif=runif(200,0,4)
unif
unif=round(unif,0)
unif
## 3. Agregado
agreg=rnbinom(200,size=20, mu=5) # vinte arvores em média 5 spp
agreg
##Calcular o  coeficiente de dispersão 

ID.curso(aleat)

ID.curso(unif)

ID.curso(agreg)

agreg

##Quando o valor próximo a 1  é considerada aleatória. 
##Podemos fazer um teste de significância simplificado 
#########################
# VOLTA A APRESENTAÇÃO ##
#########################
x=unif
x
nsim=10
test.ID <- function(dados, nsim=1000)
{ 
	ndados=length(dados)
	med=mean(dados)
	id=var(dados)/med
	simula.aleat=rpois(length(dados)*nsim, lambda=med)
	sim.dados=matrix(simula.aleat,ncol= ndados)
	sim.ID=apply(sim.dados,1,ID.curso)
	quant.ID=quantile(sim.ID, probs=c(0.025,0.975))
		if(id>=quant.ID[1] & id<=quant.ID[2])
		{ 
		cat("\n\n\n\t distribuição aleatória para alfa=0.05 \n\t ID= ",id,"\n\n\n")
		}
		if(id < quant.ID[1]) 
		{ 
		cat("\n\n\n\t distribuição uniforme, p<0.025 \n\t ID= ",id,"\n\n\n")
		}
		if(id>quant.ID[2])
		{ 
		cat("\n\n\n\t distribuição agregado, p>0.975 \n\t ID= ",id,"\n\n\n")
		}
	   resulta=c(id,quant.ID)
	   names(resulta)<-c("Indice de Dispersão", "critico 0.025", "critico 0.975")
      return(resulta)
}
###################################
###### Testanto os dados simulados
###################################
test.ID(aleat, nsim=10000)
test.ID(agreg)
test.ID(unif, nsim=10000)

##############################
#CONTINUA: Apresenta ciclos ##
##############################
############ CICLOS ##################
######################################
###Um outro instrumento importante para programar em R é o loop.
###Ele permite a aplicação de uma função ou tarefa a uma sequência pré determinada de dados
###Vamos novamente simular dados Espécies (ind) x Parcelas

x1=rpois(20,1)
x2=rpois(20,2)
x3=rpois(20,3)
x4=rpois(20,1)
sp.oc=matrix(c(x1,x2,x3,x4), ncol=4)
colnames(sp.oc)<-c("plot A", "plot B", "plot C", "plot D")
rownames(sp.oc)<-paste("sp", c(1:20))
str(sp.oc)
dim(sp.oc)
head(sp.oc)

#########################
##  VOLTA A APRESENTACÃO
#########################
###mais função!! SIMILARIDADE
sim<-function(dados)
	{
	nplot=dim(dados)[2]
	similar=matrix(NA,ncol=nplot,nrow=nplot)
	diag(similar)<-1
	rownames(similar)<-paste("plot", c(1:nplot))
	colnames(similar)<-paste("plot", c(1:nplot))
	dados[dados>0]=1
		for(i in 1:(nplot-1))
		{
		for(m in (i+1):nplot)
			{
			co.oc=sum(dados[,i]>0 & dados[,m]>0)
			total.sp=sum(dados[,i])+sum(dados[,m])-co.oc
			#similar[i,m]=co.oc/total.sp 
			similar[m,i]=co.oc/total.sp 
			}
		}
	return(similar)
	}
#########################################################
sp.oc
sim(sp.oc)
#dist(sp.oc)
str(as.dist(sim(sp.oc)))
sim(sp.oc)
debug(n.spp)
n.spp(sp.oc)
undebug(n.spp)
######### DOMINANDO o MUNDO! ####
pergunta=function()
{
perg<-readline("Escreva sua pergunta sobre a vida, o universo e tudo mais: \n\t")
resposta=(paste(perg, "\n\tA resposta é: ", 42, "\n"))
cat(resposta)
}
pergunta()
####################
####FIM!!!!!!!!!!!!#
####################
## A função de Mantel
###########################################
#EarWing species (Dermaptera) - Tesourinhas
###########################################
#diferente areas do mundo:
regiao<-c("Eurasia", "Africa","Madagascar", "Oriente", "Australia", "NZelandia", "SAmerica", "NAmerica")
## similaridade entre regiões (generos) - coeficiente de associacao:
similar <- c(.30, .14,.23,.30, -.04, .02,-.09,
                    .5, .5, .4,.04,.09,-.06,
                    .54,.5,.11,.14, .05,
                    .61, .03,-.16,-.16,
                    .15,.11,.03,
                    .14,-.06,
                    .36)
simat<-matrix(NA, length(regiao), length(regiao))
simat[lower.tri(simat)]<-similar
simat[upper.tri(simat)]<-t(simat)[upper.tri(simat)]
dimnames(simat)<-list(regiao, regiao)
## distancias das regioes hoje (saltos de dispersao)
distpres<- c(1,2,1,2,3,2,1,
             1,2,3,4,3,2,
             3,4,5,4,3,
             1,2,3,2,
             1,4,3,
             5,4,
             1)
mdistpres<-matrix(NA,length(regiao), length(regiao))
dimnames(mdistpres)<-list(regiao, regiao)
mdistpres[lower.tri(mdistpres)]<-distpres
mdistpres[upper.tri(mdistpres)]<-t(mdistpres)[upper.tri(mdistpres)]
s1<-sample(c(1:7))
mdistpres[s1,s1]
X=simat
Y=mdistpres
##############################
# funcao Mante
##############################
mantel.test<-function(X, Y, nper= 1000)
    {
        cor.obs<- cor(X[lower.tri(X)], Y[lower.tri(Y)])
        matdim<-dim(X)[1]
        resulta<-rep(NA, nper)
        resulta[1]<-cor.obs
        for(i in 2:nper)
            {
                ordsim <- sample(matdim)
                Xper <- X[ordsim, ordsim]
                resulta[i] <- cor(Xper[lower.tri(Xper)], Y[lower.tri(Y)])
            }
    return(resulta)
    }
## teste:
res.mantel<-mantel.test(simat, mdistpres)
hist(res.mantel)
abline(v=res.mantel[1], col="red")
nv<-sum(res.mantel <= -1* abs(res.mantel[1]) | res.mantel >= abs(res.mantel[])) 
nv/1000
#####################################
## distancia entre regioes -  Pangea
#####################################
distpang<- c(1,2,1,2,3,2,1,
             1,1,1,2,1,2,
             1,1,2,2,3,
             1,2,2,2,
             1,2,3,
             3,4,
             1)
mdistpang<-matrix(NA,length(regiao), length(regiao))
dimnames(mdistpang)<-list(regiao, regiao)
mdistpang[lower.tri(mdistpang)]<-distpang
mdistpang[upper.tri(mdistpang)]<-t(mdistpang)[upper.tri(mdistpang)]
#### Teste Mantel Pangea######
mantel.pan<-mantel.test(simat, mdistpang)
hist(mantel.pan)
abline(v=mantel.pan[1], col="red")
sum(mantel.pan <= mantel.pan[1])/1000
