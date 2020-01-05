## http://143.107.246.248/aulasAAO/EcoR/aula3.html

### Trapalhoes basic
# abrir o arquivo no excel antes!!

trapa <- read.table("datasets/trapa.csv", header=TRUE)

trapa
str(trapa)
dim(trapa)

### desconstruindo trapalhoes 2
trapa <- read.table("trapa.csv", header=TRUE,sep=",")
trapa
str(trapa)
trapa$estado
dim(trapa)
class(trapa)
### lendo trapalhoes 
trapa <- read.table("trapa.csv", header=TRUE,sep=",", as.is=TRUE)
trapa
str(trapa)
rownames(trapa)

## Lendo Arquivos como nome da coluna = codinome 
trapa <- read.table("trapa.csv", header=TRUE,sep=",", , as.is=TRUE,row.names=1)
trapa
class(trapa)
dim(trapa)
names(trapa)
rownames(trapa)
str(trapa)

?read.table

### SLIDE ###

### padrao decimal com ","

trapa = read.table("trapaVirgula.csv", header=T, sep=",", as.is=T)
trapa
dim(trapa)
str(trapa)
trapa = read.table("trapaVirgula.csv", header=T, sep=";", as.is=T)
trapa
str(trapa)
mean(trapa$altura)
trapa = read.table("trapaVirgula.csv", header=T,dec=",", sep=";", as.is=T)
trapa
str(trapa)
mean(trapa$altura)

###padrao ale

trapa.ale= read.table("trapa_nomes.txt", header=T, sep="/t", dec=".", as.is=T)
dir()
camilho.file<-choose.files()
# para linux 
#caminho.file=file.choose()
caminho.file
trapa.ale= read.table(caminho.file, header=T, sep="\t", dec="." , as.is=T)
trapa.ale<- read.table(caminho.file, header=T, sep="\t", as.is=T)
trapa.ale<- read.table("trapa_nome.txt", header=T, sep="\t", as.is=T)

##########  MANIPULANDO DADOS ######################

names(trapa)
trapa
rownames(trapa)
rownames(trapa)<- paste("trap", 1:4)
trapa
names(trapa)
trapa$nascimento
trapa$vivo
trapa$estado

names(trapa) <- c("COD", "NASC", "ESTADO", "VIVO", "NOME")
trapa$VIVO <- c(TRUE,TRUE,FALSE,FALSE)
trapa
str(trapa)
sum(trapa$VIVO)
table(trapa$VIVO)
trapa$ID.2009 = 2009 - trapa$NASC
rep(c(TRUE, FALSE), each=20)
class(trapa)
class(trapa$NASC)
str(trapa)

### Objeto Lista ###
a=1:5
a
b=factor(rep(c("a","b","c"), each=3))
b
c=data.frame(sec=c("XIX", "XX", "XXI"),inicio=c(1801,1901,2001))
c
minha.lista=list(um.vetor=a, um.fator=b, um.data.frame=c)
minha.lista

minha.lista$um.data.frame$inicio<-c("1800", "1900", "200")
minha.lista$inicio<-"ale"

## objetos matriz


ilhas=matrix( sample(round(c(runif(36,0,6),rep(0,4)))),ncol=8)
### Não entendeu?!
## veja as partes...
num1<- sample(round(c(runif(36,0,6),rep(0,4))))
## ainda não?
num2<-c(runif(36,0,6),rep(0,4))
num2
num2<-round(num2)
num2
sample(num2)

colnames(ilhas)<-paste("ilha",1:8)
rownames(ilhas)<-paste("sp",1:5)
ilhas



### Operadores Lógicos

ALTURA=c(1.85, 1.78, 1.92, 1.63, 1.81, 1.55)
ALTURA
SEXO=factor(rep(c("M","F"),each=3))
SEXO
ALTURA > 1.80
homens.altos <- ALTURA >= 1.80 & SEXO == "M"
homens.altos
class(homens.altos)
rank(ALTURA)
sort(ALTURA)
order(ALTURA)
ALTURA[order(ALTURA)]
ALTURA[c(6,4,2,5,1,3)]


### Totais marginais, apply
ilhas
ilhas.vf=ilhas>0
ilhas.vf
apply(ilhas.vf,1,sum) # soma as linhas
apply(ilhas.vf,2,sum) # soma as colunas (riqueza)
mean(apply(ilhas.vf,2,sum)) # media da riqueza
mean(apply(ilhas.vf,1,sum)) # media de ocorrência entre spp

###############################
### VOLTA SLIDE #############
##############################

#### INDEXAcaO "[]" ###########

(x=LETTERS[1:6])
x
x[1]
x[1:3]
x[c(1,1,3,5)]
x[-2]
x[-c(2,4)]

### INDEXAcaO COM LoGICA #####

ALTURA
PESO=c(80, 100, 115,  70,  65,  50)
SEXO
ALTURA>=1.8
SEXO=="M"
homens.altos<-ALTURA>1.8 & SEXO=="M"
PESO[homens.altos]


### ALTERANDO SUCONJUNTOS ######

ilhas.vf
riqueza=apply(ilhas.vf,2,sum)
riqueza
riqueza.indice=order(riqueza,decreasing=T)
riqueza.indice
ilhas[,riqueza.indice]
ilhas

trapa<-read.table("trapa_nome.txt", header=TRUE, sep="\t", as.is=TRUE)
trapa
trapa[,1:5]
trapa$vivo=="s"
trapa[trapa$vivo=="s",]
trapa[which(trapa$vivo=="s"),]
trapa$vivo[which(trapa$vivo=="s")]<-TRUE
trapa$vivo[which(trapa$vivo=="n")]<-FALSE
trapa$vivo<-as.logical(trapa$vivo)
trapa[,1:5]
str(trapa)
#trapa
trapa.vivo


### UM PROBLEMA ### 
## abrir "gua.clas.txt" no libreoffice
gua.clas=read.table("gua.clas.txt", header=TRUE,sep="\t", as.is=TRUE)
str(gua.clas)
nivel=unique(gua.clas[,"floresta"])
nivel
n0=table(gua.clas[,"classe2007"],gua.clas[,"floresta"])
n0
trans.abs=table(gua.clas$classe2008 ,gua.clas$classe2007, gua.clas$floresta)
trans.abs

inicial=t(t(trans.abs[,,1])/n0[,1])
inicial
tardia=t(t(trans.abs[,,2])/n0[,2])
tardia


