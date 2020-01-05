options(help_type="html")
###########################
## Tecnicas de Monte Carlos
## simulacao & permutacao
###########################
## Nosso cavalo de batalha: a função sample
##Criar um vetor com letras de "A" a "J".
(vetor=rep(LETTERS[1:10]))
## 40 letras sorteadas com reposicao
sample(vetor,size=40,replace=TRUE)
table(sample(vetor,size=40,replace=TRUE))
## O padrao do argumento 'size' e o tamanho do objeto
sample(vetor, replace=TRUE)
## O padrao do argumento 'replace' e FALSE
## Por isso o comando abaixo apenas permuta os valores
set.seed(42) # semente de n aleatorios
sample(vetor)
## E este comando gera numeros para sua megasena
sample(1:60, size=6)
## argumento 'prob' nao precisa somar um
sample(1:10,size=20,replace=TRUE, prob=1:10)
## Lei dos grandes numeros
sample( c("aa","Aa","AA"), size=10, replace=TRUE, prob=c(1,2,1) )
## Frequencias relativas com aumento de repeticoes
table( sample( c("aa","Aa","AA"), size=10, replace=T, prob=c(1,2,1) ) )/10
table( sample( c("aa","Aa","AA"), size=100, replace=T, prob=c(1,2,1) ) )/100
table( sample( c("aa","Aa","AA"), size=1e6, replace=T, prob=c(1,2,1) ) )/1e6
########################
### Apresentação  ######
########################
## Testes de permutacao ##
##Numero de besouros no conteúdo estomacal de lagartos
## Phrynosoma brevirostrae machos e femeas (Manly,1997)
sexo=factor(c(rep("m",24),rep("f",21)), levels=c("m","f"))
ncoleop=c(256,209,0,0,0,44,49,117,6,0,0,75,34,13,0,90,0,32,0,205,332,0,31,0, 0,89,0,0,0,163,289,3,843,0,158,443,311,232,179,179,19,142,100,0,432)
lagartos <- data.frame(sexo, ncoleop)
str(lagartos)
## Analises exploratorias
tapply(lagartos$ncoleop,lagartos$sexo,summary)
boxplot(ncoleop~sexo, lagartos)
?hist


hist(lagartos$ncoleop, breaks=seq(0, 1000, by=100))
hist(lagartos$ncoleop[lagartos$sexo=="m"],  breaks=seq(0, 1000, by=100),col="skyblue" , add=TRUE)
hist(lagartos$ncoleop[lagartos$sexo=="f"], breaks=seq(0, 1000, by=100),col=scales::alpha('red',.3), add=TRUE)
help.start()
##Médias
(medias <- tapply(lagartos$ncoleop,lagartos$sexo,mean))
##Diferença entre as médias
(dif.obs <- diff(medias))
## Variancias
tapply(lagartos$ncoleop,lagartos$sexo,var)
## Distribuicao
par(mfrow=c(2,2))
hist(lagartos$ncoleop[lagartos$sexo=="f"], main="Femeas")
qqnorm(lagartos$ncoleop[lagartos$sexo=="f"])
qqline(lagartos$ncoleop[lagartos$sexo=="f"])
hist(lagartos$ncoleop[lagartos$sexo=="m"], main="Machos")
qqnorm(lagartos$ncoleop[lagartos$sexo=="m"])
qqline(lagartos$ncoleop[lagartos$sexo=="m"])
par(mfrow=c(1,1))

## PERGUNTAS:**
##  *Essa diferença entre as médias é significativa???
##  * Qual minha incerteza ao afirmar que essas medias sao diferentes?
  
## Se a variação encontrada é devido à variações não relacionadas ao sexo,
## é possível gerar essa diferença permutando os dados. Caso isso seja verdade encontraremos
## frequentemente diferenças iguais ou maiores que a observada.

##Permutando
## Permuta estomagos entre bichos e calcula difreneca entre medias dos grupos
(s1.mf=sample(lagartos$ncoleop))
diff(tapply(s1.mf,lagartos$sexo,mean)) # inclua o "[]" para tirar um rotulo chato
##Play it again, Sam!
(s2.mf=sample(lagartos$ncoleop))
diff(tapply(s2.mf,lagartos$sexo,mean)[])
##Olha mãe, num comando so!
diff( tapply(sample(lagartos$ncoleop),lagartos$sexo,mean))
## e de novo
diff(tapply(sample(lagartos$ncoleop),lagartos$sexo,mean))
## Queremos mais!
## COMO????

##Criando ciclos de eventos, oras
##Vamos criar um loop
## Criamos um vetor de NAs para guardar os resultados
result<-rep(NA,1000)
## Guardamos na primeira posicao a diferenca observada
## (ha controversias, mas veja livro do Manly)
result[1]<-diff(tapply(lagartos$ncoleop,lagartos$sexo,mean))
## Uma olhadinha ...
result[1:10]
## E agora o loop: diferencas sob a hipotese nula
for(i in 2:1000){ 
  result[i]<-diff(tapply(sample(lagartos$ncoleop),lagartos$sexo,mean))
}
## Distribuicao das diferencas entre grupos sob a hipotese nula
hist(result, xlab="Diferenças sob H0", main="")
## Diferenca observada, positiva e negativa
abline(v = result[1], col="red")
abline(v = result[1]*-1, col="red")

### Cálculo do p 
maior.ou.menor <- sum( result>=result[1] | result<= -result[1] )
maior.ou.menor
length(result)
p.bi <- maior.ou.menor/length(result)
p.bi

## Femeas tem em media mais besouros no estomago?
(maior <- sum(result>=result[1]))
(p.uni=maior/length(result))

## Automatizamos tudo com a funcao vegas.t
source("vegast.r")

## Com nosso conjunto de dados dos lagartos
lag.bi <- vegas.t(lagartos$ncoleop, lagartos$sexo)
lag.uni <- vegas.t(lagartos$ncoleop, lagartos$sexo, test="uni")
########################
### Apresentação  ######
########################

########################
## Bootstrap
########################
## Intervalo de confianca da variancia de n de besouros nos estomagos das femeas
fem <- lagartos$ncoleop[lagartos$sexo=="f"]
(v.f <- var(fem)) # para facilitar
## Variancias de amostras com reposicao do vetor original
## (amostras bootstrap)
fem.varboot <- rep(NA, 10000) # vetor para resultados
fem.varboot
str(fem)
class(fem.varboot)
for(i in 1:10000){
  fem.varboot[i] <- var(sample(fem,replace=TRUE))
}
## Distribuicao destas medias
hist(fem.varboot)
## Variancia da amostra
abline(v=var(fem),col="red", lty=2)
abline(v=mean(fem.varboot),col="blue", lty=2)
## Vies do estimador
v.f
(b.f <- mean(fem.varboot))
## Vies relativo
(v.f-b.f)/b.f*100
## Intervalo de de confianca da variancia
(ic.f <- quantile(fem.varboot, c(0.025,.975)))
v.f
## No grafico

hist(fem.varboot)
## Variancia da amostra
abline(v=ic.f,col="red")

##### MACHOS - Bootstrap #####
mac <- lagartos$ncoleop[lagartos$sexo=="m"]
(v.m <- var(mac)) # para facilitar

## Variancias de amostras com reposicao do vetor original
## (amostras bootstrap)
mac.varboot <- c() # vetor para resultados
mac.varboot
for(i in 1:10000){
  mac.varboot[i] <- var(sample(mac,replace=TRUE))
}
mac.varboot
(ic.m <-quantile(mac.varboot, prob=c(0.025, 0.975)))
ic.f
################# UM GRAFICO ########################
par(mar=c(8,5,2,0))
plot(rep(c(1,2), 2), c(ic.m, ic.f), ann=FALSE, type="n", bty="n", xaxt="n", xlim=c(0.6, 2.5), cex.axis=1.3)
points(c(1,1,2,2), c(ic.m, ic.f), pch="_" , cex=1.8)
segments(x0=c(1,2),y0=c(ic.m[1], ic.f[1]), x1=c(1,2), y1=c(ic.m[2], ic.f[2]))
points(c(1,2),c(v.m, v.f), pch=19, col="red")
mtext(c("macho", "femea"), 1, at=c(1,2), cex=1.5, line=2)
mtext("Variância", 2, at=40000, cex=1.5, line=3)


ls()



## Atencao: ##
## este e o intervalo mais tosco que pode ser calculado com boostrap
## Se interessou, veja
## Manly (1997) Randomization, bootstrap and Monte Carlo methods in biology.

################################################################################
### Simulações parametricas (Monte Carlo sensu estrito)
################################################################################
## Fizemos isso na distribuição das Mandibulas de Chacal. Os parâmetros estimados podem ser utilizados para gerar valores (pseudo-values) da distribuição conhecida e comparar com o observado
###########################################
## podemos testar tb. a posição de árvores:
###########################################
## Os palmitos adultos estão distribuidos aleatoriamente?
## Média do vizinho mais próximo
################################
eut<-read.table("euterpe.txt", as.is=TRUE, header=TRUE, sep="\t")
str(eut)
hist(eut$dbh[eut$dbh>100])
eutad=eut[eut$dbh>=100,]
plot(eutad$gx, eutad$gy)
str(eutad)
resulta=matrix(NA, ncol=102, nrow=102)
for(i in 1:101)
    {
        for(j in (i+1):102)
            {
                difx2=(eutad$gx[i]-eutad$gx[j])^2
                dify2=(eutad$gy[i]-eutad$gy[j])^2
                resulta[i,j]<-sqrt(difx2 + dify2)
                resulta[j,i]<-sqrt(difx2 + dify2)
            }
          
    }
resulta[1:10,1:10]
####################
### Nearest-Neighbor
####################
nn<-apply(resulta, 1, min, na.rm=TRUE)
(mnn<-mean(nn))
############################
## Qual o nosso modelo Nulo?
############################
## os 102 palmitos adultos estão aleatoriamente na área
###########################################
## dez valores aleatórios de 0 a 320
round(runif(10,0,320),1)
###########################################
## Simulando valores aleatorios para x e y
## 102 valores para x e 102 para y
##########################################
xsim=round(runif(102,0,320),1)
ysim=round(runif(102,0,320),1)
## um outro jeito de fazer!!
ysim=sample(seq(0,320,by=0.1),120)

plot(xsim, ysim)
resim=matrix(NA, 102,102)
## o mesmo ciclo:
for(i in 1:101)
    {
        for(j in (i+1):102)
            {
                difx2=(xsim[i]-xsim[j])^2
                dify2=(ysim[i]-ysim[j])^2
                resim[i,j]<-sqrt(difx2 + dify2)
                resim[j,i]<-sqrt(difx2 + dify2)
            }
          
    }
resim[1:10, 1:10]
nn.sim=apply(resim, 1, min, na.rm=TRUE)
mnn.sim=mean(nn.sim)
mnn.sim
mnn
###################################################
### fazer isso muitas vezes e guardar o mnn.sim
###################################################
res.mnn=rep(NA, 1000)
res.mnn[1]<-mnn
for(k in 2:1000)
    {
    ......    


     res.mnn[k]<- mnn.sim   
    }
########################
### Apresentação  ######
########################

#############################################
## exemplo 
## Geographical Distribution of the Dermaptera and the Continental Drift Hypothesis
## 1966 Nature
## Dermapteras - Tesourinhas
##############################################
data.coef<-matrix(c(NA, .30, .14, .23, .30, -0.04, 0.02, -0.09, NA, NA, .50,.50, .40, 0.04, 0.09, -0.06, NA, NA, NA, .54, .50, .11, .14, 0.05, rep(NA, 4), .61, .03,-.16, -.16, rep(NA, 5), .15, .11, .03, rep(NA, 6), .14, -.06, rep(NA, 7), 0.36, rep(NA, 8)), nrow=8, ncol=8)
rownames(data.coef) <- c("Eur_Asia", "Africa", "Madag", "Orient", "Austr", "NewZea", "SoutAm", "NortAm")
colnames(data.coef) <- c("Eur_Asia", "Africa", "Madag", "Orient", "Austr", "NewZea", "SoutAm", "NortAm")
data.coef
#############################################
# Distancias entre as áreas na posição atual
############################################
dist.atual<-matrix(c(NA,1,2,1,2,3,2,1, NA, NA, 1,2,3,4,3,2, NA, NA, NA,3,4,5,4,3, rep(NA, 4),1,2,3,2, rep(NA, 5), 1,4,3, rep(NA, 6), 5,4, rep(NA, 7), 1, rep(NA, 8)), nrow=8, ncol=8)
dist.atual
dist.deriva<- matrix(c(NA,1,2,1,2,3,2,1, NA, NA, 1,1,1,2,1,2, NA, NA, NA,1,1,2,2,3, rep(NA, 4),1,2,2,2, rep(NA, 5), 1,2,3, rep(NA, 6), 3,4, rep(NA, 7), 1,  rep(NA, 8)), nrow=8, ncol=8)
rownames(dist.atual) <- colnames(dist.deriva)<- c("Eur_Asia", "Africa", "Madag", "Orient", "Austr", "NewZea", "SoutAm", "NortAm")

colnames(dist.atual)<- colnames(dist.deriva)<-  c("Eur_Asia", "Africa", "Madag", "Orient", "Austr", "NewZea", "SoutAm", "NortAm")

dist.atual
rownames(dist.deriva)<-colnames(dist.deriva)
dist.deriva

#############################
## Correlação de Pearson
#############################
cor12<-cor(as.vector(data.coef), as.vector(dist.atual), use="complete.obs")
cor13<-cor(as.vector(data.coef), as.vector(dist.deriva), use="complete.obs")
cor12 ## correlação com a distancia atual
cor13 ## correlação com a distancia antes da deriva
###################################
## Esses números são diferentes??
###################################
data.sim<-data.coef
data.sim
data.sim[upper.tri(data.sim)]<-data.coef[lower.tri(data.coef)]
data.sim
data.sim[8:1, 8:1]
sim.pos<-sample(1:8)
sim.pos
data.sim<-data.sim[sim.pos, sim.pos]
cor12.sim<-cor(as.vector(data.sim), as.vector(dist.atual), use="pairwise.complete.obs")
cor13.sim<-cor(as.vector(data.sim), as.vector(dist.deriva), use="pairwise.complete.obs")
cor12.sim
cor13.sim
cor12 ## correlação com a distancia atual
cor13 ##
########################################################
### Repetir a simulação muitas vezes ###################
#######################################################
res.cor=data.frame(sim12=rep(NA, 5000), sim13=rep(NA,5000))
str(res.cor)
res.cor[1,]<-c(cor12, cor13)
str(res.cor)
for(s in 2:5000)
    {
        sim.pos<-sample(1:8)
        data.sim<-data.sim[sim.pos, sim.pos]
        res.cor[s,1]<-cor(as.vector(data.sim), as.vector(dist.atual), use="pairwise.complete.obs")
        res.cor[s,2]<-cor(as.vector(data.sim), as.vector(dist.deriva), use="pairwise.complete.obs")
    }
str(res.cor)
par(mfrow=c(2,1))
hist(res.cor[,1])
abline(v=res.cor[1,1], col="red")
hist(res.cor[,2])
abline(v=res.cor[1,2], col="red")
#### calculando o P ###########
p12=sum(res.cor[,1]<= res.cor[1,1])/(dim(res.cor)[1])
p12
p13=sum(res.cor[,2]<= res.cor[1,2])/(dim(res.cor)[1])
p13
###############################


################################################################################
## Simulacoes de processos ecologicos
################################################################################
## Visite ecovirtual.ib.usp.br
##############################
## teste de significãncia de regressão:
body<-read.table("http://ecologia.ib.usp.br/bie5782/lib/exe/fetch.php?media=dados:animais.txt", as.is=TRUE, header=TRUE, sep=";", dec=",")
str(body)
plot(log(body$brain) ~ log(body$body))
