#######################
## Funções Matemáticas
#Logarítmo e exponenciação
log( 10 )
# Logaritmo natural
log( 10, base = 10) # Log base 10
log10(10)
# Também log de base 10
log( 10, base = 3.4076) # base 3.4076
exp( 1 )# Exponencial

############################
#pi , sin() , cos(), tan() ...
#Constante π e Funções Trigonométricas
############################
sin(0.5*pi) # Seno
cos(2*pi) # Coseno
asin(1) # Arco seno (radianos)
asin(1) / pi * 180

#############################
1 - (1 + 10^(-15))
factorial(100) # Fatorial de 100
gamma(101) #Função gama
## possibilidades de casais no Brasil
choose(1.93e+8,2)

#############################
## arredondamentos
##########################
ceiling( 4.3478 )
floor( 4.3478 )
round( 4.3478 )
round( 4.3478 , digits=3)
round( 4.3478 , digits=2)
round(4.5)
#####################################
#Atribuição e Variáveis Pré-definidas
#####################################
a <- 3.6
b <- sqrt( 35 )
c <- -2.1
a
b
c
ls()
a * b / c
a - b * c / d # d variavel não definida

#######################################
#Valores Infinitos, Indefinidos e Inexistentes
######################################
-5/0
500000000000000000/Inf
sqrt( - 1 )
2 * NA
2 * NaN

#########################
#     APRESENTAÇÃO
#########################
## Ciclagem com vetores que são multiplos
## Veja que é feita a ciclagem mas nenhum aviso é dado!
a
b
#Criação de Vetores
a = c(3.4, pi, exp(-1))
a1= c(3.4, pi, "a")
a2= c(3.4, pi, a)
a3= c(3.4, pi, a1)
a1
a2
a3
#Criação de Sequências
b = 1:8
b
2.5:10
### Ainda Sequencias
seq(from=1, to=4)
# mesmo que 1:4
seq(from=1, to=4, by=0.5)
seq(from=1, to=4, length=6)
## Sequencias com padrão
rep(5, times=3)
rep(1:5, 3)
rep(1:5,each=3)

####################
#Operações com um Vetor
####################
a = seq(0,8,2)
a
2*a
sqrt(a)
a^(1/2)

#####################
# Operações entre Vetores
#####################
a = seq(0,8,2)
a
b = c(1,15,18,3,6)
a+b
a^(1/b)

#####################
# A REGRA DA CICLAGEM
####################
b
c
c*b

######################
# Comprimento de Vetores
######################
a
b
a*b
b= c(b,0)
c= c(c, 1)
c*b
length(b)/length(a)

#########################
#cumsum(), sort(), diff()
#Também Operam sobre Todo o Vetor
#########################
b
cumsum(b)
sort(b)
sort(b, decreasing=T)
diff(b)

###################
# APRESENTAÇÃO
###################
##Exemplo de tapply com dois fatores
## Criação dos dados
sexo = factor(rep(c("M","F"),each=10) )
sexo
class(sexo)
sexo1<-rep(c("M","F"),each=10)
sexo1
class(sexo1)
sexo1<-as.factor(sexo1)
sexo1
sexo1<-factor(rep(c("M","F"),each=10), levels=c("F", "M", "H") )
sexo1

cor.olhos = factor( c(rep(c("escuros","claro"),c(7,3)),rep(c("escuros","claro"),c(5,5)) ))
cor.olhos

peso= c( rnorm(10, mean=80, sd=8), rnorm(10, mean=70, sd=8))
sexo
cor.olhos
peso

table(sexo,cor.olhos)
tapply(peso,INDEX=list(sexo,cor.olhos), mean)

##########################################
########### APRESENTAÇÃO  ################
##########################################
# quadris dos homens norte-americanos segue uma curva normal:
# com média de 14,4 polegadas, e desvio-padrão de 1,0 polegada
#######################################
#norm.ale=function(x){dnorm(x,mean=14.4, sd=1)}
curve(dnorm(x, mean=14.4, sd=1),from=11, to=18, xlab="Largura do quadril(in)", ylab= "densidade probabilistica", col="blue")
## quantos tem quadris maior que 15
pnorm(q=15,mean=14.4,sd=1,lower.tail=FALSE)
## colocar isso no gráfico
abline(v=15, col="red", lty=2)
# Qual o tamanho de assento que 95% da população cabem?
qnorm(p=0.95,mean=14.4,sd=1)
# inserir no gráfico
abline(v=16, col="green")
####################################
## Distribuição binomial: exemplo do Fazendeiro com 12 filhas
## Probabilidade de 0 a 12 sucessos, em 12 tentativas
## dada probabilidade de 0.5 para cada sucesso
probs= dbinom (0:12, size=12, prob=0.5)
probs
## Acrescentando um nome para cada elemento do objeto criado acima
names(probs) = 0:12
probs
## Exibir estes valores com cinco casas decimais
round(probs, 5)
## Grafico
plot(0:12, probs, type= "h", lwd=3, xlab="N de meninos", ylab="Probabilidade")
## Qual a probabilidade de não ter meninos?
dbinom(0, size=12, prob=0.5)
## Adicionando isto ao grafico
points(0,dbinom(0, size=12, prob=0.5), col="red", type="h", lwd=4)
## Qual a probabilidade de  ter pelos menos um menino?
## no grafico
points(1:12,dbinom(1:12, size=12, prob=0.5), col="red", type="h", lwd=8)
## Esta probabilidade é a soma das probabilidades para 1 a 12 meninos:
## usando indexação (veremos isto na aula  de amanhã)
probs[2:13]
sum(probs[2:13])
## Ou usando a funcao de probabilidade do R
pbinom(0, size=12, prob=0.5, lower.tail = F)
## E Se a probabibilidade de um menino fosse de 1/3?
probs.2= dbinom (0:12, size=12, prob=1/3)
## Compare com o grafico anterior:
plot(0:12, probs.2, type= "h", lwd=3, xlab="N de meninos", ylab="Probabilidade")
## Simulando numeros de meninos em 10 familias de 12 filhos cada
rbinom(10, size=12, prob=0.5)


## Grafico da normal com diferentes parametros (m = média e d = desvio-padrão)
## Exemplo das larguras de quadris da populacao dos EUA
## A funcao curve traca curva de uma funcao. Comparametro add=T acrescenta estas curvas ao grafico ativo
## Para entender veja a ajuda da funcao e aguarde a aula sobre graficos
curve(dnorm(x,mean=14.4,sd=1),8,26, col="red", xlab="Largura (in)", ylab="Densidade Probabilística")
curve(dnorm(x,mean=14.4,sd=2),add=T, col="blue")
curve(dnorm(x,mean=18,sd=2),add=T, col="green")
legend(17, 0.4, legend=c("m=14.4,d=1","m=14.4,d=2","m=18,d=2"), col=c("red", "blue","green"), lty=1)


