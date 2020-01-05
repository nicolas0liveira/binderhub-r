## função para construir sequencias
seq(from=0, to =100, by=10)

## para construir um gráfico!
plot(x=seq(from=0, to=100, by=10), y= seq(to=100, from =0, by=10))

## note que podemos usar funções dentro de funções para construir os argumentos!

pares <- c(2,4,6,8)
impares <- c(1,3,5,7,9)
todos.os.numeros <- c(pares,impares)

# criando sequenciaas
1:10
(1:10) * 10

seq(from=0, to=100, by=10)
seq(from=0, to=100, len=10)

rep("A", times=10)
rep(c("A", "B", "C"), times=3)
rep(c("A", "B", "C"), each=2)


#### Estatística Básica

# variância é uma medida de dispersão que mostra o quão distante cada valor desse conjunto está do valor central (médio).
# Quanto menor é a variância, mais próximos os valores estão da média; mas quanto maior ela é, mais os valores estão distantes da média

# O desvio padrão é capaz de identificar o “erro” em um conjunto de dados, caso quiséssemos substituir um dos valores coletados pela média aritmética.
# O desvio padrão aparece junto à média aritmética, informando o quão “confiável” é esse valor. Ele é apresentado da seguinte forma

area <- c(303, 379, 961, 295, 332, 47,  122, 11, 53, 2749) # criando objeto com as áreas dos fragmentos
riqueza <- c(3, 10, 20, 7, 8, 4, 8, 3, 5, 23) # criando objeto com as riquezas associadas
area   # mostra os dados do objeto area criado acima
riqueza # mostra os dados do objeto riqueza criado acima
summary(area)  # cria um resumo dos dados contidos no objeto area
summary(riqueza)    # cria um resumo dos dados contidos no objeto riqueza

mean(x=area)     # calcula a média dos dados de area
var(area)  # calcula a variância dos dados de area e guarda no objeto varea

sqrt(varea)   # calcula a raiz quadrada do objeto varea 
sd(x=area)    # calcula o desvio padrão dos valores que estão no objeto area

plot(x=area, y=riqueza, xlab="Area (ha)", ylab="Número de Espécies")  # faz um gráfico do riqueza por área

x <-c(1,1,1,1,10)
mean(x)
var(x)
sd(x)



# Exemplo
# Imagine algumas turmas onde quero observar quantidade de alunos com notas acima da média, por bimestre (1 ao 4 bimestre)

t1 <- c(5,8,10,7)  #mean 7.5  var 4.333  sd 2.08 -> 7.5 +- 2.08 alunos acima da média por bimestre
t2 <- c(8,6,6,12)  #mean 8    var 8      sd 2.82 -> 8 +- 2.82 alunos acima da média por bimestre
t3 <- c(11,9,5,10) #mean 8.75 var 6.91   sd 2.62 -> 8.75 +- 2.62 alunos acima da média por bimestre
t4 <- c(8,13,9,4)  #mean 8.5  var 13.6   sd 3.69 -> 8.5 +- 3.69 alunos acima da média por bimestre

mean(t1)
mean(t2)
mean(t3)
mean(t4)

var(t1)
var(t2)
var(t3)
var(t4)

sd(t1)
sd(t2)
sd(t3)
sd(t4)


#  Como o desvio padrão é expresso na mesma unidade dos dados observados em estudo, 
# comparar duas ou mais séries de valores que estão em unidades de medida diferentes 
# torna-se impossível. Para sanar essas dificuldades, podemos analisar a dispersão
# em termos relativos a seu valor médio, utilizando o coeficiente de variação de Pearson


100*sd(dados)/mean(dados) #dado em porcentagem
#Exemplo: Para o exemplo anterior, do cálculo do desvio-padrão.
v <-c(10,11,9,10,10,9,11)
CV = 100*sd(v)/mean(v)
CV
# 8.164966 #em torno de 8%




########################
## Indexacao "[]" &  "$"
########################
x=LETTERS[1:6]
x
x[1]
x[1:3]
x[c(1,1,3,5)]
x[-2]
x[-c(2,4)]

### INDEXAÇÃO COM LÓGICA #####
ALTURA=c(1.85, 1.78, 1.92, 1.63, 1.81, 1.55)
ALTURA
SEXO = factor(rep(c("M","F"),each=3))
SEXO
PESO <- c(80, 100, 115,  70,  65,  50)
PESO

## OPERAÇÕES LÓGICAS
ALTURA >= 1.8
SEXO=="M"

homens.altos <- (ALTURA > 1.8) & (SEXO=="M")
homens.altos

PESO[homens.altos]


### ALTERANDO SUCONJUNTOS ######

ALTURA
ALTURA > 1.8
ALTURA[ALTURA > 1.80]
ALTURA[ALTURA > 1.80] <- c(1.86, 1.93, 1.82)
ALTURA

### CRIANDO UM DATA FRAME ######

pessoas <- data.frame(alt = ALTURA, sex = SEXO) 
pessoas
pessoas$peso
pessoas$peso <- c(80, 100, 115,  70,  65,  50)
pessoas
pessoas$peso[2]
pessoas[2, 3]
pessoas[, "sex"]
pessoas[c(1, 4, 5), "alt"]



#################
## familia apply
#################

ls()
str(pessoas)

#altura media  por sexo
tapply(X = pessoas$alt, INDEX= pessoas$sex, FUN= mean)
tapply(X = pessoas$alt, INDEX= pessoas$sex, FUN= sd)
## quantos homens e mulheres?
table(pessoas$sex)
table(pessoas$sex, pessoas$alt>1.70)
## qual a media de altura e peso (independente do sexo)
apply(pessoas[,c(1,3)], 2, mean)

## e essa ultima?
aggregate(x = pessoas[,c(1,3)], by = list(pessoas$sex), FUN = mean)
## veja o help da funcao




#############################
## graficos
## funcoes de alto nivel: abrem o dispositivo de tela 
example(plot)
example(contour)
example(hclust)
### demos
demo(image)
demo(persp)
#########################
## funcoes subordinadas
example(points)
example(segments)
example(rect)
example(axis)
example(colors)
####################
