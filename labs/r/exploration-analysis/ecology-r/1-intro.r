##Ajuda
help(mean)
help.start()

## Instalando pacotes ##
library()
help.start()
install.packages("vegan")
library()
help.start()

## Carregando um pacote já instalado: Demonstracao com pacote MASS
x1 <- rnorm(n=15, mean=1, sd=3)
hist(x1)
truehist(x1)# erro: Pacote nao carregado
search()
library(MASS)
search()
truehist(x1)

## Listando e removendo objetos ##
## O que há na area de trabalho?
ls()
## R: nada, pois antes de comecar apaguei todos os objetos com
rm( list = ls() )
## Criando um vetor de 3 algarismos concatenados
A1 <- c(1,2,3)
A1 # digite o nome do objeto para exibir seu conteudo
## Existe uma maneira + simples:
## coloque o comando de atribuicao entre parenteses para já exbir seu resultado
(A2 <- c(10,20,30)) #atribuicao entre parenteses já exibe
(b <- c(A1,A2))
ls()
help(ls)
ls(pattern="A")
## Para apagar objetos com um certo padrao
a.1 <- A1
a.2 <- A2
ls()
rm( list=c("A1","A2") )
ls()
## Que e o mesmo que
rm(list=ls(pattern="A"))
## ou
rm(A1,A2)


## Classes de objetos ##
copa.70 <- "21/06/70"
copa.94 <- "17/07/94"
## diferenca
copa.94 - copa.70 # nao funfa
## Classes nao adequadas:
class(copa.70)
class(copa.94)
## Mudando para a classe de data
copa.70 <- as.Date(copa.70,format="%d/%m/%y")
copa.94 <- as.Date(copa.94,format="%d/%m/%y")
class(copa.70)
class(copa.94)
copa.94 - copa.70

##Niveis de fatores##
herb <- c("A","M","M","A","A","M","M","B","A","A","A","A","B","A")
herb.f <- factor(herb)
(herb.t <- table(herb.f))
plot(herb.t)
## Corigindo os niveis do fator
herb.f <- factor(herb, levels=c("N","B","M","A"))
(herb.t <- table(herb.f))
plot(herb.t)

##OPS! por engano digitei
rm( list = ls() )
## O que fazer?
