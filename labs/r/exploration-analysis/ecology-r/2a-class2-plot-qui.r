## Grafico
## faz o grafico da funcao Qui-quadrado com 4 graus de liberdade,
## veja ajuda funcao curve
curve(dchisq(x, df=4),0,70, xlab="Qui-quadrado, 4 g.l.", ylab="Densidade probabilística")
## Sobrepoe uma linha vermelha a partir do ponto Chi2=56,93
curve(dchisq(x, df=4), 56.93, 70, add=T, col="red", lwd=2)
