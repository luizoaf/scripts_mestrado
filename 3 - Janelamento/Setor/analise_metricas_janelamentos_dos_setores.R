diretorio_atual = "C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor"
setwd(diretorio_atual)
source("../../1 - Funcoes/funcoes.R")
setwd(diretorio_atual)


dados = read.csv("metricas_setores.csv")
setores = unique(dados$setor)


formato_ponto = 21
legenda = c("Coefficient B","Volatility")

png(filename = "imagens/relacao_B_volatilidade.png",bg = "transparent",width = 1100,height = 750)
par(mfrow=c(3,3))

for(i in 1:length(setores)){
  setor = dados[dados$setor == setores[i],]
  plot(main = setores[i],setor$coeficiente_B,xlab="Indexes of the sectors",ylab="Amplitudes",ylim=c(0, 3),col="black",pch=formato_ponto,las=1)
  points(setor$volatilidade,xlab="Indice",ylab="Volatility",col="black",pch=4)
  legend("topright", inset=.04,legenda ,col =c("black","black") ,pch=c(formato_ponto,4), horiz=F)
}
dev.off()


png(filename = "imagens/B_volatilidade_constante.png",bg = "transparent",width = 1100,height = 750)
par(mfrow=c(3,3))
for(i in 1:length(setores)){
  setor = dados[dados$setor == setores[i],]
  volatilidade = setor$volatilidade
  B = setor$coeficiente_B
  plot(main = setores[i],volatilidade*B,ylim=c(0,2),cex=1.3,ylab="Volatility x Coefficient B",xlab="Indexes of the sectors",pch=formato_ponto,las=1)
  intercept = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[1]
  slope = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[2]
  abline(a=intercept,b =slope,col=2)
}
dev.off()


# plot(main = "Todos os setoes",dados$coeficiente_B,xlab="Indexes of the sectors",ylab="Amplitudes",ylim=c(0, 3),col="black",pch=formato_ponto,las=1)
# points(dados$volatilidade,xlab="Indice",ylab="Volatility",col="black",pch=4)
# legend("topright", inset=.04,legenda ,col =c("black","black") ,pch=c(formato_ponto,4), horiz=F)

png(filename = "imagens/B_volatilidade_constante_todos_os_setores.png",bg = "transparent")
volatilidade = dados$volatilidade
B = dados$coeficiente_B
plot(main = "Todos o setores",volatilidade*B,ylim=c(0,2),cex=1.3,ylab="Volatility x Coefficient B",xlab="Indexes of the sectors",pch=formato_ponto,las=1)
intercept = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[1]
slope = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[2]
abline(a=intercept,b =slope,col=2)
dev.off()
