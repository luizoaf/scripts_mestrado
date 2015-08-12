diretorio_atual = "C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor"
setwd(diretorio_atual)
source("../../1 - Funcoes/funcoes.R")
setwd(diretorio_atual)

dados = read.csv(file="metricas_setores_sem_series_temporais.csv")

nome_series_temporais = unique(dados$setor)

relacao_b_volatilidade = function(nome_setor, metricas){
  formato_ponto = 21
  volatilidade = ((metricas$volatilidade))
  B = ((metricas$coeficiente_B))
  plot(main =nome_setor, volatilidade*B,ylim=c(0,2),cex=1.3,ylab="Volatility x Coefficient B",xlab="Indexes of the sectors",pch=formato_ponto,las=1, cex.lab=1.5,cex.main = 1.7)
  intercept = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[1]
  slope = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[2]
  abline(a=intercept,b =slope,col=2)
}

inversamente_proporcionais = function(nome_setor,metricas){
  legenda = c("Coefficient B","Volatility")
  plot(main =nome_setor,unique(metricas$coeficiente_B[order(metricas$coeficiente_B,decreasing=T)]),xlab="Indexes of the sectors",ylab="Amplitudes",ylim=c(0, 4),col="black",pch=formato_ponto,las=1, cex.lab=1.5,cex.main = 1.7)
  points(unique(metricas$volatilidade[order(metricas$volatilidade,decreasing=F)]),xlab="Indice",ylab="Volatility",col="black",pch=4)
  legend("topright", inset=.04,legenda ,col =c("black","black") ,pch=c(formato_ponto,4), horiz=F)
}
# setor = nome_series_temporais[1]
png("imagens/relacao_b_volatilidade_para_cada_serie_temporal.png",bg = "transparent",width = 600,height = 750)
par(mfrow = c(4,2))
for(setor in nome_series_temporais){
  serie_temporal_setor = dados[dados$setor == setor,]
  relacao_b_volatilidade(setor,serie_temporal_setor)
}
dev.off()

png("imagens/inversamente_proporcionais_b_volatilidade_para_cada_serie_temporal.png",bg = "transparent",width = 600,height = 750)
par(mfrow = c(4,2))
for(setor in nome_series_temporais){
  serie_temporal_setor = dados[dados$setor == setor,]
  inversamente_proporcionais(setor,serie_temporal_setor)
}
dev.off()



# nome_series_temporais = nome_series_temporais[2:length(nome_series_temporais)]

nome_series_temporais = c("Bens industriais e\nmaterial de transporte",
                          "Consumo não cíclico e\nalimentos processados",
                          "Consumo não cíclico e\nbebidas",
                          "Consumo não cíclico e\nfumo",
                          "Consumo não cíclico e\nprodutos de uso pessoal e de limpeza",
                          "Construção e transporte,\nconstrução e engenharia",
                          "Utilidade pública, \nágua e saneamento")
relacao_b_volatilidade = function(nome_setor, metricas){
  formato_ponto = 21
  volatilidade = ((metricas$volatilidade))
  B = ((metricas$coeficiente_B))
  plot(main =nome_setor, volatilidade*B,ylim=c(0,2),cex=1.5,ylab="Volatilidade * B",xlab="Meses",pch=formato_ponto,las=1, cex.lab=1.5,cex.main = 1.7)
  intercept = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[1]
  slope = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[2]
  abline(a=intercept,b =slope,col=1,lwd=2)
}

# png("imagens/cte_setores.png",bg = "transparent",width = 1092,height = 721)
par(mfrow = c(3,3))
for(i in 2:length( unique(dados$setor))){
  serie_temporal_setor = dados[dados$setor == unique(dados$setor)[i],]
  relacao_b_volatilidade(nome_series_temporais[i-1],serie_temporal_setor)
}
# dev.off()

inversamente_proporcionais = function(nome_setor,metricas){
  formato_ponto = 21
  legenda = c("Coeficiente B","Volatilidade")
  plot(main =nome_setor,unique(metricas$coeficiente_B[order(metricas$coeficiente_B,decreasing=T)]),xlab="Meses",ylab="Valores",ylim=c(0, 4),col="black",pch=formato_ponto,las=1, cex.lab=1.5,cex.main = 1.7)
  points(unique(metricas$volatilidade[order(metricas$volatilidade,decreasing=F)]),xlab="Indice",ylab="Volatility",col="black",pch=4)
  legend("topright", inset=.04,legenda ,col =c("black","black") ,pch=c(formato_ponto,4), horiz=F)
}

par(mfrow = c(3,3))
for(i in 2:length( unique(dados$setor))){
  serie_temporal_setor = dados[dados$setor == unique(dados$setor)[i],]
  inversamente_proporcionais(nome_series_temporais[i-1],serie_temporal_setor)
}
