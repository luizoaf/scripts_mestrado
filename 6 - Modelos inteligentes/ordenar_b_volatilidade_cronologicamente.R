################################# B ##########################

setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/6 - Modelos inteligentes")
# setores = read.csv("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/series_temporais_setores.csv")
setores = read.csv("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor/metricas_setores_sem_series_temporais.csv")
todos_setores = unique(setores$setor)

setor_b_volatilidade = function(ativo_financeiro){
  ativo = subset(setores,setores$setor == ativo_financeiro,select = c(coeficiente_B, volatilidade))
  return(ativo)
}

i=1
b_volatilidade_ordenado = data.frame(mes = 1:72)
for(i in 1:length(todos_setores)){
  setor = todos_setores[i]
  serie_temporal_setor = setor_b_volatilidade(setor)
  colnames(serie_temporal_setor) = paste(setor,"_",names(serie_temporal_setor),sep="")
  b_volatilidade_ordenado = cbind(b_volatilidade_ordenado,serie_temporal_setor)
}

write.table(b_volatilidade_ordenado,file="b_volatilidade_ordenado.csv",row.names=F)
# 
# dados = read.table("b_volatilidade_ordenado.csv",head=T)
# plot(dados$Const.e.Transp...Constr.e.Engenh_coeficiente_B)
# points(dados$Const.e.Transp...Constr.e.Engenh_volatilidade,col="red")
