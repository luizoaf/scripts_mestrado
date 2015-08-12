diretorio_atual = "C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor"
setwd(diretorio_atual)
source("../../1 - Funcoes/funcoes.R")
setwd(diretorio_atual)

dados = read.csv(file="../../2 - Extrair base de dados das ações do  Yahoo Finanças/Acoes/papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")

series_temporais_setores = read.csv(file="../../2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/series_temporais_setores.csv")

series_temporais_setores = cria_serie_retornos(series_temporais_setores[,2:ncol(series_temporais_setores)])

################################ JANELAMENTO #################################

dias_mes = 20
dias_ano = 240
total_dias = nrow(series_temporais_setores)
inicio_janelamento = seq(from=1,to=total_dias,by=dias_mes)
fim_janelamento = seq(from=1,to=total_dias,by=dias_mes)+dias_ano

janelamentos_indices = data.frame(inicio = inicio_janelamento,fim = fim_janelamento)
janelamentos_indices = janelamentos_indices[janelamentos_indices$fim<=total_dias,]
# write.table(x = janelamentos_indices,file="janelamentos_indices.csv",row.names=F)

##############################################################################
linha_janelamento = 1
todos_janelamentos = data.frame()
grupo_janelamento = 1
for(linha_janelamento in 1:nrow(janelamentos_indices)){
  indices_janelamento = janelamentos_indices$inicio[linha_janelamento]:janelamentos_indices$fim[linha_janelamento]
  # determina as series temporais para cada janelamento  
  series_temporais = series_temporais_setores[indices_janelamento,]
  
  todos_janelamentos = rbind(todos_janelamentos,cbind(data.frame(grupo_janelamento = grupo_janelamento),series_temporais))
  grupo_janelamento = grupo_janelamento + 1
}

grupos_janelamento = unique(todos_janelamentos$grupo_janelamento)
metricas = data.frame()
nome_dos_setores = data.frame()
indice_dos_nomes_dos_setores= 1
grupo_do_janelamento = 1

for(grupo_janelamento in grupos_janelamento){
  # determina as series temporais para cada janelamento  
  series_temporais = todos_janelamentos[todos_janelamentos$grupo_janelamento==grupo_janelamento,]
  
  # a partir do 2, para eliminar grupo_janelamento
  for(indice_setor in 2:ncol(series_temporais)){
    nome_do_setor =  names(series_temporais)[indice_setor]
    serie = series_temporais[,indice_setor]
    
    source("../../4 - Calcula metricas/previsao_exponencial.R")
    setwd(diretorio_atual)
    metricas = rbind(metricas, cbind(eixo_x_frequencias,alvo,previsao,sse,a,coeficiente_B,volatilidade,grupo_do_janelamento))
    colnames(metricas) = c("eixo_x_frequencias","alvo","previsao","sse","a","coeficiente_B","volatilidade","grupo_do_janelamento")
    nome_dos_setores = rbind(nome_dos_setores,(data.frame(setor = rep(x = nome_do_setor,length(alvo)))))
    # write.table(metricas,paste(names(series_temporais)[i],grupo_janelamento,"combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv"),row.names=F,sep=",")
    formato_ponto = 21
    
#     png(paste(sse,".png"))
#     plot( eixo_x_frequencias,log(alvo),xlab="Returns",ylab="Sector Consumption Not Cyclical / Processed Food" ,pch=formato_ponto,las=1)
#     lines(eixo_x_frequencias,log(previsao),col="gray",lwd=2)
#     dev.off()
  }
  grupo_do_janelamento = grupo_do_janelamento + 1
}

metricas$setor = nome_dos_setores$setor
metricas$b_volatilidade = metricas$coeficiente_B*metricas$volatilidade
write.csv(metricas,file="metricas_setores_com_series_temporais.csv",row.names=F)
# # metricas = read.csv("metricas_setores_com_series_temporais.csv")
# 
# # grupo 2 e 16
# melhor_caso = metricas[order(metricas$sse,decreasing = F),]
# formato_ponto = 21
# 
# # # colocar em inglês
# # # LOG LOG
# # "Cons N Básico / Alimentos Processados" 
# setEPS()
# postscript("imagens/melhor_exponencial_log_log.eps")
# # plot(rnorm(100), main="Hey Some Data")
# # melhor_caso_log_log = metricas[melhor_caso$grupo_do_janelamento==2 & melhor_caso$setor=="Const.e.Transp...Constr.e.Engenh",]
# melhor_caso_log_log = metricas[metricas$sse==0.00292659853138067,]
# atual = melhor_caso_log_log[,"alvo"]
# previsao = melhor_caso_log_log[,"previsao"]
# # main = paste("Outlier\nSSE: ",unique(melhor_caso_log_log$sse),"\nB: ",unique(melhor_caso_log_log$coeficiente_B)," volat.: ",unique(melhor_caso_log_log$volatilidade))
# # "Consumo não Cíclico/Alimentos Processados"
# plot( log(melhor_caso_log_log[,c(1,2)]),xlab="LOG Returns",ylab="LOG Construction and Transportation",pch=formato_ponto,las=1)
# # ,pch=20
# lines(log(melhor_caso_log_log[,c(1,3)]),col="gray",lwd=2)
# dev.off()
# 
# ###################################################################
# #SEMILOG
# # LOG no Y, normal no X
# #"Cons N Cíclico / Pr Pessoal Limp"  # colocar em inglÊs
# setEPS()
# postscript("imagens/segunda_melhor_exponencial_semi_log.eps")
# # unique(metricas$setor)
# segundo_melhor_caso_semi_log = metricas[metricas$sse==0.022100772001507,]
# # segundo_melhor_caso_semi_log = metricas[melhor_caso$grupo_do_janelamento==17 & melhor_caso$setor=="Cons.N.Básico...Alimentos.Processados",]
# atual = segundo_melhor_caso_semi_log[,"alvo"]
# previsao = segundo_melhor_caso_semi_log[,"previsao"]
# # main = paste("Outlier\nSSE: ",unique(segundo_melhor_caso_semi_log$sse),"\nB: ",unique(segundo_melhor_caso_semi_log$coeficiente_B)," volat.: ",unique(segundo_melhor_caso_semi_log$volatilidade))
# # Consumo não Cíclico/Produtos de uso Pessoal e Limpeza
# plot( segundo_melhor_caso_semi_log[,"eixo_x_frequencias"],log(segundo_melhor_caso_semi_log[,"alvo"]),xlab="Returns",ylab="Sector Consumption Not Cyclical / Processed Food" ,pch=formato_ponto,las=1)
# lines(segundo_melhor_caso_semi_log[,"eixo_x_frequencias"],log(segundo_melhor_caso_semi_log[,"previsao"]),col="gray",lwd=2)
# # mtext("aaaaaaaaaaaaaaadownvar",side=1,line=2,col=1)
# dev.off()
# 
