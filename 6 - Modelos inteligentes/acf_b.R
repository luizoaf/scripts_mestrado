# setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/6 - Modelos inteligentes")
# # setores = read.csv("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/series_temporais_setores.csv")
# setores = read.csv("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor/metricas_setores_sem_series_temporais.csv")
# todos_setores = unique(setores$setor)
# 
# setor_b_volatilidade = function(ativo_financeiro){
#   ativo = subset(setores,setores$setor == ativo_financeiro,select = c(coeficiente_B, volatilidade))
#   return(ativo)
# }
# setor = todos_setores[1]
# serie_temporal_setor = setor_b_volatilidade(setor)
# plot(serie_temporal_setor$volatilidade)
# type = c("correlation", "covariance", "partial")
# autocorrelacao = acf(serie_temporal_setor$volatilidade, type = c("correlation"))
# serie_temporal_setor$coeficiente_B,serie_temporal_setor$volatilidade# 
autocorrelacao = ccf(serie_temporal_setor$coeficiente_B,serie_temporal_setor$volatilidade,
                                        type = c("correlation"), ylab = "cross-correlation")

#objetivo é selecionar os lags que não estão dentro da correlação baixa(dentro da faixa azul)
linha_azul_superior = var(serie_temporal_setor$volatilidade)*(2)
linha_azul_inferior = -var(serie_temporal_setor$volatilidade)*(2)
lags_relevantes = data.frame(lag = autocorrelacao$lag,correlacao = as.vector(autocorrelacao$acf))
# lags_relevantes = lags_relevantes[abs(lags_relevantes$correlacao)>=.7,]
lags_alta_correlacao = lags_relevantes$lag[(lags_relevantes$lag > 0 & lags_relevantes$correlacao>linha_azul_superior) |
                                             (lags_relevantes$correlacao >-1 & lags_relevantes$correlacao<linha_azul_inferior)]
print("Coeficiente B")
print(length(lags_alta_correlacao))
print((lags_alta_correlacao))
lags_alta_correlacao = lags_alta_correlacao[lags_alta_correlacao>0]#>=
lags_alta_correlacao = rev(lags_alta_correlacao)
indices_B = lags_alta_correlacao
# tem_zero =F
# if(0 %in% indices_B ){
#   indices_B = indices_B+1
#   tem_zero = T
# }
volatilidade = data.frame(volatilidade = serie_temporal_setor$volatilidade)
coeficiente_B = data.frame(coeficiente_B = serie_temporal_setor$coeficiente_B)
tamanho = max(indices_B)


dias_mes = 1
dias_ano = tamanho
total_dias = nrow(volatilidade)
inicio_janelamento = seq(from=1,to=total_dias,by=dias_mes)
fim_janelamento = seq(from=1,to=total_dias,by=dias_mes)+dias_ano

janelamentos_indices = data.frame(inicio = inicio_janelamento,fim = fim_janelamento)
janelamentos_indices = janelamentos_indices[janelamentos_indices$fim<=total_dias,]

# head(serie_temporal_setor,n=10)
# linha_janelamento = 1
# linha_janelamento = nrow(janelamentos_indices)
janelamento = data.frame()
for(linha_janelamento in 1:nrow(janelamentos_indices)){
  
  #   indices_janelamento = janelamentos_indices$inicio[linha_janelamento]:janelamentos_indices$fim[linha_janelamento]
  indices = c(rev((janelamentos_indices$fim[linha_janelamento]-1):janelamentos_indices$inicio[linha_janelamento]),janelamentos_indices$fim[linha_janelamento])
  lags_B = coeficiente_B[indices[1:(length(indices)-1)],]
  valor_volatilidade = volatilidade[(indices[length(indices)]),]
  janelamento = rbind(janelamento,c(lags_B,valor_volatilidade))
}

# if(tem_zero ){
#   colnames(janelamento) = c(rev(0:(tamanho-1)),"alvo")
#   #   lags_alta_correlacao = lags_alta_correlacao -1
# }else{
colnames(janelamento) = c((tamanho):1,"alvo")
# }
janelamento = janelamento[,c(lags_alta_correlacao,"alvo")]
# volatilidade[1:11,]
# coeficiente_B[1:11,]
# volatilidade[11:22,]
# coeficiente_B[2:11,]
# head(janelamento)

# janelamento = na.omit(janelamento)
# 
# porc_teste = .25
# neuronio = 9
# iteracao = 1500
# learnParams = .3

require("RSNNS")


# 
# porcentagem.teste = c(.25,.3)
# iteracoes = c(300,500,1000,1500)
# neuronios.na.camada.escondida =  c(9,13,15)
# learnFuncParams = c(.3,.2,.1)
# 
# porcentagem.teste = c(.3)
# iteracoes = c(300,500)
# neuronios.na.camada.escondida =  c(9)
# learnFuncParams = c(.3)

# modelo = mlp

# learnParams = learnFuncParams[1]
# porc_teste = porcentagem.teste[1]
# iteracao = iteracoes[1]
# neuronio = neuronios.na.camada.escondida[1]
dados_treino = janelamento
indice = 1
resultados = data.frame()
for(iteracao in iteracoes){
  for(neuronio in neuronios.na.camada.escondida){
    for(learnParams in learnFuncParams){
      for(porc_teste in porcentagem.teste){
        for( i in 1:30){
          source("mlp.R")
          resultados = rbind(resultados,c(indice,learnParams,porc_teste,iteracao,neuronio ,sse_treino,sse_teste,mape_treino,mape_teste,
                                          mse_treino,mse_teste,POCID_treino,POCID_teste,nmse_treino,nmse_teste,arv_treino,arv_teste))
        }
        indice = indice+1
      }
    }
  }
}

colnames(resultados) = c("combinacao","learnParams","porc_teste","iteracao","neuronio" ,"sse_treino","sse_teste","mape_treino","mape_teste",
                         "mse_treino","mse_teste","POCID_treino","POCID_teste","nmse_treino","nmse_teste","arv_treino","arv_teste")


# combinacoes = unique(resultados[,c("combinacao","learnParams", "porc_teste",  "iteracao",    "neuronio" )])
# 
# dados = resultados
# coeficiente.de.confianca = .95
# intervalos<-function(dados){
#   ic <- epi.conf(dados,conf = coeficiente.de.confianca)
#   return(c(ic$lower,ic$est,ic$upper))
# }
# 
# ic_combinacoes<- as.data.frame(as.matrix(aggregate(dados$mape_treino,list(as.character(dados$combinacao)),FUN = intervalos)))
# colnames(ic_combinacoes) = c("combinacao","minimo","media","maximo")
# # png(questao,bg ="transparent", width = 950,height = 950)
# print(ggplot(ic_combinacoes, aes(x=combinacao,y=media, fill= combinacao)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
#       # + geom_bar()+ geom_hline(aes(yintercept=mean(dados$sse_teste))))
# # dev.off()
# 
# combinacoes_ic = merge(combinacoes,ic_combinacoes)
# write.csv(combinacoes_ic,file="combinacoes_ic_b.csv",row.names=F)
# 
# combinacoes_ic_data = read.csv("combinacoes_ic_b.csv")
# print(ggplot(combinacoes_ic_data, aes(x=combinacao,y=.5, fill= combinacao)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.3))
# 
# 
# # grupo_1 = subset(resultados,resultados$combinacao==1)
# # grupo_2 = subset(resultados,resultados$combinacao==2)
# # # shapiro.test(grupo_1$sse_teste)
# # wilcox.test(grupo_1$sse_teste,grupo_2$sse_teste,paired = T)
# 
# combinacoes_ic_data = combinacoes_ic_data[order(combinacoes_ic_data$maximo,decreasing = T),]
# melhores = combinacoes_ic_data[combinacoes_ic_data$maximo[nrow(combinacoes)]>combinacoes_ic_data$minimo,] 
# melhores = melhores[order(melhores$maximo,decreasing = T),]
# melhor = melhores[nrow(melhores),]
# # combinacao learnParams porc_teste iteracao neuronio     minimo      media     maximo
# # 67         67         0.3       0.25     1500       15 0.01170624 0.01836296 0.02501969
# print(ggplot(melhores, aes(x=combinacao,y=media, fill= combinacao)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
