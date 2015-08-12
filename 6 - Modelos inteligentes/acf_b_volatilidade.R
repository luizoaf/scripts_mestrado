########### DEFINICAO MAIOR LAG ############
volatilidade = data.frame(volatilidade = serie_temporal_setor$volatilidade)
coeficiente_B = data.frame(coeficiente_B = serie_temporal_setor$coeficiente_B)
#### volatilidade ####
autocorrelacao = acf(serie_temporal_setor$volatilidade, type = c("correlation"))

#objetivo é selecionar os lags que não estão dentro da correlação baixa(dentro da faixa azul)
linha_azul_superior = var(serie_temporal_setor$volatilidade)*(2)
linha_azul_inferior = -var(serie_temporal_setor$volatilidade)*(2)
lags_relevantes = data.frame(lag = autocorrelacao$lag,correlacao = as.vector(autocorrelacao$acf))
# lags_relevantes = lags_relevantes[abs(lags_relevantes$correlacao)>=.7,]
lags_alta_correlacao = lags_relevantes$lag[(lags_relevantes$lag > 0 & lags_relevantes$correlacao>linha_azul_superior) |
                                             (lags_relevantes$correlacao >-1 & lags_relevantes$correlacao<linha_azul_inferior)]
lags_alta_correlacao_volatilidade = rev(lags_alta_correlacao)
tamanho_volatilidade = max(lags_alta_correlacao_volatilidade)


#### B ####
autocorrelacao = ccf(serie_temporal_setor$coeficiente_B,serie_temporal_setor$volatilidade,
                     type = c("correlation"), ylab = "cross-correlation")

#objetivo é selecionar os lags que não estão dentro da correlação baixa(dentro da faixa azul)
linha_azul_superior = var(serie_temporal_setor$volatilidade)*(2)
linha_azul_inferior = -var(serie_temporal_setor$volatilidade)*(2)
lags_relevantes = data.frame(lag = autocorrelacao$lag,correlacao = as.vector(autocorrelacao$acf))
# lags_relevantes = lags_relevantes[abs(lags_relevantes$correlacao)>=.7,]
lags_alta_correlacao = lags_relevantes$lag[(lags_relevantes$lag > 0 & lags_relevantes$correlacao>linha_azul_superior) |
                                             (lags_relevantes$correlacao >-1 & lags_relevantes$correlacao<linha_azul_inferior)]
lags_alta_correlacao = lags_alta_correlacao[lags_alta_correlacao>0]#>=
lags_alta_correlacao_b = rev(lags_alta_correlacao)
indices_B = lags_alta_correlacao_b
tamanho_b = max(indices_B)

tamanho = max(c(tamanho_volatilidade,tamanho_b))
######### volatilidade ###############
dias_mes = 1
dias_ano = tamanho
total_dias = nrow(volatilidade)
inicio_janelamento = seq(from=1,to=total_dias,by=dias_mes)
fim_janelamento = seq(from=1,to=total_dias,by=dias_mes)+dias_ano

janelamentos_indices = data.frame(inicio = inicio_janelamento,fim = fim_janelamento)
janelamentos_indices = janelamentos_indices[janelamentos_indices$fim<=total_dias,]


# linha_janelamento = 1
janelamento_volatilidade = data.frame()
for(linha_janelamento in 1:nrow(janelamentos_indices)){
  
  #   indices_janelamento = janelamentos_indices$inicio[linha_janelamento]:janelamentos_indices$fim[linha_janelamento]
  indices = c(rev((janelamentos_indices$fim[linha_janelamento]-1):janelamentos_indices$inicio[linha_janelamento]),janelamentos_indices$fim[linha_janelamento])
  janelamento_volatilidade = rbind(janelamento_volatilidade,volatilidade[indices,])
}

colnames(janelamento_volatilidade) = c(tamanho:1,"alvo")
janelamento_volatilidade = janelamento_volatilidade[,c(lags_alta_correlacao_volatilidade,"alvo")]
colnames(janelamento_volatilidade) = c(paste(names(janelamento_volatilidade)[1:(ncol(janelamento_volatilidade)-1)],"_v",sep=""),"alvo")




########### B ###########
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
janelamento_b = data.frame()
for(linha_janelamento in 1:nrow(janelamentos_indices)){
  
  #   indices_janelamento = janelamentos_indices$inicio[linha_janelamento]:janelamentos_indices$fim[linha_janelamento]
  indices = c(rev((janelamentos_indices$fim[linha_janelamento]-1):janelamentos_indices$inicio[linha_janelamento]),janelamentos_indices$fim[linha_janelamento])
  lags_B = coeficiente_B[indices[1:(length(indices)-1)],]
  valor_volatilidade = volatilidade[(indices[length(indices)]),]
  janelamento_b = rbind(janelamento_b,c(lags_B,valor_volatilidade))
}

# if(tem_zero ){
#   colnames(janelamento) = c(rev(0:(tamanho-1)),"alvo")
#   #   lags_alta_correlacao = lags_alta_correlacao -1
# }else{
colnames(janelamento_b) = c((tamanho):1,"alvo")
# }
janelamento_b = janelamento_b[,c(lags_alta_correlacao_b,"alvo")]

colnames(janelamento_b) = c(paste(names(janelamento_b)[1:(ncol(janelamento_b)-1)],"_b",sep=""),"alvo")
########################### MERGE ################
lags_b_volatilidade = cbind(janelamento_b[,1:(ncol(janelamento_b)-1)],janelamento_volatilidade[,1:(ncol(janelamento_volatilidade)-1)])
lags_b_volatilidade$alvo = janelamento_b$alvo
# lags_b_volatilidade = merge(janelamento_b,janelamento_volatilidade)
# lags_b_volatilidade = lags_b_volatilidade[,setdiff(names(lags_b_volatilidade),"alvo")]






dados_treino = lags_b_volatilidade

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
#           resultados = rbind(resultados,c(indice,learnParams,porc_teste,iteracao,neuronio ,sse_treino,sse_teste,mape_treino,mape_teste,
#                                           mse_treino,mse_teste,POCID_treino,POCID_teste,nmse_treino,nmse_teste,arv_treino,arv_teste))
        }
        indice = indice+1
      }
    }
  }
}

# colnames(resultados) = c("combinacao","learnParams","porc_teste","iteracao","neuronio" ,"sse_treino","sse_teste","mape_treino","mape_teste",
#                          "mse_treino","mse_teste","POCID_treino","POCID_teste","nmse_treino","nmse_teste","arv_treino","arv_teste")


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

# ic_combinacoes<- as.data.frame(as.matrix(aggregate(dados$mape_treino,list(as.character(dados$combinacao)),FUN = intervalos)))
# colnames(ic_combinacoes) = c("combinacao","minimo","media","maximo")
# # png(questao,bg ="transparent", width = 950,height = 950)
# print(ggplot(ic_combinacoes, aes(x=combinacao,y=media, fill= combinacao)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
# # + geom_bar()+ geom_hline(aes(yintercept=mean(dados$sse_teste))))
# # dev.off()
# 
# 
# 
# 
# 
# 
# combinacoes_ic = merge(combinacoes,ic_combinacoes)
# write.csv(combinacoes_ic,file="combinacoes_ic_b_volatilidade.csv",row.names=F)
# 
# 
# 
# 
# combinacoes_ic_data = read.csv("combinacoes_ic_b_volatilidade.csv")
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
# #           combinacao learnParams porc_teste iteracao neuronio     minimo     media     maximo
# # 61         61         0.3       0.25     1500       13 0.02504367 0.0347577 0.04447174
# print(ggplot(melhores, aes(x=combinacao,y=media, fill= combinacao)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
