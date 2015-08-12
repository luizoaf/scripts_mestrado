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

# model_b_vo = mlp

learnParams = learnFuncParams[1]
porc_teste = porcentagem.teste[1]
iteracao = iteracoes[1]
neuronio = neuronios.na.camada.escondida[1]

dados_treino$id = 1:nrow(dados_treino)
dados_treino= dados_treino[sample(1:nrow(dados_treino) ,length(1:nrow(dados_treino))), 1:ncol(dados_treino)]
indices = dados_treino$id
dados_treino = dados_treino[,1:(ncol(dados_treino)-1)]
enValues= dados_treino[,1:(ncol(dados_treino)-1)]
enTargets = dados_treino[,ncol(dados_treino)]
patterns_b_v = splitForTrainingAndTest(enValues, enTargets,ratio=porc_teste)
model_b_v = mlp(patterns_b_v$inputsTrain, patterns_b_v$targetsTrain, size = neuronio, 
            learnFuncParams = learnParams, maxit = iteracao, inputsTest = patterns_b_v$inputsTest, 
            targetsTest = patterns_b_v$targetsTest) 

# predicao_treino <-  as.vector(model_b_v$fitted.values)
# targets_treino = patterns_b_v$targetsTrain

predicao_teste_b_v <- as.vector( model_b_v$fittedTestValues)
targets_teste = patterns_b_v$targetsTest


# par(mfrow=c(3,2))
# plot(main=paste(setor,": treino\nSSE:",sse_treino),targets_treino,type="l")
# lines(predicao_treino,col="red")

# plot(main=paste(setor,": teste\nSSE:",sse_teste),targets_teste,type="l",cex.id = 2,lwd=3,ylim=c(0.3,1.2))
# lines(predicao_teste,col="black",lwd=3,lty=3)

if(i == 4){
plot(main=nome_series_temporais[i],targets_teste,type="o",lwd=2,pch=16,xlab="Meses",ylab="Volatilidade",col=1,ylim=c(0.4,.8))
  
}else{
  plot(main=nome_series_temporais[i],targets_teste,type="o",lwd=2,pch=16,xlab="Meses",ylab="Volatilidade",col=1,ylim=c(0.5,1.5))
}
# lines(predicao_teste_b_v,col=2,lwd=3,lty=3)
lines(predicao_teste_b_v, type="o", pch=17,lwd=1, col="red")

