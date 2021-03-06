########### DEFINICAO MAIOR LAG ############
volatilidade = data.frame(volatilidade = serie_temporal_setor$volatilidade)
coeficiente_B = data.frame(coeficiente_B = serie_temporal_setor$coeficiente_B)
#### volatilidade ####
autocorrelacao = acf(serie_temporal_setor$volatilidade, type = c("correlation"))

#objetivo � selecionar os lags que n�o est�o dentro da correla��o baixa(dentro da faixa azul)
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

#objetivo � selecionar os lags que n�o est�o dentro da correla��o baixa(dentro da faixa azul)
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






dados = lags_b_volatilidade

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
# 

# dados_treino$id = 1:nrow(dados_treino)
# dados_treino= dados_treino[sample(1:nrow(dados_treino) ,length(1:nrow(dados_treino))), 1:ncol(dados_treino)]
# indices = dados_treino$id
# dados_treino = dados_treino[,1:(ncol(dados_treino)-1)]
# enValues= dados_treino[,1:(ncol(dados_treino)-1)]
# enTargets = dados_treino[,ncol(dados_treino)]

dados_entrada = function(dados){
  return(dados[,1:(ncol(dados)-1)])
}

dados_alvo = function(dados){
  return(dados[,ncol(dados)])
}

# Dividindo 50% de treino, 25% de valida��o e 25% de teste
cinquenta_porcento = .5 * nrow(dados) 
setenta_e_quinco_porcento = .75 * nrow(dados)

indice_treino = 1:cinquenta_porcento
indice_validacao = (cinquenta_porcento + 1):setenta_e_quinco_porcento
indice_teste = (setenta_e_quinco_porcento + 1):nrow(dados)

dados$id = 1:nrow(dados)
dados = dados[sample(1:nrow(dados) ,length(1:nrow(dados))), 1:ncol(dados)]
dados_treinamento = dados[indice_treino,]
dados_validacao = dados[indice_validacao,]
dados_teste = dados[indice_teste,]

indice_treino = dados_treinamento$id
indice_validacao = dados_validacao$id
indice_teste = dados_teste$id

dados_treinamento = dados_treinamento[,1:(ncol(dados_treinamento)-1)]
dados_validacao = dados_validacao[,1:(ncol(dados_validacao)-1)]
dados_teste = dados_teste[,1:(ncol(dados_teste)-1)]


################## TREINO, VALIDACAO e TESTE
entrada_treinamento = dados_entrada(dados_treinamento)
alvo_treinamento = dados_alvo(dados_treinamento)

entrada_validacao = dados_entrada(dados_validacao)
alvo_validacao = dados_alvo(dados_validacao)

entrada_teste = dados_entrada(dados_teste)
alvo_teste = dados_alvo(dados_teste)

############ CRIA MLP ############
model_b_v = mlp(entrada_treinamento, alvo_treinamento, size = neuronio, 
            learnFuncParams = learnParams, maxit = iteracao, inputsTest = entrada_teste, 
            targetsTest = alvo_teste) 

predicao_treino <-  as.vector(predict(model_b_v,entrada_treinamento))
predicao_validacao <- as.vector(predict(model_b_v,entrada_validacao))
predicao_teste <- as.vector(predict(model_b_v,entrada_teste))

# par(mfrow=c(3,2))
# plot(main=paste(setor,": treino\nSSE:",sse_treino),targets_treino,type="l")
# lines(predicao_treino,col="red")

# plot(main=paste(setor,": teste\nSSE:",sse_teste),targets_teste,type="l",cex.id = 2,lwd=3,ylim=c(0.3,1.2))
# lines(predicao_teste,col="black",lwd=3,lty=3)

if(i == 4){
plot(main=nome_series_temporais[i],alvo_teste,type="o",lwd=2,pch=16,xlab="Meses",ylab="Volatilidade",col=1)
# ylim=c(0.4,.8)
  
}else{
  plot(main=nome_series_temporais[i],alvo_teste,type="o",lwd=2,pch=16,xlab="Meses",ylab="Volatilidade",col=1,ylim=c(0.5,1.5))
}
# lines(predicao_teste_b_v,col=2,lwd=3,lty=3)
lines(predicao_teste, type="o", pch=17,lwd=1, col="red")
variavel_entrada = "volatilidade_e_b"
source("calcula_metricas.R")
