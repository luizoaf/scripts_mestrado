# setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/6 - model_vos inteligentes")
# # setores = read.csv("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/series_temporais_setores.csv")
# setores = read.csv("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor/metricas_setores_sem_series_temporais.csv")
# todos_setores = unique(setores$setor)
# # df2=aggregate(setores$coeficiente_B,list(setores$setor), FUN= unique, na.rm=TRUE)
# # head(df2)
# # 
# # names(setores)
# 
# # i_ativo_financeiro = 1
# # s = subset(setores,setores$setor == ativo_financeiro,select = c(setor,coeficiente_B, volatilidade))
# 
# setor_b_volatilidade = function(ativo_financeiro){
#   ativo = subset(setores,setores$setor == ativo_financeiro,select = c(coeficiente_B, volatilidade))
#   return(ativo)
# }
# # setor = todos_setores[1]
# # serie_temporal_setor = setor_b_volatilidade(setor)
# # # plot(serie_temporal_setor$volatilidade)
# # type = c("correlation", "covariance", "partial")
# autocorrelacao = acf(serie_temporal_setor$volatilidade, type = c("correlation"))
# # pacf(serie_temporal_setor$volatilidade)
# # ccf(serie_temporal_setor$coeficiente_B,serie_temporal_setor$volatilidade)
# # 
# # par(mfrow=c(3,3))
# # todos_setores = unique(setores$setor)
# # for(setor in todos_setores){
# #   acf(main=setor,setor_b_volatilidade(setor)$volatilidade, type = c("correlation"))
# # }
# 
# #objetivo é selecionar os lags que não estão dentro da correlação baixa(dentro da faixa azul)
# linha_azul_superior = var(serie_temporal_setor$volatilidade)*(2)
# linha_azul_inferior = -var(serie_temporal_setor$volatilidade)*(2)
# lags_relevantes = data.frame(lag = autocorrelacao$lag,correlacao = as.vector(autocorrelacao$acf))
# # lags_relevantes = lags_relevantes[abs(lags_relevantes$correlacao)>=.7,]
# lags_alta_correlacao = lags_relevantes$lag[(lags_relevantes$lag > 0 & lags_relevantes$correlacao>linha_azul_superior) |
#                       (lags_relevantes$correlacao >-1 & lags_relevantes$correlacao<linha_azul_inferior)]
# lags_alta_correlacao = rev(lags_alta_correlacao)
# print("Volatilidade")
# print(length(lags_alta_correlacao))
# print(lags_alta_correlacao)
# # mais recente é o último
# volatilidade = data.frame(volatilidade = serie_temporal_setor$volatilidade)
# tamanho = max(lags_alta_correlacao)
# lags = 
# for()
# lags =
#   


dados_treino = janelamento_volatilidade[indices,]

# merge(dados_treino,data.frame(alvo= enTargets))
enValues= dados_treino[,1:(ncol(dados_treino)-1)]
enTargets = dados_treino[,ncol(dados_treino)]
patterns_v = splitForTrainingAndTest(enValues, enTargets,ratio=porc_teste)
model_v = mlp(patterns_v$inputsTrain, patterns_v$targetsTrain, size = neuronio, 
            learnFuncParams = learnParams, maxit = iteracao, inputsTest = patterns_v$inputsTest, 
            targetsTest = patterns_v$targetsTest) 

# predicao_treino <-  as.vector(model_v$fitted.values)
# targets_treino = patterns_v$targetsTrain

predicao_teste_v <- as.vector( model_v$fittedTestValues)
targets_teste = patterns_v$targetsTest
lines(predicao_teste_v, col="blue",lwd=1,pch=18,type="o")
