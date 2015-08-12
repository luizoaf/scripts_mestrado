
# dados_treino = janelamento
# dados_treino = normalizacao_transformacao_linear(.15,.85,dados_treino)
# dados_treino = as.data.frame(normalizeData(dados_treino,type="0_1"))
dados_treino= dados_treino[sample(1:nrow(dados_treino) ,length(1:nrow(dados_treino))), 1:ncol(dados_treino)]
enValues= dados_treino[,1:(ncol(dados_treino)-1)]
enTargets = dados_treino[,ncol(dados_treino)]
patterns = splitForTrainingAndTest(enValues, enTargets,ratio=porc_teste)
model = mlp(patterns$inputsTrain, patterns$targetsTrain, size = neuronio, 
            learnFuncParams = learnParams, maxit = iteracao, inputsTest = patterns$inputsTest, 
            targetsTest = patterns$targetsTest) 

predicao_treino <-  as.vector(model$fitted.values)
targets_treino = patterns$targetsTrain

predicao_teste <- as.vector( model$fittedTestValues)
targets_teste = patterns$targetsTest


sse_treino = sum((predicao_treino-targets_treino)^2)
sse_teste = sum((predicao_teste-targets_teste)^2)
sse_treino
sse_teste

par(mfrow=c(3,2))
plot(main=paste(setor,": treino\nSSE:",sse_treino),targets_treino,type="l")
lines(predicao_treino,col="red")

# plot(main=paste(setor,": teste\nSSE:",sse_teste),targets_teste,type="l",cex.id = 2,lwd=3,ylim=c(0.3,1.2))
# lines(predicao_teste,col="black",lwd=3,lty=3)


# plot(main="Construção e Transporte / Construção e Engenharia",targets_teste,type="l",cex.id = 2,lwd=3,ylim=c(0.3,1.2))
# lines(predicao_teste,col="black",lwd=3,lty=3)

# lines(predicao_teste,col="red")
# png(file= paste("Submission",indice,".png",sep=""))
# par(mfrow=c(2,1))
# plotIterativeError(model) 
# plotRegressionError(predicao_teste, targets_teste, pch = 3) 

plotIterativeError(model) 
plotRegressionError(main="Treino: Regressão resíduos",patterns$targetsTrain, model$fitted.values) 
plotRegressionError(main="Teste: Regressão resíduos",patterns$targetsTest, model$fittedTestValues) 
hist(main="Resíduos treino",model$fitted.values - patterns$targetsTrain)
# dev.off()


sse_treino = sum((predicao_treino-targets_treino)^2)
sse_teste = sum((predicao_teste-targets_teste)^2)
sse_treino
sse_teste

mse_treino = mean((predicao_treino-targets_treino)^2)
mse_teste = mean((predicao_teste-targets_teste)^2)
mse_treino
mse_teste


rmse_treino = sqrt(mse_treino)
rmse_teste = sqrt(mse_teste)
rmse_treino
rmse_teste

mape_treino = mean(100*abs(predicao_treino-targets_treino)/targets_treino)
mape_teste = mean(100*abs(predicao_teste-targets_teste)/targets_teste)
mape_treino
mape_teste


mse_treino = mean((predicao_treino-targets_treino)^2)
mse_teste = mean((predicao_teste-targets_teste)^2)
mse_treino
mse_teste

# Predict On Change In Direction – Medida de desempenho
# que apresenta o erro percentual para previsão de tendência
# instantânea
# directional symmetry (DS)
# O POCID terá resultado entre 0 (zero) e 100 (cem), e, quanto mais próximo de
# 100, melhor será o modelo de previsão. Essa métrica de desempenho é muito
# importante quando aplicada ao mercado de ações, pois a correta previsão de subidas e
# descidas das cotações das ações afetam diretamente os ganhos e as perdas financeiras.
# predicao_teste = 1:10
# targets_teste = 3:12
POCID = function(previsao,alvo){
  valores_d = c()
  indices_d=1
  for(i in 2:length(previsao)){
    d = 0
    if((previsao[i]-previsao[i-1])*
         (alvo[i]-alvo[i-1]) >=0){
      d = 1
    }
    valores_d[indices_d] = d
    indices_d = indices_d + 1
    
  }
  POCID_resultado = (100/(length(previsao)-1))*sum(valores_d)
  return(POCID_resultado)
}
# O POCID terá resultado entre 0 (zero) e 100 (cem), e, quanto mais próximo de
# 100, melhor será o modelo de previsão. Essa métrica de desempenho é muito
# importante quando aplicada ao mercado de ações, pois a correta previsão de subidas e
# descidas das cotações das ações afetam diretamente os ganhos e as perdas financeiras.
POCID_treino = POCID(predicao_treino,targets_treino)
POCID_teste = POCID(predicao_teste,targets_teste)
POCID_treino
POCID_teste

# U de THEIL - NMSE (Normalised Mean Square Error)
# A métrica U de THEIL – também conhecida como NMSE [17], que será chamado
# nesse trabalho através do seu acrônimo THEIL, e cujo valor mede a relação entre os
# quadrados do sistema testado e o quadrado dos erros de um modelo do tipo Random
# Walk, caminho aleatório, segundo o qual a previsão de um determinado valor da série
# no instante t é calculada a partir de uma dependência linear com o seu último retardo
# no tempo t-1. A partir daí tem-se output$ = output$ - r$, em que r$ é um termo
# aleatório gerado por uma distribuição uniforme de números aleatórios, e a equação
# final
# sendo THEIL = 1, o modelo testado tem desempenho igual ao Random Walk; quando
# THEIL > 1, o desempenho é inferior e, se THEIL < 1, o modelo é superior ao
# Random Walk. Quanto mais próximo de 0 (zero) o THEIL, melhor o resultado de um
# modelo.
# nrmse_treino = nrmse(predicao_treino,targets_treino)
# nrmse_teste =  nrmse(predicao_teste,targets_teste)
# nrmse_treino = mse(predicao_treino,targets_treino)/mse(targets_treino,rep(0,length(predicao_treino)))
# nrmse_teste = mse(predicao_teste,targets_teste)/mse(targets_teste,rep(0,length(predicao_teste)))
# nrmse_treino
# nrmse_teste
# require(rgp)
# ?nmse
# nmse(predicao_treino,targets_treino)
# nmse(predicao_teste,targets_teste)
# 
# mean((predicao_treino-targets_treino)^2/(mean(predicao_treino)*mean(targets_treino)))
# 
# (1/length(predicao_treino)) * sum((predicao_treino-targets_treino)^2)/(mean(predicao_treino)*mean(targets_treino))

# sum((predicao_treino-targets_treino)^2)/(sum(targets_treino)^2)
nmse_treino = mean((predicao_treino-targets_treino)^2)/(mean(predicao_treino)*mean(targets_treino))
nmse_teste = mean((predicao_teste-targets_teste)^2)/(mean(predicao_teste)*mean(targets_teste))
nmse_treino
nmse_teste
# ARV (Average Relative Variance)
# A medida de desempenho ARV mede o ganho de desempenho do modelo testado em
# relação a outro que realiza previsões apenas calculando a média aritmética das
# observações da série. Ela tem como equação

arv = function(previsao,alvo){
  numerador = sum((previsao-alvo)^2)
  media = mean(alvo)
  denominador = sum((previsao-media)^2)
  arv_resultado = (numerador/denominador)
  #*(1/length(previsao))
  if(is.na(arv_resultado) | arv_resultado == Inf){
    return(1) # igual a media
  }
  return(arv_resultado)
}
# arv = function(previsao,alvo){
#   numerador = sum((previsao-alvo)^2)
#   denominador = sum((previsao-mean(alvo))^2)
#   arv_resultado = (1/length(previsao))*(numerador/denominador)
#   return(arv_resultado)
# }
arv_treino = arv(predicao_treino,targets_treino)
arv_teste = arv(predicao_teste,targets_teste)
arv_treino
arv_teste

# MSE – (Mean of Squared Errors), MAPE – (Mean Absolute
#                                         Percentual Error), U de THEIL ou NMSE – (Normalised Mean Square Error),
# POCID – (Prediction On Change In Direction), ARV – (Average Relative Variance)
# e SLG – (Sum of Losses and Gains) a medida proposta. As métricas de desenpenho
# 
# x = previsao_treino
# normalized = (x-min(x))/(max(x)-min(x))
# normalize(x)

# tem que normalizar
# previsao_treino = predicao_treino[,1] + elimina_o_zero
# alvo_treino = targets_treino + elimina_o_zero
# mape_treino = mean(abs(predicao_treino[,1]-alvo_treino)/abs(alvo_treino))
# mape_treino
# mape_teste = sqrt(mse_teste)
# mape_treino
# mape_teste
# arv = function(previsao,alvo){
#   numerador = sum((previsao-alvo)^2)
#   denominador = sum((previsao-mean(alvo))^2)
#   arv_resultado = (numerador/denominador)*(1/length(previsao))
#   if(is.na(arv_resultado) | arv_resultado == Inf){
#     return(1) # igual a media
#   }
#   return(arv_resultado)
# }
# previsao = c(3,2,3,3,3)
# alvo=1:5
# arv(previsao,previsao)

# previsao = y
# alvo = y
# numerador = sum(previsao-alvo)^2
# denominador = sum(previsao-mean(alvo))^2
# arv_resultado = (1/length(previsao))*(numerador/denominador)
arv_treino
arv_teste
