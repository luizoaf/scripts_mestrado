dados = janelamento_b

dados_treinamento = dados[indice_treino,]
dados_validacao = dados[indice_validacao,]
dados_teste = dados[indice_teste,]

entrada_treinamento = dados_entrada(dados_treinamento)
alvo_treinamento = dados_alvo(dados_treinamento)

entrada_validacao = dados_entrada(dados_validacao)
alvo_validacao = dados_alvo(dados_validacao)

entrada_teste = dados_entrada(dados_teste)
alvo_teste = dados_alvo(dados_teste)


model_b = mlp(entrada_treinamento, alvo_treinamento, size = neuronio, 
              learnFuncParams = learnParams, maxit = iteracao, inputsTest = entrada_teste, 
              targetsTest = alvo_teste) 

predicao_treino <-  as.vector(predict(model_b,entrada_treinamento))
predicao_validacao <- as.vector(predict(model_b,entrada_validacao))
predicao_teste <- as.vector(predict(model_b,entrada_teste))

lines(predicao_teste, col="green",lwd=1,pch=18,type="o")
variavel_entrada = "b"
source("calcula_metricas.R")