dados = janelamento_b

dados_treinamento = dados[indice_treino,]
dados_validacao = dados[indice_validacao,]
dados_teste = dados[indice_teste,]

entrada_treinamento = dados_entrada(dados_treinamento)
alvo_treinamento = dados_alvo(dados_treinamento)

entrada_teste = dados_entrada(dados_teste)
alvo_teste = dados_alvo(dados_teste)


model_b = mlp(entrada_treinamento, alvo_treinamento, size = neuronio, 
              learnFuncParams = learnParams, maxit = iteracao, inputsTest = entrada_teste, 
              targetsTest = alvo_teste) 

predicao_teste_b <- as.vector( model_b$fittedTestValues)
lines(predicao_teste_b, col="green",lwd=1,pch=18,type="o")
