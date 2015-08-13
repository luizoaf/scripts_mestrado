dados = janelamento_volatilidade

dados_treinamento = dados[indice_treino,]
dados_validacao = dados[indice_validacao,]
dados_teste = dados[indice_teste,]

entrada_treinamento = dados_entrada(dados_treinamento)
alvo_treinamento = dados_alvo(dados_treinamento)

entrada_teste = dados_entrada(dados_teste)
alvo_teste = dados_alvo(dados_teste)


model_v = mlp(entrada_treinamento, alvo_treinamento, size = neuronio, 
                learnFuncParams = learnParams, maxit = iteracao, inputsTest = entrada_teste, 
                targetsTest = alvo_teste) 

predicao_teste_v <- as.vector( model_v$fittedTestValues)
lines(predicao_teste_v, col="blue",lwd=1,pch=18,type="o")

