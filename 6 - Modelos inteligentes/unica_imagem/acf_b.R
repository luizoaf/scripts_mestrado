
dados_treino = janelamento_b[indices,]
enValues= dados_treino[,1:(ncol(dados_treino)-1)]
enTargets = dados_treino[,ncol(dados_treino)]
patterns = splitForTrainingAndTest(enValues, enTargets,ratio=porc_teste)
model = mlp(patterns$inputsTrain, patterns$targetsTrain, size = neuronio, 
            learnFuncParams = learnParams, maxit = iteracao, inputsTest = patterns$inputsTest, 
            targetsTest = patterns$targetsTest) 

predicao_treino <-  as.vector(model$fitted.values)
targets_treino = patterns$targetsTrain

predicao_teste_b <- as.vector( model$fittedTestValues)
targets_teste = patterns$targetsTest
# lines(predicao_teste_b,col=4)
lines(predicao_teste_b, col="green",lwd=1,pch=18,type="o")
