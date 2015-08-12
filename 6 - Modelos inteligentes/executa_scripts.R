################################# B ##########################

setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/6 - Modelos inteligentes")
# setores = read.csv("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/series_temporais_setores.csv")
setores = read.csv("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor/metricas_setores_sem_series_temporais.csv")
todos_setores = unique(setores$setor)

setor_b_volatilidade = function(ativo_financeiro){
  ativo = subset(setores,setores$setor == ativo_financeiro,select = c(coeficiente_B, volatilidade))
  return(ativo)
}

normalizacao_transformacao_linear = function(lim_min_norm,lim_max_norm,serie_temporal){
  dados_normalizados = (((lim_max_norm - lim_min_norm)*(serie_temporal - min(serie_temporal)))/(max(serie_temporal) - min(serie_temporal))) +lim_min_norm
  return(dados_normalizados)
}
require("RSNNS")
require("epiR")
require("ggplot2")

porcentagem.teste = c(.25)
iteracoes = c(2200)
neuronios.na.camada.escondida =  c(5)
learnFuncParams = c(.3)
# i=1
coluna_setores = as.character(rep(todos_setores,each = 90))
# coluna_setores = as.character(rep(todos_setores,each = 60))
# variavel_entrada = as.character(rep(c("Volatilidade","Coeficiente B"),each = 30,length(todos_setores)))
variavel_entrada = as.character(rep(c("Volatilidade","Volatilidade e Coeficiente B","Coeficiente B"),each = 30,length(todos_setores)))
todos_resultados = data.frame()
i=5
for(i in 1:length(todos_setores)){
  setor = todos_setores[i]
  serie_temporal_setor = setor_b_volatilidade(setor)
  source("acf_volatilidade.R")
  png(paste(setor,"_ACF_volatilidade.png",sep=""))
  plot(autocorrelacao)
  dev.off()
  todos_resultados = rbind(todos_resultados,resultados)  
  source("acf_b_volatilidade.R")
  png(paste(setor,"_ACF_b_volatilidade.png",sep=""))
  plot(autocorrelacao)
  dev.off()
  todos_resultados = rbind(todos_resultados,resultados) 
  source("acf_b.R")
  png(paste(setor,"_ACF_b.png",sep=""))
  plot(autocorrelacao)
  dev.off()
  todos_resultados = rbind(todos_resultados,resultados) 
#     source("avalia_resultado.R")
}

todos_resultados$setor = coluna_setores
todos_resultados$variavel_entrada = variavel_entrada

# write.csv(todos_resultados,file="todos_resultados.csv",row.names=F)
# write.csv(todos_resultados,file="todos_resultados_sem_lag_0.csv",row.names=F)
dados = todos_resultados
coeficiente.de.confianca = .95
intervalos<-function(dados){
  ic <- epi.conf(dados,conf = coeficiente.de.confianca)
  return(c(ic$lower,ic$est,ic$upper))
}


i=1
coluna_dos_setores = as.character(rep(todos_setores,each = 3))
# coluna_dos_setores = as.character(rep(todos_setores,each = 2))
ic_todos_setores = data.frame()
for(i in 1:length(todos_setores)){
  dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i]),]
  #   ic_combinacoes<- as.data.frame(as.matrix(aggregate(dados$mape_treino,list(as.character(dados$combinacao)),FUN = intervalos)))
  ic_combinacoes =as.data.frame(as.matrix(aggregate(dados$mape_teste,list(c(dados$variavel_entrada)),FUN=intervalos)))
  colnames(ic_combinacoes) = c("combinacao","minimo","MAPE","maximo")
  ic_todos_setores = rbind(ic_todos_setores,ic_combinacoes)
  #   print(aggregate(todos_resultados$mape_treino,list(c(todos_resultados$variavel_entrada)),FUN=mean))
}

ic_todos_setores$setor = coluna_dos_setores

print(ggplot(ic_todos_setores, aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))

plot1 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[2],], aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
plot2 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[3],], aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
plot3 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[4],], aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
plot4 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[5],], aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
plot5 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[6],], aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
plot6 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[7],], aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
plot7 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[8],], aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
require(gridExtra)
grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7, ncol=2,nrow=4)
# 3 4 8
i = 8
as.character(todos_setores[i])
dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i]),]
volatilidade = dados[dados$variavel_entrada == "Volatilidade",]$mape_teste
coeficiente_b = dados[dados$variavel_entrada == "Coeficiente B",]$mape_teste
volatilidade_coeficiente_b = dados[dados$variavel_entrada == "Volatilidade e Coeficiente B",]$mape_teste
# wilcox.test(volatilidade,volatilidade_coeficiente_b,paired = F)
wilcox.test(volatilidade,coeficiente_b,paired = F,alternative = "g")
# wilcox.test(coeficiente_b,volatilidade_coeficiente_b,paired = T)






ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[i],], aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)

# todos_resultados = read.csv("todos_resultados_sem_lag_0.csv")
# 
# i = 1
# as.character(todos_setores[i])
# dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i]),]
# volatilidade = dados[dados$variavel_entrada == "Volatilidade",]$POCID_teste
# coeficiente_b = dados[dados$variavel_entrada == "Coeficiente B",]$POCID_teste
# volatilidade_coeficiente_b = dados[dados$variavel_entrada == "Volatilidade e Coeficiente B",]$POCID_teste
# # wilcox.test(volatilidade,coeficiente_b,paired = T)
# wilcox.test(volatilidade,volatilidade_coeficiente_b,paired = T)
# # wilcox.test(coeficiente_b,volatilidade_coeficiente_b,paired = T)
# ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[i],], aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# 
# names(resultados)
# 
# plota_grid("mape_teste","MAPE")
# plota_grid("POCID_teste","POCID")
# plota_grid("mse_teste","MSE")
# plota_grid("arv_teste","ARV")
# plota_grid("nrmse_teste","nrmse")
# 
# coluna ="POCID_teste"
# nome_coluna="POCID"
# plota_grid = function(coluna,nome_coluna){
#   i=1
#   coluna_dos_setores = as.character(rep(todos_setores,each = 3))
#   ic_todos_setores = data.frame()
#   for(i in 1:length(todos_setores)){
#     dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i]),]
#     #   ic_combinacoes<- as.data.frame(as.matrix(aggregate(dados$mape_treino,list(as.character(dados$combinacao)),FUN = intervalos)))
#     ic_combinacoes =as.data.frame(as.matrix(aggregate(dados[,coluna],list(c(dados$variavel_entrada)),FUN=intervalos)))
#     colnames(ic_combinacoes) = c("combinacao","minimo",nome_coluna,"maximo")
#     ic_todos_setores = rbind(ic_todos_setores,ic_combinacoes)
#     #   print(aggregate(todos_resultados$mape_treino,list(c(todos_resultados$variavel_entrada)),FUN=mean))
#   }
#   
#   ic_todos_setores$setor = coluna_dos_setores
#   
#   #   print(ggplot(ic_todos_setores, aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
#   
#   plot1 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[2],], aes(x=combinacao,y=nome_coluna, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
#   plot2 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[3],], aes(x=combinacao,y=nome_coluna, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
#   plot3 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[4],], aes(x=combinacao,y=nome_coluna, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
#   plot4 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[5],], aes(x=combinacao,y=nome_coluna, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
#   plot5 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[6],], aes(x=combinacao,y=nome_coluna, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
#   plot6 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[7],], aes(x=combinacao,y=nome_coluna, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
#   plot7 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[8],], aes(x=combinacao,y=nome_coluna, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
#   require(gridExtra)
#   print(grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7, ncol=2,nrow=4))
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # i=1
# # coluna_dos_setores = as.character(rep(todos_setores,each = 3))
# # ic_todos_setores = data.frame()
# # for(i in 1:length(todos_setores)){
# #   dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i]),]
# #   #   ic_combinacoes<- as.data.frame(as.matrix(aggregate(dados$mape_treino,list(as.character(dados$combinacao)),FUN = intervalos)))
# #   ic_combinacoes =as.data.frame(as.matrix(aggregate(dados$nrmse_teste,list(c(dados$variavel_entrada)),FUN=intervalos)))
# #   colnames(ic_combinacoes) = c("combinacao","minimo","nmse","maximo")
# #   ic_todos_setores = rbind(ic_todos_setores,ic_combinacoes)
# #   #   print(aggregate(todos_resultados$mape_treino,list(c(todos_resultados$variavel_entrada)),FUN=mean))
# # }
# # 
# # ic_todos_setores$setor = coluna_dos_setores
# # 
# # #   print(ggplot(ic_todos_setores, aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
# # 
# # plot1 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[2],], aes(x=combinacao,y=nrmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# # plot2 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[3],], aes(x=combinacao,y=nrmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# # plot3 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[4],], aes(x=combinacao,y=nrmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# # plot4 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[5],], aes(x=combinacao,y=nrmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# # plot5 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[6],], aes(x=combinacao,y=nrmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# # plot6 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[7],], aes(x=combinacao,y=nrmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# # plot7 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[8],], aes(x=combinacao,y=nrmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# # require(gridExtra)
# # print(grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7, ncol=2,nrow=4))
# 
# 
# 
# 
# i=1
# coluna_dos_setores = as.character(rep(todos_setores,each = 3))
# ic_todos_setores = data.frame()
# for(i in 1:length(todos_setores)){
#   dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i]),]
#   #   ic_combinacoes<- as.data.frame(as.matrix(aggregate(dados$mape_treino,list(as.character(dados$combinacao)),FUN = intervalos)))
#   ic_combinacoes =as.data.frame(as.matrix(aggregate(dados$POCID_teste,list(c(dados$variavel_entrada)),FUN=intervalos)))
#   colnames(ic_combinacoes) = c("combinacao","minimo","POCID","maximo")
#   ic_todos_setores = rbind(ic_todos_setores,ic_combinacoes)
#   #   print(aggregate(todos_resultados$mape_treino,list(c(todos_resultados$variavel_entrada)),FUN=mean))
# }
# 
# ic_todos_setores$setor = coluna_dos_setores
# 
# #   print(ggplot(ic_todos_setores, aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
# 
# plot1 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[2],], aes(x=combinacao,y=POCID, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot2 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[3],], aes(x=combinacao,y=POCID, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot3 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[4],], aes(x=combinacao,y=POCID, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot4 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[5],], aes(x=combinacao,y=POCID, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot5 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[6],], aes(x=combinacao,y=POCID, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot6 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[7],], aes(x=combinacao,y=POCID, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot7 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[8],], aes(x=combinacao,y=POCID, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# require(gridExtra)
# print(grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7, ncol=2,nrow=4))

# 
# 
# 
# 
# 
# 
# 
# 
# 
# i=1
# coluna_dos_setores = as.character(rep(todos_setores,each = 3))
# ic_todos_setores = data.frame()
# for(i in 1:length(todos_setores)){
#   dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i]),]
#   #   ic_combinacoes<- as.data.frame(as.matrix(aggregate(dados$mape_treino,list(as.character(dados$combinacao)),FUN = intervalos)))
#   ic_combinacoes =as.data.frame(as.matrix(aggregate(dados$nmse_teste,list(c(dados$variavel_entrada)),FUN=intervalos)))
#   colnames(ic_combinacoes) = c("combinacao","minimo","nmse","maximo")
#   ic_todos_setores = rbind(ic_todos_setores,ic_combinacoes)
#   #   print(aggregate(todos_resultados$mape_treino,list(c(todos_resultados$variavel_entrada)),FUN=mean))
# }
# 
# ic_todos_setores$setor = coluna_dos_setores
# 
# #   print(ggplot(ic_todos_setores, aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
# 
# plot1 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[2],], aes(x=combinacao,y=nmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot2 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[3],], aes(x=combinacao,y=nmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot3 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[4],], aes(x=combinacao,y=nmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot4 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[5],], aes(x=combinacao,y=nmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot5 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[6],], aes(x=combinacao,y=nmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot6 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[7],], aes(x=combinacao,y=nmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot7 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[8],], aes(x=combinacao,y=nmse, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# require(gridExtra)
# print(grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7, ncol=2,nrow=4))
# 
# 
# 
# 
# i=1
# coluna_dos_setores = as.character(rep(todos_setores,each = 3))
# ic_todos_setores = data.frame()
# for(i in 1:length(todos_setores)){
#   dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i]),]
#   #   ic_combinacoes<- as.data.frame(as.matrix(aggregate(dados$mape_treino,list(as.character(dados$combinacao)),FUN = intervalos)))
#   ic_combinacoes =as.data.frame(as.matrix(aggregate(dados$arv_teste,list(c(dados$variavel_entrada)),FUN=intervalos)))
#   colnames(ic_combinacoes) = c("combinacao","minimo","ARV","maximo")
#   ic_todos_setores = rbind(ic_todos_setores,ic_combinacoes)
#   #   print(aggregate(todos_resultados$mape_treino,list(c(todos_resultados$variavel_entrada)),FUN=mean))
# }
# 
# ic_todos_setores$setor = coluna_dos_setores
# 
# #   print(ggplot(ic_todos_setores, aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
# 
# plot1 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[2],], aes(x=combinacao,y=ARV, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot2 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[3],], aes(x=combinacao,y=ARV, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot3 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[4],], aes(x=combinacao,y=ARV, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot4 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[5],], aes(x=combinacao,y=ARV, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot5 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[6],], aes(x=combinacao,y=ARV, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot6 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[7],], aes(x=combinacao,y=ARV, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot7 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[8],], aes(x=combinacao,y=ARV, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# require(gridExtra)
# print(grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7, ncol=2,nrow=4))
# 
# 
# 
# 
# 
# 
# 
# 
# i=1
# coluna_dos_setores = as.character(rep(todos_setores,each = 3))
# ic_todos_setores = data.frame()
# for(i in 1:length(todos_setores)){
#   dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i]),]
#   #   ic_combinacoes<- as.data.frame(as.matrix(aggregate(dados$mape_treino,list(as.character(dados$combinacao)),FUN = intervalos)))
#   ic_combinacoes =as.data.frame(as.matrix(aggregate(dados$mse_teste,list(c(dados$variavel_entrada)),FUN=intervalos)))
#   colnames(ic_combinacoes) = c("combinacao","minimo","MSE","maximo")
#   ic_todos_setores = rbind(ic_todos_setores,ic_combinacoes)
#   #   print(aggregate(todos_resultados$mape_treino,list(c(todos_resultados$variavel_entrada)),FUN=mean))
# }
# 
# ic_todos_setores$setor = coluna_dos_setores
# 
# #   print(ggplot(ic_todos_setores, aes(x=combinacao,y=MAPE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
# 
# plot1 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[2],], aes(x=combinacao,y=MSE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot2 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[3],], aes(x=combinacao,y=MSE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot3 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[4],], aes(x=combinacao,y=MSE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot4 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[5],], aes(x=combinacao,y=MSE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot5 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[6],], aes(x=combinacao,y=MSE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot6 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[7],], aes(x=combinacao,y=MSE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# plot7 <- ggplot(ic_todos_setores[ic_todos_setores$setor == todos_setores[8],], aes(x=combinacao,y=MSE, fill= setor)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2)
# require(gridExtra)
# print(grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7, ncol=2,nrow=4))
# 
# 
# setores = unique(todos_resultados$setor)
# entrada = unique( todos_resultados$variavel_entrada)
# i=1
# for(i in 1:length(setores)){
#   auxiliar_setor = todos_resultados[todos_resultados$setor == setores[i],]
#   print(setores[i])
#   for(j in 1:length(entrada)){
#     b = auxiliar_setor[auxiliar_setor$variavel_entrada == entrada[j],]
#     
#     
#     print(entrada[j])
#     print(min(b$POCID_teste))
#   }
# }
