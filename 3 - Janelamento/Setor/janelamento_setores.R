diretorio_atual = "C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor"
setwd(diretorio_atual)
source("../../1 - Funcoes/funcoes.R")
setwd(diretorio_atual)

dados = read.csv(file="../../2 - Extrair base de dados das ações do  Yahoo Finanças/Acoes/papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")

series_temporais_setores = read.csv(file="../../2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/series_temporais_setores.csv")

series_temporais_setores = cria_serie_retornos(series_temporais_setores[,2:ncol(series_temporais_setores)])

####### JANELAMENTO ######

dias_mes = 20
dias_ano = 240
total_dias = nrow(series_temporais_setores)
inicio_janelamento = seq(from=1,to=total_dias,by=dias_mes)
fim_janelamento = seq(from=1,to=total_dias,by=dias_mes)+dias_ano

janelamentos_indices = data.frame(inicio = inicio_janelamento,fim = fim_janelamento)
janelamentos_indices = janelamentos_indices[janelamentos_indices$fim<=total_dias,]
# write.table(x = janelamentos_indices,file="janelamentos_indices.csv",row.names=F)

##########################

metricas = data.frame()
indice_setor=1
linha_janelamento = 1
nome_dos_setores = c()
indice_dos_nomes_dos_setores= 1
grupo_do_janelamento = 1

for(linha_janelamento in 1:nrow(janelamentos_indices)){
  
  indices_janelamento = janelamentos_indices$inicio[linha_janelamento]:janelamentos_indices$fim[linha_janelamento]
  
  # determina as series temporais para cada janelamento  
  series_temporais = series_temporais_setores[indices_janelamento,]
  for(indice_setor in 1:ncol(series_temporais)){
    #     indice_setor=1
    nome_do_setor =  names(series_temporais)[indice_setor]
    nome_dos_setores[indice_dos_nomes_dos_setores] = nome_do_setor
    serie = series_temporais[,indice_setor]
    
    source("../../4 - Calcula metricas/previsao_exponencial.R")
    setwd(diretorio_atual)
    metricas = rbind(metricas, cbind(sse,a,coeficiente_B,volatilidade,grupo_do_janelamento))
    colnames(metricas) = c("sse","a","coeficiente_B","volatilidade","grupo_do_janelamento")
    
    indice_dos_nomes_dos_setores = indice_dos_nomes_dos_setores + 1
    # write.table(metricas,paste(names(series_temporais)[i],grupo_janelamento,"combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv"),row.names=F,sep=",")
  }
  grupo_do_janelamento = grupo_do_janelamento + 1
}

metricas$setor = nome_dos_setores[1:nrow(metricas)]
metricas$b_volatilidade = metricas$coeficiente_B*metricas$volatilidade
write.csv(metricas,file="metricas_setores.csv",row.names=F)
teste = read.csv("metricas_setores.csv")


######################################## volatilidade * B #############################################
formato_ponto = 21
setEPS()
postscript("imagens/constante.eps")
volatilidade = (unique(metricas$volatilidade))
B = (unique(metricas$coeficiente_B))
# ,pch=16
plot(volatilidade*B,ylim=c(0,2),cex=1.3,ylab="Volatility x Coefficient B",xlab="Indexes of the sectors",pch=formato_ponto,las=1)
intercept = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[1]
slope = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[2]
abline(a=intercept,b =slope,col=2)
# abline(a=1,b =0,lwd=2,col="gray") #"gray"
dev.off()

########################## Inversamente_proporcionais_b_volatilidade #################################
setEPS()
postscript("imagens/inversamente_proporcionais_b_volatilidade.eps")
legenda = c("Coefficient B","Volatility")
plot(unique(metricas$coeficiente_B[order(metricas$coeficiente_B,decreasing=T)]),xlab="Indexes of the sectors",ylab="Amplitudes",ylim=c(0, 3),col="black",pch=formato_ponto,las=1)
points(unique(metricas$volatilidade[order(metricas$volatilidade,decreasing=F)]),xlab="Indice",ylab="Volatility",col="black",pch=4)
legend("topright", inset=.04,legenda ,col =c("black","black") ,pch=c(formato_ponto,4), horiz=F)
dev.off()

####################################################################################################

# 
# formato_ponto = 21
# # LOG LOG
# # "Cons N Básico / Alimentos Processados" # colocar em inglÊs
# setEPS()
# postscript("imagens/melhor_exponencial_log_log.eps")
# # plot(rnorm(100), main="Hey Some Data")
# pior_1 = subset(eixo_x_y,eixo_x_y$i==571 )
# atual = pior_1[,"alvo"]
# previsao = pior_1[,"previsao"]
# # main = paste("Outlier\nSSE: ",unique(pior_1$sse),"\nB: ",unique(pior_1$coeficiente_B)," volat.: ",unique(pior_1$volatilidade))
# # "Consumo não Cíclico/Alimentos Processados"
# plot( log(pior_1[,c(1,2)]),xlab="Returns",ylab="Sector Consumption Not Cyclical / Processed Food",pch=formato_ponto,las=1)
# # ,pch=20
# lines(log(pior_1[,c(1,3)]),col="gray",lwd=2)
# dev.off()
# 
# ###################################################################
# #SEMILOG
# # LOG no Y, normal no X
# #"Cons N Cíclico / Pr Pessoal Limp"  # colocar em inglÊs
# setEPS()
# postscript("imagens/segunda_melhor_exponencial_semi_log.eps")
# # unique(eixo_x_y$setor)
# pior_1 = subset(eixo_x_y,eixo_x_y$i==29 )
# atual = pior_1[,"alvo"]
# previsao = pior_1[,"previsao"]
# # main = paste("Outlier\nSSE: ",unique(pior_1$sse),"\nB: ",unique(pior_1$coeficiente_B)," volat.: ",unique(pior_1$volatilidade))
# # Consumo não Cíclico/Produtos de uso Pessoal e Limpeza
# plot( pior_1[,"eixo_x_frequencias"],log(pior_1[,"alvo"]),xlab="Returns",ylab="Sector Consumption Not Cyclical /  Products of use Personal and Cleaning" ,pch=formato_ponto,las=1)
# lines(pior_1[,"eixo_x_frequencias"],log(pior_1[,"previsao"]),col="gray",lwd=2)
# # mtext("aaaaaaaaaaaaaaadownvar",side=1,line=2,col=1)
# dev.off()
# 
# 


# 
# # 
# # dados = metricas
# # k =3
# retorna_cluster = function(dados,k){
#   #   dados = metricas
#   #   k = 3
#   #   dados = subset(metricas,metricas$tempo==semestre)
#   iter = 45
#   agrupamento = dados[,c("coeficiente_B","volatilidade")]
#   #     dados[,1] = log(dados[,1])
#   #     dados[,2] = log(dados[,2])
#   
#   km = kmeans (x = agrupamento, centers = k, iter.max = iter)
#   agrupamento$cluster = km$cluster
#   grupos = unique(km$cluster)
#   #   print(grupos)
#   dados$cluster = agrupamento$cluster
#   dados$risco_b = ""
#   dados$risco_b[dados$cluster == grupos[1]] = "moderado"
#   dados$risco_b[dados$cluster == grupos[2]] = "conservador"
#   dados$risco_b[dados$cluster == grupos[3]] = "arrojado"
#   
#   dados$cor = ""
#   #   dados$cor[dados$cluster == grupos[1]] = "black"
#   #   dados$cor[dados$cluster == grupos[2]] = "green"
#   #   dados$cor[dados$cluster == grupos[3]] = "red"
#   dados$cor[dados$cluster == grupos[1]] = "black"
#   dados$cor[dados$cluster == grupos[2]] = "green"
#   dados$cor[dados$cluster == grupos[3]] = "red"
#   
#   
#   cor = c()
#   #   dados$cor[dados$cluster == grupos[1]] = "black"
#   #   dados$cor[dados$cluster == grupos[2]] = "green"
#   #   dados$cor[dados$cluster == grupos[3]] = "red"
#   cor[dados$cluster == grupos[1]] = "black"
#   cor[dados$cluster == grupos[2]] = "green"
#   cor[dados$cluster == grupos[3]] = "red"
#   head(dados)
#   #   cluster_ordem = unique( km$cluster)
#   legenda = c("conservador","moderado","arrojado")
#   plot(main= paste("Para K = ",k,sep =""),agrupamento$coeficiente_B~agrupamento$volatilidade,xlab="Volatility",ylab="Coefficient B", col = cor,pch = 20, cex = 0.9)
#   #   points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
#   legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)
#   # points(km$centers,col=1:k, pch = 8,lwd=3)
#   
#   #   agrupamento = agrupamento[order(agrupamento$cluster),]
#   
#   # dados
#   return(dados)
# }
# # # eixo_x_y_sem_outlier = metricas[metricas$coeficiente_B<3,]
# agrupamento_dados = retorna_cluster(metricas,3)
# # # outlier = metricas[metricas$coeficiente_B>3,]
# # # outlier$cor = "green"
# # # outlier$cluster = unique(agrupamento_dados$cluster[agrupamento_dados$risco_b=="conservador"])
# # # outlier$risco_b = "conservador"
# # # agrupamento_dados = rbind( agrupamento_dados,outlier)
# # # head(agrupamento_dados)
# # legenda = c("conservador","moderado","arrojado")
# # plot(main= paste("Para K = ",3,sep =""),agrupamento_dados$coeficiente_B~agrupamento_dados$volatilidade,xlab="Volatility",ylab="Coefficient B", col = agrupamento_dados$cor,pch = 20, cex = 0.9)
# # #   points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
# # legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)
# # 
# # # semestre = "2014"
# # # retorna_cluster ("2014")
# # # cluster_2014_1 = cbind(data.frame(cluster = retorna_cluster(semestre)),metricas[metricas$tempo==semestre,c("colunas","coeficiente_B","volatilidade","tempo" )])
# # 
# # # write.table(cluster_2014_1,"cluster_2014_1.csv",row.names=F,sep=",")
# 
# 
# write.table(metricas,"calculo_b_volatilidade_mais_pontos.csv",row.names=F,sep=",")
