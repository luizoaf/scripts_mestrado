diretorio_atual = "C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor"
setwd(diretorio_atual)
source("../../1 - Funcoes/funcoes.R")
setwd(diretorio_atual)

dados = read.csv(file="../../2 - Extrair base de dados das ações do  Yahoo Finanças/Acoes/papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")

series_temporais_setores = read.csv(file="../../2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/series_temporais_setores.csv")

series_temporais_setores = cria_serie_retornos(series_temporais_setores[,2:ncol(series_temporais_setores)])

################################ JANELAMENTO #################################

dias_mes = 20
dias_ano = 240
total_dias = nrow(series_temporais_setores)
inicio_janelamento = seq(from=1,to=total_dias,by=dias_mes)
fim_janelamento = seq(from=1,to=total_dias,by=dias_mes)+dias_ano

janelamentos_indices = data.frame(inicio = inicio_janelamento,fim = fim_janelamento)
janelamentos_indices = janelamentos_indices[janelamentos_indices$fim<=total_dias,]
# write.table(x = janelamentos_indices,file="janelamentos_indices.csv",row.names=F)

##############################################################################

metricas = data.frame()
indice_setor=1
nome_dos_setores = c()
indice_dos_nomes_dos_setores= 1
grupo_do_janelamento = 1

for(linha_janelamento in 1:nrow(janelamentos_indices)){
  
  indices_janelamento = janelamentos_indices$inicio[linha_janelamento]:janelamentos_indices$fim[linha_janelamento]
  
  # determina as series temporais para cada janelamento  
  series_temporais = series_temporais_setores[indices_janelamento,]
  for(indice_setor in 1:ncol(series_temporais)){
    nome_do_setor =  names(series_temporais)[indice_setor]
    nome_dos_setores[indice_dos_nomes_dos_setores] = nome_do_setor
    serie = series_temporais[,indice_setor]
    
    source("../../4 - Calcula metricas/previsao_exponencial.R")
    setwd(diretorio_atual)
    metricas = rbind(metricas, cbind(sse,a,coeficiente_B,volatilidade,grupo_do_janelamento))
    colnames(metricas) = c("sse","a","coeficiente_B","volatilidade","grupo_do_janelamento")
    
    indice_dos_nomes_dos_setores = indice_dos_nomes_dos_setores + 1
    # write.table(metricas,paste(names(series_temporais)[i],grupo_janelamento,"combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv"),row.names=F,sep=",")
    
    formato_ponto = 21
    ############################# IMAGENS DAS EXPONENCIAIS ############################
    ######### SEM TRANSFORMAÇÃO #######
    png(paste("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor/imagens/exponenciais/",sse,".png"),bg= "transparent")
    plot(main=paste(nome_do_setor ,"\nCoeficiente B: ",coeficiente_B,"\nVolatilidade",volatilidade, "\nSSE:",sse),
         eixo_x_frequencias,(alvo),pch=formato_ponto,las=1)
    lines(eixo_x_frequencias,(previsao),col="gray",lwd=2)
    dev.off()
    ######### SEMI LOG #######
    png(paste("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor/imagens/exponenciais semi log/",sse,".png"),bg= "transparent")
    plot(main=paste(nome_do_setor ,"\nCoeficiente B: ",coeficiente_B,"\nVolatilidade",volatilidade, "\nSSE:",sse),
         eixo_x_frequencias,log(alvo),pch=formato_ponto,las=1)
    lines(eixo_x_frequencias,log(previsao),col="gray",lwd=2)
    dev.off()
    ###### LOG LOG ###########
    png(paste("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor/imagens/exponenciais log log/",sse,".png"),bg= "transparent")
    plot(main=paste(nome_do_setor ,"\nCoeficiente B: ",coeficiente_B,"\nVolatilidade",volatilidade, "\nSSE:",sse),
         log(eixo_x_frequencias),log(alvo),pch=formato_ponto,las=1)
    lines(log(eixo_x_frequencias),log(previsao),col="gray",lwd=2)
    dev.off()
    #####################################################################################
  }
  grupo_do_janelamento = grupo_do_janelamento + 1
}

metricas$setor = nome_dos_setores[1:nrow(metricas)]
metricas$b_volatilidade = metricas$coeficiente_B*metricas$volatilidade
write.csv(metricas,file="metricas_setores_sem_series_temporais.csv",row.names=F)
# metricas = read.csv("metricas_setores_sem_series_temporais.csv")
# 
# 
# ######################################## volatilidade * B #############################################
# formato_ponto = 21
# setEPS()
# postscript("imagens/constante.eps")
# volatilidade = (unique(metricas$volatilidade))
# B = (unique(metricas$coeficiente_B))
# # ,pch=16
# plot(volatilidade*B,ylim=c(0,2),cex=1.3,ylab="Volatility x Coefficient B",xlab="Indexes of the sectors",pch=formato_ponto,las=1)
# intercept = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[1]
# slope = regressao.simples(1:(length(volatilidade)),(B*volatilidade))[2]
# abline(a=intercept,b =slope,col=2)
# # abline(a=1,b =0,lwd=2,col="gray") #"gray"
# dev.off()
# # 
# ########################## Inversamente_proporcionais_b_volatilidade #################################
# setEPS()
# postscript("imagens/inversamente_proporcionais_b_volatilidade.eps")
# legenda = c("Coefficient B","Volatility")
# plot(unique(metricas$coeficiente_B[order(metricas$coeficiente_B,decreasing=T)]),xlab="Indexes of the sectors",ylab="Amplitudes",ylim=c(0, 3),col="black",pch=formato_ponto,las=1)
# points(unique(metricas$volatilidade[order(metricas$volatilidade,decreasing=F)]),xlab="Indice",ylab="Volatility",col="black",pch=4)
# legend("topright", inset=.04,legenda ,col =c("black","black") ,pch=c(formato_ponto,4), horiz=F)
# dev.off()
# 
# ####################################################################################################
