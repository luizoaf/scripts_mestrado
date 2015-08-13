# setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/1 - Funcoes")
setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/1 - Funcoes")

if(require(minpack.lm) == FALSE){
  install.packages("minpack.lm")
}

trim <- function (x) sub("\\s+$", "", x)

# calcular o VaR - Value At Risk
if(require(PerformanceAnalytics) == FALSE){
  install.packages("PerformanceAnalytics")
}

require(PerformanceAnalytics)
require(minpack.lm) 
require(hydroGOF)
normalizacao_transformacao_linear = function(lim_min_norm,lim_max_norm,serie_temporal){
  dados_normalizados = (((lim_max_norm - lim_min_norm)*(serie_temporal - min(serie_temporal)))/(max(serie_temporal) - min(serie_temporal))) +lim_min_norm
  return(dados_normalizados)
}

inverte_indices_data_frame = function(data_frame){
  return(as.data.frame(data_frame[rev(1:nrow(data_frame)),]))
}

funcao_modulos_das_diferencas = function(dados,coluna){
  modulos_das_diferencas = c()
  for(i in 1:(nrow(dados)-1)){
    modulos_das_diferencas[i] = abs(log(dados[i,coluna]) - log(dados[i+1,coluna]))
  }
  dp = sd(modulos_das_diferencas)
  serie_retornos_normalizado = data.frame(modulos_das_diferencas/dp)
  serie_retornos_normalizado
}

normalizacao_log_da_serie_temporal = function(dados_periodo){
  dados_periodo = na.omit(dados_periodo)
  #   dados_periodo = dados_periodo[,-1] #eliminação da data
  
  #Datas precisam estar da mais recente para a mais antiga
  #   dados_periodo = inverte_indices_data_frame(dados_periodo)
  
  serie_retorno_todas_colunas = data.frame(funcao_modulos_das_diferencas(dados_periodo,names(dados_periodo)[1]))
  for(i in 2:ncol(dados_periodo)){
    serie_retorno_todas_colunas = cbind(serie_retorno_todas_colunas,funcao_modulos_das_diferencas(dados_periodo,names(dados_periodo)[i]))
  }
  colnames(serie_retorno_todas_colunas) = names(dados_periodo)
  return(serie_retorno_todas_colunas)
}

cria_serie_retornos = function(dados_periodo){
  dados_periodo = na.omit(dados_periodo)
  #   dados_periodo = dados_periodo[,-1] #eliminação da data
  
  #Datas precisam estar da mais recente para a mais antiga
  #   dados_periodo = inverte_indices_data_frame(dados_periodo)
  
  serie_retorno_todas_colunas = data.frame(funcao_modulos_das_diferencas(dados_periodo,names(dados_periodo)[1]))
  for(i in 2:ncol(dados_periodo)){
    serie_retorno_todas_colunas = cbind(serie_retorno_todas_colunas,funcao_modulos_das_diferencas(dados_periodo,names(dados_periodo)[i]))
  }
  colnames(serie_retorno_todas_colunas) = names(dados_periodo)
  return(serie_retorno_todas_colunas)
}

regressao.simples = function(x,y ){
  
  x.media = mean(x)
  y.media  = mean(y)
  a=0
  b =0
  for(i in 1:length(x)){
    a = a + (x[i] - x.media)*(y[i]-y.media)
    b = b + (x[i] -x.media)^2
  }
  theta1 = a/b
  theta0 = y.media - theta1 *x.media
  return (c(theta0,theta1))
}

#Correcao na formatacao dos dados
correcao_formatacao_dados = function(dados,coluna,limiar,divisao){
  coluna = dados[,coluna]
  for(i in 1:length(coluna)){
    if(coluna[i] > limiar){
      if(coluna[i] > 1000000){
        coluna[i] = NA
      }else{
        coluna[i] = coluna[i]/divisao
      }
    }
  }
  return(coluna)
}

data.loading <- function(tickers, start.date, end.date) {
  # Change the locale
  sl <- Sys.setlocale(locale="US")
  
  # Create the universe of dates
  all.dates <- seq(as.Date(start.date), as.Date(end.date), by="day")
  all.dates <- subset(all.dates,weekdays(all.dates) != "Sunday" & weekdays(all.dates) != "Saturday")
  all.dates.char <- as.matrix(as.character(all.dates))
  
  # Create sparse matrices
  open <- matrix(NA, NROW(all.dates.char), length(tickers))
  hi <- open
  low <- open
  close <- open
  volume <- open
  adj.close <- open
  
  # Name the rows correctly
  rownames(open) <- all.dates.char
  rownames(hi) <- all.dates.char
  rownames(low) <- all.dates.char
  rownames(close) <- all.dates.char
  rownames(volume) <- all.dates.char
  rownames(adj.close) <- all.dates.char
  
  # Split the start and end dates to be used in the ULR later on
  splt <- unlist(strsplit(start.date, "-"))
  a <- as.character(as.numeric(splt[2])-1)
  b <- splt[3]
  c <- splt[1]
  
  splt <- unlist(strsplit(end.date, "-"))
  d <- as.character(as.numeric(splt[2])-1)
  e <- splt[3]
  f <- splt[1]
  
  # Create the two out of the three basic components for the URL loading
  str1 <- "http://ichart.finance.yahoo.com/table.csv?s="
  str3 <- paste("&a=", a, "&b=", b, "&c=", c, "&d=", d, "&e=", e, "&f=", f, "&g=d&ignore=.csv", sep="")
  
  # Main loop for all assets
  for (i in seq(1,length(tickers),1))
  {
    str2 <- tickers[i]
    strx <- paste(str1,str2,str3,sep="")
    x <- read.csv(strx)
    
    datess <- as.matrix(x[1])
    
    replacing <- match(datess, all.dates.char)
    open[replacing,i] <- as.matrix(x[2])
    hi[replacing,i] <- as.matrix(x[3])
    low[replacing,i] <- as.matrix(x[4])
    close[replacing,i] <- as.matrix(x[5])
    volume[replacing,i] <- as.matrix(x[6])
    adj.close[replacing,i] <- as.matrix(x[7])
  }
  
  # Name the cols correctly
  colnames(open) <- tickers
  colnames(hi) <- tickers
  colnames(low) <- tickers
  colnames(close) <- tickers
  colnames(volume) <- tickers
  colnames(adj.close) <- tickers
  
  # Return the ouput
  #return(list(open=open, high=hi, low=low, close=close, volume=volume, adj.close=adj.close))
  
  #está retornando apenas as datas e o adj.close, que são o que me interessam, mas tem essas colunas comentadas acima.
  dados = data.frame( adj.close=adj.close)
  datas = row.names(dados)
  dados$datas = datas
  return(dados)
}


tratamento_nao_tiver_dados <- function(acao) {
  out <- tryCatch(
{
  nrow(data.frame(data.loading(acoes[acao],data_inicio,data_fim)))
},
error=function(cond) {
  return(NA)
},
warning=function(cond) {
  return(NULL)
},
finally={
}
  )    
return(out)
}

# criaÃ§Ã£o fdp seguindo a estratÃ©gia utilizada por Paulo
funcao_distribuicao_probabilidade = function(serie_retorno){
  #   valor_incremento = 0.01
  #   vetor_incremento = seq(from=incremento,to=max(serie_retorno),by=incremento)
  #   0.01 0.10 0.19 0.28 0.37 0.46 0.55 0.64 0.73 0.82 0.91 1.00 1.09
  #   seq(from=0.01,to=max(serie),by=0.09)
  #   vetor_incremento = seq(from=valor_incremento,to=max(serie),by=valor_incremento_fdp)
  #   incremento = 0.05
  incremento = 0.05
  vetor_incremento = seq(from=0.01,to=max(serie_retorno),by=incremento)
  cont= 0
  indice = 1
  vetor_x_valor_serie_retorno = c()
  vetor_y_frequencia = c()
  
  for(q in vetor_incremento){
    for(i in 1:length(serie_retorno)){
      if(serie_retorno[i]>=q){
        cont = cont +1
      }
    }
    vetor_x_valor_serie_retorno[indice] = q
    vetor_y_frequencia[indice] = cont
    indice = indice+1
    cont=0
  }
  
  minimo_frequencia = min(vetor_y_frequencia)
  maximo_frequencia = max(vetor_y_frequencia)
  vetoryNorm = c()
  for(i in 1:length(vetor_y_frequencia)){
    vetoryNorm[i] = (1-0.01)*((((vetor_y_frequencia[i]-minimo_frequencia))/(maximo_frequencia-minimo_frequencia))+0.01);
  }
  serie_retorno_densidade = data.frame(valor_serie_retorno_eixo_x = vetor_x_valor_serie_retorno,frequencia_eixo_y = vetoryNorm)
  serie_retorno_densidade = na.omit(serie_retorno_densidade)
  #remocao da cauda, como valores acima de 2.5 sera considerado cauda
  serie_retorno_densidade = serie_retorno_densidade[serie_retorno_densidade$valor_serie_retorno_eixo_x<2.5,]
  return(serie_retorno_densidade)
}

calcula_volatilidade = function(serie_retorno){
  return(mean(serie_retorno))
}

resultado_funcao_exponencial = function(a,x,coeficiente_B){
  #   a*exp(b*x)
  #   a*exp(coefB*x)
  return(a*exp(x*-coeficiente_B))
  #   return(exp(x*-coeficiente_B))
  
}

intervalos_confianca<-function(amostra){
  if(length(amostra) >= 30){
    ic <- epi.conf(amostra,conf = coeficiente.de.confianca)
  }
  else{
    ic.student <- t.test(amostra, conf = coeficiente.de.confianca)
    return(ic.student$conf.int)
  }
  return(c(ic$lower,ic$upper))
}

janelamento = function(dados,tamanho_janela){
  grupo = 1
  grupos = c()
  for(i in 1:nrow(dados)){
    if(i%%tamanho_janela==0){
      grupos[i] = grupo
      grupo = grupo + 1
    }else{
      grupos[i] = grupo
    }
  }
  
  dados$grupo = grupos
  #grupo é o último grupo
  qnt_elementos_ultimo_grupo = nrow(dados[dados$grupo==grupo,])
  if(qnt_elementos_ultimo_grupo != tamanho_janela){
    #remove o último grupo
    dados = dados[dados$grupo!=grupo,]
  }
  return(dados)
}

# Abaixo de cada setor tem uma string vazia, a função popula a string vazia pelo nome do setor
correcao_coluna_setores = function(df_setores){
  setores_atual = ""
  setores = as.character(df_setores$Setor)
  correcao_setores = c()
  j_correcao_setores = 1
  for(i in 2:length(setores)){
    setor = setores[i]
    if(setor ==""){
      correcao_setores[j_correcao_setores] =setores_atual 
      j_correcao_setores = j_correcao_setores+1
    }else{
      setores_atual = setor
      correcao_setores[j_correcao_setores] =setores_atual 
      j_correcao_setores = j_correcao_setores+1
      
    }
  }
  return(correcao_setores)
}

plot_previsao_com_B_e_exponencial = function(nome_do_setor,eixo_x_frequencias,exponencial){
  #   png(paste("Coeficiente B ",coeficiente_B, " SSE ",sse," Incremento ",incremento,".png",sep=""))
  plot(main=paste(nome_do_setor ,"\nCoeficiente B: ",coeficiente_B,"Volatilidade",volatilidade, "\nSSE:",sse," a: ",a),funcao_distribuicao_probabilidade(serie),ylab="Densidade",xlab="Série de retornos",pch=20)
  lines(exponencial~eixo_x_frequencias,col="blue")
  legend("topright", inset=.05, c("F.D.P.","Exponencial"), lwd= 3,col = c("black","blue"), horiz=TRUE)
  #   dev.off()
}
plot_janelamento = function(dados,grupo_para_plotagem){
  coeficiente_B = grupo_coeficiente_B$coeficiente_B[grupo_para_plotagem]
  sse =  grupo_coeficiente_B$sse[grupo_para_plotagem]
  volatilidade =  grupo_coeficiente_B$volatilidade[grupo_para_plotagem]
  serie = na.omit(subset(serie_retornos_normalizado,dados$grupo == grupo_para_plotagem))[,1]
  eixo_x =  funcao_distribuicao_probabilidade(na.omit(subset(serie_retornos_normalizado,dados$grupo == grupo_para_plotagem))$Adj.Close)[,1]
  exponencial = resultado_funcao_exponencial(eixo_x)
  
  plot(main=paste("Janela",grupo_para_plotagem," ",janelamento,"dias.\nCoeficiente B: ",coeficiente_B," SSE:",sse,"\n 1/Volatilidade: ",1/volatilidade),funcao_distribuicao_probabilidade(serie),type="b",ylab="Densidade",xlab="SÃ©rie de retornos")
  lines(exponencial~eixo_x,col="blue")
  legend("topright", inset=.05, c("F.D.P.","Exponencial"), lwd= 3,col = c("black","blue"), horiz=FALSE)
}


plot_comportamento_volatilidade_B = function(base_de_dados){
  plot(base_de_dados$Volatilidade[order(base_de_dados$Volatilidade,decreasing=T)],ylab="Volatilidade",xlab="Coeficiente B",col="red",ylim=c(0,2))
  points(base_de_dados$Coeficiente_B[order(base_de_dados$Coeficiente_B,decreasing=F)],col="blue")
}

# coeficiente_B = grupo_coeficiente_B$coeficiente_B[grupo_para_plotagem]
# sse =  grupo_coeficiente_B$sse[grupo_para_plotagem]
# volatilidade =  grupo_coeficiente_B$volatilidade[grupo_para_plotagem]
# serie = na.omit(subset(serie_retornos_normalizado,dados$grupo == grupo_para_plotagem))[,1]
# eixo_x =  funcao_distribuicao_probabilidade(na.omit(subset(serie_retornos_normalizado,dados$grupo == grupo_para_plotagem))$Adj.Close)[,1]
# exponencial = resultado_funcao_exponencial(eixo_x)
# 
# plot(main=paste("Janela",grupo_para_plotagem," ",janelamento,"dias.\nCoeficiente B: ",coeficiente_B," SSE:",sse,"\n 1/Volatilidade: ",1/volatilidade),funcao_distribuicao_probabilidade(serie),type="b",ylab="Densidade",xlab="Serie de retornos")
# lines(exponencial~eixo_x,col="blue")
# legend("topright", inset=.05, c("F.D.P.","Exponencial"), lwd= 3,col = c("black","blue"), horiz=FALSE)
# df_setores_2015 = read.csv(file="../2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores_2015.txt")
# df_setores = read.table(file="../2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores_2015.txt",sep="\t",head=T) #setores_2015
df_setores = read.csv(file="../2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores.csv")
df_setores_codigo_acao = data.frame(codigo = df_setores$Código[2:nrow(df_setores)],
                                    acao = df_setores$Ação[2:nrow(df_setores)],
                                    setores = correcao_coluna_setores(df_setores))

relacao_setores_acoes_sem_periodo = function(dados){
  papeis = names(dados)[2:ncol(dados)]
  codigo_acoes = substr(papeis, 0, nchar(papeis)-3) # remocao do ".SA"
  df_codigo_menos_acoes = data.frame(codigo = codigo_acoes)
  relacao_setores_acoes_menos_acoes = merge(x=df_setores_codigo_acao,y=df_codigo_menos_acoes,by="codigo",all=FALSE)
  return(relacao_setores_acoes_menos_acoes)
}

acoes_por_setores_sem_periodo = function(dados){
  relacao_setores_acoes_menos_acoes = relacao_setores_acoes_sem_periodo(dados)
  quantidade_acoes_por_setor_menos_acoes = aggregate(relacao_setores_acoes_menos_acoes$setores,list(relacao_setores_acoes_menos_acoes$setores),FUN=length)
  colnames(quantidade_acoes_por_setor_menos_acoes) = c("Setor","Quantidade_de_Acoes_pesquisadas")
  quantidade_acoes_por_setor_menos_acoes = quantidade_acoes_por_setor_menos_acoes[order(quantidade_acoes_por_setor_menos_acoes$Quantidade_de_Acoes,decreasing=T),]
  
  # Todas acoes
  codigos =as.character(df_setores$Código[2:length(df_setores$Código)])
  df_codigo = data.frame(codigo = codigos)
  relacao_setores_acoes = merge(x=df_setores_codigo_acao,y=df_codigo,by="codigo",all=FALSE)
  quantidade_acoes_por_setor = aggregate(relacao_setores_acoes$setores,list(relacao_setores_acoes$setores),FUN=length)
  colnames(quantidade_acoes_por_setor) = c("Setor","Quantidade_de_Acoes_todas_acoes")
  quantidade_acoes_por_setor = quantidade_acoes_por_setor[order(quantidade_acoes_por_setor$Quantidade_de_Acoes,decreasing=T),]
  
  # merge dos setores do periodo com o de todos os setores com todas as acoes possiveis
  setores = merge(quantidade_acoes_por_setor,quantidade_acoes_por_setor_menos_acoes,by="Setor",all=TRUE)
  setores[is.na(setores)] = 0
  setores$porcentagem = 100*(setores$Quantidade_de_Acoes_pesquisadas/setores$Quantidade_de_Acoes_todas_acoes)
  setores = setores[order(setores$porcentagem,decreasing=T),]
  return(setores)
}


setores_100_porcento_sem_periodo = function(dados){
  setores_100_porcento = acoes_por_setores_sem_periodo(dados) 
  setores_100_porcento = setores_100_porcento$Setor[setores_100_porcento$porcentagem ==100]
  setores_100_porcento = as.character(setores_100_porcento)
  return(setores_100_porcento)
}
calcula_series_temporais_dos_setores = function(dados){
  #   semestre_acoes = subset(dados,dados$datas==semestre)
  serie_retornos_por_semestre = dados[,2:ncol(dados)]
  #   serie_retornos_por_semestre = cria_tabela_serie_retornos_de_todas_as_acoes(semestre_acoes)
  setores_media_acoes = data.frame(1)
  setores_100_porcento = setores_100_porcento_sem_periodo(dados)
  #       setor = setores_100_porcento[6]
  relacao_setores_acoes_menos_acoes = relacao_setores_acoes_sem_periodo(dados)
  for( setor in setores_100_porcento){
    acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
    if(length(acoes_do_setor)!=1){ # nao ira calcular a media quando tiver apenas 1 acao
      medias_por_setor = apply(serie_retornos_por_semestre[,acoes_do_setor],MARGIN=1,FUN=mean)
    }else{
      medias_por_setor = serie_retornos_por_semestre[,acoes_do_setor]
    }
    setores_media_acoes = cbind(setores_media_acoes,medias_por_setor)
  }
  setores_media_acoes = setores_media_acoes[,-1]
  #     head(setores_media_acoes)
  ### mudanca de ordem ###
  #   setores_media_acoes = cria_tabela_serie_retornos_de_todas_as_acoes(setores_media_acoes)
  
  colnames(setores_media_acoes) = setores_100_porcento_sem_periodo(dados)
  return(setores_media_acoes)
}


dado_semestre_retorna_media_serie_retornos_por_setor_sem_periodo = function(dados){
  #   semestre_acoes = subset(dados,dados$datas==semestre)
  serie_retornos_por_semestre = dados[,2:ncol(dados)]
  #   serie_retornos_por_semestre = cria_tabela_serie_retornos_de_todas_as_acoes(semestre_acoes)
  setores_media_acoes = data.frame(1)
  setores_100_porcento = setores_100_porcento_sem_periodo(dados)
  #     setor = setores_100_porcento[1]
  relacao_setores_acoes_menos_acoes = relacao_setores_acoes_sem_periodo(dados)
  for( setor in setores_100_porcento){
    acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
    if(length(acoes_do_setor)!=1){ # nao ira calcular a media quando tiver apenas 1 acao
      medias_por_setor = apply(serie_retornos_por_semestre[,acoes_do_setor],MARGIN=1,FUN=mean)
    }else{
      medias_por_setor = serie_retornos_por_semestre[,acoes_do_setor]
    }
    setores_media_acoes = cbind(setores_media_acoes,medias_por_setor)
  }
  #   setores_media_acoes = setores_media_acoes[,-1]
  #   head(setores_media_acoes)
  ### mudanca de ordem ###
  setores_media_acoes = cria_tabela_serie_retornos_de_todas_as_acoes(setores_media_acoes)
  
  colnames(setores_media_acoes) = setores_100_porcento_sem_periodo(dados)
  return(setores_media_acoes)
}



########################### setores por periodo ###################################
relacao_setores_acoes = function(dados,periodo){
  periodo_acoes = dados[dados$datas==periodo,]
  papeis = names(periodo_acoes)[2:ncol(periodo_acoes)]
  codigo_acoes = substr(papeis, 0, nchar(papeis)-3) # remocao do ".SA"
  df_codigo_menos_acoes = data.frame(codigo = codigo_acoes)
  relacao_setores_acoes_menos_acoes = merge(x=df_setores_codigo_acao,y=df_codigo_menos_acoes,by="codigo",all=FALSE)
  return(relacao_setores_acoes_menos_acoes)
}
acoes_por_setores_por_periodo = function(dados,periodo){
  relacao_setores_acoes_menos_acoes = relacao_setores_acoes(dados,periodo)
  quantidade_acoes_por_setor_menos_acoes = aggregate(relacao_setores_acoes_menos_acoes$setores,list(relacao_setores_acoes_menos_acoes$setores),FUN=length)
  colnames(quantidade_acoes_por_setor_menos_acoes) = c("Setor","Quantidade_de_Acoes_pesquisadas")
  quantidade_acoes_por_setor_menos_acoes = quantidade_acoes_por_setor_menos_acoes[order(quantidade_acoes_por_setor_menos_acoes$Quantidade_de_Acoes,decreasing=T),]
  
  # Todas acoes
  codigos =as.character(df_setores$Código[2:length(df_setores$Código)])
  df_codigo = data.frame(codigo = codigos)
  relacao_setores_acoes = merge(x=df_setores_codigo_acao,y=df_codigo,by="codigo",all=FALSE)
  quantidade_acoes_por_setor = aggregate(relacao_setores_acoes$setores,list(relacao_setores_acoes$setores),FUN=length)
  colnames(quantidade_acoes_por_setor) = c("Setor","Quantidade_de_Acoes_todas_acoes")
  quantidade_acoes_por_setor = quantidade_acoes_por_setor[order(quantidade_acoes_por_setor$Quantidade_de_Acoes,decreasing=T),]
  
  # merge dos setores do periodo com o de todos os setores com todas as acoes possiveis
  setores = merge(quantidade_acoes_por_setor,quantidade_acoes_por_setor_menos_acoes,by="Setor",all=TRUE)
  setores[is.na(setores)] = 0
  setores$porcentagem = 100*(setores$Quantidade_de_Acoes_pesquisadas/setores$Quantidade_de_Acoes_todas_acoes)
  setores = setores[order(setores$porcentagem,decreasing=T),]
  return(setores)
}
# write.table(quantidade_acoes_por_setor_menos_acoes,"quantidade_acoes_por_setor_49_acoes.csv",sep=",",row.names=F)

setores_100_porcento_por_periodo = function(periodo,dados){
  setores_100_porcento = acoes_por_setores_por_periodo(dados,periodo) 
  setores_100_porcento = setores_100_porcento$Setor[setores_100_porcento$porcentagem ==100]
  setores_100_porcento = as.character(setores_100_porcento)
  return(setores_100_porcento)
}

dado_semestre_retorna_media_serie_retornos_por_setor = function(semestre,dados){
  semestre_acoes = subset(dados,dados$datas==semestre)
  serie_retornos_por_semestre = semestre_acoes[,2:ncol(semestre_acoes)]
  #   serie_retornos_por_semestre = cria_tabela_serie_retornos_de_todas_as_acoes(semestre_acoes)
  setores_media_acoes = data.frame(1)
  setores_100_porcento = setores_100_porcento_por_periodo(semestre,dados)
  for( setor in setores_100_porcento){
    relacao_setores_acoes_menos_acoes = relacao_setores_acoes(dados,semestre)
    acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
    if(length(acoes_do_setor)!=1){ # nao ira calcular a media quando tiver apenas 1 acao
      medias_por_setor = apply(serie_retornos_por_semestre[,acoes_do_setor],MARGIN=1,FUN=mean)
    }else{
      medias_por_setor = serie_retornos_por_semestre[,acoes_do_setor]
    }
    setores_media_acoes = cbind(setores_media_acoes,medias_por_setor)
  }
  #   setores_media_acoes = setores_media_acoes[,-1]
  #   head(setores_media_acoes)
  ### mudanca de ordem ###
  setores_media_acoes = cria_tabela_serie_retornos_de_todas_as_acoes(setores_media_acoes)
  
  colnames(setores_media_acoes) = setores_100_porcento_por_periodo(semestre,dados)
  return(setores_media_acoes)
}
setor_por_periodo = function(semestre,dados){
  semestre_acoes = subset(dados,dados$datas==semestre)
  serie_retornos_por_semestre = semestre_acoes[,2:ncol(semestre_acoes)]
  setores_media_acoes = data.frame(1)
  setores_100_porcento = setores_100_porcento_por_periodo(semestre,dados)
  for( setor in setores_100_porcento){
    relacao_setores_acoes_menos_acoes = relacao_setores_acoes(dados,semestre)
    acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
    if(length(acoes_do_setor)!=1){ # nao ira calcular a media quando tiver apenas 1 acao
      medias_por_setor = apply(serie_retornos_por_semestre[,acoes_do_setor],MARGIN=1,FUN=mean)
    }else{
      medias_por_setor = serie_retornos_por_semestre[,acoes_do_setor]
    }
    setores_media_acoes = cbind(setores_media_acoes,medias_por_setor)
  }
  setores_media_acoes = setores_media_acoes[,-1]
  
  colnames(setores_media_acoes) = setores_100_porcento_por_periodo(semestre,dados)
  return(setores_media_acoes)
}

#estou utilizando em beta.R
calcular_risco_beta = function(setor,bovespa){
  variacoes_setor = c()
  variacoes_bovespa = c()
  for(i in 1:(length(setor)-1)){
    variacoes_setor[i] = ((setor[i+1]/setor[i])-1)*100
    variacoes_bovespa[i] = ((bovespa[i+1]/bovespa[i])-1)*100
  }
  variancia = var(variacoes_bovespa)
  covariancia = cov(variacoes_bovespa,variacoes_setor)
  beta = covariancia/variancia
  beta
}

calcula_risco_beta_sem_periodo = function(indice_setor,dados_bovespa_setores){
  setor = names(dados_bovespa_setores)[indice_setor]
  acao = dados_bovespa_setores[,indice_setor]
  ibovespa = dados_bovespa_setores[,"BVSP"]
  variacoes_acao = c()
  variacoes_ibovespa = c()
  for(i in 1:(length(acao)-1)){
    variacoes_acao[i] = ((acao[i+1]/acao[i])-1)*100
    variacoes_ibovespa[i] = ((ibovespa[i+1]/ibovespa[i])-1)*100
  }
  # plot(variacoes_ibovespa)
  variancia = var(variacoes_ibovespa)
  covariancia = cov(variacoes_ibovespa,variacoes_acao)
  beta = covariancia/variancia
  #   data.frame(setor = setor,beta = beta)
  #   data.frame(beta = beta)
  beta
}



calcula_risco_beta = function(indice_setor,periodo,dados_bovespa_setores){
  setor = names(dados_bovespa_setores)[indice_setor]
  acao = subset(dados_bovespa_setores[,indice_setor],dados_bovespa_setores$datas==periodo)
  ibovespa = dados_bovespa_setores[dados_bovespa_setores$datas==periodo,"BVSP"]
  variacoes_acao = c()
  variacoes_ibovespa = c()
  for(i in 1:(length(acao)-1)){
    variacoes_acao[i] = ((acao[i+1]/acao[i])-1)*100
    variacoes_ibovespa[i] = ((ibovespa[i+1]/ibovespa[i])-1)*100
  }
  # plot(variacoes_ibovespa)
  variancia = var(variacoes_ibovespa)
  covariancia = cov(variacoes_ibovespa,variacoes_acao)
  beta = covariancia/variancia
  data.frame(tempo = periodo, setor = setor,beta = beta)
}