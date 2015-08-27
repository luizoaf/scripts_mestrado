# setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")
# source("../../1 - Funcoes/funcoes.R")
setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")
trata_mes = function(mes){
  if(mes=="jan"){
    return("01")
  }
  if(mes=="fev"){
    return("02")
  }
  if(mes=="mar"){
    return("03")
  }
  if(mes=="abr"){
    return("04")
  }
  if(mes=="mai"){
    return("05")
  }
  if(mes=="jun"){
    return("06")
  }
  if(mes=="jul"){
    return("07")
  }
  if(mes=="ago"){
    return("08")
  }
  if(mes=="set"){
    return("09")
  }
  if(mes=="out"){
    return("10")
  }
  if(mes=="nov"){
    return("11")
  }
  if(mes=="dez"){
    return("12")
  }
}

trata_dia = function(dia){
  #   dia = as.numeric(as.character(dia))
  if((dia < 10) == TRUE){
    return(paste("0",dia,sep=""))
  }else{
    return(paste(dia,"",sep=""))
    
  }
}



dados = read.table("IEE.csv",sep=";",header=T,dec=",")
dados_estruturados = data.frame(datas=c(),IEE=c())
# dia = 1
for(linha in 1:nrow(dados)){
  for(mes in names(dados)[2:13]){
    valor_fechamento = dados[linha,mes]
    ano = dados[linha,"ano"]
    mes = trata_mes(mes)
    dia = trata_dia(dados[linha,"dia"])    
    data_estruturada = paste(ano,"-",mes,"-",dia,sep="")
    dados_estruturados = rbind(dados_estruturados,
                               data.frame(datas = data_estruturada,
                                          IEE = valor_fechamento))
  }
}

# dados_estruturados = dados_estruturados[na.omit(dados_estruturados[,1]),]
datas = as.Date(as.character(dados_estruturados[,"datas"]),format = "%Y-%m-%d")
dados_estruturados$datas = datas
dados_estruturados = dados_estruturados[!is.na(dados_estruturados[,1]),]
indices_ordem_datas = order(dados_estruturados$datas)
dados_estruturados = dados_estruturados[indices_ordem_datas,]






# datas
# dados_estruturados$datas = datas[indices_ordem_datas]
dados_estruturados = na.omit(dados_estruturados)
# write.csv(file="setores_organizados.csv",dados_estruturados,row.names=F)


# arquivos = paste("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores/IEE/",
#                  dir("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores/IEE/"),sep="")
# 
# arquivo = arquivos[2]
# arquivo
# dados_estruturados = data.frame(datas=c(),IEE=c())
# datas = c()
# valores_fechamento = c()
# for(arquivo in arquivos){
#   dados = read.table(arquivo,sep=";",header=T,dec=",")
#   dados = dados[1:31,1:14]
#   # dia = 1
#   i =1
# #   meses = names(dados)[2:13]
#   for(mes in 2:13){
#     for(linha in 1:nrow(dados)){
#       #     print(dados[linha,1])
#       valor_fechamento = dados[linha,mes]
# #       ano = dados[linha,"ano"]
# #       mes = trata_mes(as.character(names(dados)[mes]))
# #       dia = trata_dia(as.numeric(as.character(dados[linha,"dia"])))    
# #       data_estruturada = paste(ano,"-",mes,"-",dia,sep="")
# #       datas[i] = data_estruturada
#       valores_fechamento[i] = valor_fechamento
#       i = i+1
#       #       dados_estruturados = rbind(dados_estruturados,
#       #                                  data.frame(datas = data_estruturada,
#       #                                             IEE = valor_fechamento))
#     }
#   }
# }
# 

# ano = unique(dados$ano)
# dados_estruturados$datas =  seq(as.Date(paste(ano,"-01-01",sep="")), as.Date(paste(ano,"-12-31",sep="")), by="day")

# 
# # datas = as.Date(as.character(dados_estruturados[,"datas"]),format = "%Y-%m-%d")
# datas = seq(as.Date(paste(ano,"-01-01",sep="")), as.Date(paste(ano,"-12-31",sep="")), by="day")
# 
# 
# dados_estruturados = data.frame(datas = datas, IEE = valores_fechamento)
# # datas
# indices_ordem_datas = order(datas)
# dados_estruturados = dados_estruturados[indices_ordem_datas,]










# 
# 
# dados = read.table("IEE.csv",sep=";",header=T,dec=",")
# dados_estruturados = data.frame(datas=c(),IEE=c())
# # dia = 1
# for(linha in 1:nrow(dados)){
#   for(mes in names(dados)[2:13]){
#     valor_fechamento = dados[linha,mes]
#     ano = dados[linha,"ano"]
#     mes = trata_mes(mes)
#     dia = trata_dia(dados[linha,"dia"])    
#     data_estruturada = paste(ano,"-",mes,"-",dia,sep="")
#     dados_estruturados = rbind(dados_estruturados,
#                                data.frame(datas = data_estruturada,
#                                           IEE = valor_fechamento))
#   }
# }
# # dados_estruturados = na.omit(dados_estruturados)
# # datas = as.Date(as.character(dados_estruturados[,"datas"]),format = "%Y-%m-%d")
# # datas
# indices_ordem_datas = order(datas)
# dados_estruturados = dados_estruturados[indices_ordem_datas,]
# # dados_estruturados$datas = datas[indices_ordem_datas]
# dados_estruturados = na.omit(dados_estruturados)
# write.csv(file="setores_organizados.csv",dados_estruturados,row.names=F)



# arquivos = paste("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setor/",dir("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setor/"),sep="")
# dados = read.csv(file="../Acoes/papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")
# acoes_nomes = names(dados)
# 
# # arquivo = arquivos[1]
# porcentagens = data.frame(setor=c(),porc_acoes_compoem_setor=c())
# for(arquivo in arquivos){
#   dados_setores = read.csv(arquivo,sep=";")
#   acoes_do_setor = paste(dados_setores[1:(nrow(dados_setores)-2),1],".SA",sep="")
#   porcentagem_acoes_compoem_setor = length(intersect(acoes_do_setor,acoes_nomes))/length(acoes_do_setor)
#   print(porcentagem_acoes_compoem_setor)
# }
# dados_setores = read.csv("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setor/UTIL.csv",sep=";")
# acoes_do_setor = paste(dados_setores[1:(nrow(dados_setores)-2),1],".SA",sep="")







dados = inverte_indices_data_frame(dados)

calcula_setores = function(dados){
  #   semestre_acoes = subset(dados,dados$datas==semestre)
  serie_retornos_por_semestre = dados[,2:ncol(dados)]
  #   serie_retornos_por_semestre = cria_tabela_serie_retornos_de_todas_as_acoes(semestre_acoes)
  setores_media_acoes = data.frame(1)
  setores_100_porcento = setores_100_porcento_sem_periodo(dados)
  #   setor = "Consumo Cíclico / Tecid Vest Calç"
  #         setor = setores_100_porcento[1]
  relacao_setores_acoes_menos_acoes = relacao_setores_acoes_sem_periodo(dados)
  for( setor in setores_100_porcento){
    print(paste("Setor: ",setor))
    acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
    print(paste("Acoes: ",acoes_do_setor))
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

acoes_dos_setores = function(dados){
  #   semestre_acoes = subset(dados,dados$datas==semestre)
  serie_retornos_por_semestre = dados[,2:ncol(dados)]
  #   serie_retornos_por_semestre = cria_tabela_serie_retornos_de_todas_as_acoes(semestre_acoes)
  setores_media_acoes = data.frame(1)
  setores_100_porcento = setores_100_porcento_sem_periodo(dados)
  relacao_setores_acoes_menos_acoes = relacao_setores_acoes_sem_periodo(dados)
  for( setor in setores_100_porcento){
    print(setor)
    acoes_do_setor = paste(relacao_setores_acoes_menos_acoes$codigo[relacao_setores_acoes_menos_acoes$setores==setor],".SA",sep="")
    print(acoes_do_setor)
    if(length(acoes_do_setor)!=1){ # nao ira calcular a media quando tiver apenas 1 acao
      medias_por_setor = apply(serie_retornos_por_semestre[,acoes_do_setor],MARGIN=1,FUN=mean)
    }else{
      medias_por_setor = serie_retornos_por_semestre[,acoes_do_setor]
    }
    setores_media_acoes = cbind(setores_media_acoes,medias_por_setor)
  }
  #   setores_media_acoes = setores_media_acoes[,-1]
  #   return(setores_media_acoes)
} 
acoes_dos_setores(dados)

series_temporais_setores = calcula_series_temporais_dos_setores(dados)
series_temporais_setores = cbind(data.frame(BVSP = dados$BVSP),series_temporais_setores)
series_temporais_setores = cbind(data.frame(data = dados$datas),series_temporais_setores)



setor_sem_serie_tratada = "Consumo Cíclico / Tecid Vest Calç"

series_temporais_setores = series_temporais_setores[,setdiff(names(series_temporais_setores),setor_sem_serie_tratada)]
#  names(series_temporais_setores)

png("imagens/setores.png",bg="transparent",width = 1000,height = 800)
par(mfrow = c(4,2))
for(indice_setor in 2:ncol(series_temporais_setores)){
  print(plot(main = names(series_temporais_setores)[indice_setor],
             series_temporais_setores[,indice_setor],ylab=""))
}
dev.off()
write.csv(series_temporais_setores,file="series_temporais_setores.csv",row.names=F)

dados = read.csv("series_temporais_setores.csv")
write.table(dados,file="series_temporais_setores_estruturados.csv",row.names=F,sep=";",dec = ",")
# plot(dados$Const.e.Transp...Constr.e.Engenh[1600:1678])
