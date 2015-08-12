setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")
source("../../1 - Funcoes/funcoes.R")
setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")

dados = read.csv(file="../Acoes/papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")
dados = inverte_indices_data_frame(dados)

calcula_setores = function(dados){
  #   semestre_acoes = subset(dados,dados$datas==semestre)
  serie_retornos_por_semestre = dados[,2:ncol(dados)]
  #   serie_retornos_por_semestre = cria_tabela_serie_retornos_de_todas_as_acoes(semestre_acoes)
  setores_media_acoes = data.frame(1)
  setores_100_porcento = setores_100_porcento_sem_periodo(dados)
  #   setor = "Consumo Cíclico / Tecid Vest Calç"
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




series_temporais_setores = calcula_series_temporais_dos_setores(dados)
series_temporais_setores = cbind(data.frame(BVSP = dados$BVSP),series_temporais_setores)
series_temporais_setores = cbind(data.frame(data = dados$datas),series_temporais_setores)



setor_sem_serie_tratada = "Consumo Cíclico / Tecid Vest Calç"

series_temporais_setores = series_temporais_setores[,setdiff(names(series_temporais_setores),setor_sem_serie_tratada)]
#  names(series_temporais_setores)

png("imagens/setores.png",bg="transparent",width = 1000,height = 800)
par(mfrow = c(3,3))
for(indice_setor in 2:ncol(series_temporais_setores)){
  print(plot(main = names(series_temporais_setores)[indice_setor],
             series_temporais_setores[,indice_setor],ylab=""))
}
dev.off()
write.csv(series_temporais_setores,file="series_temporais_setores.csv",row.names=F)
# 
# 
# cotacoes_2010 = read.csv("dados/COTAHIST_A2010.txt")
# 
# # teste1 = cotacoes_2010[1,]
# # substr(teste1,5,25)
# # 
# # acoes_2010 = data.frame()
# # # :nrow(cotacoes_2010)
# # for(linha in 1:nrow(cotacoes_2010)){
# #   acao_linha = cotacoes_2010[linha,]
# #   valor_acao = substr(acao_linha,109,121)
# #   preult = as.double(paste(substr(valor_acao,1,11),".",substr(valor_acao,12,nchar(valor_acao)),sep=""))
# #   
# #   acoes_2010 = rbind(acoes_2010,data.frame(data=substr(acao_linha,3,10),
# #                                            codbdi = substr(acao_linha,11,12),
# #                                            codneg = substr(acao_linha,13,24),
# #                                            tpmerc = substr(acao_linha,25,27),
# #                                            nomres = substr(acao_linha,28,39),
# #                                            preult = preult))  
# # }
# # 
# # v = function(acao_linha){
# #   acao_linha = acao_linha[,1]
# #   valor_acao = substr(acao_linha,109,121)
# #   preult = as.double(paste(substr(valor_acao,1,11),".",substr(valor_acao,12,nchar(valor_acao)),sep=""))
# #   acoes_2010 = data.frame(data=substr(acao_linha,3,10),
# #                           codbdi = substr(acao_linha,11,12),
# #                           codneg = substr(acao_linha,13,24),
# #                           tpmerc = substr(acao_linha,25,27),
# #                           nomres = substr(acao_linha,28,39),
# #                           preult = preult)
# #   return(acoes_2010)
# # }
# 
# 
# 
# # 
# # define_data= function(acao_linha){
# #   #   acao_linha = acao_linha[,1]
# #   #   valor_acao = substr(acao_linha,109,121)
# #   #   preult = as.double(paste(substr(valor_acao,1,11),".",substr(valor_acao,12,nchar(valor_acao)),sep=""))
# #   #   acoes_2010 = data.frame(data=
# #   #                           codbdi = substr(acao_linha,11,12),
# #   #                           codneg = substr(acao_linha,13,24),
# #   #                           tpmerc = substr(acao_linha,25,27),
# #   #                           nomres = substr(acao_linha,28,39),
# #   #                           preult = preult)
# #   return(substr(acao_linha,3,10))
# # }
# trim <- function (x) sub("\\s+$", "", x)
# 
# dados_2010= cotacoes_2010
# # nrow(dados_2010)
# # dados_2010 = na.omit(dados_2010)
# datas = substr(dados_2010[1:nrow(dados_2010),],3,10)
# # codbdi = substr(dados_2010,11,12)
# codneg = trim(as.character(substr(dados_2010[1:nrow(dados_2010),],13,24)))
# # tpmerc = substr(dados_2010,25,27)
# nomres = substr(dados_2010[1:nrow(dados_2010),],28,39)
# 
# valor_acao =  trim(as.character(substr(dados_2010[1:nrow(dados_2010),],109,121)))
# preult = as.double(paste(substr(valor_acao,1,11),".",substr(valor_acao,12,nchar(valor_acao)),sep=""))
# 
# 
# acoes_2010 = data.frame(data= datas,
#                         codneg = codneg,
#                         #                         nomres = nomres,
#                         fechamento = preult)
# hering = acoes_2010[as.character(acoes_2010$data) == "20101029" & as.character(acoes_2010$codneg)=="HGTX3",]
# hering = acoes_2010[as.character(acoes_2010$data) == "20101101" & as.character(acoes_2010$codneg)=="HGTX3",]
# # datas = aggregate(cotacoes_2010[,1],list(cotacoes_2010[,1]),FUN=define_data)
# 
# # base_de_dados = list.files("dados")
# 
# # 
# # w =function(acao_linha){
# #   return( data.frame(ui = substr(acao_linha,109,121)),ai =  substr(acao_linha,25,27))
# # }
# # v(cotacoes_2010[1,])
# # t = data.frame( a = cotacoes_2010[1:2,])
# # t$grupo = 1:2
# # x = aggregate(t[,1],list(t[,1]),FUN=w)
# 
# 
# 
# acoes = as.character(read.csv("acoes.csv",sep=";")[,1])[1:69]
# acoes_filtro = acoes_2010[acoes_2010$codneg %in% acoes,c("data","codneg","fechamento"),]
# 
# intersecao = aggregate(acoes_filtro$data,list(acoes_filtro$codneg),FUN=length)
# maximo_dias = max(intersecao$x)
# 
# configura_estrutura_dados = aggregate(acoes_filtro$fechamento,list(acoes_filtro$codneg),FUN = t)
# 
# # configura_estrutura_dados = aggregate(acoes_filtro$fechamento,list(acoes_filtro$codneg),FUN = t)
# # dados_estruturados = t(data.frame(acoes = acoes))
# # colnames(dados_estruturados) = acoes
# # # configura_estrutura_dados = as.data.frame(t(configura_estrutura_dados),haed=T)
# # as.vector(unlist(teste$V1))
# # as.vector(unlist(teste$V1))
# dados_estruturados = data.frame(1:maximo_dias)
# acoes_com_max_dias = c()
# acoes = as.character(configura_estrutura_dados[,1])
# i = 1
# # acao = 1
# for(acao in 1:nrow(configura_estrutura_dados)){
#   fechamentos =  as.vector(unlist(configura_estrutura_dados[acao,]$x))
#   if(length(fechamentos) == maximo_dias){
#     dados_estruturados = cbind(dados_estruturados, fechamentos)
#     acoes_com_max_dias[i] = acoes[acao]
#     i = i +1
#   }
# }
# dados_estruturados = dados_estruturados[,-1]
# colnames(dados_estruturados) = acoes_com_max_dias
# 
# 
# 
# 
# ########## extrair datas com acoes que tiveram o max de dias ########
# 
# df_acoes_com_o_max_dias = acoes_2010[as.character(acoes_2010$codneg) %in% acoes_com_max_dias,]
# df_datas = data.frame(1:maximo_dias)
# for(indice_codigo in 1:length(unique(df_acoes_com_o_max_dias$codneg))){
#   datas = df_acoes_com_o_max_dias[df_acoes_com_o_max_dias$codneg == df_acoes_com_o_max_dias$codneg[indice_codigo],"data"] 
#   df_datas = cbind(df_datas,datas)
#   
# }
# indice_das_datas_das_acoes = 2
# df_datas = df_datas[,-1]
# indices_remover_dias_diferentes = c()
# j = 1
# for(indice_das_datas_das_acoes in 2:ncol(df_datas)){
#   intersecao_quantidade_datas = length(intersect(df_datas[,1],df_datas[,indice_das_datas_das_acoes]))# conta as datas em comum da primeira acao com as demais
#   if(intersecao_quantidade_datas != maximo_dias){
#     indices_remover_dias_diferentes[j] = indice_das_datas_das_acoes
#     j = j + 1
#   }
# }
# if(!is.null(indices_remover_dias_diferentes)){
#   dados_estruturados[,-indices_remover_dias_diferentes] # removo as colunas que não tem as datas em comum
# }
# #####################################################################
