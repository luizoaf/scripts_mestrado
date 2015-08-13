setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")
source("../../1 - Funcoes/funcoes.R")
setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")

dados = read.csv(file="../Acoes/papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")
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
