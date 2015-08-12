diretorio_atual = "C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/3 - Janelamento/Setor"
setwd(diretorio_atual)
source("../../1 - Funcoes/funcoes.R")
setwd(diretorio_atual)

dados = read.csv(file="../../2 - Extrair base de dados das ações do  Yahoo Finanças/Acoes/papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")

series_temporais_setores = read.csv(file="../../2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/series_temporais_setores.csv")

# series_temporais_setores = cria_serie_retornos(series_temporais_setores[,2:ncol(series_temporais_setores)])

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
nome_dos_setores = c()
indice_dos_nomes_dos_setores= 1
grupo_do_janelamento = 1
# indice_setor=3
# linha_janelamento = 1
for(linha_janelamento in 1:nrow(janelamentos_indices)){
  
  indices_janelamento = janelamentos_indices$inicio[linha_janelamento]:janelamentos_indices$fim[linha_janelamento]
  
  # determina as series temporais para cada janelamento  
  series_temporais = series_temporais_setores[indices_janelamento,]
  for(indice_setor in 2:ncol(series_temporais)){
    nome_do_setor =  names(series_temporais)[indice_setor]
    nome_dos_setores[indice_dos_nomes_dos_setores] = nome_do_setor
    
    bovespa = series_temporais$BVSP
    serie = series_temporais[,indice_setor]
    beta = calcular_risco_beta(serie,bovespa)
    metricas = rbind(metricas, cbind(grupo_do_janelamento,beta))
    colnames(metricas) = c("grupo_do_janelamento","beta")
    
    indice_dos_nomes_dos_setores = indice_dos_nomes_dos_setores + 1
    # write.table(metricas,paste(names(series_temporais)[i],grupo_janelamento,"combinacao_janelamento_30_incrementos_0_01_0_28_todas_acoes.csv"),row.names=F,sep=",")
  }
  grupo_do_janelamento = grupo_do_janelamento + 1
}

metricas$setor = nome_dos_setores[1:nrow(metricas)]
write.csv(metricas,file="Beta_dos_setores.csv",row.names=F)
# teste = read.csv("Beta_dos_setores.csv")
