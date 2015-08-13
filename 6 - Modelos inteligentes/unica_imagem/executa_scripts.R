################################# B ##########################

setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/6 - Modelos inteligentes/unica_imagem")
# setores = read.csv("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/series_temporais_setores.csv")
setores = read.csv("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/3 - Janelamento/Setor/metricas_setores_sem_series_temporais.csv")
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
nome_series_temporais = c("BSP","Bens industriais e\nmaterial de transporte",
                          "Consumo não cíclico e\nalimentos processados",
                          "Consumo não cíclico e\nbebidas",
                          "Consumo não cíclico e\nfumo",
                          "Consumo não cíclico e\nprodutos de uso pessoal e de limpeza",
                          "Construção e transporte,\nconstrução e engenharia",
                          "Utilidade pública, \nágua e saneamento")
i=4
# for(i in 1:length(todos_setores)){
  setor = todos_setores[i]
#   png(paste("previsoes/",setor,".png",sep=""),bg = "transparent",height = 450,width = 1050)
  serie_temporal_setor = setor_b_volatilidade(setor)
  source("acf_b_volatilidade.R")
  source("acf_volatilidade.R")
  source("acf_b.R")
  legend("topright",title="Variáveis de entrada" ,inset=.05, c("Alvo(volatilidade)","B e vol.","Vol.","B"), lwd= 3,col = c("black","red","blue","green"), horiz=TRUE)
#   dev.off()
# }