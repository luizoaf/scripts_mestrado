setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/6 - Modelos inteligentes")
# setores = read.csv("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/series_temporais_setores.csv")
setores = read.csv("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/3 - Janelamento/Setor/metricas_setores_sem_series_temporais.csv")



for(setor_nome in unique(setores$setor)){
  setor = subset(setores,setores$setor == setor_nome)
  setor = setor[,c("coeficiente_B","volatilidade")]
  write.table(setor,paste("series_temporais_b_volatilidade_setores/",setor_nome,".csv",sep=""), row.names=FALSE,sep=";",dec=",")
}



