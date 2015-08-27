setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")
source("../../1 - Funcoes/funcoes.R")
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




arquivos = paste("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores_financeiros/",
                 dir("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores_financeiros/"),sep="")

# dados = read.table("IEE.csv",sep=";",header=T,dec=",")
# dados = read.table("IMOB.csv",sep=";",header=T,dec=",")
# nova_base_de_dados = data.frame()
colunas =  unlist(strsplit(dir("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores_financeiros/"),".csv"))
# for(arquivo in arquivos){
for(indice_setor in 1:length(colunas)){
  # indice_setor = 7
  arquivo = arquivos[indice_setor]
  # serie_temporal = function(arquivo){
  dados = read.table(arquivo,sep=";",header=T,dec=",")
  dados = dados[1:248,]
  dados_estruturados = data.frame(datas=c(),valor_fechamento=c())
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
                                            valor_fechamento = valor_fechamento))
    }
  }
  
  # dados_estruturados = dados_estruturados[na.omit(dados_estruturados[,1]),]
  datas = as.Date(as.character(dados_estruturados[,"datas"]),format = "%Y-%m-%d")
  dados_estruturados$datas = datas
  dados_estruturados = dados_estruturados[!is.na(dados_estruturados[,1]),]
  indices_ordem_datas = order(dados_estruturados$datas)
  dados_estruturados = dados_estruturados[indices_ordem_datas,]
  dados_estruturados = na.omit(dados_estruturados)
  dados_estruturados = inverte_indices_data_frame(dados_estruturados)
  write.csv(file=paste("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores_estruturados/",colunas[indice_setor],"_estruturado.csv",sep=""),dados_estruturados,row.names=F)
}

