setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")
source("../../1 - Funcoes/funcoes.R")
setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")



arquivos = paste("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores_estruturados/",
                 dir("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores_estruturados/"),sep="")

colunas =   unlist(strsplit(dir("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores/setores_financeiros/"),".csv"))
# i = 1
# for(arquivo in arquivos){
#   setor = read.csv(arquivo)
#   setor
#   write.csv(file=paste(colunas[indice_setor],"_estruturado.csv",sep=""),dados_estruturados,row.names=F)
#   i = i+1
# }

dados = read.csv(arquivos[1])
colnames(dados) = c("datas",colunas[1])

arquivo = arquivos[2]
setor = read.csv(arquivo)
colnames(setor) = c("datas",colunas[2])
dados = merge(dados,setor,by="datas")
dados = inverte_indices_data_frame(dados)

arquivo = arquivos[3]
setor = read.csv(arquivo)
colnames(setor) = c("datas",colunas[3])
dados = merge(dados,setor,by="datas")
dados = inverte_indices_data_frame(dados)

arquivo = arquivos[4]
setor = read.csv(arquivo)
colnames(setor) = c("datas",colunas[4])
dados = merge(dados,setor,by="datas")
dados = inverte_indices_data_frame(dados)

arquivo = arquivos[5]
setor = read.csv(arquivo)
colnames(setor) = c("datas",colunas[5])
dados = merge(dados,setor,by="datas")
dados = inverte_indices_data_frame(dados)

arquivo = arquivos[6]
setor = read.csv(arquivo)
colnames(setor) = c("datas",colunas[6])
dados = merge(dados,setor,by="datas")
dados = inverte_indices_data_frame(dados)

arquivo = arquivos[7]
setor = read.csv(arquivo)
colnames(setor) = c("datas",colunas[7])
dados = merge(dados,setor,by="datas")
dados = inverte_indices_data_frame(dados)

dados = na.omit(dados)

write.csv(file = "series_temporais_setores.csv",dados,row.names = F)
