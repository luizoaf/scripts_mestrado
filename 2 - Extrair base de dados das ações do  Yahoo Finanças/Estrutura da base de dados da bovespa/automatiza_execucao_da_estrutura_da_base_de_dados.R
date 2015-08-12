setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")
source("../../1 - Funcoes/funcoes.R")
setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")
# arquivo = "COTAHIST_A2014.TXT"
arquivos = list.files("dados")
# arquivo = arquivos[1]
for(arquivo in arquivos){
  diretorio = paste("dados/",arquivo,sep="")
  print(diretorio)
  base_de_dados = read.csv(diretorio)
  source("estrutura_base_de_dados.R")
  print(paste("Quantidade de dias não nulos: ",nrow(dados_estruturados)))
  print(paste("Quantidade de ações: ",ncol(dados_estruturados)-1))
  print("Ações que possuiram o número máximo de valores não nulos de dias: ")
  print(names(dados_estruturados)[2:ncol(dados_estruturados)])
  print("#########################################################################################")
}