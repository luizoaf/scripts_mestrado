setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das a��es do  Yahoo Finan�as/Setores")
source("../../1 - Funcoes/funcoes.R")
setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das a��es do  Yahoo Finan�as/Setores")
# arquivo = "COTAHIST_A2014.TXT"
arquivos = list.files("dados")
# arquivo = arquivos[1]
for(arquivo in arquivos){
  diretorio = paste("dados/",arquivo,sep="")
  print(diretorio)
  base_de_dados = read.csv(diretorio)
  source("estrutura_base_de_dados.R")
  print(paste("Quantidade de dias n�o nulos: ",nrow(dados_estruturados)))
  print(paste("Quantidade de a��es: ",ncol(dados_estruturados)-1))
  print("A��es que possuiram o n�mero m�ximo de valores n�o nulos de dias: ")
  print(names(dados_estruturados)[2:ncol(dados_estruturados)])
  print("#########################################################################################")
}