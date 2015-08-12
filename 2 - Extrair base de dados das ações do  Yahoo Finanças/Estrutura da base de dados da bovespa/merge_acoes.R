setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")
source("../../1 - Funcoes/funcoes.R")
setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Setores")

arquivos = list.files("dados_estruturados")
arquivos = arquivos[order(arquivos)]
ano_analisado = 9
# acoes_que_compoe_bovespa = c("datas","^BVSP",as.character(read.csv("acoes.csv",sep=";")[,1])[1:69])
acoes_que_compoe_bovespa =  c("datas","^BVSP",as.character(read.csv("acoes_atualizado.csv",sep=";")[1:68,1]))

####################### intersecao acoes ##############################

# arquivos = list.files("acoes")
# diretorio = paste("acoes/",arquivos[ano_analisado],sep="")
# arquivos = arquivos[(ano_analisado+1):length(arquivos)]
arquivos = list.files("dados_estruturados")
arquivos = arquivos[order(arquivos)]
diretorio = paste("dados_estruturados/",arquivos[ano_analisado],sep="")
arquivos = arquivos[(ano_analisado+1):length(arquivos)]

acoes_em_comum = c()
base_de_dados = read.csv(diretorio)
acoes_em_comum = intersect(names(base_de_dados),acoes_que_compoe_bovespa)
length(acoes_em_comum)

# arquivo = arquivos[1]
for(arquivo in arquivos){
  diretorio = paste("dados_estruturados/",arquivo,sep="")
  print(diretorio)
  base_de_dados = read.csv(diretorio)
  acoes_do_ano = names(base_de_dados)
  
  acoes_em_comum = intersect(acoes_em_comum,acoes_do_ano)
  acoes_em_comum = intersect(acoes_em_comum,acoes_que_compoe_bovespa)
}
# length(acoes_em_comum)
acoes_em_comum = acoes_em_comum[order(acoes_em_comum)]


intersecao_acoes = acoes_em_comum[acoes_em_comum %in%acoes_que_compoe_bovespa]
length(acoes_em_comum[acoes_em_comum %in%acoes_que_compoe_bovespa]) #23 acoes desde 2000

#######################################################################

arquivos = list.files("dados_estruturados")
arquivos = arquivos[order(arquivos)]
diretorio = paste("dados_estruturados/",arquivos[ano_analisado],sep="")
arquivos = arquivos[(ano_analisado+1):length(arquivos)]



base_de_dados = read.csv(diretorio)
# acoes_da_base_de_dados = names(base_de_dados)
# 
# acoes_da_base_de_dados_sendo_as_que_compoem_a_bovespa = intersect(acoes_que_compoe_bovespa, acoes_da_base_de_dados)
# acoes_da_base_de_dados_sendo_as_que_compoem_a_bovespa
# dados_acoes_compoem_bovespa = base_de_dados[,acoes_da_base_de_dados_sendo_as_que_compoem_a_bovespa]
dados_acoes_compoem_bovespa = base_de_dados[,intersecao_acoes]
# head(base_de_dados[,as.character(intersecao_acoes)[2]])
# acoes_em_comum = c()

# acoes_em_comum = names(base_de_dados)
# arquivo = arquivos[1]
for(arquivo in arquivos){
  diretorio = paste("dados_estruturados/",arquivo,sep="")
  print(diretorio)
  base_de_dados = read.csv(diretorio)
  acoes_da_base_de_dados = names(base_de_dados)
  
  acoes_da_base_de_dados_sendo_as_que_compoem_a_bovespa = intersect(names(dados_acoes_compoem_bovespa),acoes_da_base_de_dados)
  dados_acoes_compoem_bovespa = rbind(dados_acoes_compoem_bovespa,base_de_dados[,intersecao_acoes])
}
# dados_acoes_compoem_bovespa

datas = dados_acoes_compoem_bovespa$datas
dados_acoes_compoem_bovespa = subset(dados_acoes_compoem_bovespa,select = -c(datas))
dados_acoes_compoem_bovespa = cbind(datas,dados_acoes_compoem_bovespa)
acoes_sem_SA = names(dados_acoes_compoem_bovespa)
colunas = paste(acoes_sem_SA[2:length(acoes_sem_SA)],".SA",sep="")
mudanca_colunas = c("datas",colunas)
colnames(dados_acoes_compoem_bovespa) = mudanca_colunas
setores = calcula_series_temporais_dos_setores(dados_acoes_compoem_bovespa)
acoes_por_setores_sem_periodo(dados_acoes_compoem_bovespa)


datas = dados_acoes_compoem_bovespa$datas
datas = paste(substr(datas,1,4),"-",substr(datas,5,6),"-",substr(datas,7,8),sep="")
dados_acoes_compoem_bovespa$datas = datas

dados = read.csv(file="../Acoes/papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")
dados = inverte_indices_data_frame(dados)
acoes_em_comum = intersect(names(dados),names(dados_acoes_compoem_bovespa))
tem_em_yahoo_nao_em_bovespa = setdiff(names(dados),names(dados_acoes_compoem_bovespa))
tem_em_bovespa_nao_em_yahoo = setdiff(names(dados_acoes_compoem_bovespa),names(dados))

yahoo_acoes_em_comum = dados[,acoes_em_comum]
bovespa_acoes_em_comum = dados_acoes_compoem_bovespa[,acoes_em_comum]
colnames(bovespa_acoes_em_comum) = c("data",paste(names(bovespa_acoes_em_comum)[2:ncol(bovespa_acoes_em_comum)],"_bovespa",sep=""))
colnames(yahoo_acoes_em_comum) = c("data",paste(names(yahoo_acoes_em_comum)[2:ncol(yahoo_acoes_em_comum)],"_yahoo",sep=""))

# colnames(dados_acoes_compoem_bovespa) = c("data",paste(names(dados_acoes_compoem_bovespa)[2:ncol(dados_acoes_compoem_bovespa)],"_bovespa",sep=""))
# colnames(dados) = c("data",paste(names(dados)[2:ncol(dados)],"_yahoo",sep=""))

merge_yahoo_bovespa = merge(bovespa_acoes_em_comum,yahoo_acoes_em_comum,by="data")
# intersect(names(dados_acoes_compoem_bovespa),names(dados))
indices = (length(merge_yahoo_bovespa$data)-50):length(merge_yahoo_bovespa$data)
faixa_temporal = merge_yahoo_bovespa$data[indices]
plot(merge_yahoo_bovespa$BBAS3.SA_bovespa[indices],type="l",ylim=c(0,40))
lines(merge_yahoo_bovespa$BBAS3.SA_yahoo[indices],col=2)

# acoes_2015 = as.character(read.csv(file="acoes_atualizado.csv",sep=";")[1:68,1])
# write.csv(merge_yahoo_bovespa,file="merge_yahoo_bovespa_apenas_acoes_em_comum.csv",row.names=F)

# df_setores_codigo_acao
# setores_2015 = read.table("setores_2015.txt",sep="\t",head=T)
