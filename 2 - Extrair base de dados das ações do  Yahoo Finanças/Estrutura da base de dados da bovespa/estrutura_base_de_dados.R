# base_de_dados = read.csv(arquivo)
base_de_dados = na.omit(base_de_dados)

datas = substr(base_de_dados[1:nrow(base_de_dados),],3,10)
codneg = trim(as.character(substr(base_de_dados[1:nrow(base_de_dados),],13,24)))
nomres = trim(substr(base_de_dados[1:nrow(base_de_dados),],28,39))
valor_acao =  trim(as.character(substr(base_de_dados[1:nrow(base_de_dados),],109,121)))
preult = as.double(paste(substr(valor_acao,1,11),".",substr(valor_acao,12,nchar(valor_acao)),sep=""))
write.csv(file=paste("acoes_intersecao_",arquivo,".csv",sep=""),data.frame(codigo=codneg),row.names=F)
df_acoes_do_ano = data.frame(data= datas,
                             codneg = codneg,
                             nomres = nomres,
                             fechamento = preult)
# unique(codneg)[order(unique(codneg))]
# teste = df_acoes_do_ano[  df_acoes_do_ano$nomres %in% "IBOVESPA",]
df_acoes_do_ano = na.omit(df_acoes_do_ano)
# df_acoes_do_ano[as.character(df_acoes_do_ano$codneg)%in%  "BVMF3",]
# "BVMF3" = bovespa

# Acoes 2014
# acoes = c("^BVSP",as.character(read.csv("acoes.csv",sep=";")[,1])[1:69]) 
#Acoes 2015
acoes = c("^BVSP",as.character(read.csv("acoes_atualizado.csv",sep=";")[1:68,1]))
print(paste("Quantidade de ações que compõem a IBOVESPA: ",length(intersect(df_acoes_do_ano$codneg,acoes)),",De um total de: ",length(acoes)-1))

# acoes_filtro = df_acoes_do_ano[df_acoes_do_ano$data %in% dados_estruturados$datas &
#                                  df_acoes_do_ano$codneg %in% as.character(quantidade_de_datas_uteis$Group.1)
#                                ,c("data","codneg","fechamento")]

acoes_filtro = df_acoes_do_ano[df_acoes_do_ano$codneg %in% acoes,c("data","codneg","fechamento"),]

quantidade_de_datas_uteis = aggregate(acoes_filtro$data,list(acoes_filtro$codneg),FUN=length)
maximo_dias = max(quantidade_de_datas_uteis$x) - 10
print(maximo_dias)
quantidade_de_datas_uteis = na.omit(quantidade_de_datas_uteis[quantidade_de_datas_uteis$x>=maximo_dias,])
quantidade_de_datas_uteis = quantidade_de_datas_uteis[order(quantidade_de_datas_uteis$x,decreasing =F),]
acao_menor_qtd_dias_uteis = as.character(quantidade_de_datas_uteis$Group.1[1])
# acoes_filtro = df_acoes_do_ano[df_acoes_do_ano$codneg %in% as.character(quantidade_de_datas_uteis$Group.1),c("data","codneg","fechamento"),]

configura_estrutura_dados = aggregate(acoes_filtro$fechamento,list(acoes_filtro$codneg),FUN = t)
# as.vector(unlist(configura_estrutura_dados["HGTX3",]$x))
datas = as.character(na.omit(subset(acoes_filtro$data,acoes_filtro$codneg==acao_menor_qtd_dias_uteis)))
# acoes = unique(as.character(acoes_filtro$codneg))
# acoes = setdiff(acoes, acao_menor_qtd_dias_uteis) # sem a acao com menor valor de dados uteis proximo dos 240 dias
# for(acao in acoes){
#   
# }
dados_estruturados = data.frame( datas = datas,
                                 acao_menor_qtd_dias_uteis = as.vector(unlist(configura_estrutura_dados[with(configura_estrutura_dados,as.character(configura_estrutura_dados$Group.1)==acao_menor_qtd_dias_uteis),]$x)))
colnames(dados_estruturados) = c("data",acao_menor_qtd_dias_uteis)

#com as datas da acao com menor quantidade de dias uteis próximo dos 240 dias por ano
acoes_com_mais_de_240_dias = as.character(quantidade_de_datas_uteis$Group.1)
acoes_filtro = df_acoes_do_ano[as.character(df_acoes_do_ano$data) %in% as.character(dados_estruturados$data) &
                                 df_acoes_do_ano$codneg %in% acoes_com_mais_de_240_dias
                               ,c("data","codneg","fechamento")]
acoes = unique(as.character(acoes_filtro$codneg))
acoes = setdiff(acoes, acao_menor_qtd_dias_uteis) # sem a acao com menor valor de dados uteis proximo dos 240 dias
# acao = acoes[1]

# acao_separada = acoes_filtro[acao == as.character(acoes_filtro$codneg),]

nome_das_acoes = c("datas",acao_menor_qtd_dias_uteis)
i = 3
apenas_datas_uteis = data.frame(data = as.character(dados_estruturados$data))
acao = acoes[3]
for(acao in acoes){
  acao_separada = subset(acoes_filtro,acao == as.character(acoes_filtro$codneg))
  #     dados_estruturados = cbind(dados_estruturados,acao_separada$fechamento )
  merge_da_intersecao_dias_da_acao = merge(apenas_datas_uteis,acao_separada[,c("data","fechamento")],by="data",all=T) # gera um warning por conta do nome da coluna fechamento que repete, mas tá certo
  dados_estruturados = cbind(dados_estruturados,as.data.frame(merge_da_intersecao_dias_da_acao$fechamento))
  nome_das_acoes[i] = acao
  i = i + 1
}
dados_estruturados = na.omit(dados_estruturados)
colnames(dados_estruturados) = nome_das_acoes

# indice_acao= 1
# for(indice_acao in 1:nrow(configura_estrutura_dados)){
#   acao = as.character(configura_estrutura_dados$Group.1[indice_acao])
#   if(acao!=acao_menor_qtd_dias_uteis){
#     fechamentos =  as.vector(unlist(configura_estrutura_dados[indice_acao,]$x))
#     #   if(length(fechamentos) >= 240){
#     if(length(fechamentos) == maximo_dias){
#       
#       dados_estruturados = cbind(dados_estruturados, fechamentos)
#       acoes_com_max_dias[i] = acoes[indice_acao]
#       i = i +1
#     }
#   }
# }

# acoes = as.character(quantidade_de_datas_uteis$Group.1[quantidade_de_datas_uteis$x>=240])
# qtd_dias_mais_proximo_de_240 = 



# dados_estruturados = data.frame(1:maximo_dias)
# acoes_com_max_dias = c()
# acoes = as.character(configura_estrutura_dados[,1])
# i = 1
# # acao = 1
# for(acao in 1:nrow(configura_estrutura_dados)){
#   fechamentos =  as.vector(unlist(configura_estrutura_dados[acao,]$x))
# #   if(length(fechamentos) >= 240){
# if(length(fechamentos) == maximo_dias){
#     
#     dados_estruturados = cbind(dados_estruturados, fechamentos)
#     acoes_com_max_dias[i] = acoes[acao]
#     i = i +1
#   }
# }
# dados_estruturados = dados_estruturados[,-1]
# colnames(dados_estruturados) = acoes_com_max_dias

# df_acoes_com_o_max_dias = df_acoes_do_ano[as.character(df_acoes_do_ano$codneg) %in% acoes_com_max_dias,]
# datas = df_acoes_com_o_max_dias[df_acoes_com_o_max_dias$codneg == df_acoes_com_o_max_dias$codneg[1],"data"] #datas da primeira acao( ela já fez a intersecao com as demais, logo, sao iguais)
# datas = paste(substr(datas,1,4),"-",substr(datas,5,6),"-",substr(datas,7,8),sep="")
# dados_estruturados = cbind(data.frame(datas=datas),dados_estruturados)
# dados_estruturados = na.omit(dados_estruturados)
write.csv(dados_estruturados,file=paste("dados_estruturados/",arquivo,".csv",sep=""),row.names=F)
