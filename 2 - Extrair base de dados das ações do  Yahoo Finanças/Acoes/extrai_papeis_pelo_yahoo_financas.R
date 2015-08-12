setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Acoes")
source("../../1 - Funcoes/funcoes.R")
setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/2 - Extrair base de dados das ações do  Yahoo Finanças/Acoes")

acoes = paste(as.character(read.csv("acoes.csv",sep=";")[,1])[1:69],".SA",sep="")
# acoes_que_compoe_bovespa =  as.character(read.csv("acoes_atualizado.csv",sep=";")[1:68,1])
data_inicio = "2008-01-01"
data_fim = "2014-12-31"

# Function and example code for loading finance data from Yahoo
# without the need of any additional package.
#
# Written by Fotis Papailias & Dimitrios Thomakos on Dec. 31, 2011
# Contact Details: papailias@quantf.com<script type="text/javascript">
# /* <![CDATA[ */
#                (function(){try{var s,a,i,j,r,c,l,b=document.getElementsByTagName("script");l=b[b.length-1].previousSibling;a=l.getAttribute('data-cfemail');if(a){s='';r=parseInt(a.substr(0,2),16);for(j=2;a.length-j;j+=2){c=parseInt(a.substr(j,2),16)^r;s+=String.fromCharCode(c);}s=document.createTextNode(s);l.parentNode.replaceChild(s,l);}}catch(e){}})();
#              /* ]]> */
#   </script>
#   #                  dimitrios.thomakos@gmail.com<script type="text/javascript">
#   /* <![CDATA[ */
#                  (function(){try{var s,a,i,j,r,c,l,b=document.getElementsByTagName("script");l=b[b.length-1].previousSibling;a=l.getAttribute('data-cfemail');if(a){s='';r=parseInt(a.substr(0,2),16);for(j=2;a.length-j;j+=2){c=parseInt(a.substr(j,2),16)^r;s+=String.fromCharCode(c);}s=document.createTextNode(s);l.parentNode.replaceChild(s,l);}}catch(e){}})();
#                /* ]]> */
#   </script>, thomakos@quantf.com<script type="text/javascript">
#   /* <![CDATA[ */
#                  (function(){try{var s,a,i,j,r,c,l,b=document.getElementsByTagName("script");l=b[b.length-1].previousSibling;a=l.getAttribute('data-cfemail');if(a){s='';r=parseInt(a.substr(0,2),16);for(j=2;a.length-j;j+=2){c=parseInt(a.substr(j,2),16)^r;s+=String.fromCharCode(c);}s=document.createTextNode(s);l.parentNode.replaceChild(s,l);}}catch(e){}})();
#                /* ]]> */
#   </script>
#
# All material is provided for use as is, with no guarrantees, either expressed or implied.
# Copyright (C) under the authors' names Papailias, Fotis and Thomakos, Dimitrios for both
#
#-------------------------------------------------------------------------#
#             Quantitative Finance & Technical Trading                    #
#                     http://www.quantf.com                               #
#-------------------------------------------------------------------------#
#
# PLEASE MAINTAIN THIS HEADER IN ALL COPIES OF THIS FILE THAT YOU USE

###############################################################################################
# Main Function
#
# Input
# -----
#   tickers (text strings)
#   start.date (dates)
#   end.date (dates)
#
# Output
# -------
# 6 Double Matrices: Open, High, Low, Close, Volume, Adj. Close
###############################################################################################

data.loading <- function(tickers, start.date, end.date)
{
  # Change the locale
  sl <- Sys.setlocale(locale="US")
  
  # Create the universe of dates
  all.dates <- seq(as.Date(start.date), as.Date(end.date), by="day")
  all.dates <- subset(all.dates,weekdays(all.dates) != "Sunday" & weekdays(all.dates) != "Saturday")
  all.dates.char <- as.matrix(as.character(all.dates))
  
  # Create sparse matrices
  open <- matrix(NA, NROW(all.dates.char), length(tickers))
  hi <- open
  low <- open
  close <- open
  volume <- open
  adj.close <- open
  
  # Name the rows correctly
  rownames(open) <- all.dates.char
  rownames(hi) <- all.dates.char
  rownames(low) <- all.dates.char
  rownames(close) <- all.dates.char
  rownames(volume) <- all.dates.char
  rownames(adj.close) <- all.dates.char
  
  # Split the start and end dates to be used in the ULR later on
  splt <- unlist(strsplit(start.date, "-"))
  a <- as.character(as.numeric(splt[2])-1)
  b <- splt[3]
  c <- splt[1]
  
  splt <- unlist(strsplit(end.date, "-"))
  d <- as.character(as.numeric(splt[2])-1)
  e <- splt[3]
  f <- splt[1]
  
  # Create the two out of the three basic components for the URL loading
  str1 <- "http://ichart.finance.yahoo.com/table.csv?s="
  str3 <- paste("&a=", a, "&b=", b, "&c=", c, "&d=", d, "&e=", e, "&f=", f, "&g=d&ignore=.csv", sep="")
  
  # Main loop for all assets
  for (i in seq(1,length(tickers),1))
  {
    str2 <- tickers[i]
    strx <- paste(str1,str2,str3,sep="")
    x <- read.csv(strx)
    
    datess <- as.matrix(x[1])
    
    replacing <- match(datess, all.dates.char)
    open[replacing,i] <- as.matrix(x[2])
    hi[replacing,i] <- as.matrix(x[3])
    low[replacing,i] <- as.matrix(x[4])
    close[replacing,i] <- as.matrix(x[5])
    volume[replacing,i] <- as.matrix(x[6])
    adj.close[replacing,i] <- as.matrix(x[7])
  }
  
  # Name the cols correctly
  colnames(open) <- tickers
  colnames(hi) <- tickers
  colnames(low) <- tickers
  colnames(close) <- tickers
  colnames(volume) <- tickers
  colnames(adj.close) <- tickers
  
  # Return the ouput
  #return(list(open=open, high=hi, low=low, close=close, volume=volume, adj.close=adj.close))
  
  #está retornando apenas as datas e o adj.close, que são o que me interessam, mas tem essas colunas comentadas acima.
  dados = data.frame( adj.close=adj.close)
  datas = row.names(dados)
  dados$datas = datas
  return(dados)
}



#### Adicional criado por Luiz Filho, para baixar vários papéis e dar o merge neles ####

tratamento_nao_tiver_dados <- function(acao) {
  out <- tryCatch(
{
  nrow(data.frame(data.loading(acoes[acao],data_inicio,data_fim)))
},
error=function(cond) {
  return(NA)
},
warning=function(cond) {
  return(NULL)
},
finally={
}
  )    
return(out)
}

#ponha as ações que tenha interesse nesse vetor, ex: c("ABEV3.SA", "ALLL3.SA" ,"BBAS3.SA" ), estou pegando todas as ações que são utilizadas a ibovespa
#que está nesse arquivo acoes.csv
# acoes = paste(as.character(read.csv("acoes.csv",sep=";")[,1])[1:69],".SA",sep="")
# # formato AAAA-MM-DD
# # data_inicio = "2005-11-18"
# # data_fim = "2012-12-16"
# data_inicio = "2008-01-01"
# # data_fim = "2014-06-30"
# data_fim = "2014-12-31"

dados = data.frame(data.loading("^BVSP",data_inicio,data_fim))
colnames(dados) = c("BVSP","datas")
for(acao in 1:length(acoes)){
  if(!is.null(unlist(lapply(acao, tratamento_nao_tiver_dados)))){
    qnt_linhas_sem_NA = nrow(na.omit(data.loading(acoes[acao],data_inicio,data_fim)))
    #estou escolhendo apenas os papéis que tenham pelo menos mais de 1400 dias com dados não nulos entre a faixa temporal que defini anteriormente
    if(qnt_linhas_sem_NA> 1827*.95){
      dados = merge(dados,data.loading(acoes[acao],data_inicio,data_fim),by="datas")
    }
    
  }
}
#uso para saber se o merge dos papéis está sendo representativo, caso não esteja, mudo a comparação com a qnt_linhas_sem_NA
qnt_na = c()
for(i in 1:ncol(dados)){
  qnt_linhas_sem_NA = length(na.omit(dados[,i]))
  qnt_na[i] = qnt_linhas_sem_NA
}
# mean(qnt_na)
# boxplot(qnt_na)

# apenas os dias em comum que nao possuem valores nulos
dados = na.omit(dados)
dados$datas  = as.Date(dados$datas)
#Datas precisam estar da mais recente para a mais antiga
dados = inverte_indices_data_frame(dados)

#Outliers foram corrigidos na epoca que extrai dos dados
#Correcao na formatacao dos dados
# correcao_formatacao_dados = function(coluna,limiar,divisao){
#   coluna = dados[,coluna]
#   for(i in 1:length(coluna)){
#     if(coluna[i] > limiar){
#       #casos extremos, como valor acima de 1 milhao estao sendo removidos, so tem 5 pontos assim, de SBSP3.SA.
#       if(coluna[i] > 1000000){
#         coluna[i] = NA
#       }else{
#         coluna[i] = coluna[i]/divisao
#       }
#     }
#   }
#   return(coluna)
# }
# for(coluna in 2:ncol(dados)){
#   nome_coluna = names(dados)[coluna]
#   dados[,coluna] = correcao_formatacao_dados(coluna=nome_coluna,limiar=500,divisao=1000)
# }

# dados_sem_NA = na.omit(dados)
# par(mfrow=c(4,4))
# for(i in 2:17){
#   plot(dados_sem_NA[,i])
# }
# 
# for(i in 18:33){
#   plot(main=i,dados_sem_NA[,i])
# }
# for(i in 14:59){
#   plot(dados_sem_NA[,i])
# }

#remoção da ação FIBR3.SA
# dados = dados[,-27]
# plot(main = names(dados)[27],dados_sem_NA[,27])


# 
qnt_dias_ano = aggregate(dados$datas,list(format( dados$datas,"%Y")),FUN=length)
colnames(qnt_dias_ano) = c("Ano","qnt_dias")
qnt_dias_ano
dados_teste = dados[format( dados$datas,"%Y")>=2008,]
dados_teste = na.omit(dados_teste)
qnt_dias_ano = aggregate(dados_teste$datas,list(format( dados_teste$datas,"%Y")),FUN=length)
colnames(qnt_dias_ano) = c("Ano","qnt_dias")
qnt_dias_ano
sum(qnt_dias_ano$qnt_dias)
sum(qnt_dias_ano$qnt_dias)/1827
#Salvo em um .csv
# write.table(file="papeis_da_ibovespa_2008_a_2014_2_com_IBOVESPA.csv",dados_teste,sep=",",row.names=F,quote=T)
# write.table(file="papeis_da_ibovespa_2008_a_2014_com_IBOVESPA.csv",dados_teste,sep=",",row.names=F,quote=T)
# write.table(file="papeis_da_ibovespa_2008_a_2014_com_97_5_IBOVESPA.csv",dados_teste,sep=",",row.names=F,quote=T)
write.table(file="papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv",dados,sep=",",row.names=F,quote=T)
# compara_com_atual = read.csv("papeis_da_ibovespa_2008_a_2014_com_95_IBOVESPA.csv")
