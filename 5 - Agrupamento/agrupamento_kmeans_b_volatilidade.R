setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/5 - Agrupamento")
dados = read.csv("../3 - Janelamento/Setor/metricas_setores_sem_series_temporais.csv")
k = 3

retorna_cluster = function(dados,k){
  iter = 45
  agrupamento = dados[,c("coeficiente_B","volatilidade")]
  km = kmeans (x = agrupamento, centers = k, iter.max = iter)
  agrupamento$cluster = km$cluster
  grupos = unique(km$cluster)
  #   print(grupos)
  dados$cluster = agrupamento$cluster
  dados$risco_b = ""
  dados$risco_b[dados$cluster == grupos[1]] = "arrojado"
  dados$risco_b[dados$cluster == grupos[2]] = "moderado"
  dados$risco_b[dados$cluster == grupos[3]] = "conservador"
  
  dados$cor = ""
  dados$cor[dados$risco_b =="moderado"] = "black"
  dados$cor[dados$risco_b == "arrojado"] = "red"
  dados$cor[dados$risco_b == "conservador"] = "green"
  
  #   legenda = c("conservador","moderado","arrojado")
  #   plot(main= paste("Para K = ",k,sep =""),agrupamento$coeficiente_B~agrupamento$volatilidade,xlab="Volatility",ylab="Coefficient B", col = dados$cor,pch = 20, cex = 0.9)
  #   points(km$centers[,1]~km$centers[,2],col=4, pch = 8,lwd=2)
  #   legend("topright", inset=.05,legenda , lwd= 3,col =c("green","black","red") , horiz=TRUE)
  centroides = as.data.frame(km$centers)
  centroides = centroides[order(centroides$volatilidade,decreasing=T),]
  centroides = cbind(centroides,data.frame(risco = c("arrojado","moderado","conservador")))
  #   centroides = cbind(centroides,data.frame(risco = c("arrojado","conservador")))
  centroides$cor = ""
  centroides$cor[centroides$risco == "conservador"] = "green"
  centroides$cor[centroides$risco == "moderado"] = "black"
  centroides$cor[centroides$risco == "arrojado"] = "red"
  
  #   plot( main=periodo,centroides$coeficiente_B~centroides$volatilidade,col=centroides$cor, pch = 8,lwd=3,xlim=c(0,2.5),ylim=c(0,4.5))
  #   plot( centroides$coeficiente_B~centroides$volatilidade,col=centroides$cor, pch = 8,lwd=3,xlim=c(0,2.5),ylim=c(0,4.5))
  
  png("imagem/agrupamento_kmeans.png",bg="transparent")
  plot(main= paste("Para K = ",k,sep =""),agrupamento$coeficiente_B~agrupamento$volatilidade,xlab="Volatility",ylab="Coefficient B", col = dados$cor,pch = 20, cex = 0.9)
  points(centroides$volatilidade,centroides$coeficiente_B,col=centroides$cor,pch = 8,lwd= 7)
  dev.off()
  return(centroides)
}

centroides = retorna_cluster(dados,3)

pontos_novos = dados
novos_pontos_classificados = data.frame()
ponto_estudado = 1
for(ponto_estudado in 1:nrow(dados)){
  ponto_centroide = 1:k # os 3 possiveis centroides dos riscos
  distancia = sqrt((pontos_novos$volatilidade[ponto_estudado] - centroides$volatilidade[ponto_centroide])^2 + (pontos_novos$coeficiente_B[ponto_estudado] - centroides$coeficiente_B[ponto_centroide])^2)
  todas_distancias = data.frame(distancias = distancia,risco =centroides$risco)
  indice_menor_distancia = which.min(todas_distancias$distancias)
  risco = as.character(todas_distancias$risco[indice_menor_distancia])
  agrupamento = cbind(pontos_novos[ponto_estudado,],risco)
  distancia_euclidiana = distancia[indice_menor_distancia]
  agrupamento = cbind(agrupamento,distancia_euclidiana)
  
  # calcula SSE de B e volatilidade
  #   alvo_volatilidade = centroides$volatilidade[ponto_centroide][indice_menor_distancia]
  #   alvo_B = centroides$coeficiente_B[ponto_centroide][indice_menor_distancia]
  
  #   volatilidade = pontos_novos$volatilidade[ponto_estudado]
  #   coeficiente_b = pontos_novos$coeficiente_B[ponto_estudado]
  
  #   sse_volatilidade = sum(( volatilidade-alvo_volatilidade)^2)
  #   sse_coeficiente_b = sum(( coeficiente_b-alvo_B)^2)
  
  #   agrupamento = cbind(agrupamento,sse_volatilidade)
  #   agrupamento = cbind(agrupamento,sse_coeficiente_b)
  #   agrupamento = cbind(agrupamento,periodo)
  novos_pontos_classificados = rbind(novos_pontos_classificados,agrupamento)
}
novos_pontos_classificados$cor = ""
novos_pontos_classificados$cor[novos_pontos_classificados$risco == "moderado"] = "black"
novos_pontos_classificados$cor[novos_pontos_classificados$risco == "arrojado"] = "red"
novos_pontos_classificados$cor[novos_pontos_classificados$risco == "conservador"] = "green"


write.table(novos_pontos_classificados,file="agrupamento_distancias_b_e_volatilidade.csv",row.names=F)
write.table(centroides,file="centroides_b_e_volatilidade.csv",row.names=F)

# print(paste("Soma da Dist. Euclidiana:",sum(agrupamento$distancia_euclidiana^2)/2))
# agrupamento_periodo = merge(novos_pontos_classificados,teste, by = intersect(names(novos_pontos_classificados), names(teste)))
# novos_pontos_classificados_com_setores = rbind(novos_pontos_classificados_com_setores,agrupamento_periodo)
















# faixa_temporal = unique(dados$tempo)
novos_pontos_classificados_com_setores = data.frame()
todos_centroides = data.frame()
# periodos = rep(2008:2014,each = 3)
for( periodo in faixa_temporal){
  #   periodo = 2008
  faixa_temporal_teste = periodo
  faixa_temporal_treino = setdiff(faixa_temporal, periodo)
  
  teste = subset(dados,dados$tempo == faixa_temporal_teste)
  treino = subset(dados,dados$tempo %in% faixa_temporal_treino)
  png(paste(periodo,".png",sep=""))
  centroides = retorna_cluster(periodo,treino,k)
  todos_centroides = rbind(todos_centroides,centroides)
  # 
  # treino = subset(dados,dados$tempo< 2014)
  # teste = subset(dados,dados$tempo == 2014)
  # centroides = retorna_cluster(treino,3)
  # pontos_novos = data.frame(volatilidade = c(0.9,1.1),coeficiente_B = c(1.50,0.8))
  pontos_novos = teste[,c("volatilidade","coeficiente_B")]
  #     points(pontos_novos$coeficiente_B~pontos_novos$volatilidade,col="violet")")
  
  novos_pontos_classificados = data.frame()
  #     ponto_estudado = 1# indices de todos os pontos
  
  for(ponto_estudado in 1:nrow(pontos_novos)){
    
    ponto_centroide = 1:k # os 3 possiveis centroides dos riscos
    distancia = sqrt((pontos_novos$volatilidade[ponto_estudado] - centroides$volatilidade[ponto_centroide])^2 + (pontos_novos$coeficiente_B[ponto_estudado] - centroides$coeficiente_B[ponto_centroide])^2)
    todas_distancias = data.frame(distancias = distancia,risco =centroides$risco)
    indice_menor_distancia = which.min(todas_distancias$distancias)
    risco = as.character(todas_distancias$risco[indice_menor_distancia])
    agrupamento = cbind(pontos_novos[ponto_estudado,],risco)
    distancia_euclidiana = distancia[indice_menor_distancia]
    agrupamento = cbind(agrupamento,distancia_euclidiana)
    
    # calcula SSE de B e volatilidade
    alvo_volatilidade = centroides$volatilidade[ponto_centroide][indice_menor_distancia]
    alvo_B = centroides$coeficiente_B[ponto_centroide][indice_menor_distancia]
    
    volatilidade = pontos_novos$volatilidade[ponto_estudado]
    coeficiente_b = pontos_novos$coeficiente_B[ponto_estudado]
    
    sse_volatilidade = sum(( volatilidade-alvo_volatilidade)^2)
    sse_coeficiente_b = sum(( coeficiente_b-alvo_B)^2)
    
    agrupamento = cbind(agrupamento,sse_volatilidade)
    agrupamento = cbind(agrupamento,sse_coeficiente_b)
    agrupamento = cbind(agrupamento,periodo)
    novos_pontos_classificados = rbind(novos_pontos_classificados,agrupamento)
  }
  print(paste("Ano: ",periodo,"Soma da Dist. Euclidiana:",sum(agrupamento$distancia_euclidiana^2)/2))
  agrupamento_periodo = merge(novos_pontos_classificados,teste, by = intersect(names(novos_pontos_classificados), names(teste)))
  novos_pontos_classificados_com_setores = rbind(novos_pontos_classificados_com_setores,agrupamento_periodo)
  
  agrupamento_periodo$cor = ""
  agrupamento_periodo$cor[agrupamento_periodo$risco == "moderado"] = "black"
  agrupamento_periodo$cor[agrupamento_periodo$risco == "arrojado"] = "red"
  agrupamento_periodo$cor[agrupamento_periodo$risco == "conservador"] = "green"
  points(agrupamento_periodo$coeficiente_B~agrupamento_periodo$volatilidade,col=agrupamento_periodo$cor)
  dev.off()
  #   print(teste)
  #   print(treino)
  #   break
  
}
todos_centroides = cbind(todos_centroides,periodos)

# # novos_pontos_classificados = data.frame(pontos_novos$volatilidade)
# 
# novos_pontos_classificados$cor = ""
# #   dados$cor[dados$cluster == grupos[1]] = "black"
# #   dados$cor[dados$cluster == grupos[2]] = "green"
# #   dados$cor[dados$cluster == grupos[3]] = "red"
# novos_pontos_classificados$cor[novos_pontos_classificados$risco == "moderado"] = "black"
# novos_pontos_classificados$cor[novos_pontos_classificados$risco == "arrojado"] = "red"
# novos_pontos_classificados$cor[novos_pontos_classificados$risco == "conservador"] = "green"
# 
# points(novos_pontos_classificados$volatilidade,novos_pontos_classificados$coeficiente_B,col=novos_pontos_classificados$cor,lwd=1)
# # aggregate(dados$a,list(dados$tempo),FUN=length)

novos_pontos_classificados_com_setores = novos_pontos_classificados_com_setores[,-10]#remove i
novos_pontos_classificados_com_setores = novos_pontos_classificados_com_setores[,-10]#remove tempo, fica só periodo(teste)

novos_pontos_classificados_com_setores$cor = ""
novos_pontos_classificados_com_setores$cor[novos_pontos_classificados_com_setores$risco == "moderado"] = "black"
novos_pontos_classificados_com_setores$cor[novos_pontos_classificados_com_setores$risco == "arrojado"] = "red"
novos_pontos_classificados_com_setores$cor[novos_pontos_classificados_com_setores$risco == "conservador"] = "green"
plot(main="Agrupamento por B e volatilidade",novos_pontos_classificados_com_setores$volatilidade,novos_pontos_classificados_com_setores$coeficiente_B,col=novos_pontos_classificados_com_setores$cor,las=1,xlab="Volatility",ylab="Coefficient B")

# points(novos_pontos_classificados_com_setores$coeficiente_B~novos_pontos_classificados_com_setores$volatilidade,col=novos_pontos_classificados_com_setores$cor)
# write.table(novos_pontos_classificados_com_setores,file="agrupamento_sse_b_e_volatilidade_k_2.csv",row.names=F)
write.table(novos_pontos_classificados_com_setores,file="agrupamento_sse_b_e_volatilidade.csv",row.names=F)
write.table(todos_centroides,file="centroides.csv",row.names=F)
