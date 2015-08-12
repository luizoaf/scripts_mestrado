require(ggplot2)
combinacoes_b_volatilidade = read.csv("combinacoes_ic_b_volatilidade.csv")

combinacoes_b_volatilidade = combinacoes_b_volatilidade[order(combinacoes_b_volatilidade$maximo,decreasing = T),]
combinacoes_b_volatilidade = tail(combinacoes_b_volatilidade,1)


combinacoes_volatilidade = read.csv("combinacoes_ic_volatilidade.csv")
combinacoes_volatilidade = combinacoes_volatilidade[order(combinacoes_volatilidade$maximo,decreasing = T),]
combinacoes_volatilidade = tail(combinacoes_volatilidade,1)


combinacoes_b = read.csv("combinacoes_ic_b.csv")
combinacoes_b = combinacoes_b[order(combinacoes_b$maximo,decreasing = T),]
combinacoes_b = tail(combinacoes_b,1)


dados = rbind(combinacoes_b_volatilidade,combinacoes_volatilidade)
dados = rbind(dados,combinacoes_b)
dados$metodo = c("b_volatilidae","volatilidade","b")
colunas = names(dados)
colunas[7]="SSE"
colnames(dados) = colunas
write.csv(file=paste(setor,"_comparacoes.csv",sep=""),dados)

png(paste(setor,"_comparacoes.png",sep=""))
print(ggplot(dados, aes(x=metodo,y=SSE)) + geom_errorbar(aes(ymin = minimo, ymax = maximo),width=.2))
dev.off()
