require(ggplot2)
require(epiR)
setwd("C:/Users/V1d4 L0k4/Desktop/Tabelas dos experimentos/6 - Modelos inteligentes")
todos_resultados = read.csv("todos_resultados.csv")
todos_setores = unique(todos_resultados$setor)
metricas = c("mape_teste","POCID_teste","mse_teste","nmse_teste","arv_teste")

coeficiente.de.confianca = .95
intervalos<-function(dados){
  ic <- epi.conf(dados,conf = coeficiente.de.confianca)
  return(c(ic$lower,ic$est,ic$upper))
}

analise_metricas_desempenho = function(){
  resultados_metricas = data.frame()
  for(i_metricas in 1:5){
    for(i_setor in 2:8){
      metrica = metricas[i_metricas]
      as.character(todos_setores[i_setor])
      dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i_setor]),]
      volatilidade = dados[dados$variavel_entrada == "Volatilidade",metrica]
      coeficiente_b = dados[dados$variavel_entrada == "Coeficiente B",metrica]
      volatilidade_coeficiente_b = dados[dados$variavel_entrada == "Volatilidade e Coeficiente B",metrica]
      melhor_caso = 1
      resultados_metricas = rbind(resultados_metricas,data.frame(setor = as.character(todos_setores[i_setor]),metrica = metrica,
                                                                 melhor_caso_b=round(min(coeficiente_b)[melhor_caso], 4),
                                                                 melhor_caso_volatilidade=round(min(volatilidade)[melhor_caso], 4),
                                                                 melhor_caso_v_b=round(min(volatilidade_coeficiente_b)[melhor_caso], 4)))
      #     colnames(resultados_metricas) = c("setor","metrica","p_valor")
    }
  }
  print(resultados_metricas)
}

MELHOR_CASO = analise_metricas_desempenho()


write.csv(file="melhores_casos_metricas_desempenho.csv",MELHOR_CASO)









comparacoes = function(comparacao){
  resultados_metricas = data.frame()
  for(i_metricas in 1:5){
    for(i_setor in 2:8){
      metrica = metricas[i_metricas]
      as.character(todos_setores[i_setor])
      dados = todos_resultados[todos_resultados$setor == as.character(todos_setores[i_setor]),]
      volatilidade = dados[dados$variavel_entrada == "Volatilidade",metrica]
      coeficiente_b = dados[dados$variavel_entrada == "Coeficiente B",metrica]
      volatilidade_coeficiente_b = dados[dados$variavel_entrada == "Volatilidade e Coeficiente B",metrica]
      # wilcox.test(volatilidade,volatilidade_coeficiente_b,paired = F)
      if(comparacao == "v_com_b"){
        comparacao_v_B = wilcox.test(volatilidade,coeficiente_b,paired = F,alternative = "g")
      }
      if(comparacao == "v_b_com_v"){
        comparacao_v_B = wilcox.test(volatilidade,volatilidade_coeficiente_b,paired = F,alternative = "g")
      }
      resultados_metricas = rbind(resultados_metricas,data.frame(setor = as.character(todos_setores[i_setor]),metrica = metrica,p_valor=round(comparacao_v_B$p.value, 2)))
      #     colnames(resultados_metricas) = c("setor","metrica","p_valor")
    }
  }
  print(comparacao)
  print(resultados_metricas)
}
comparacoes("v_com_b")
comparacoes("v_b_com_v")
