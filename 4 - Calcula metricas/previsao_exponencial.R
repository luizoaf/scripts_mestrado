setwd("C:/Users/V1d4 L0k4/Dropbox/IJCNN 2015/scripts_mestrado/4 - Calcula metricas")
eixo_x_frequencias = funcao_distribuicao_probabilidade(serie)$valor_serie_retorno_eixo_x
alvo = funcao_distribuicao_probabilidade(serie)$frequencia_eixo_y

freq_alvo = data.frame( x = eixo_x_frequencias,y = alvo)
funcao_exp <- function(b,x,a) {a*exp(-b*x) }

mod <- nlsLM(y ~ funcao_exp(b,x,a), data = freq_alvo, 
             start = list(a=0.1,b = 0.1),algorithm = "LM",
             control = list(maxiter = 500,
                            tol = 1e-05, minFactor = 0.0009765625, printEval = F, 
                            warnOnly = FALSE, trace = T))
previsao = predict(mod, list(x = freq_alvo$x))
sse = sum(( previsao-alvo)^2)
a = coef(mod)[1]
coeficiente_B = coef(mod)[2]
volatilidade = calcula_volatilidade(serie)

# plot_previsao_com_B_e_exponencial(nome_do_setor,eixo_x_frequencias,previsao)