#-------------------------------------------------#
#                                                 #
#                    TRABALHO                     #
#            ANÁLISE DE SERIES TEMPORAIS          #   
#                                                 #
#-------------------------------------------------#

#-------------------------------------------------#
#                                                 #
#                  BIBLIOTECAS                    #
#                                                 #
#-------------------------------------------------#

library(forecast)
library(readxl)
library(reshape2)
library(tseries)
library(lmtest)
library(tidyverse)
library(dygraphs)
library(forecast)
library(ggtsdiag)
library(ggfortify)


#-------------------------------------------------#
#                                                 #
#                    DADOS                        #
#                                                 #
#-------------------------------------------------#


#Lendo os dados
temp2016 = read.csv(file.choose(), head = T, sep = ",")
attach(temp2016)


temp2016 = ts(temp2016)
temp2016

#Separando os dados de janeiro de 2016 a junho de 2021 pra prever os dados de 
#julho a dezembro de 2021

temp = ts(temp2016[1:66])


#-------------------------------------------------#
#                                                 #
#                  TESTES                         #
#                                                 #
#-------------------------------------------------#


plot(temp, main = "Plot temperatura janeiro de 2016 a junho de 2021",
     ylab = "Temperatura", xlab = 'Tempo')
dygraph(temp, ylab = "",main = 'Temperatura do Planeta Terra Entre 2016 e 2021')
dygraph(
  data = temp,
  main = "Temperatura do Planeta Terra Entre 2016 e 2021",
  xlab = "Mês",
  ylab = "Temperatura do Planeta Terra"
)

#Teste Dick Fuler
#h0: series nao estacionaria
#ha: series estacionaria

#Usando lag 1
adf.test(temp, k = 1)
# Augmented Dickey-Fuller Test
# 
# data:  temp
# Dickey-Fuller = -3.813, Lag order = 1, p-value = 0.02349
# alternative hypothesis: stationary

#Como  temos pvalor < 0.05, rejeitamos h0 em razão de ha e dizemos que a 
#diferença da serie é estacionaria
#Logo vamos considerar estacionária


#vendo correlação
acf(temp,lag.max = 36)

#Notamos os 2 primeiros acima do intervalo de confiança 
tempgraf <- temp
tempgraf %>% ggtsdisplay(main = "Temperatura do Planeta Terra Entre janeiro de 2016 e junho 2021 ")



#-------------------------------------------------#
#                                                 #
#                  MODELOS                        #
#                                                 #
#-------------------------------------------------#

#----------------- MODELO ARIMA 

#modelo auto arima
mod_auto = auto.arima(temp)
summary(mod_auto)

#Modelo 1 ARIMA(1,0,0)
mod1 = arima(temp, order = c(1,0,0), xreg = NULL, include.mean = TRUE)
mod1

#Modelo 2 ARIMA(1,0,1)
mod2 = arima(temp, order = c(2,0,1), xreg = NULL, include.mean = TRUE)
mod2

#-------------------------------------------------#
#                                                 #
#         ESTIMATIVAS ARIMA(1,0,0)                #
#                                                 #
#-------------------------------------------------#


#Teste de Coeficiestes para ver se é significativo
coeftest(mod1)
# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
#   ar1       0.670030   0.092105  7.2746 3.474e-13 *
#   intercept 0.940385   0.039660 23.7114 < 2.2e-16 *
#   ---
#   Signif. codes:  0 '*' 0.001 '*' 0.01 '' 0.05 '.' 0.1 ' ' 1

mod1
# Coefficients:
#         ar1     intercept
#       0.6700     0.9404
# s.e.  0.0921     0.0397
# 
# sigma^2 estimated as 0.01198:  log likelihood = 52.06,  aic = -98.13

#Logo, o primeiro parametro é phi = 0.67, portanto vemos tbm que a 
#serie é estacionaria. Temos tbm que a variancia do ruidoo branco é 0.01198
#E o aic = -98.13


#-------------------------------------------------#
#                                                 #
#         ESTIMATIVAS ARIMA(2,0,1)                #
#                                                 #
#-------------------------------------------------#

#Teste de Coeficiestes para ver se é significativo
coeftest(mod2)
# z test of coefficients:
#   
#   Estimate Std. Error  z value  Pr(>|z|)    
#   ar1        1.638611   0.088492  18.5170 < 2.2e-16 *
#   ar2       -0.683846   0.088713  -7.7085 1.273e-14 *
#   ma1       -1.000000   0.039386 -25.3899 < 2.2e-16 *
#   intercept  0.932339   0.011852  78.6677 < 2.2e-16 *
#   ---
#   Signif. codes:  0 '*' 0.001 '*' 0.01 '' 0.05 '.' 0.1 ' ' 1

mod2
#arima(x = temp, order = c(2, 0, 1), xreg = NULL, include.mean = TRUE)

#Coefficients:
#        ar1      ar2      ma1    intercept
#      1.6386  -0.6838  -1.0000     0.9323
#s.e.  0.0885   0.0887   0.0394     0.0119

#sigma^2 estimated as 0.0112:  log likelihood = 53.41,  aic = -96.81

#Logo, o primeiro parametro é phi_1 = 1.6386, phi_2 = -0.6838 .
#Temos tbm que a variancia do ruido branco é 0.0112
#E o aic = -96.81


#-------------------------------------------------#
#                                                 #
#            RESÍDUOS ARIMA(1,0,0)                #
#                                                 #
#-------------------------------------------------#

# histograma; gráfico probabilístico; testes de normalidade: 
#   Shapiro-Wilk e do Jarque-Bera; gráfico dos resíduos versus ordem e das autocorrelações 
# dos resíduos e verifique adequação dos modelos por meio do teste do Box-Ljung-Pierce 
# (Faça o gráfico até o "lag"=36);
#Residuos
res_temperatura1 = mod1$residuals
hist(res_temperatura1, main = "Histograma dos residuos modelo 1", xlab = "Temperatura", ylab = "Frequencia")
table(mod1$residuals)
summary(dados$idade)

base_mod1 = data.frame(mod1$residuals)
base_mod1$mod1.residuals
residuosmod1 = ggplot(data = base_mod1, aes(x = mod1.residuals)) +
  geom_histogram(fill="lightblue", bins = 7) +
  ggtitle("Histograma dos resíduos do modelo ARIMA (1,0,0)")+
  xlab("Resíduos")+
  ylab("Frequência")
residuosmod1

#QQPLOT com a linha
ggplot(data = base_mod1, aes(sample = mod1.residuals))+
  stat_qq()+
  stat_qq_line(fill="lightblue")+
  ggtitle("QQPLOT dos resíduos do modelo ARIMA (1,0,0)")+
  labs(x="",y="")


#Shapiro test
#H0: provem de pop normal
#Ha: não provem de pop normal
shapiro.test(res_temperatura1)
# Shapiro-Wilk normality test
# 
# data:  res_temperatura1
# W = 0.98777, p-value = 0.7639

#Logo por shapiro, dizemos que é normal


#JK
#H0: provem de pop normal
#Ha: não provem de pop normal
jarque.bera.test(res_temperatura1)

# Jarque Bera Test
# 
# data:  res_temperatura1
# X-squared = 0.71281, df = 2, p-value = 0.7002

#Logo usando jarque bera tambem dizemos que é normal


#Box Pierce
#h0: o ruido é branco
#ha: o ruido nao é branco
Box.test(res_temperatura1)

# Box-Pierce test
# 
# data:  res_temperatura1
# X-squared = 0.021402, df = 1, p-value = 0.8837

#Logo dizemos que o ruido é branco

#As residuos padronizados, autocorrelacoes dos residuos, pvalor do teste box.pierce

ggtsdiag(auto.arima(mod1$residuals))
tsdiag(mod1, gof.lag = 36)

#Como  nenhum pvalor é menor que 0.05% o modelo é um bom modelo


#-------------------------------------------------#
#                                                 #
#            RESÍDUOS ARIMA(2,0,1)                #
#                                                 #
#-------------------------------------------------#


# histograma; gráfico probabilístico; testes de normalidade: 
#   Shapiro-Wilk e do Jarque-Bera; gráfico dos resíduos versus ordem e das autocorrelações 
# dos resíduos e verifique adequação dos modelos por meio do teste do Box-Ljung-Pierce 
# (Faça o gráfico até o "lag"=36);

res_temperatura1 = mod1$residuals
hist(res_temperatura1, main = "Histograma dos residuos modelo 1", xlab = "Temperatura", ylab = "Frequencia")
table(mod1$residuals)
summary(dados$idade)

base_mod2 = data.frame(mod2$residuals)
base_mod2$mod2.residuals
residuosmod2 = ggplot(data = base_mod2, aes(x = mod2.residuals)) +
  geom_histogram(fill="lightblue", bins = 7) +
  ggtitle("Histograma dos resíduos do modelo ARIMA (2,0,1)")+
  xlab("Resíduos")+
  ylab("Frequência")
residuosmod2

#QQPLOT com a linha
ggplot(data = base_mod2, aes(sample = mod2.residuals))+
  stat_qq()+
  stat_qq_line(fill="lightblue")+
  ggtitle("QQPLOT dos resíduos do modelo ARIMA (2,0,1)")+
  labs(x="",y="")


#Shapiro test
#H0: provem de pop normal
#Ha: não provem de pop normal
shapiro.test(res_temperatura2)
# Shapiro-Wilk normality test
# 
# data:  res_temperatura1
# W = 0.98777, p-value = 0.7639

#Logo por shapiro, dizemos que é normal


#JK
#H0: provem de pop normal
#Ha: não provem de pop normal
jarque.bera.test(res_temperatura2)

# Jarque Bera Test
# 
# data:  res_temperatura1
# X-squared = 0.71281, df = 2, p-value = 0.7002

#Logo usando jarque bera tambem dizemos que é normal


#Box Pierce
#h0: o ruido é branco
#ha: o ruido nao é branco
Box.test(res_temperatura2)

# Box-Pierce test
# 
# data:  res_temperatura1
# X-squared = 0.021402, df = 1, p-value = 0.8837

#Logo dizemos que o ruido é branco

#As residuos padronizados, autocorrelacoes dos residuos, pvalor do teste box.pierce
tsdiag(mod2, gof.lag = 36)
ggtsdiag(auto.arima(mod2$residuals))
tsdiag(mod1, gof.lag = 36)
#Como  nenhum pvalor é menor que 0.05% o modelo é um bom modelo


#-------------------------------------------------#
#                                                 #
#            PREVISÃO ARIMA(1,0,0)                #
#                                                 #
#-------------------------------------------------#

prev1 = predict(mod1, n.ahead = 6)
prev1
mes = 1:6



#Julho a Dezembro  de 2021 (valor REAL)

previsao1 = c(0.8731242, 0.8953184, 0.9101892, 0.9201531, 0.9268292, 0.9313023)
errop = prev1$se

prev1$pred

tempo = temp2016[67:72]

errop1 = c( 0.1094490, 0.1317459, 0.1406106, 0.1444134, 0.1460885, 0.1468343)

plot(mes, tempo, type = "o", xlim = c(1,6), ylim = c(0,2),xlab = "Janeiro a Junho", ylab = "Temperatura", main = "Previsão")

prev = data.frame(tempo)


legend(1,1.5,c("predito","limites"), col = c("red",'blue'),lty =c(1,1))
LS1 = previsao1 + 1.96*errop1
LI1 = previsao1 - 1.96*errop1

lines(LS1,col = "blue",type = "o")
lines(LI1,col = "blue",type = "o")
lines(previsao1, col = "red",type = "o")
lines(tempo)


prv = forecast( mod1, level = c(80,95), h =6, title = "a")
plot(prv, main = "Previsão - Modelo ARIMA (1,0,0)")

#Gráfico previsão da temperatura
autoplot(prv, ts.colour = 'black',predict.colour = 'red',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'lightblue') + ggtitle("Previsão Temperatura - Modelo ARIMA (1,0,0)") + labs(x="Meses",y="Temperatura do planeta Terra")


#-------------------------------------------------#
#                                                 #
#            PREVISÃO ARIMA(2,0,1)                #
#                                                 #
#-------------------------------------------------#
prev2 = predict(mod2, n.ahead = 6)
prev2
mes = 1:6


#Julho a Dezembro  de 2021 (valor REAL)

previsao2 = c(0.8800820, 0.9098556, 0.9312333, 0.9459023, 0.9553201, 0.9607209)

tempo = temp2016[67:72]

errop2 = c( 0.1064761, 0.1270503, 0.1334194, 0.1348000, 0.1348270, 0.1349689)

plot(mes, tempo, type = "o", xlim = c(1,6), ylim = c(0,2),xlab = "Janeiro a Junho", ylab = "Temperatura", main = "Previsão")

legend(1,1.5,c("predito","limites"), col = c("red",'blue'),lty =c(1,1))
LS1 = previsao2 + 1.96*errop2
LI1 = previsao2 - 1.96*errop2

lines(LS1,col = "blue",type = "o")
lines(LI1,col = "blue",type = "o")
lines(previsao2, col = "red",type = "o")
lines(tempo)


prv2 = forecast( mod2, level = c(80,95), h =6)
plot(prv)
prv

autoplot(prv2, ts.colour = 'black',predict.colour = 'red',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'lightblue') + ggtitle("Previsão Temperatura - Modelo ARIMA (2,0,1)") + labs(x="Meses",y="Temperatura do planeta Terra")


#-------------------------------------------------#
#                                                 #
#    ERRO QUADRATICO MÉDIO MINIMO DA PREVISÃO     #
#                                                 #
#-------------------------------------------------#

previsao1 = as.numeric(prev1$pred)
previsao2 = as.numeric(prev2$pred)

sum((tempo - previsao1)^2)/6
# 0.003501985

sum((tempo - previsao2)^2)/6
# 0.004577779


