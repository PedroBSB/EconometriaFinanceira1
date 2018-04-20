################################################################################################
#################################   Exemplo 4.3    #############################################
################################################################################################
library(quantmod)
library(rugarch)
#Cria um novo ambiente para armazenar os dados
stockData <- new.env() 

#Especifica as datas de interesse
startDate = as.Date("2010-01-01")
endDate = as.Date("2017-12-31")

#Obtêm os dados do ativo PETR4 e PETR3
ativos<-c("PETR4.SA","PETR3.SA")
getSymbols(ativos, src="yahoo",from=startDate,to=endDate)

#Plota a série temporal dos preços de fechamento
plot.xts(PETR4.SA$PETR4.SA.Close,major.format="%b/%d/%Y",
         main="PETR4.SA.",ylab="Close price.",xlab="Time")

#Calcula o log-retorno.
diffPETR4.SA<-na.omit(diff(log(PETR4.SA)))

#Plota a série temporal dos preços de fechamento
plot.xts(diffPETR4.SA$PETR4.SA.Close,major.format="%b/%d/%Y",
         main="PETR4.SA.",ylab="Log-return Close price.",xlab="Time")

#Retorno
ret<-diffPETR4.SA$PETR4.SA.Close

#Autocorrelacao
acf(ret)

#Testando Estacionariedade
tseries::adf.test(ret, alternative="stationary")

#Especificação
spec1<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = NULL), 
           mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = NULL), 
           distribution.model = "norm")
           
#Estima os parâmetros
fit1<-ugarchfit(data=ret,spec=spec1)
plot(fit1, which = "all")

#Equação da média
plot(x = seq(1,nrow(ret)), y = ret, type = "l",  col = "steel blue",
     xlab = "Dia", ylab = "Returno/Volatilidade", main = "PETR4 retornos")
lines(x = seq(1,nrow(ret)), y = fit1@fit$fitted, col = "green") # Ajuste do MA(1)
lines(x = seq(1,nrow(ret)), y = 2*fit1@fit$sigma, col = "red") # +2 sigma(t), aproximadamente upper 95% confidence line
lines(x = seq(1,nrow(ret)), y = -2*fit1@fit$sigma, col = "red") # -2 sigma(t), aproximadamente lower 95% confidence line
legend("topright", legend = c("Retorno", "Ajustado", "+/-2 sigma(t)"),
       lty = 1, col = c("steel blue", "green", "red"))

#Equação da volatilidade
plot(x = seq(1,nrow(ret)), y = fit1@fit$z, type = "l",  col = "red", xlab = "Day",
     ylab = "Resíduos padronizados", main = "PETR4 resíduos padronizados")
abline(a = 2, b = 0, col = "blue") # aproximadamente upper 95% confidence interval line
abline(a = 0, b = 0, col = "light gray") # horizontal line to origin
abline(a = -2, b = 0, col = "blue") # aproximadamente lower 95% condidence interval line
legend("bottomright", legend = c("Resíduos padronizados (z)", "Appox 95% limite"),
       lty = "solid", col = c("red", "blue"))

#Interpreta os parãmetros
#Alpha -  short run presistence
#Beta -  long run persitence
#omega - unconditional variance
fit1@fit$matcoef

#Forecast
previsao <- ugarchforecast(fit1, n.ahead = 10)

#Volatility
sigma(previsao)

#Ajustado
fitted(previsao)

################################################################################################
#################################   Exemplo 4.4    #############################################
################################################################################################

#VaR histórico:
quantile(ret,probs=0.05)


#VaR por meio do GARCH
alpha <- 0.05
VaR1 <- as.numeric(quantile(fit1, probs = alpha)) 

#Prevendo o VaR para o futuro

#Passo 1: Resíduo padronizado
sresi<-fit1@fit$z

#Passo2: Obtem-se sigma_t+1 e mu_t+1
mu.t1 <- previsao@forecast$seriesFor[1]
sigma.t1<-previsao@forecast$sigmaFor[1]

#Passo3: Para o período t+1 gera-se um conjunto de Retornos
uniforme<-ceiling(runif(1000)*length(sresi))
Retornos<-(sresi[uniforme]*sigma.t1)+mu.t1

#Passo4: Calcular o VaR
quantile(Retornos, 0.05)
