################################################################################################
#################################   Exemplo 3.3    #############################################
################################################################################################
#Gera um ARMA(1,0)
serie1<-arima.sim(model=list(Ar=-0.5),n=1000)

#Renomeia as variáveis
plot(serie1,type="l")

#Autocorrelacao
acf(serie1)

#Gera um ARMA(0,1)
serie2<-arima.sim(model=list(Ar=0, Ma=2),n=1000)

#Renomeia as variáveis
plot(serie2,type="l")

#Autocorrelacao
acf(serie2)

#Gera um ARMA(1,1)
serie3<-arima.sim(model=list(Ar=3, Ma=2),n=1000)

#Renomeia as variáveis
plot(serie3,type="l")

#Autocorrelacao
acf(serie3)

#Gera um ARMA(2,0)
serie4<-arima.sim(model=list(Ar=c(3,-4), Ma=0),n=1000)

#Renomeia as variáveis
plot(serie4,type="l")

#Autocorrelacao
acf(serie4)

#Gera um ARMA(0,2)
serie5<-arima.sim(model=list(Ar=0, Ma=c(-0.6,-4)),n=1000)

#Renomeia as variáveis
plot(serie5,type="l")

#Autocorrelacao
acf(serie5)

#Gera um ARMA(1,2)
serie6<-arima.sim(model=list(Ar=3, Ma=c(-0.6,-4)),n=1000)

#Renomeia as variáveis
plot(serie6,type="l")

#Autocorrelacao
acf(serie6)

#Gera um ARMA(2,1)
serie7<-arima.sim(model=list(Ar=c(-0.6,-4), Ma=4),n=1000)

#Renomeia as variáveis
plot(serie7,type="l")

#Autocorrelacao
acf(serie7)

#Gera um ARMA(2,2)
serie8<-arima.sim(model=list(Ar=c(-0.6,-4), Ma=c(4,3)),n=1000)

#Renomeia as variáveis
plot(serie8,type="l")

#Autocorrelacao
acf(serie8)

################################################################################################
#################################   Exemplo 3.3    #############################################
################################################################################################
library(forecast)
fit1<-arima(serie4,order = c(1, 0, 0))
rmse1<-accuracy(fit1)[2]
fit2<-arima(serie4,order = c(2, 0, 0))
rmse2<-accuracy(fit2)[2]
fit3<-arima(serie4,order = c(0, 0, 1))
rmse3<-accuracy(fit3)[2]
fit4<-arima(serie4,order = c(1, 0, 2))
rmse4<-accuracy(fit4)[2]
fit5<-arima(serie4,order = c(1, 0, 1))
rmse5<-accuracy(fit5)[2]
fit6<-arima(serie4,order = c(1, 0, 2))
rmse6<-accuracy(fit6)[2]
fit7<-arima(serie4,order = c(2, 0, 1))
rmse7<-accuracy(fit7)[2]
fit8<-arima(serie4,order = c(2, 0, 2))
rmse8<-accuracy(fit8)[2]
plot(seq(1,8),c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6,rmse7,rmse8),type="l")

################################################################################################
#################################   Exemplo 3.4    #############################################
################################################################################################
library(forecast)
fit1<-arima(serie4,order = c(1, 0, 0))
aic1<-AIC(fit1)
fit2<-arima(serie4,order = c(2, 0, 0))
aic2<-AIC(fit2)[2]
fit3<-arima(serie4,order = c(0, 0, 1))
aic3<-AIC(fit3)[2]
fit4<-arima(serie4,order = c(1, 0, 2))
aic4<-AIC(fit4)[2]
fit5<-arima(serie4,order = c(1, 0, 1))
aic5<-AIC(fit5)[2]
fit6<-arima(serie4,order = c(1, 0, 2))
aic6<-AIC(fit6)[2]
fit7<-arima(serie4,order = c(2, 0, 1))
aic7<-AIC(fit7)[2]
fit8<-arima(serie4,order = c(2, 0, 2))
aic8<-AIC(fit8)[2]
plot(seq(2,8),c(aic2,aic3,aic4,aic5,aic6,aic7,aic8),type="l")

################################################################################################
#################################   Exemplo 3.5    #############################################
################################################################################################
library(sas7bdat)
dados<-read.sas7bdat("Data\\sgunemployment.sas7bdat")
plot(dados$PercentUnemployed,type="l")

#Testando Estacionariedade
tseries::adf.test(dados$PercentUnemployed, alternative="stationary")

#Buscando a parametrização
auto.arima(dados$PercentUnemployed, stepwise=FALSE)

#Comparando modelos
fit1<-arima(dados$PercentUnemployed,order = c(1, 0, 0))
accuracy(fit1)
fit2<-arima(dados$PercentUnemployed,order = c(1, 0, 1))
accuracy(fit2)

#Resultado do modelo
library(lmtest)
coeftest(fit2)

#Fazendo previsão
library(ggplot2)
yfor<-predict(fit2,n.ahead = 20)
yfor<-as.data.frame(yfor)
colnames(yfor)<-c("serie2","SE")
dados<-dplyr::bind_rows(data.frame("serie2"=serie2),yfor)
dados$LI<-dados$serie2-2*dados$SE
dados$LS<-dados$serie2+2*dados$SE
dados$Time<-seq(1,nrow(dados))
ggplot() + 
  geom_line(data = dados[950:1020,], aes(x = Time, y = serie2), color = "red") +
  geom_line(data = dados[950:1020,], aes(x = Time, y = LI), color = "blue") +
  geom_line(data = dados[950:1020,], aes(x = Time, y = LS), color = "blue") +
  xlab('Data') +
  ylab('PercentUnemployed')
