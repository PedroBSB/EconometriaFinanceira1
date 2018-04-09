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


################################################################################################
#################################   Exemplo 3.6    #############################################
################################################################################################
#Lê os dados
library(tidyverse)
library(tibbletime)
vinhos.tbl<-readr::read_delim("Data/Vinho.txt",delim=" ", col_names = FALSE)
glimpse(vinhos.tbl) 

#Renomeia as variáveis
vinhos.tbl <- vinhos.tbl %>%  
  rename(Ano = X1,
         Mes = X2,
         Kilolitros = X3)
#Cria a data
vinhos.tbl <- vinhos.tbl %>% 
  mutate(Data = as.Date(paste0("01-",Mes,"-",Ano) , format = "%d-%m-%Y"))

#Transforma o objeto em uma série temporal
serie <- as_tbl_time(vinhos.tbl, index = Data) 

#Faz o gráfico
serie %>% ggplot(aes(x=Data,y=Kilolitros)) +
  geom_line(color="royalblue")+
  labs(x="Data",y="Kilolitros",title="Venda de vinhos em Kilolitros",
       caption="Fonte: Brockwell e Davis (2002)")+
  theme(plot.caption=element_text(hjust=0),
        plot.title=element_text(face="bold",size=18))

#Remove tendência e sazonalidade
vinhosComp.ts <- ts(serie$Kilolitros, frequency=12)
vinhosComp <- decompose(vinhosComp.ts, type = "additive")

#Testando Estacionariedade
tseries::adf.test(na.omit(vinhosComp$random), alternative="stationary")

#Buscando a parametrização
auto.arima(na.omit(vinhosComp$random), stepwise=FALSE)

#Comparando modelos
fit1<-arima(na.omit(vinhosComp$random), order = c(4, 0, 0), seasonal = c(1,0,0))
accuracy(fit1)

#Previsão:
yfor<-predict(fit1,n.ahead = 20)


################################################################################################
#################################   Exemplo 3.7    #############################################
################################################################################################

#Biblioteca
library(fpp)
library(forecast)
library(TSA)
#Dados
data(ausbeer)
#Pega só uma parte da série
timeserie_beer <- ausbeer
#Plota a série
plot(as.ts(timeserie_beer))
#Estima a tendência por Médias Móveis
trend_beer <- ma(timeserie_beer, order = 4, centre = T)
#Plota a tendência e a série
plot(as.ts(timeserie_beer))
lines(trend_beer)
plot(as.ts(trend_beer))
#Remove a tendência
detrend_beer <- timeserie_beer - trend_beer
plot(as.ts(detrend_beer))

#Removendo a Sazonalidade (Método 1: Análise Espectral)
p <- periodogram(na.omit(detrend_beer))
#Plota o periodograma (pico em 0.25 Hz)
plot(p)
#Pega os maiores picos
dd <- data.frame(freq=p$freq, spec=p$spec)
order <- dd[order(-dd$spec),]
top2 <-  head(order, 2)
top2
#Converte a frequência para período
periodo <- 1/top2$f
periodo
per<-periodo[1]

#Separa os dados
y<-as.numeric(na.omit(detrend_beer))
t<-seq(1,length(y))

#Modela série fourier
reslm <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t))
summary(reslm)
plot(y~t,type="l")
lines(fitted(reslm)~t,col=4,lty=2)  

#Ruído 1:
noise1 <- residuals(reslm)
plot(noise1,type="l")

#Removendo a Sazonalidade (Método 2: Diferenciação da média)
m_beer <- t(matrix(data = detrend_beer, nrow = 4))
seasonal_beer <- colMeans(m_beer, na.rm = T)
plot(as.ts(rep(seasonal_beer,16)))
#Ruído
noise2 <- timeserie_beer - trend_beer - seasonal_beer
plot(as.ts(noise2))

#### Modela o ruído
#Testando Estacionariedade
tseries::adf.test(na.omit(noise1), alternative="stationary")

#Buscando a parametrização
auto.arima(na.omit(noise1), stepwise=FALSE)

#Comparando modelos
fit3<-arima(na.omit(noise1), order = c(3, 0, 2),  include.mean = TRUE)
coeftest(fit3)

#Previsão (Ruído):
yfor<-predict(fit3,n.ahead = 20)
serie.noise<-yfor$pred
serie.noise<-c(noise1,serie.noise)

#Previsão (Sazonalidade)
t<-seq(1,length(y)+20)
sazo.x<-data.frame(sin(2*pi/per*t)+cos(2*pi/per*t))
sazo.pred <-predict(reslm,newdata=sazo.x)

#Previsão (Tendência)''
tenden.pred<-rep(trend_beer[207],20)
tenden.pred<-c(trend_beer[1:207],tenden.pred)

#Previsão (Final)
final<-tenden.pred+sazo.pred+serie.noise
plot(final,type="l",col="red")
lines(as.numeric(timeserie_beer))



################################################################################################
#################################   Exemplo 3.8    #############################################
################################################################################################

library(gmm)
data(Finance) #help(Finance)
df.fin <- data.frame(rm=Finance[1:500,"rm"], rf=Finance[1:500,"rf"])

#Estacionariedade
tseries::adf.test(df.fin[,1], alternative="stationary")
tseries::adf.test(df.fin[,2], alternative="stationary")
df.fin$rf2<-c(NA,diff(df.fin$rf))
tseries::adf.test(na.omit(df.fin[,3]), alternative="stationary")

#Modelo 1 (rejeita a hipótese nula de subreidentificabilidade):
fit1 <- gmm(rm ~ rf2, ~rf2, data=df.fin)
summary(fit1)

#Autocorrelação:
dwtest(fit1)

df.fin<-na.omit(df.fin)
fit2<-gmm(rm ~ rf2, ~rf2, data=df.fin, kernel="Bartlett", bw=bwNeweyWest)
summary(fit2)
dwtest(fit2)

#Modelo 2
g <- function(theta, x) {
  m.1 <- x[,"rm"] - theta[1] - theta[2]*x[,"rf"]
  m.z <- (x[,"rm"] - theta[1] - theta[2]*x[,"rf"])*x[,"rf"]
  f <- cbind(m.1, m.z)
  return(f)
}
fit3<-gmm(g, df.fin, t0=c(0,0), method = "BFGS", control=list(fnscale=1e-8))
summary(fit3)



