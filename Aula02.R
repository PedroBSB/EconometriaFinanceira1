rm(list=ls())
library(tibbletime)
library(tidyverse)

################################################################################################
#################################   Exemplo 2.3    #############################################
################################################################################################
#Lê os dados
eletr.tbl<- read.table('Data\\elec.txt', skip = 11, header = FALSE, sep =' ')
glimpse(eletr.tbl) 

#Renomeia as variáveis
eletr.tbl <- eletr.tbl %>%  
                rename(Ano = V1,
                       Mes = V2,
                       KW = V3)
#Cria a data
eletr.tbl <- eletr.tbl %>% 
              mutate(Data = as.Date(paste0("01-",Mes,"-",Ano) , format = "%d-%m-%Y"))

#Transforma o objeto em uma série temporal
serie <- as_tbl_time(eletr.tbl, index = Data) 

#Faz o gráfico
serie %>% ggplot(aes(x=Data,y=KW)) +
               geom_line(color="royalblue")+
               labs(x="Data",y="KW",title=" Monthly Australian electricity production (10^6 KWH)",
               caption="Fonte: Brockwell e Davis (2002)")+
               theme(plot.caption=element_text(hjust=0),
               plot.title=element_text(face="bold",size=18))

#Autocorrelation
plot(acf(serie$KW), ci = 0.99)


################################################################################################
#################################   Exemplo 2.4    #############################################
################################################################################################

#Gera um AR(1)
serie<-arima.sim(model=list(Ar=-0.5),n=1000)

#Renomeia as variáveis
plot(serie,type="l")

#Separa os dados
serie.train<-serie[1:950]
serie.valid<-serie[950:1000]

#Acha os parâmetros do preditor linear
mu<-mean(serie.train)
n<-length(serie.train)

#Gamma
auto<-acf(serie,lag.max = n, type="covariance")
Gamma<-matrix(NA,n+1,n+1)
Gamma[1,]<-auto$acf
for(i in 2:n){
  temp<-Gamma[1,1:i]
  t1<-rev(temp)
  Gamma[i,]<-Gamma[1,]
  Gamma[i,1:i]<-t1
}
Gamma0<-Gamma[-(n+1),-(n+1)]

#Sistema linear
h<-1
gamma.h<-Gamma[1,2:(n+1)]
a<-solve(Gamma0,gamma.h)

#Forecast
prev<- mu+(t(serie.train)-mu)%*%a
prev

#Forecast usando as funções
ts.plot(serie.train)
fit<- arima(serie.train,order=c(1,0,0))
AR_forecast <- predict(fit, n.ahead = 1)$pred
AR_forecast_se <- predict(fit, n.ahead = 1)$se


################################################################################################
#################################   Exemplo 2.8    #############################################
################################################################################################
library(forecastSNSTS)

#Coeficientes preditos
phi<-predCoef(serie.train,1,50,950,0)
phi.vec<-as.numeric(phi$coef)
forecast<-t(phi.vec)%*%serie.train[901:950]
