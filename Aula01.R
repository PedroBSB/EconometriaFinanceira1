#Exemplo 1.2
library(quantmod)
library(dplyr)

##### Faz o download dos dados
#Microsoft
MSFT<-as.data.frame(getSymbols("MSFT", from="2000-01-03",to="2007-08-27", auto.assign=F))
MSFT$Time<-rownames(MSFT)
#National Western Life Insurance Company (NWL)
NWL<-as.data.frame(getSymbols("NWL", from="2000-01-03",to="2007-08-27", auto.assign=F))
NWL$Time<-rownames(NWL)
#SP500
SP500<-as.data.frame(getSymbols("^GSPC", from="2000-01-03",to="2007-08-27", auto.assign=F))
SP500$Time<-rownames(SP500)

##### Calcula o retorno di�rio
MSFT.ret <- MSFT %>%
  mutate(MSFT = c(NA,diff(log(MSFT.Adjusted)))) %>%
  select(Time, MSFT)
NWL.ret <- NWL %>%
  mutate(NWL = c(NA,diff(log(NWL.Adjusted)))) %>% 
  select(Time, NWL)
SP500.ret <- SP500 %>%
  mutate(SP500 = c(NA,diff(log(GSPC.Adjusted)))) %>% 
  select(Time, SP500)

##### Junta os dados
dados.ret<- MSFT.ret %>% 
            full_join(NWL.ret, by="Time") %>% 
            full_join(SP500.ret, by="Time")


#Modelo de um fator
mod1<-lm(MSFT~1+SP500, data = dados.ret)
print(summary(mod1),digits=8)

#Uma vez que alpha=-0.00029 caso o coeficiente fosse significante, poder�amos afirmar que em m�dia a uma subperformance anual de
# -0.00029*12*100 = -0.348%. Possui um risco sistem�tico alto uma vez que beta= 1.2470, o qual 
#� maior do que 1. O risco espec�fico expresso em volatilidade anual �
#0.01659789*sqrt(360)*100=31.49%


mod2<-lm(NWL~1+SP500, data = dados.ret)
print(summary(mod2),digits=8)

#Uma vez que alpha=0.00009 caso o coeficiente fosse significante, poder�amos afirmar que em m�dia a uma sobreperformance anual de
# 0.00009*12*100 = 0.108%. Possui um risco sistem�tico baixo uma vez que beta= 0.6339, o qual 
#� muito menor do que 1. O risco espec�fico expresso em volatilidade anual �
#0.01700293*sqrt(360)*100=32.26%