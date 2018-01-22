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

##### Calcula o retorno diário
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

#Uma vez que alpha=-0.00029 caso o coeficiente fosse significante, poderíamos afirmar que em média a uma subperformance anual de
# -0.00029*12*100 = -0.348%. Possui um risco sistemático alto uma vez que beta= 1.2470, o qual 
#é maior do que 1. O risco específico expresso em volatilidade anual é
#0.01659789*sqrt(360)*100=31.49%


mod2<-lm(NWL~1+SP500, data = dados.ret)
print(summary(mod2),digits=8)

#Uma vez que alpha=0.00009 caso o coeficiente fosse significante, poderíamos afirmar que em média a uma sobreperformance anual de
# 0.00009*12*100 = 0.108%. Possui um risco sistemático baixo uma vez que beta= 0.6339, o qual 
#é muito menor do que 1. O risco específico expresso em volatilidade anual é
#0.01700293*sqrt(360)*100=32.26%
