rm(list=ls())
library(tibbletime)
library(tidyverse)

################################################################################################
#################################   Exemplo 1.1    #############################################
################################################################################################
#Lê os dados
vinhos.tbl<-readr::read_delim("Data/Vinho.txt",delim=" ", col_names = FALSE)
glimpse(vinhos.df) 

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



################################################################################################
#################################   Exemplo 1.4    #############################################
################################################################################################

#Simulando o White Noise
Data< -create_series(~'2018', 'daily')
X <- tibble(c(0,rnorm(nrow(Data)-1,5,10)))

#Cria a série
WN <- bind_cols(X, Data) 
colnames(WN) <- c("X","Data")
WN <- as_tbl_time(WN, index = Data) 

#Simulando o passeio aleatório
RW <- WN %>% 
      mutate(S=cumsum(X))

#Faz o gráfico
RW %>% ggplot(aes(x=Data,y=S)) +
  geom_line(color="royalblue")+
  labs(x="Data",y="Passeio aleatório",title="Simulação", caption="Exemplo 1.4 ")

################################################################################################
#################################   Exemplo 1.5    #############################################
################################################################################################

#Vinho
vinhosComp.ts <- ts(serie$Kilolitros, frequency=12)
vinhosComp <- decompose(vinhosComp.ts, type = "additive")
plot(vinhosComp)

#White Noise
WN.ts <- ts(WN$X, frequency=12)
WN <- decompose(WN.ts, type = "additive")
plot(WN)

#Random Walk
RW.ts <- ts(RW$S, frequency=12)
RW <- decompose(RW.ts, type = "additive")
plot(RW)


################################################################################################
#################################   Exemplo 1.10    ############################################
################################################################################################

#Vinho
acf(vinhosComp.ts, lag.max = 10, type = "covariance")
acf(vinhosComp.ts, lag.max = 10, type = "correlation")

#White Noise
acf(WN.ts, lag.max = 10, type = "covariance")
acf(WN.ts, lag.max = 10, type = "correlation")

#Random Walk
acf(RW.ts, lag.max = 10, type = "covariance")
acf(RW.ts, lag.max = 10, type = "correlation")


