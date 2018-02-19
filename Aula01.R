rm(list=ls())
#Exemplo 1.1
library(tibbletime)
library(tidyverse)

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

vinhos.tbl %>% ggplot(aes(x=Data,y=Kilolitros)) +
               geom_line(color="royalblue")+
               labs(x="Data",y="Kilolitros",title="Venda de vinhos em Kilolitros",
               caption="Fonte: Brockwell e Davis (2002)")+
               theme(plot.caption=element_text(hjust=0),
               plot.title=element_text(face="bold",size=18))


