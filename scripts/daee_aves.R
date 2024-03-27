#Estudo de Impacto Ambiental DAEE Governo de SP
 #Guilherme Cavicchioli | setembro de 2023 

#carregando os pacotes###########
library(dplyr)
library(reshape)
library(vegan)
library(ggplot2)
library(xlsx)###Exporta os resultados
library(tidyverse)
library(here)
library(broom)
library(VennDiagram)+
library(lubridate)
library(readxl)
#install.packages("lubridate")
#########Dados do Kobo

# Juntando as planilhas importadas do sheets com "procv" ----      
data_daee<-openxlsx::read.xlsx(here::here("data","kobo_camp2.xlsx"),sheet = 1) #importa os dados
data_sps_daee<-openxlsx::read.xlsx(here::here("data", "kobo_camp2.xlsx"),sheet = 2) #importa os dados


####juntando as tabelas do Kobo Tool Box
dados$data_hora <- as.POSIXct(dados$data_hora, format = "%Y-%m-%d %H:%M:%S")

dados_pontos <- openxlsx::read.xlsx(here::here("data","camp7_ago23", "tijoa.xlsx"),sheet = 1)
dados_sps <- openxlsx::read.xlsx(here::here("data","camp7_ago23", "tijoa.xlsx"),sheet = 2)
names(dados_sps)
kobo <- left_join(data_daee,data_sps_daee, by = c("_uuid"= "_submission__uuid"))

kobo$data <- as.POSIXct(kobo$data, format = "%Y-%m-%d %H:%M:%S", tz="GMT")

# Mudando formato de data para o padrão do R
kobo$`_submission_time` <- format(strptime(as.character(kobo$`_submission_time`)), "%h-%m-%s")
# Transformando a hora pra formato  h:m:s
kobo$`_submission_time` <- parse_hm(as.character(kobo$`_submission_time`))

openxlsx::write.xlsx(kobo,here::here("data","kobo2.xlsx"),
                     rowNames = FALSE, quote = FALSE)



#Importando os dados#####
dados<-openxlsx::read.xlsx(here::here("data","bd_daee.xlsx"),sheet = 1) #importa os dados
dados[,c("value")] <- as.integer(dados[,"value"])#transforma a coluna "value" em números inteiros
dados [is.na(dados)]<-0
names(dados)
bd_daee<- filter(dados, !nid %in% "1", !id_genero %in% "1", !familia %in% "1") # & !Checklist %in% "1") # & H == "F") # # & Campanha %in% c("2015-1","2015-2","2016-1","2016-2","2017-1","2017-2")
names(bd_daee)
tb_daee <- as_tibble(bd_daee)



### Elaborando tabela de Checklist por campanha-------
check <- cast(filter(dados, !NID %in% "1", !Id_genero %in% "1"), Taxon + nome_popular + Habitat + Sens + END + SP + BR + IUCN + Ntax ~ tramo, value = "value", sum) #& !Ponto %in% c("60-3","65-3")

#checklist <- cast(check, Taxon + Nome.popular + Habitat + Sens + END + SP + BR + IUCN+ Ntax ~ campanha, value = "value", sum)
#check <- check[,-ncol(15)]
check[check == 0] <- ""
check[,"Ntax"] <- as.integer(check[,"Ntax"])
check <- arrange(check, Ntax)
check <- subset(check, select = -Ntax)
# library(xlsx)                      
write.xlsx(as.data.frame(check), here::here("data", "check_preliminar.xlsx"), row.names = FALSE)

secundarios <- dplyr::filter(dados,!nid %in% "1", !id_apenas_genero %in% "1", 
                             !familia %in% "1", tecnica == "Dados de Base" )
dados_campo <-dplyr::filter(bd_daee, tecnica == "Lista de Mackinnon")
dados_curva<-cast(dados_campo, Taxon ~ Amostra, value = "value", sum)
