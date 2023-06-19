# Cleaning and organization of Federal Investment database provided by SIGA BRASIL


# Reading Library

library(magrittr)  ;  library(data.table)  ; library(dplyr)    ; library(foreign)  ; library(dplyr)
library(readr)     ;  library(readxl)      ; library(pillar)   ; library(tidyverse); library(tidyr)
library(scales)    ;  library(geobr)       ; library(devtools) ; library(stringr)  ; library(janitor)
library(stringr)   ;  library(BETS)        ; library(stringi)

#library(tidylog)

# Define directory

setwd("D:\\Pedro Jorge\\IPEA\\SIGABRASIL\\")
 
##  Separating the three parts for earch year
 
parte1 <- fread("parte1.csv", dec = ",", fill = T)  %>%
  data.frame(check.names = TRUE)

parte2 <- fread("parte2.csv", dec = ",") %>%
   select(-c(UG.UF)) %>% rename(UG.UF=CEP) %>% 
   data.frame(check.names = TRUE) %>%
   dplyr::rename(Liquidado = Liquidado..Subelemento.,
                 Despesa.Executada = Despesa.Executada..Subelemento.)
parte3 <- fread("parte3.csv", dec = ",") %>% 
   select(-c(UG.UF)) %>% rename(UG.UF=CEP) %>% 
   data.frame(check.names = TRUE) %>%
   dplyr::rename(Liquidado = Liquidado..Favorecido.,
                 Despesa.Executada = Despesa.Executada..Subelemento.)
parte4 <- fread("parte4.csv", dec = ",") %>% 
  data.frame(check.names = TRUE)
parte5 <- fread("2019.csv", dec = ",") %>% 
  data.frame(check.names = TRUE) 
temp <- fread("LOA_2007.csv", dec = ",") %>% 
  select(-c(UG.UF)) %>% rename(UG.UF=CEP) %>% 
  data.frame(check.names = TRUE) %>%
  dplyr::rename(Liquidado = Liquidado..Favorecido.,
                Despesa.Executada = Despesa.Executada..Subelemento.)

full <- rbind(parte1,parte2,parte3,parte4,parte5, use.names=FALSE) %>% 
 filter(Ano!="Ano") %>% 
 mutate(UG.UF = as.numeric(UG.UF))

setwd("D:\\Pedro Jorge\\IPEA\\SIGABRASIL\\SIGABRASIL-2020")

 ano <- 2000
 while(ano!=2020){
   ano = ano + 1
   temp <- full %>% filter(Ano==ano)
   write.csv2(temp, paste0(ano,".csv"), row.names = F)
 }
# 
# 
# 
# # Salve for search municipalities
# 
 write.csv2(full,"full_base.csv",row.names = F)
 
 full <- full %>% select(c(UG.UF)) %>% 
   mutate(CEP  = as.character(UG.UF),
          CEP = ifelse(nchar(CEP)==7,paste0("0",CEP),CEP)) %>% 
   distinct(CEP, .keep_all = T)
 
 write.csv2(full,"full_base_CEP.csv",row.names = F)
# 
rm(full,parte1,parte2,parte3,parte4,parte5)

# DOWNLOAD DATABASE WITH GEOBR PACKAGE

base_referencia <- read_municipality(code_muni = "all", year = 2017) %>% 
  select(-c(geom)) %>% 
  mutate(state = ifelse(code_state== 11,"RONDÔNIA",ifelse(code_state== 12,"ACRE",
                 ifelse(code_state== 13,"AMAZONAS",ifelse(code_state== 14,"RORAIMA",
                 ifelse(code_state== 15,"PARÁ",ifelse(code_state== 16,"AMAPA",
                 ifelse(code_state== 17,"TOCANTINS",ifelse(code_state== 21,"MARANHAO",
                 ifelse(code_state== 22,"PIAUÍ",ifelse(code_state== 23,"CEARÁ",
                 ifelse(code_state== 24,"RIO GRANDE DO NORTE",ifelse(code_state== 25,"PARAÍBA",
                 ifelse(code_state== 26,"PERNAMBUCO",ifelse(code_state== 27,"ALAGOAS",
                 ifelse(code_state== 28,"SERGIPE",ifelse(code_state== 29,"BAHIA",
                 ifelse(code_state== 31,"MINAS GERAIS",ifelse(code_state== 32,"ESPÍRITO SANTO",
                 ifelse(code_state== 33,"RIO DE JANEIRO",ifelse(code_state== 35,"SÃO PAULO",
                 ifelse(code_state== 41,"PARANÁ",ifelse(code_state== 42,"SANTA CATARINA",
                 ifelse(code_state== 43,"RIO GRANDE DO SUL",ifelse(code_state== 50,"MATO GROSSO DO SUL",
                 ifelse(code_state== 51,"MATO GROSSO",ifelse(code_state== 52,"GOIÁS",
                 ifelse(code_state== 53,"DISTRITO FEDERAL",0))))))))))))))))))))))))))),
         state2 =ifelse(code_state== 11,"RONDONIA",ifelse(code_state== 12,"ACRE",
                 ifelse(code_state== 13,"AMAZONAS",ifelse(code_state== 14,"RORAIMA",
                 ifelse(code_state== 15,"PARA",ifelse(code_state== 16,"AMAPA",
                 ifelse(code_state== 17,"TOCANTINS",ifelse(code_state== 21,"MARANHAO",
                 ifelse(code_state== 22,"PIAUI",ifelse(code_state== 23,"CEARA",
                 ifelse(code_state== 24,"RIO GRANDE DO NORTE",ifelse(code_state== 25,"PARAIBA",
                 ifelse(code_state== 26,"PERNAMBUCO",ifelse(code_state== 27,"ALAGOAS",
                 ifelse(code_state== 28,"SERGIPE",ifelse(code_state== 29,"BAHIA",
                 ifelse(code_state== 31,"MINAS GERAIS",ifelse(code_state== 32,"ESPIRITO SANTO",
                 ifelse(code_state== 33,"RIO DE JANEIRO",ifelse(code_state== 35,"SAO PAULO",
                 ifelse(code_state== 41,"PARANA",ifelse(code_state== 42,"SANTA CATARINA",
                 ifelse(code_state== 43,"RIO GRANDE DO SUL",ifelse(code_state== 50,"MATO GROSSO DO SUL",
                 ifelse(code_state== 51,"MATO GROSSO",ifelse(code_state== 52,"GOIAS",
                 ifelse(code_state== 53,"DISTRITO FEDERAL",0))))))))))))))))))))))))))),
        regiao = ifelse(code_state <= 19,"Norte",
                 ifelse(code_state >= 20 & code_state <= 29,"Nordeste",
                 ifelse(code_state >= 30 & code_state <= 39,"Sudeste",
                 ifelse(code_state >= 40 & code_state <= 49,"Sul",
                 ifelse(code_state >= 50,"Centro-Oeste",0))))),
    regiao2 = ifelse(code_state <= 19,"NORTE",
                 ifelse(code_state >= 20 & code_state <= 29,"NORDESTE",
                 ifelse(code_state >= 30 & code_state <= 39,"SUDESTE",
                 ifelse(code_state >= 40 & code_state <= 49,"SUL",
                 ifelse(code_state >= 50,"CENTRO-OESTE",0))))),
    regiao3 = ifelse(code_state <= 19,"NORTE",
                     ifelse(code_state >= 20 & code_state <= 29,"NORDESTE",
                     ifelse(code_state >= 30 & code_state <= 39,"SUDESTE",
                     ifelse(code_state >= 40 & code_state <= 49,"SUL",
                     ifelse(code_state >= 50,"CENTRO OESTE",0))))),
    abbrev_state= ifelse(code_state== 11, "RO",
     ifelse(code_state== 12, "AC",
     ifelse(code_state== 13, "AM",
     ifelse(code_state== 14, "RR",
     ifelse(code_state== 15, "PA",
     ifelse(code_state== 16, "AP",
     ifelse(code_state== 17, "TO",
     ifelse(code_state== 21, "MA",
     ifelse(code_state== 22, "PI",
     ifelse(code_state== 23, "CE",
     ifelse(code_state== 24, "RN",
     ifelse(code_state== 25, "PB",
     ifelse(code_state== 26, "PE",
     ifelse(code_state== 27, "AL",
     ifelse(code_state== 28, "SE",
     ifelse(code_state== 29, "BA",
     ifelse(code_state== 31, "MG",
     ifelse(code_state== 32, "ES",
     ifelse(code_state== 33, "RJ",
     ifelse(code_state== 35, "SP",
     ifelse(code_state== 41, "PR",
     ifelse(code_state== 42, "SC",
     ifelse(code_state== 43, "RS",
     ifelse(code_state== 50, "MS",
     ifelse(code_state== 51, "MT",
     ifelse(code_state== 52, "GO",
     ifelse(code_state== 53, "DF",0))))))))))))))))))))))))))),
    abbrev_state2 = paste0("- ",abbrev_state),
    abbrev_state3 = paste0("-",abbrev_state),
                        abbrev_state4 = paste0(", ",abbrev_state)) 

## Tratar nome dos municípios

base_referencia <- base_referencia %>% 
  mutate(name_muni2 = iconv(base_referencia$name_muni,from="UTF-8",to="ASCII//TRANSLIT"),
         name_muni2 = gsub("'"," ",name_muni2)) %>% 
  mutate_if(is.character, str_to_upper)

base_estados <- read_state(code_state = "all", year = 2017) %>% as.data.frame() %>% 
  select(-c(geom))

## USE 2014 DATABASE WITH PARAMETER -----

Parameter <- read.csv(file = "2014parametro.csv", sep = ";", dec = ",",
                      fileEncoding = "Latin1") %>% 
  mutate(cod.ug = as.numeric(substr(UG..Cod.Desc.,1,7))) %>% 
  select(c(cod.ug,UG.UF))

Parameter <- Parameter[!duplicated(Parameter$cod.ug), ]

## CREATE A FUNCTION FOR MANIPULATING (Steps one, two and three together) ----

siga_brasil <- function(year=NULL){
  
  print(year)
x <- fread(input = paste0(year,".csv"), dec = ",",
           encoding = "Latin-1") %>%
  data.frame(check.names = TRUE) %>% 
  filter(GND..Cod. == 4) %>% 
  mutate(Localidade.UF = ifelse(Localidade.UF %in% c("NE","NO","CO","SD","SL"),"ND",Localidade.UF),
         UF = as.character(Localidade.UF),
         UG..Cod.Desc. = as.character(UG..Cod.Desc.),
         UG..Cod.Desc. = gsub("MATO G.DO SUL","MATO GROSSO DO SUL",UG..Cod.Desc.),
         UG..Cod.Desc. = gsub("MATO G DO SUL","MATO GROSSO DO SUL",UG..Cod.Desc.),
         UG..Cod.Desc. = gsub("RIO G.DO SUL","RIO GRANDE DO SUL",UG..Cod.Desc.),
         UG..Cod.Desc. = gsub("RIO G DO SUL","RIO GRANDE DO SUL" ,UG..Cod.Desc.),
         UG..Cod.Desc. = gsub("RIO G. SUL","RIO GRANDE DO SUL",UG..Cod.Desc.),
         UG..Cod.Desc. = gsub("RIO G.DO NORTE","RIO GRANDE DO NORTE",UG..Cod.Desc.),
         UG..Cod.Desc. = gsub("RIO G DO NORTE","RIO GRANDE DO NORTE",UG..Cod.Desc.),
         UG..Cod.Desc. = gsub("RIO G. DO NORTE","RIO GRANDE DO NORTE",UG..Cod.Desc.),
         Cod..Ação = substr(Ação..Cod.Desc....Ajustada.,1,4),
         Cod..Subtítulo = substr(Subtítulo..Cod.Desc.,1,4),
         Ação..Cod.Desc....Ajustada. = substr(Ação..Cod.Desc....Ajustada.,8,999),
         Subtítulo..Cod.Desc. = substr(Subtítulo..Cod.Desc.,8,999))

x$UF_nova <- NA
x$Reg_nova <- NA

## FIRST STEP: USING SUB ELEMENTO DESPESA VARIABLE ----

x <- x %>% 
  mutate(sub_elemento_cod = substr(Mod..Aplic...Cod.Desc.,1,2)) %>% 
  mutate(sub_elemento_cod_uf = as.numeric(substr(Sub.elemento.Despesa..Cod.Desc.,7,8)),
         UF_elemento =ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 1,"AC",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 3,"AL",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 5,"AM",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 2,"AM",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 4,"AP",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 7,"BA",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 9,"CE",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 11,"DF",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 21,"MS",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 19,"MT",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 39,"RS",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 47,"SE",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 15,"GO",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 33,"PI",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 27,"PB",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 25,"PA",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 29,"PR",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 42,"RR",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 23,"MG",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 22,"MG",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 35,"RJ",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 37,"RN",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 45,"SP",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 43,"SC",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 13,"ES",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 31,"PE",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 48,"TO",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 17,"MA",
                      ifelse(sub_elemento_cod >= 30 & sub_elemento_cod <= 49 & sub_elemento_cod_uf == 41,"RO",
                      'ND'))))))))))))))))))))))))))))))  

                                                                               

## STEP TWO: UF EXTRACTION BY UG UF -----

# USING CEP

# Reference
# https://weber.eti.br/entry/utilidade-faixas-de-cep-por-estado.html
# https://mundoeducacao.bol.uol.com.br/curiosidades/o-que-significam-os-numeros-cep.htm


x <- x %>% 
        mutate(cod.ug = as.numeric(substr(UG..Cod.Desc.,1,7)),
               cod.ug1 = as.numeric(substr(UG..Cod.Desc.,1,4)),
               Estado =   ifelse(substr(UG.UF, 1,1)== 0,   'SP',   # São Paulo
                          ifelse(substr(UG.UF, 1,1)== 1,   'SP',   # São Paulo
                          ifelse(substr(UG.UF, 1,1)== 3,   'MG',   # Minas Gerais
                          ifelse(substr(UG.UF, 1,1)== 9,   'RS',   # Rio Grande do Sul
                          ifelse(substr(UG.UF, 1,2)==29,   'ES',   # Espirito Santo
                          ifelse(substr(UG.UF, 1,2)==49,   'SE',   # Sergipe
                          ifelse(substr(UG.UF, 1,2)==64,   'PI',   # Piauí
                          ifelse(substr(UG.UF, 1,2)==65,   'MA',   # Maranhão
                          ifelse(substr(UG.UF, 1,3)==689,  'AP',   # Amapá
                          ifelse(substr(UG.UF, 1,3)==699,  'AC',   # Acre
                          ifelse(substr(UG.UF, 1,3)==693,  'RR',   # Roraima
                          ifelse(substr(UG.UF, 1,2)==77,   'TO',   # Tocantins
                          ifelse(substr(UG.UF, 1,2)==79,   'MS',   # Mato Grosso do Sul
                          ifelse(substr(UG.UF, 1,2)==78 & substr(UG.UF, 1,3) != 789,   'MT',   # Mato Grosso
                          ifelse(substr(UG.UF, 1,2)==57,   'AL',   # Alagoas
                          ifelse(substr(UG.UF, 1,2)==58,   'PB',   # Paraíba
                          ifelse(substr(UG.UF, 1,2)==59,   'RN',   # Rio Grande do Norte
                          ifelse(between( as.numeric(substr(UG.UF, 1,2)),20,28),   'RJ', # Rio de Janeiro
                          ifelse(between( as.numeric(substr(UG.UF, 1,2)),40,48),   'BA', # Bahia
                          ifelse(between( as.numeric(substr(UG.UF, 1,2)),80,87),   'PR', # Parana
                          ifelse(between( as.numeric(substr(UG.UF, 1,2)),88,89),   'SC', # Santa Catarina
                          ifelse(between( as.numeric(substr(UG.UF, 1,2)),50,56),   'PE', # Pernambuco
                          ifelse(between( as.numeric(substr(UG.UF, 1,2)),60,63),   'CE', # ceará
                          ifelse(between( as.numeric(substr(UG.UF, 1,2)),66,68),   'PA', # Pará
                          ifelse(between( as.numeric(substr(UG.UF, 1,3)),690,692), 'AM', # Amazonas
                          ifelse(between( as.numeric(substr(UG.UF, 1,3)),694,698), 'AM', # Amazonas
                          ifelse(between( as.numeric(substr(UG.UF, 1,3)),700,727), 'DF', # Distrito Federal
                          ifelse(between( as.numeric(substr(UG.UF, 1,3)),730,736), 'DF', # Distrito Federal
                          ifelse(between( as.numeric(substr(UG.UF, 1,3)),768,769), 'RO', # Rondônia
                          ifelse(between( as.numeric(substr(UG.UF, 1,3)),789,789), 'RO', # Rondônia
                          ifelse(between( as.numeric(substr(UG.UF, 1,3)),728,729), 'GO', # Goiás
                          ifelse(between( as.numeric(substr(UG.UF, 1,3)),737,767), 'GO','ND'
                          )))))))))))))))))))))))))))))))))

x <- x %>% mutate(
               Estado =   ifelse(UG.UF > 1 & UG.UF <= 110011,'EX',
                          ifelse(cod.ug == 120091 & UG.UF == 1,'EX',
                          ifelse((UG.UF > 110111 & cod.ug1 == 2402) | (UG.UF > 110111 & cod.ug1 == 2400) ,'EX',Estado
                                 ))),
               Estado =   ifelse(UG.UF > 1 & UG.UF <= 9999999 & Estado !='EX','SP',Estado)
               )

## STEP THREE: UF EXTRACTION BY UG UF ----- 

x <- x %>% 
  left_join(Parameter, by = c("cod.ug"="cod.ug")) %>% 
  rename(Estado2 = UG.UF.y) %>% 
  mutate(Estado2 = as.character(Estado2))

}

ano=2006
base <- NULL
while(ano!=2022){
  ano = ano + 1
  temp <- siga_brasil(year=ano)
  base <- plyr::rbind.fill(base,temp)  
  gc(TRUE)
  rm(temp)
}

base <- as.data.table(apply(base,2, function(x) ifelse(x=="NA",NA,x)))

#a <-  base

base <- base %>% 
  mutate(Dotação.Inicial   = as.numeric(Dotação.Inicial),
         Autorizado        = as.numeric(Autorizado),
         Empenhado         = as.numeric(Empenhado),
         Liquidado         = as.numeric(Liquidado),
         Despesa.Executada = as.numeric(Despesa.Executada),
         Pago              = as.numeric(Pago),
         RP.Pago           = as.numeric(RP.Pago))

## STEP FOUR: LOCATE BY KEY WORDS 

# Estados sem acento

for (estados in sort(unique(base_referencia$state2))) {
  
  base$UF_nova <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,estados) & base$UF_nova != "PA",estados,base$UF_nova)
      
  base$UF_nova <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados) & base$UF_nova != "PA",estados,base$UF_nova)
  
  base$UF_nova <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,estados) & base$UF_nova != "PA",estados,base$UF_nova)
  
}

# Estados com acento

for (estados in sort(unique(base_referencia$state))) {
  
  base$UF_nova <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,estados),estados,base$UF_nova)
  
  base$UF_nova <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados),estados,base$UF_nova)
  
  base$UF_nova <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,estados),estados,base$UF_nova)  
  
  base$UF_nova <- gsub("-","",base$UF_nova)
}

# Estados Abreviados (2 a 4)

for (estados in sort(unique(base_referencia$abbrev_state3))) {
  
  base$UF_nova <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,estados),estados,base$UF_nova)
  
  base$UF_nova <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados),estados,base$UF_nova)

  base$UF_nova <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,estados),estados,base$UF_nova)
    
}

for (estados in sort(unique(base_referencia$abbrev_state4))) {
  
  base$Reg_nova <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,estados),estados,base$Reg_nova)
  
  base$Reg_nova <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados),estados,base$Reg_nova)
  
  base$UF_nova <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,estados),estados,base$UF_nova)
  
}

for (estados in sort(unique(base_referencia$regiao2))) {
  
  base$Reg_nova <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,estados),estados,base$Reg_nova)
  
  base$Reg_nova <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados),estados,base$Reg_nova)
  
  base$UF_nova <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,estados),estados,base$UF_nova)
  
}

for (estados in sort(unique(base_referencia$abbrev_state2))) {
  
  base$UF_nova <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,estados),estados,base$UF_nova)
  
  base$UF_nova <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados),estados,base$UF_nova)
  
  base$UF_nova <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,estados),estados,base$UF_nova)
  
}

for (estados in sort(unique(base_referencia$regiao3))) {
  
  base$Reg_nova <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,estados),estados,base$Reg_nova)
  
  base$Reg_nova <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados),estados,base$Reg_nova)
  
  base$UF_nova <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,estados),estados,base$UF_nova)

  }


base$Reg_nova <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"estados"),estados,base$Reg_nova)

base$Reg_nova <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados),estados,base$Reg_nova)

ug_base <- read_excel("2020 - UG POR UF.xlsx") %>% 
  select(c(code_ug,uf_ug)) %>% 
  filter(nchar(code_ug) == 6) %>% 
  mutate(code_ug = as.character(code_ug)) %>% distinct(code_ug,uf_ug) %>% 
  mutate(uf_ug = ifelse(uf_ug %in% c("CODIGO INVALIDO",
                                     "SEM INFORMACAO") | is.na(uf_ug),"ND",uf_ug),
         uf_ug =ifelse(uf_ug == "PARA","PA",
                ifelse(uf_ug == "AMAZONAS","AM",
                ifelse(uf_ug == "AMAPA","AP",       
                ifelse(uf_ug == "ACRE","AC",
                ifelse(uf_ug == "DISTRITO FEDERAL","DF",
                ifelse(uf_ug == "PERNAMBUCO","PE",
                ifelse(uf_ug == "BAHIA","BA",
                ifelse(uf_ug == "TOCANTINS","TO",   
                ifelse(uf_ug == "CEARÁ","CE",
                ifelse(uf_ug == "CEARA","CE",
                ifelse(uf_ug == "PIAUÍ","PI",
                ifelse(uf_ug == "PIAUI","PI",
                ifelse(uf_ug == "MINAS GERAIS","MG",
                ifelse(uf_ug == "GOIÁS","GO",
                ifelse(uf_ug == "ALAGOAS","AL",
                ifelse(uf_ug == "MARANHAO","MA",
                ifelse(uf_ug == "PARAÍBA","PB",
                ifelse(uf_ug == "RIO DE JANEIRO","RJ",
                ifelse(uf_ug == "PARANA","PR",
                ifelse(uf_ug == "PARAIBA","PB",
                ifelse(uf_ug == "RONDONIA","RO",
                ifelse(uf_ug == "PARA","PA",
                ifelse(uf_ug == "PARÁ","PA",
                ifelse(uf_ug == "SAO PAULO","SP",
                ifelse(uf_ug == "MATO GROSSO","MT",
                ifelse(uf_ug == "MATO GROSSO DO SUL","MS",
                ifelse(uf_ug == "RIO GRANDE DO SUL","RS",
                ifelse(uf_ug == "RIO GRANDE DO NORTE","RN",
                ifelse(uf_ug == "SANTA CATARINA","SC",
                ifelse(uf_ug == "ESPIRITO SANTO","ES",
                ifelse(uf_ug == "ESPÍRITO SANTO","ES",
                ifelse(uf_ug == "RONDÔNIA","RO",
                ifelse(uf_ug == "RORAIMA","RR",
                ifelse(uf_ug == "SÃO PAULO","SP",
                ifelse(uf_ug == "SERGIPE","SE",
                ifelse(uf_ug == "GOIAS","GO",
                ifelse(uf_ug == "PARANÁ","PR",
                 uf_ug))))))))))))))))))))))))))))))))))))))

base <- base %>% rename(code_ug = cod.ug) %>% left_join(ug_base)

# LATEST CHECKS

for (estados in sort(unique(base_referencia$regiao3))) {
  
  base$UFis <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados) & base$UFis=="ND",estados,base$UFis)
  
}

base$UFis <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,"OBTENÇÃO DE MEIOS DA MARINHA"),"EX",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"TECNOLOGIA NUCLEAR DA MARINHA"),"RJ",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"IMPLANTAÇÃO DE ESTALEIRO E BASE NAVAL PARA CONSTRUÇÃO E MANUTENÇÃO DE SUBMARINOS CONVENCIONAIS E NUCLEARES"),"RJ",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"CONSTRUÇÃO DE SUBMARINO DE PROPULSÃO NUCLEAR"),"RJ",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"APOIO À REALIZAÇÃO DE GRANDES EVENTOS"),"RJ",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"CONSTRUÇÃO DE SUBMARINOS CONVENCIONAIS"),"RJ",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"APRESTAMENTO DA MARINHA"),"RJ",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"MELHORAMENTOS NO CANAL DE NAVEGAÇÃO DA HIDROVIA DOS RIOS PARANÁ E PARAGUAI"),"CO",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"DESENVOLVIMENTO TECNOLÓGICO DA MARINHA"),"RJ",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"CAPACITAÇÃO PROFISSIONAL DA MARINHA"),"RJ",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"OPERACIONALIZACAO DAS ACOES DE SEGURANCA PUBLICA PARA AS OLIMPIADAS E PARAOLIMPIADAS RIO 2016"),"RJ",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"DESENVOLVIMENTO E IMPLEMENTAÇÃO DO SISTEMA DE GERENCIAMENTO DA AMAZÔNIA AZUL (SISGAAZ)"),"NO",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"MANUTENÇÃO DO SISTEMA DE PROTEÇÃO DA AMAZÔNIA - SIPAM"),"NO",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"PARTICIPAÇÃO BRASILEIRA EM MISSÕES DE PAZ"),"EX",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"IMUNOBIOLÓGICOS E INSUMOS PARA PREVENÇÃO E CONTROLE DE DOENÇAS"),"EX",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"APOIO LOGÍSTICO À PESQUISA CIENTÍFICA NA ANTÁRTICA"),"EX",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"LONDRES"),"EX",base$UFis)
base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"SUPERINTENDÊNCIA DA ZONA FRANCA DE MANAUS - SUFRAMA"),"AM",base$UFis)

base$UFis <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,"MATOPIBA") & base$UFis=="ND","NE",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"RIO SÃO FRANCISCO") & base$UFis=="ND","NE",base$UFis)
base$UFis <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,"CENTRO-OESTE") & base$UFis=="ND","CO",base$UFis)
base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"CENTRO-OESTE") & base$UFis=="ND","CO",base$UFis)
base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"VALES DO SÃO FRANCISCO E DO PARNAÍBA") & base$UFis=="ND","NE",base$UFis)
base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"RIO SÃO FRANCISCO") & !(grepl("SE"|"BA"|"CE"|"PB"|"PE"|"PI"|"RN"|"NE",base$UFis)),"NE",base$UFis)
base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"RIOS JAGUARIBE") & !(grepl("SE"|"BA"|"CE"|"PB"|"PE"|"PI"|"RN"|"NE",base$UFis)),"NE",base$UFis)
base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"RIO JAGUARIBE") & !(grepl("SE"|"BA"|"CE"|"PB"|"PE"|"PI"|"RN"|"NE",base$UFis)),"NE",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"RIO JAGUARIBE") & !(grepl("SE"|"BA"|"CE"|"PB"|"PE"|"PI"|"RN"|"NE",base$UFis)),"NE",base$UFis)
base$UFis <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,"RIO SÃO FRANCISCO") & !(grepl("SE"|"BA"|"CE"|"PB"|"PE"|"PI"|"RN"|"NE",base$UFis)),"NE",base$UFis)
base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"AMAZÔNIA") & base$UFis=="ND","NO",base$UFis)
base$UFis <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,"EXTERIOR") & base$UFis=="ND","EX",base$UFis) 



base$UFis <- ifelse(str_detect(base$Região,"NORDESTE") & base$UFis=="ND","NE",base$UFis) 
base$UFis <- ifelse(str_detect(base$Região,"SUL")      & base$UFis=="ND","SL",base$UFis) 
base$UFis <- ifelse(str_detect(base$Região,"NORTE")    & base$UFis=="ND","NO",base$UFis) 
base$UFis <- ifelse(str_detect(base$Região,"SUDESTE")  & base$UFis=="ND","SD",base$UFis) 

base$UFis <- ifelse(str_detect(base$UG..Cod.Desc.,"PORTO ALEGRE"),"RS",base$UFis)

#base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"CÂMARA")  & base$UFis == "DF","ND",base$UFis)
#base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"SENADO")  & base$UFis == "DF","ND",base$UFis)
#base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"SUPREMO") & base$UFis == "DF","ND",base$UFis)
#base$UFis <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"PRESIDÊNCIA") & base$UFis == "DF","ND",base$UFis)
#base$UFis <- ifelse(str_detect(base$Órgão.Superior..Cod.Desc.,"PRESIDENCIA") & base$UFis == "DF","ND",base$UFis)
#base$UFis <- ifelse(str_detect(base$Órgão.Superior..Cod.Desc.,"SENADO") & base$UFis == "DF","ND",base$UFis)
#base$UFis <- ifelse(str_detect(base$Órgão.Superior..Cod.Desc.,"SUPREMO") & base$UFis == "DF","ND",base$UFis)
#base$UFis <- ifelse(str_detect(base$UG..Cod.Desc.,"PRESIDENCIA") & base$UFis == "DF","ND",base$UFis)
#base$UFis <- ifelse(str_detect(base$UG..Cod.Desc.,"SENADO") & base$UFis == "DF","ND",base$UFis)
#base$UFis <- ifelse(str_detect(base$UG..Cod.Desc.,"SUPREMO") & base$UFis == "DF","ND",base$UFis)


## STEP FIVE: UNION OF WORKSHEETS AND MANUAL ANALYSIS

base <- base %>%
    mutate(UF_nova = ifelse(Subtítulo..Cod.Desc.=="NACIONAL",NA,UF_nova),
           UF_nova = gsub("- ","",UF_nova),
           UF_nova = gsub("-","",UF_nova),
           Reg_nova = gsub(", ","",Reg_nova),
           Reg_nova = ifelse(Região != "Nacional" | Região != "NÃO INFORMADO" |
                               is.na(Região),
                             Região,Reg_nova),
           Reg_nova = ifelse(Reg_nova == "PB"|
                               Reg_nova == "PE"|
                               Reg_nova == "CE"|
                               Reg_nova == "RN"|
                               Reg_nova == "PI"|
                               Reg_nova == "BA"|
                               Reg_nova == "AL"|
                               Reg_nova == "SE","Nordeste",
                      ifelse(Reg_nova == "PB"|
                               Reg_nova == "ES"|
                               Reg_nova == "MG"|
                               Reg_nova == "RJ"|
                               Reg_nova == "SP","Sudeste",
                      ifelse(Reg_nova == "PB"|
                               Reg_nova == "AM"|
                               Reg_nova == "RR"|
                               Reg_nova == "AP"|
                               Reg_nova == "PA"|
                               Reg_nova == "TO"|
                               Reg_nova == "RO"|
                               Reg_nova == "AC","Norte",
                      ifelse(Reg_nova == "PA"|
                               Reg_nova == "SC"|
                               Reg_nova == "RS","Sul",
                      ifelse(Reg_nova == "GO"|
                               Reg_nova == "MT"|
                               Reg_nova == "MS"|
                               Reg_nova == "DF","Centro-Oeste",0))))),
           UFis = ifelse(Reg_nova!=0,Reg_nova,UFis),
           UFis = ifelse(Estado!="DF",Estado,UFis),
          #UFis = ifelse(Estado!=Estado2 & Estado!="EX",Estado2,UFis),
           UFis = ifelse((UF_elemento != 'ND' | is.na(UF_elemento)) 
                         & sub_elemento_cod >= 30  
                         & sub_elemento_cod <= 49,UF_elemento,UFis),
           UFis = ifelse(is.na(UFis),UF_nova,UFis),
           UFis = ifelse(UFis == "PARA","PA",
                         ifelse(UFis == "AMAZONAS","AM",
                         ifelse(UFis == "AMAPA","AP",       
                         ifelse(UFis == "ACRE","AC",
                         ifelse(UFis == "DISTRITO FEDERAL","DF",
                         ifelse(UFis == "PERNAMBUCO","PE",
                         ifelse(UFis == "BAHIA","BA",
                         ifelse(UFis == "TOCANTINS","TO",   
                         ifelse(UFis == "CEARÁ","CE",
                         ifelse(UFis == "CEARA","CE",
                         ifelse(UFis == "PIAUÍ","PI",
                         ifelse(UFis == "PIAUI","PI",
                         ifelse(UFis == "MINAS GERAIS","MG",
                         ifelse(UFis == "GOIÁS","GO",
                         ifelse(UFis == "ALAGOAS","AL",
                         ifelse(UFis == "MARANHAO","MA",
                         ifelse(UFis == "PARAÍBA","PB",
                         ifelse(UFis == "RIO DE JANEIRO","RJ",
                         ifelse(UFis == "PARANA","PR",
                         ifelse(UFis == "PARAIBA","PB",
                         ifelse(UFis == "RONDONIA","RO",
                         ifelse(UFis == "PARA","PA",
                         ifelse(UFis == "PARÁ","PA",
                         ifelse(UFis == "SAO PAULO","SP",
                         ifelse(UFis == "MATO GROSSO","MT",
                         ifelse(UFis == "MATO GROSSO DO SUL","MS",
                         ifelse(UFis == "RIO GRANDE DO SUL","RS",
                         ifelse(UFis == "RIO GRANDE DO NORTE","RN",
                         ifelse(UFis == "SANTA CATARINA","SC",
                         ifelse(UFis == "ESPIRITO SANTO","ES",
                         ifelse(UFis == "ESPÍRITO SANTO","ES",
                         ifelse(UFis == "RONDÔNIA","RO",
                         ifelse(UFis == "RORAIMA","RR",
                         ifelse(UFis == "SÃO PAULO","SP",
                         ifelse(UFis == "SERGIPE","SE",
                         ifelse(UFis == "GOIAS","GO",
                         ifelse(UFis == "PARANÁ","PR",
                                UFis))))))))))))))))))))))))))))))))))))),
           UF = ifelse(is.na(UF),"NÃO APLICÁVEL",UF),
           UF = ifelse(UF == "NÃO INFORMADO" | UF == "NÃO APLICÁVEL",NA,UF),
           UFis = ifelse(!is.na(UF) # & UF != "EX"
                         ,UF,UFis),
           UFis = ifelse(UFis == 0, "ND", UFis),
           UFis = ifelse(is.na(UFis),"ND",UFis),
           UFis = ifelse(is.na(UFis),Reg_nova,UFis)) %>% 
    left_join(base_estados, by = c("UFis"="abbrev_state"), copy = FALSE)


base <- base %>% 
  mutate(UFis =  ifelse(UFis == "Nordeste" | UFis == "NORDESTE", "NE",
                 ifelse(UFis == "Norte" | UFis == "NORTE", "NO",
                 ifelse(UFis == "Sudeste" | UFis == "SUDESTE", "SD",
                 ifelse(UFis == "Sul" | UFis == "SUL", "SL",
                 ifelse(UFis == "Centro Oeste" | UFis == "CENTRO OESTE" | 
                        UFis == "Centro-Oeste", "CO",
                        UFis))))),
         #UFis  = ifelse(UFis == "ND",name_region,UFis),
         UFis  = ifelse(UFis == "ND" & Estado != "DF",Estado,UFis),
         UFis  = ifelse(UFis == "DF",Estado,UFis),
         REGis =ifelse(is.na(name_region),UFis,name_region),
         REGis =ifelse(REGis == "Nordeste" | REGis == "NORDESTE" | REGis == "NE", "NE",
                       ifelse(REGis == "Norte" | REGis == "NORTE" | REGis == "NO", "NO",
                       ifelse(REGis == "Sudeste" | REGis == "SUDESTE" | REGis == "SD", "SD",
                       ifelse(REGis == "Sul" | REGis == "SUL" | REGis == "SL", "SL",
                       ifelse(REGis == "Centro Oeste" | REGis == "CENTRO OESTE" | REGis == "CO" |
                              REGis == "Centro-Oeste", "CO",
                       ifelse(REGis == "EX","EX",
                              REGis)))))))

base <- base %>% 
  mutate(UFis  = ifelse(`Região` == "NACIONAL" & UFis=="DF","ND",UFis),
         REGis = ifelse(`Região` == "NACIONAL" & UFis=="DF","ND",REGis))
#         UFis  = ifelse(UF_elemento=="DF" & UFis=="ND","DF",UFis),
#         REGis = ifelse(UF_elemento=="DF" & UFis=="ND","CO",REGis))


#table(a$`UF*`, useNA = 'ifany')

base <- base %>% 
  mutate(UFis  = ifelse(UFis == 0,"ND",UFis),
         UFis  = ifelse(is.na(UFis),"ND",UFis),
         UFis =  ifelse(UFis == "Nordeste" | UFis == "NORDESTE", "NE",
                 ifelse(UFis == "Norte" | UFis == "NORTE", "NO",
                 ifelse(UFis == "Sudeste" | UFis == "SUDESTE", "SD",
                 ifelse(UFis == "Sul" | UFis == "SUL", "SL",
                 ifelse(UFis == "Centro Oeste" | UFis == "CENTRO OESTE" | UFis == "CENTROOESTE" | 
                        UFis == "Centro-Oeste", "CO",
                        UFis))))),
         UFis     = ifelse(is.na(UFis) & is.na(Localidade.UF),Estado,UFis),
         #UFis     = ifelse((is.na(UFis) | UFis == "ND") & uf_ug != "DF",uf_ug,UFis),
         REGis = UFis,
         REGis = 
             ifelse(REGis == "PB"|
                    REGis == "MA"|
                    REGis == "PE"|
                    REGis == "CE"|
                    REGis == "RN"|
                    REGis == "PI"|
                    REGis == "BA"|
                    REGis == "AL"|
                    REGis == "SE","NE",
             ifelse(
                    REGis == "ES"|
                    REGis == "MG"|
                    REGis == "RJ"|
                    REGis == "SP","SD",
             ifelse(
                    REGis == "AM"|
                    REGis == "RR"|
                    REGis == "AP"|
                    REGis == "PA"|
                    REGis == "TO"|
                    REGis == "RO"|
                    REGis == "AC","NO",
             ifelse(REGis == "PR"|
                    REGis == "PA"|
                    REGis == "SC"|
                    REGis == "RS","SL",
             ifelse(REGis == "GO"|
                    REGis == "MT"|
                    REGis == "MS"|
                    REGis == "DF","CO",REGis))))),
         uf_ug    = ifelse(is.na(uf_ug),"ND",uf_ug),
        #UFis     = ifelse(is.na(UFis),"ND",UFis),
         UFis     = gsub(", ","",UFis),
         Região   = ifelse(Região == "NE","REGIAO NORDESTE",Região),
         Região   = ifelse(Região == "NO","REGIAO NORTE",Região),
         Região   = ifelse(Região == "SL","REGIÃO SUL",Região),
         Região   = ifelse(Região == "SD","REGIAO SUDESTE",Região),
         Região   = ifelse(Região == "CO","REGIAO CENTRO OESTE",Região),
         UFis     = ifelse(!(UFis %in% c("SE","BA","CE","PB","PE","PI","RN")) & grepl("NE",Região),"NE",UFis),
         UFis     = ifelse(!(UFis %in% c("GO","DF","MT","MS")) & grepl("CO",Região),"CO",UFis),
         UFis     = ifelse(!(UFis %in% c("PR","PA","SC","RS")) & grepl("SL",Região),"SL",UFis),
         UFis     = ifelse(!(UFis %in% c("AM","RR","AP","PA","TO","RO","AC")) & grepl("NO",Região),"NO",UFis),
         UFis     = ifelse(!(UFis %in% c("ES","MG","RJ","SP")) & grepl("SD",Região),"SD",UFis),
         "UF*"    = ifelse(!is.na(UF),UF,UFis),
         "Região*"= REGis,
         "UF*_novo" =  ifelse(UFis=="ND" & UFis!="DF" &
                              uf_ug != "DF",uf_ug,`UF*`))


gc(reset = TRUE, full = TRUE)

## Deflacionando

igp_di <- readxl::read_excel(path = "IGP-DI.xlsx", sheet = 2) %>% 
  mutate(Ano = substr(ano,1,4)) %>% select(-c(ano)) %>% 
  rename(igp_di = acumulado12m)

base <- left_join(base,igp_di)

base <- base %>%
  mutate(Dotação.Inicial_igpdi   = deflate(Dotação.Inicial,indice, type  = 'index'),
         Autorizado_igpdi        = deflate(Autorizado,indice, type  = 'index'),
         Empenhado_igpdi         = deflate(Empenhado,indice, type  = 'index'),
         Liquidado_igpdi         = deflate(Liquidado,indice, type  = 'index'),
         Despesa.Executada_igpdi = deflate(Despesa.Executada,indice, type  = 'index'),
         Pago_igpdi              = deflate(Pago,indice, type  = 'index'),
         RP.Pago_igpdi           = deflate(RP.Pago,indice, type  = 'index'))

### Force changing

#for (estados in sort(unique(base_referencia$state2))) {
#  
#  base$`UF*_novo` <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,estados) & base$`UF*_novo` == "ND",estados,base$`UF*_novo`)
#  
#  base$`UF*_novo` <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados) & base$`UF*_novo` == "ND",estados,base$`UF*_novo`)
#  
#  base$`UF*_novo` <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,estados) & base$`UF*_novo` == "ND",estados,base$`UF*_novo`)
#  
#}

for (estados in sort(unique(base_referencia$state))) {
  
  base$`UF*_novo` <- ifelse(str_detect(base$Ação..Cod.Desc....Ajustada.,estados) & base$`UF*_novo` == "ND",estados,base$`UF*_novo`)
  
  base$`UF*_novo` <- ifelse(str_detect(base$Subtítulo..Cod.Desc.,estados) & base$`UF*_novo` == "ND",estados,base$`UF*_novo`)
  
  base$`UF*_novo` <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,estados) & base$`UF*_novo` == "ND",estados,base$`UF*_novo`)
  
}

base$`UF*_novo` <- ifelse(str_detect(base$UO..Cod.Desc...Ajustado.,"IPEA") & base$`UF*_novo` == "ND","DF",base$`UF*_novo`)

base <- base %>% 
  mutate(Sub.elemento.Despesa..Cod.Desc. = stri_trans_general(str = Sub.elemento.Despesa..Cod.Desc., 
                                                              id = "Latin-ASCII"))

base <- base %>% 
  mutate(temp = Sub.elemento.Despesa..Cod.Desc.) %>% 
  separate(temp, c("value","nome")," - ")

for (estados in sort(unique(base_referencia$state2))) {
  
  base$`UF*_novo` <- ifelse(base$nome == estados & base$`UF*_novo` == "ND",estados,base$`UF*_novo`)
  
}

base <- base %>% 
    mutate(`UF*_novo` = 
           ifelse(`UF*_novo` == "PARA","PA",
           ifelse(`UF*_novo` == "AMAZONAS","AM",
           ifelse(`UF*_novo` == "AMAPA","AP",       
           ifelse(`UF*_novo` == "ACRE","AC",
           ifelse(`UF*_novo` == "DISTRITO FEDERAL","DF",
           ifelse(`UF*_novo` == "PERNAMBUCO","PE",
           ifelse(`UF*_novo` == "BAHIA","BA",
           ifelse(`UF*_novo` == "TOCANTINS","TO",   
           ifelse(`UF*_novo` == "CEARÁ","CE",
           ifelse(`UF*_novo` == "CEARA","CE",
           ifelse(`UF*_novo` == "PIAUÍ","PI",
           ifelse(`UF*_novo` == "PIAUI","PI",
           ifelse(`UF*_novo` == "MINAS GERAIS","MG",
           ifelse(`UF*_novo` == "GOIÁS","GO",
           ifelse(`UF*_novo` == "ALAGOAS","AL",
           ifelse(`UF*_novo` == "MARANHAO","MA",
           ifelse(`UF*_novo` == "PARAÍBA","PB",
           ifelse(`UF*_novo` == "RIO DE JANEIRO","RJ",
           ifelse(`UF*_novo` == "PARANA","PR",
           ifelse(`UF*_novo` == "PARAIBA","PB",
           ifelse(`UF*_novo` == "RONDONIA","RO",
           ifelse(`UF*_novo` == "PARA","PA",
           ifelse(`UF*_novo` == "PARÁ","PA",
           ifelse(`UF*_novo` == "SAO PAULO","SP",
           ifelse(`UF*_novo` == "MATO GROSSO","MT",
           ifelse(`UF*_novo` == "MATO GROSSO DO SUL","MS",
           ifelse(`UF*_novo` == "RIO GRANDE DO SUL","RS",
           ifelse(`UF*_novo` == "RIO GRANDE DO NORTE","RN",
           ifelse(`UF*_novo` == "SANTA CATARINA","SC",
           ifelse(`UF*_novo` == "ESPIRITO SANTO","ES",
           ifelse(`UF*_novo` == "ESPÍRITO SANTO","ES",
           ifelse(`UF*_novo` == "RONDÔNIA","RO",
           ifelse(`UF*_novo` == "RORAIMA","RR",
           ifelse(`UF*_novo` == "SÃO PAULO","SP",
           ifelse(`UF*_novo` == "SERGIPE","SE",
           ifelse(`UF*_novo` == "GOIAS","GO",
           ifelse(`UF*_novo` == "PARANÁ","PR",
                  `UF*_novo`))))))))))))))))))))))))))))))))))))))


  

siop <- readxl::read_excel("SIOP 2002-2020 - AÇÃO - GND 04 - acabamento em 07maio2021.xlsx") %>% 
  filter(!is.na(Ação)) %>% 
  distinct(Ação) %>% 
  separate(Ação, c("codigo","nome_funcional"), " - ") %>% 
  distinct(codigo, .keep_all = T)

base <- base %>% 
  mutate(codigo = substr(Funcional,13,16)) %>% 
  left_join(siop)

base <- base %>%
  select(-c(UF_nova,UFis,value,nome,
             Reg_nova,code_state,
             sub_elemento_cod,name_state,
             sub_elemento_cod_uf,UF_elemento,
             code_ug,cod.ug1,
             Estado,Estado2,uf_ug ,
             code_region)) %>% 
  mutate(Dotação.Inicial = as.numeric(Dotação.Inicial)/1000000,
         Autorizado = as.numeric(Autorizado)/1000000,
         Empenhado = as.numeric(Empenhado)/1000000,
         Liquidado = as.numeric(Liquidado)/1000000,
         Despesa.Executada = as.numeric(Despesa.Executada)/1000000,
         Pago = as.numeric(Pago)/1000000,
         RP.Pago = as.numeric(RP.Pago)/1000000,
         Dotação.Inicial_igpdi = as.numeric(Dotação.Inicial_igpdi)/1000000,
         Autorizado_igpdi = as.numeric(Autorizado_igpdi)/1000000,
         Empenhado_igpdi = as.numeric(Empenhado_igpdi)/1000000,
         Liquidado_igpdi = as.numeric(Liquidado_igpdi)/1000000,
         Despesa.Executada_igpdi = as.numeric(Despesa.Executada_igpdi)/1000000,
         Pago_igpdi = as.numeric(Pago_igpdi)/1000000,
         RP.Pago_igpdi = as.numeric(RP.Pago_igpdi)/1000000)

base <- base %>% 
  separate(Funcional, c("Cod Função",	"Cod Subfunção","Cod Programa","Cod Ação","Cod Subtítulo"), "\\.") %>% 
  mutate(`Cod Programa`  = ifelse(is.na(`Cod Programa`),`Cod Função`,`Cod Programa`),
         `Cod Subfunção` = ifelse(is.na(`Cod Subfunção`),`Cod Função`,`Cod Subfunção`),
         `Cod Ação`      = ifelse(is.na(`Cod Ação`),`Cod Função`,`Cod Ação`),
         `Cod Subtítulo` = ifelse(is.na(`Cod Subtítulo`),`Cod Função`,`Cod Subtítulo`),
         "Cod Ação*"     = `Cod Ação`)

temp <- readxl::read_excel("SIGA 2001-2016 INV FED REG mi.xlsx", sheet = 1)

temp <- temp %>% 
  select(c(`Função`,`Subfunção`,`Programa`,`Ação`,`Ação*`,`Subtítulo`)) %>% 
  separate(`Função`, c("Cod Função","Função"), " - ") %>% 
  separate(`Subfunção`, c("Cod Subfunção","Subfunção"), " - ") %>% 
  separate(`Programa`, c("Cod Programa","Programa"), " - ") %>% 
  separate(`Ação`, c("Cod Ação","Ação"), " - ") %>% 
  separate(`Ação*`, c("Cod Ação*","Ação*"), " - ") %>% 
  separate(`Subtítulo`, c("Cod Subtítulo","Subtítulo"), " - ")

temp1 <- temp %>% 
  group_by(`Cod Função`,`Cod Subfunção`,`Cod Programa`,`Cod Ação`,`Cod Ação*`,`Cod Subtítulo`) %>% 
  mutate(n = row_number()) %>% 
  filter(n>1) %>%
  select(-c(n)) %>%
  subset(`Cod Ação` %in% siop$codigo) %>% 
  distinct(`Cod Função`,`Cod Subfunção`,`Cod Programa`,`Cod Ação`,`Cod Ação*`,`Cod Subtítulo`, .keep_all = T)

temp <- temp %>% subset(!(`Cod Ação` %in% temp1$`Cod Ação`)) %>% 
  group_by(`Cod Função`,`Cod Subfunção`,`Cod Programa`,`Cod Ação`,`Cod Ação*`,`Cod Subtítulo`) %>% 
  mutate(n = row_number()) %>% 
  filter(n==1) %>%
  select(-c(n)) %>% 
  distinct(`Cod Função`,`Cod Subfunção`,`Cod Programa`,`Cod Ação`,`Cod Ação*`,`Cod Subtítulo`, .keep_all = T)

temp <- rbind(temp,temp1)  %>% 
  distinct(`Cod Função`,`Cod Subfunção`,`Cod Programa`,`Cod Ação`,`Cod Ação*`,`Cod Subtítulo`, .keep_all = T)

temp1 <- temp %>% ungroup() %>% distinct(`Cod Função`,`Função`) %>% distinct(`Cod Função`, .keep_all = T) 
temp2 <- temp %>% ungroup() %>% distinct(`Cod Subfunção`,`Subfunção`) %>% distinct(`Cod Subfunção`, .keep_all = T)
temp3 <- temp %>% ungroup() %>% distinct(`Cod Programa`,`Programa`) %>% distinct(`Cod Programa`, .keep_all = T)
temp4 <- temp %>% ungroup() %>% distinct(`Cod Ação`,`Ação`) %>% distinct(`Cod Ação`, .keep_all = T)
temp5 <- temp %>% ungroup() %>% distinct(`Cod Ação*`,`Ação*`) %>% distinct(`Cod Ação*`, .keep_all = T)



base <- left_join(base,temp1) %>% left_join(temp2) %>% 
  left_join(temp3) %>% left_join(temp4) %>% left_join(temp5) %>% 
  mutate(`Função`   = paste0(`Cod Função`," - ",`Função`),
         `Subfunção`= paste0(`Cod Subfunção`," - ",`Subfunção`),
         `Programa` = paste0(`Cod Programa`," - ",`Programa`),
         `Ação`     = paste0(`Cod Ação`," - ",`Ação`),
         #`Subtítulo`= paste0(`Cod Subtítulo`," - ",`Subtítulo`),
         `Ação*`    = paste0(`Cod Ação*`," - ",`Ação*`)) %>% 
  select(-c("Cod Função","Cod Subfunção","Cod Programa","Cod Ação","Cod Ação*"))
rm(temp1,temp2,temp3,temp4,temp5,temp)

base <- base %>% 
  dplyr::rename(UG.UF = UG.UF.x) %>% 
  select(c(Ano,Localidade.UF,	Região,
           "UF*",	"Região*",	"UF*_novo",
           Mod..Aplic...Cod.Desc.,	Elemento.Despesa..Cod.Desc.,
           Subfunção,Função,
           Programa,GND..Cod.,
           Sub.elemento.Despesa..Cod.Desc.,
           Órgão..Cod.Desc.,	Órgão.Superior..Cod.Desc.,
           UG..Cod.Desc.,	UG.UF,	UO..Cod.Desc...Ajustado.,
           Ação..Cod.Desc....Ajustada., codigo,	nome_funcional,
           Função,	Subfunção,	Programa,	Ação,	"Ação*", 
           igp_di,	acumulado,	indice, Empenhado,	Despesa.Executada,
           Empenhado_igpdi,	Despesa.Executada_igpdi))

base <- base %>% 
  mutate(Região   = ifelse(grepl("NORDESTE",Região),"NE",Região),
         Região   = ifelse(grepl("CENTRO OESTE",Região),"CO",Região),
         Região   = ifelse(grepl("SUL",Região),"SL",Região),
         Região   = ifelse(grepl("NORTE",Região),"NO",Região),
         Região   = ifelse(grepl("SUDESTE",Região),"SD",Região),
         `Região*` = ifelse(grepl("NE|CO|SL|NO|SD",Região) & `Região*`=="ND",Região,`Região*`),
         `UF*_novo`= ifelse(grepl("NE|CO|SL|NO|SD",`Região*`) & `UF*_novo`=="ND",`Região*`,`UF*_novo`))

a <- base %>% filter(Região == "NE")

table(a$`UF*_novo`)

base <- base %>% 
  mutate(`UF*_novo`     = ifelse((`UF*_novo` %in% c("CO","SL","NO","SD")) & grepl("NE",Região),"NE",`UF*_novo`),
         `UF*_novo`     = ifelse((`UF*_novo` %in% c("NE","SL","NO","SD")) & grepl("CO",Região),"CO",`UF*_novo`),
         `UF*_novo`     = ifelse((`UF*_novo` %in% c("CO","NE","NO","SD")) & grepl("SL",Região),"SL",`UF*_novo`),
         `UF*_novo`     = ifelse((`UF*_novo` %in% c("CO","SL","NE","SD")) & grepl("NO",Região),"NO",`UF*_novo`),
         `UF*_novo`     = ifelse((`UF*_novo` %in% c("CO","SL","NO","NE")) & grepl("SD",Região),"SD",`UF*_novo`),
         `Região*`     = ifelse((`Região*` %in% c("CO","SL","NO","SD")) & grepl("NE",Região),"NE",`Região*`),
         `Região*`     = ifelse((`Região*` %in% c("NE","SL","NO","SD")) & grepl("CO",Região),"CO",`Região*`),
         `Região*`     = ifelse((`Região*` %in% c("CO","NE","NO","SD")) & grepl("SL",Região),"SL",`Região*`),
         `Região*`     = ifelse((`Região*` %in% c("CO","SL","NE","SD")) & grepl("NO",Região),"NO",`Região*`),
         `Região*`     = ifelse((`Região*` %in% c("CO","SL","NO","NE")) & grepl("SD",Região),"SD",`Região*`),
         `UF*_novo`     = ifelse(!(`UF*_novo` %in% c("SE","BA","CE","PB","PE","PI","RN")) & grepl("NE",Região),"NE",`UF*_novo`),
         `UF*_novo`     = ifelse(!(`UF*_novo` %in% c("GO","DF","MT","MS")) & grepl("CO",Região),"CO",`UF*_novo`),
         `UF*_novo`     = ifelse(!(`UF*_novo` %in% c("PR","PA","SC","RS")) & grepl("SL",Região),"SL",`UF*_novo`),
         `UF*_novo`     = ifelse(!(`UF*_novo` %in% c("AM","RR","AP","PA","TO","RO","AC")) & grepl("NO",Região),"NO",`UF*_novo`),
         `UF*_novo`     = ifelse(!(`UF*_novo` %in% c("ES","MG","RJ","SP")) & grepl("SD",Região),"SD",`UF*_novo`))

base <- base %>% 
  mutate(`Região*` = ifelse(`Região*` == ", PE","NE",`Região*`),
         `Região*` = ifelse((`UF*_novo` %in% c("SE","BA","CE","PB","PE","PI","RN","NE","AL","MA")) & `Região*` == "EX","NE",`Região*`),
         `Região*` = ifelse((`UF*_novo` %in% c("GO","DF","MT","MS","CO")) & `Região*` == "EX","CO",`Região*`),
         `Região*` = ifelse((`UF*_novo` %in% c("PR","SC","RS","SL")) & `Região*` == "EX","SL",`Região*`),
         `Região*` = ifelse((`UF*_novo` %in% c("AM","RR","AP","PA","TO","RO","AC","NO")) & `Região*` == "EX","NO",`Região*`),
         `Região*` = ifelse((`UF*_novo` %in% c("ES","MG","RJ","SP","SD")) & `Região*` == "EX","SD",`Região*`))

base <- base %>% 
  mutate(`Região*` = ifelse((`UF*_novo` %in% c("SE","BA","CE","PB","PE","PI","RN","NE","AL","MA")) & `Região*` == "ND","NE",`Região*`),
         `Região*` = ifelse((`UF*_novo` %in% c("GO","DF","MT","MS","CO")) & `Região*` == "ND","CO",`Região*`),
         `Região*` = ifelse((`UF*_novo` %in% c("PR","SC","RS","SL")) & `Região*` == "ND","SL",`Região*`),
         `Região*` = ifelse((`UF*_novo` %in% c("AM","RR","AP","PA","TO","RO","AC","NO")) & `Região*` == "ND","NO",`Região*`),
         `Região*` = ifelse((`UF*_novo` %in% c("ES","MG","RJ","SP","SD")) & `Região*` == "ND","SD",`Região*`))

a <- filter(base, `Região*` == "ND")

table(a$`UF*_novo`)

## Corrigindo colunas (mantendo padrão)

# base <- base %>% 
#   mutate(stringr::str_to_title(str))

# Matching string variables
# siop$name.nyse <- "" # Creating an empty column
# for(i in 1:dim(siop)[1]) {
#   x <- agrep(siop$nome[i], temp$Ação..Cod.Desc....Ajustada.,
#              ignore.case=TRUE, value=TRUE,
#              max.distance = 0.05, useBytes = TRUE)
#   x <- paste0(x,"")
#   siop$name.nyse[i] <- x
# } 

# a2 <- base %>% group_by(Ano) %>% 
#   mutate(Despesa.Executada = as.numeric(gsub(",",".",Despesa.Executada)),
#          Empenhado =  as.numeric(gsub(",",".",Empenhado))) %>% 
#   dplyr::summarise(Despesa_Executada =  sum(Despesa.Executada, na.rm = T),
#                    Empenhado =  sum(Empenhado, na.rm = T)) %>% 
#   mutate(diff = Despesa_Executada - Empenhado)
# 
# write.csv2(a2,"comparacao.csv", row.names = F)

# TEST
ano <- 2020
while(ano!=2022){
  ano <- ano + 1
  base_teste <- base %>% 
    filter(Ano==ano)
  write.csv2(base_teste, file = paste0("LOA ",ano,".csv"), row.names = F)  
}

sample <- base %>% 
  filter(Empenhado!=0 | Despesa.Executada!=0)

write.csv2(sample,"amostra.csv",row.names=F)


base <- fread("amostra.csv") 

sample <- sample %>% filter(`UF*`=="ND") %>% 
  mutate(teste = as.numeric(gsub(",",".",Despesa.Executada_igpdi)) - as.numeric(gsub(",",".",Empenhado_igpdi)))

sample2 <- sample %>% filter(teste!=0)

a <- sample %>% filter(`UF*_novo`=="PA") %>% filter(is.na(UF))

a <- sample %>% filter(grepl("PARA",UO..Cod.Desc...Ajustado.) & `UF*_novo`=="PA") # %>% filter(`UF*_novo`=="ND")

a1 <- base %>% group_by(`UFis`) %>% 
  dplyr::summarise(Despesa_Executada =  sum(Despesa.Executada_igpdi, na.rm = T),) %>% 
  mutate(comparison = (Despesa_Executada/sum(Despesa_Executada)*100)) %>% 
  filter(UFis %in% c("ND","DF"))


a2 <- base %>% group_by(`UF*_novo`,Ano) %>% filter(Ano<= 2018) %>% 
  dplyr::summarise(Despesa_Executada =  sum(Despesa.Executada, na.rm = T),
                   Empenhado =  sum(Empenhado, na.rm = T)) %>% 
  group_by(Ano) %>%
  mutate(teste = ifelse(Despesa_Executada==Empenhado,1,0)) %>% 
  mutate(comparison = (Despesa_Executada/sum(Despesa_Executada)*100)) 

#a4 <- a3 %>% filter(Despesa.Executada!=0)
#table(a4$Localidade.UF, useNA = "ifany")
#table(a4$`UF*`, useNA = "ifany")

a3 <- base %>% 
  mutate(Localidade.UF = ifelse(Localidade.UF %in% c("NÃO APLICÁVEL","NÃO INFORMADO"),"ND",Localidade.UF),
         Localidade.UF = ifelse(is.na(Localidade.UF),"ND",Localidade.UF)) %>% 
  group_by(Localidade.UF,Ano) %>% filter(Ano<= 2018) %>% 
  dplyr::summarise(Despesa_Executada =  sum(Despesa.Executada, na.rm = T),
                   Empenhado =  sum(Empenhado, na.rm = T)) %>% 
  group_by(Ano) %>% 
  mutate(teste = ifelse(Despesa_Executada==Empenhado,1,0)) %>% 
  mutate(comparison = (Despesa_Executada/sum(Despesa_Executada)*100)) 

teste <- left_join(a2 %>% select(c(`UF*_novo`,Ano,comparison,Empenhado)) %>% 
                                   rename(comparison_novo = comparison,
                                          Empenhado_novo = Empenhado,
                                          UF = "UF*_novo"),
                   a3 %>% select(c("Localidade.UF",Ano,comparison,Empenhado)) %>% 
                                   rename(comparison = comparison,
                                          UF = "Localidade.UF")) %>% 
  filter(UF == "ND") %>% 
  mutate(teste = ifelse(comparison_novo<comparison,"Melhor","Pior"))

a <- base %>% mutate(x = `UF*_novo`,
                     x = ifelse(x=="PA",Localidade.UF,x),
                     x = ifelse(Estado=="PA" | Localidade.UF=="PA","PA",x))
#filter(`UF*_novo`=="PA") %>% filter(Estado=="DF")
table(a$`UF*_novo`)

a3 <- base %>% group_by(`UF*_novo`) %>% filter(Ano<= 2018) %>% 
  dplyr::summarise(Despesa_Executada =  sum(Despesa.Executada_igpdi, na.rm = T)) %>% 
  #group_by(Ano) %>%
  mutate(comparison = (Despesa_Executada/sum(Despesa_Executada)*100))  %>% 
  filter(`UF*_novo` %in% c("PA"))

a <- sample %>% filter(`UF*` != "PA" & `UF*_novo` == "PA") #  %>% filter(`UF*_novo`=="ND")

table(a$`UF*`)

b <- base %>% filter(UFis=="ND" & Ano == 2005)


write.csv2(base, file = "LOA.csv", row.names = F)

# Apenas exterior

exterior <- base %>% filter(`UF*` == "EX")

write.csv2(exterior, file = "exterior.csv", row.names = F)


## STEP SIX: SPREADHSEET ORGANIZATION -----

 

## UNI?O PARA EXPORTA??O PARA SQL -----
