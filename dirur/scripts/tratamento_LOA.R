library(tidyverse)
library(readr)
library(dplyr)
library(readxl)
library(openxlsx)
library(data.table)

usuario <- Sys.getenv("USERNAME")

setwd(paste("C:/Users/",
            usuario,
            "/Desktop/ipea-promob-main/ipea-promob-main/dados_estatísticos/Demanda - Nelson", sep = ""))





# Define vetor de anos
years <- 2000:2020

# Itera atraves dos arquivos
for (year in years) {
  file_name <- paste0("LOA ", year, ".csv")
  var_name <- paste0("LOA_", year)
  assign(var_name, fread(file_name, sep = ";", dec = ","))
}

SIGA_2021 <- fread("SIGA 2021.csv", sep = ";", dec = ",")
SIGA_2022 <- fread("SIGA 2022.csv", sep = ";", dec = ",")




# Cria vetor de nomes
new_names <- c("ANO", "LOCALIDADE_UF", "REGIAO", "UF*", "REGIAO*", "UF*_NOVO",
               "ORGAO_COD_DESC", "ORGAO_SUPERIOR_COD_DESC", "UG_COD_DESC", "UG_UF", "UO_COD_DESC_AJUSTADO",
               "ACAO_COD_DESC_AJUSTADA", "CODIGO", "NOME_FUNCIONAL", "FUNCAO", "SUBFUNCAO", "PROGRAMA",
               "ACAO", "ACAO*", "SUBTITULO", "IGP_DI", "ACUMULADO", "INDICE",
               "EMPENHADO", "DESPESA_EXECUTADA", "EMPENHADO_IGPDI", "DESPESA_EXECUTADA_IGPDI")

# Lista
dataframes <- list(LOA_2000, LOA_2001, LOA_2002, LOA_2003, LOA_2004, LOA_2005, LOA_2006,
                   LOA_2007, LOA_2008, LOA_2009, LOA_2010, LOA_2011, LOA_2012, LOA_2013,
                   LOA_2014, LOA_2015, LOA_2016, LOA_2017, LOA_2018, LOA_2019, LOA_2020)

# Renomeia para cada data frame
dataframes <- lapply(dataframes, function(df) {
  names(df) <- new_names
  return(df)
})



# Atualiza os nomes dos dataframes
LOA_2000 <- dataframes[[1]]
LOA_2001 <- dataframes[[2]]
LOA_2002 <- dataframes[[3]]
LOA_2003 <- dataframes[[4]]
LOA_2004 <- dataframes[[5]]
LOA_2005 <- dataframes[[6]]
LOA_2006 <- dataframes[[7]]
LOA_2007 <- dataframes[[8]]
LOA_2008 <- dataframes[[9]]
LOA_2009 <- dataframes[[10]]
LOA_2010 <- dataframes[[11]]
LOA_2011 <- dataframes[[12]]
LOA_2012 <- dataframes[[13]]
LOA_2013 <- dataframes[[14]]
LOA_2014 <- dataframes[[15]]
LOA_2015 <- dataframes[[16]]
LOA_2016 <- dataframes[[17]]
LOA_2017 <- dataframes[[18]]
LOA_2018 <- dataframes[[19]]
LOA_2019 <- dataframes[[20]]
LOA_2020 <- dataframes[[21]]


LOA_FINAL <- do.call(rbind, lapply(paste0("LOA_", 2001:2020), function(x) get(x)))
