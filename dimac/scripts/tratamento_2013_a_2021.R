library(tidyverse)
library(readr)
library(dplyr)
library(readxl)
library(tidyr) 

# Leitura das bases brutas

# setwd("C:/Users/USUARIO/Documents/Bases_brutas/Finbra/Finbra_receitas")

usuario <- Sys.getenv("USERNAME")

setwd(paste("C:/Users/",
            usuario,
            "/Desktop/IPEA DIMAC/codigos/Documents/Bases_brutas/Finbra/Finbra_receitas", sep = ""))

# Inicializa lista
rec_munic_list <- list()


# Define o perido 
start_year <- 2013
end_year <- 2021

# Itera sob os anos e carrega cada CSV 
for (year in start_year:end_year) {
  # Gera o caminho para leitura massificada
  file_path <- paste0("finbra_rec_munic_", year, ".csv")
  
  # Carrega com o encoding especifico
  rec_munic <- read.csv2(file_path, skip = 3, fileEncoding = "latin1")
  
  # Adiciona a coluna de 'ano'
  rec_munic$ano <- year
  
  # Remove campo desnecessario
  rec_munic <- rec_munic[, -7]
  
  # Atribui a uma variavel unica 
  assign(paste0("rec_munic_", year), rec_munic)
  
  # Adicionao data frame a lista
  rec_munic_list[[year]] <- rec_munic
  
}


# Combina todos os data frames em um unico
rec_munic_2013_2021 <- do.call(rbind, rec_munic_list)

# Remove data frames individuais
rm(list = paste0("rec_munic_", start_year:end_year))

# Extrai coluna 'codigo'
rec_munic_2013_2021$codigo <- substring(rec_munic_2013_2021$Conta, 1, 13)




# -- Filtragem das Variaveis  -- -- -- --

# -- -- Variáveis  2013-2014  -- -- 
# Registre-se que o layout dos dados pós-2013 e diferente do layout dos dados ate 2012, de modo que as (5) variáveis
# N_MUNIC, UF, Populacao, ano e Cod_IBGE não precisam ser filtradas. 

# Os codigos de interesse das variaveis extraidas no bienio 2013-2014 vao abaixo:

#Total Receitas
#1.0.0.0.00.00.00 - Receitas Correntes
#1.1.0.0.00.00.00 - Receita Tributaria
#1.1.1.2.04.30.00 - Retido nas Fontes
#1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana IPTU
#1.1.1.2.08.00.00 - Imposto sobre Transmissao Inter Vivos" de Bens Imoveis e de Direitos Reais sobre Imoveis ITBI"
#1.1.1.3.05.00.00 - Imposto sobre Servicos de Qualquer Natureza ISSQN
#1.2.1.0.00.00.00 - Contribuicoes Sociais
#1.3.2.0.00.00.00 - Receitas de Valores Mobiliarios
#1.3.2.2.00.00.00 - Dividendos
#1.3.2.3.00.00.00 - Participacoes
#1.7.0.0.00.00.00 - Transferencias Correntes
#1.7.2.0.00.00.00 - Transferencias Intergovernamentais
#1.7.2.1.00.00.00 - Transferencias da Uniao
#1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participacao dos Municipios FPM
#1.7.2.2.00.00.00 - Transferencias dos Estados
#1.7.2.2.01.01.00 - Cota-Parte do ICMS
#1.7.2.2.01.02.00 - Cota-Parte do IPVA
#1.9.1.1.00.00.00 - Multas e Juros de Mora dos Tributos (para a construcao das variaveis IPTU, ISS, e ITBI)
#1.9.1.3.00.00.00 - Multas e Juros de Mora da Divida Ativa dos Tributos (para a construcao das variaveis IPTU, ISS, e ITBI)
#1.9.3.1.00.00.00 - Receita da Divida Ativa Tributaria (para a construcao das variaveis IPTU, ISS, e ITBI)
#2.0.0.0.00.00.00 - Receitas de Capital
#2.1.0.0.00.00.00 - Operacoes de Credito
#2.3.0.0.00.00.00 - Amortizacao de Emprestimos
#2.4.0.0.00.00.00 - Transferencias de Capital
#7.0.0.0.00.00.00 - Receitas Correntes Intra-Orcamentarias
#8.0.0.0.00.00.00 - Receitas de Capital Intra-Orcamentarias

# Extracao dos dados brutos de 2013-2017 (as receitas totais de 2018 vem tambem no bojo)
# As variaveis aqui sao calculadas TODAS (em principio) liquidas de deducoes
# Alem disto, a partir de 2014 as receitas intraorcamentarias ficam MUITO mais detalhadas do que
# os agregados disponiveis ate 2013.
# Por fim, a partir de 2019 as despesas passam a ser explicitamente reportadas LIQUIDAS de intraorcamentarias




rec_finbra_2013_2014 <- filter(rec_munic_2013_2021, codigo %in% c("Total Receita", "1.0.0.0.00.00", "1.1.0.0.00.00",
                                                                  "1.1.1.2.02.00", "1.1.1.2.08.00", "1.1.1.3.05.00",
                                                                  "1.1.1.2.04.30", "1.2.1.0.00.00","1.3.2.0.00.00",
                                                                  "1.3.2.2.00.00", "1.3.2.3.00.00", "1.7.0.0.00.00",
                                                                  "1.7.2.0.00.00", "1.7.2.1.00.00", "1.7.2.1.01.02",
                                                                  "1.7.2.2.00.00", "1.7.2.2.01.01", "1.7.2.2.01.02",
                                                                  "1.9.1.1.00.00", "1.9.1.3.00.00", "1.9.3.1.00.00",
                                                                  "2.0.0.0.00.00", "2.1.0.0.00.00", "2.3.0.0.00.00",
                                                                  "2.4.0.0.00.00", "7.0.0.0.00.00", "8.0.0.0.00.00"))

rec_finbra_2013_2014_brutas <- filter(rec_finbra_2013_2014, Coluna == "Receitas Brutas Realizadas"|Coluna == "Receitas Realizadas")
rec_finbra_2013_2014_deducs <- filter(rec_finbra_2013_2014, Coluna != "Receitas Brutas Realizadas"& Coluna != "Receitas Realizadas")

rec_finbra_2013_2014_deducsa <- group_by(rec_finbra_2013_2014_deducs, ano, Cod.IBGE, codigo)
rec_finbra_2013_2014_deducsa <- summarise(rec_finbra_2013_2014_deducsa, deducs = sum(Valor))

rec_finbra_2013_2014_liquida <- left_join(rec_finbra_2013_2014_brutas, rec_finbra_2013_2014_deducsa)

rec_finbra_2013_2014_liquida[is.na(rec_finbra_2013_2014_liquida)] <- 0

rec_finbra_2013_2014_liquida$deducs <- ifelse(rec_finbra_2013_2014_liquida$deducs >= 0,
                                              rec_finbra_2013_2014_liquida$deducs,
                                              (-1)*rec_finbra_2013_2014_liquida$deducs)

rec_finbra_2013_2014_liquida<- mutate(rec_finbra_2013_2014_liquida, rec_liquida = Valor-deducs)

rm(rec_finbra_2013_2014, rec_finbra_2013_2014_brutas, rec_finbra_2013_2014_deducs, 
   rec_finbra_2013_2014_deducsa)

rec_finbra_2013_2014_liquida[9] <- NULL
rec_finbra_2013_2014_liquida[7] <- NULL
rec_finbra_2013_2014_liquida[5] <- NULL
rec_finbra_2013_2014_liquida[3] <- NULL

rec_finbra_2013_2014_liquida <- filter(rec_finbra_2013_2014_liquida, ano %in% c(2013, 2014))

rec_finbra_2013_2014 <- rec_finbra_2013_2014_liquida %>% pivot_wider(names_from = "Conta", values_from = c("rec_liquida", "deducs"))
rec_finbra_2013_2014[35:62] <- NULL

names(rec_finbra_2013_2014)[1] <- "N_MUNIC"
names(rec_finbra_2013_2014)[5] <- "rec_total"
names(rec_finbra_2013_2014)[6] <- "rec_corr"
names(rec_finbra_2013_2014)[7] <- "rec_trib"
names(rec_finbra_2013_2014)[8] <- "IPTU_PRINC"
names(rec_finbra_2013_2014)[9] <- "IRRF"
names(rec_finbra_2013_2014)[10] <- "ITBI_PRINC"
names(rec_finbra_2013_2014)[11] <- "ISSQN_PRINC"
names(rec_finbra_2013_2014)[12] <- "Contrib_soc"
names(rec_finbra_2013_2014)[13] <- "VAL_MOB"
names(rec_finbra_2013_2014)[14] <- "transf_corr"
names(rec_finbra_2013_2014)[15] <- "transf_corr_intergov"
names(rec_finbra_2013_2014)[16] <- "transf_corr_uniao"
names(rec_finbra_2013_2014)[17] <- "FPM"
names(rec_finbra_2013_2014)[18] <- "transf_corr_estados"
names(rec_finbra_2013_2014)[19] <- "ICMS"
names(rec_finbra_2013_2014)[20] <- "IPVA"
names(rec_finbra_2013_2014)[21] <- "multas_e_juros_mora_trib"
names(rec_finbra_2013_2014)[22] <- "multas_e_juros_mora_divat_trib"
names(rec_finbra_2013_2014)[23] <- "divat_trib"
names(rec_finbra_2013_2014)[24] <- "rec_capital"
names(rec_finbra_2013_2014)[25] <- "transf_cap"
names(rec_finbra_2013_2014)[26] <- "rec_corr_intraa"
names(rec_finbra_2013_2014)[27] <- "rec_emprestimos"
names(rec_finbra_2013_2014)[28] <- "AMORT"
names(rec_finbra_2013_2014)[29] <- "rec_cap_intraa"
names(rec_finbra_2013_2014)[30] <- "DIVIDENDOS"
names(rec_finbra_2013_2014)[31] <- "PARTICIPS"
names(rec_finbra_2013_2014)[32] <- "rec_corr_intrab"
names(rec_finbra_2013_2014)[33] <- "rec_cap_intrab"
names(rec_finbra_2013_2014)[34] <- "rec_deduc"

rec_finbra_2013_2014[is.na(rec_finbra_2013_2014)] <- 0

rec_finbra_2013_2014$rec_corr_intra <- ifelse(rec_finbra_2013_2014$rec_corr_intraa==0,
                                              rec_finbra_2013_2014$rec_corr_intrab, 
                                              rec_finbra_2013_2014$rec_corr_intraa)

rec_finbra_2013_2014$rec_cap_intra <- ifelse(rec_finbra_2013_2014$rec_cap_intraa==0,
                                             rec_finbra_2013_2014$rec_cap_intrab, 
                                             rec_finbra_2013_2014$rec_cap_intraa)

rec_finbra_2013_2014[32:33]<- NULL
rec_finbra_2013_2014[29]<- NULL
rec_finbra_2013_2014[26]<- NULL

rm(rec_finbra_2013_2014_liquida)

rec_finbra_2013_2014$Cod.IBGE <- substring(rec_finbra_2013_2014$Cod.IBGE, 1, 6)

# -- -- Variáveis  2015-2017  -- -- 
# Entre 2015 e 2017 o plano de contas ficou mais detalhado para os nossos propósitos porque passou a 
# informar a receita de multas e juros e da dívida ativa POR TRIBUTO. As novas variáveis de interesse
# vão abaixo

#Total Receitas
#1.0.0.0.00.00.00 - Receitas Correntes
#1.1.0.0.00.00.00 - Receita Tributária
#1.1.1.2.04.30.00 - Retido nas Fontes
#1.1.1.2.02.00.00 - Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU
#1.1.1.2.08.00.00 - Imposto sobre Transmissão Inter Vivos" de Bens Imóveis e de Direitos Reais sobre Imóveis ¿ ITBI"
#1.1.1.3.05.00.00 - Imposto sobre Serviços de Qualquer Natureza ¿ ISSQN
#1.2.1.0.00.00.00 - Contribuições Sociais
#1.3.2.0.00.00.00 - Receitas de Valores Mobiliários
#1.3.2.2.00.00.00 - Dividendos
#1.9.2.3.01.00.00 - Retorno de Investimentos Mediante Participação em Empresas e Projetos (substituindo 1.3.2.3.00.00.00 - Participações)
#1.7.0.0.00.00.00 - Transferências Correntes
#1.7.2.0.00.00.00 - Transferências Intergovernamentais
#1.7.2.1.00.00.00 - Transferências da União
#1.7.2.1.01.02.00 - Cota-Parte do Fundo de Participação dos Municípios ¿ FPM
#1.7.2.1.01.03.00 - Cota-Parte do Fundo de Participação dos Municípios - 1% Cota Anual
#1.7.2.1.01.04.00 - Cota-Parte do Fundo de Participação dos Municípios - 1% Cota entregue no mês de julho (67)(I)
#1.7.2.2.00.00.00 - Transferências dos Estados
#1.7.2.2.01.01.00 - Cota-Parte do ICMS
#1.7.2.2.01.02.00 - Cota-Parte do IPVA
#1.9.1.1.38.00.00 - Multas e Juros de Mora do Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU
#1.9.1.1.39.00.00 - Multas e Juros de Mora do Imposto sobre a Transmissão Inter Vivos de Bens Imóveis - ITBI
#1.9.1.1.40.00.00 - Multas e Juros de Mora do Imposto sobre Serviços de Qualquer Natureza ¿ ISS
#1.9.1.3.11.00.00 - Multas e Juros de Mora da Dívida Ativa do Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU
#1.9.1.3.12.00.00 - Multas e Juros de Mora da Dívida Ativa do Imposto sobre a Transmissão Inter Vivos de Bens Imóveis - ITBI
#1.9.1.3.13.00.00 - Multas e Juros de Mora da Dívida Ativa do Imposto sobre Serviços de Qualquer Natureza ¿ ISS
#1.9.3.1.11.00.00 - Receita da Dívida Ativa do Imposto sobre a Propriedade Predial e Territorial Urbana ¿ IPTU
#1.9.3.1.12.00.00 - Receita da Dívida Ativa do Imposto sobre a Transmissão Inter Vivos de Bens Imóveis - ITBI
#1.9.3.1.13.00.00 - Receita da Dívida Ativa do Imposto sobre Serviços de Qualquer Natureza ¿ ISS
#2.0.0.0.00.00.00 - Receitas de Capital
#2.1.0.0.00.00.00 - Operações de Crédito
#2.3.0.0.00.00.00 - Amortização de Empréstimos
#2.4.0.0.00.00.00 - Transferências de Capital
#7.0.0.0.00.00.00 - Receitas Correntes Intra-Orçamentárias
#8.0.0.0.00.00.00 - Receitas de Capital Intra-Orçamentárias

rec_finbra_2015_2017 <- filter(rec_munic_2013_2021, 
                               codigo %in% c("Total Receita", "1.0.0.0.00.00", "1.1.0.0.00.00", "1.1.1.2.02.00",
                                             "1.1.1.2.08.00", "1.1.1.3.05.00", "1.1.1.2.04.30", "1.2.1.0.00.00",
                                             "1.3.2.0.00.00", "1.3.2.2.00.00", "1.3.2.3.00.00", "1.9.2.3.01.00",
                                             "1.7.0.0.00.00", "1.7.2.0.00.00", "1.7.2.1.00.00", "1.7.2.1.01.02",
                                             "1.7.2.1.01.03", "1.7.2.2.00.00", "1.7.2.1.01.04", "1.7.2.2.01.01",
                                             "1.7.2.2.01.02", "1.9.1.1.38.00", "1.9.1.1.39.00", "1.9.1.1.40.00",
                                             "1.9.1.3.11.00", "1.9.1.3.12.00", "1.9.1.3.13.00", "1.9.3.1.11.00",
                                             "1.9.3.1.12.00", "1.9.3.1.13.00", "2.0.0.0.00.00", "2.1.0.0.00.00",
                                             "2.3.0.0.00.00", "2.4.0.0.00.00", "7.0.0.0.00.00", "8.0.0.0.00.00"))


rec_finbra_2015_2017 <- filter(rec_finbra_2015_2017, ano %in% c(2015, 2016, 2017))


rec_finbra_2015_2017_brutas <- filter(rec_finbra_2015_2017, Coluna == "Receitas Brutas Realizadas"|Coluna == "Receitas Realizadas")
rec_finbra_2015_2017_deducs <- filter(rec_finbra_2015_2017, Coluna != "Receitas Brutas Realizadas"& Coluna != "Receitas Realizadas")

rec_finbra_2015_2017_deducsa <- group_by(rec_finbra_2015_2017_deducs, ano, Cod.IBGE, codigo)

rec_finbra_2015_2017_deducsa <- summarise(rec_finbra_2015_2017_deducsa, deducs = sum(Valor))

rec_finbra_2015_2017_liquida <- left_join(rec_finbra_2015_2017_brutas, rec_finbra_2015_2017_deducsa)

rec_finbra_2015_2017_liquida[is.na(rec_finbra_2015_2017_liquida)] <- 0

rec_finbra_2015_2017_liquida$deducs <- ifelse(rec_finbra_2015_2017_liquida$deducs>=0,
                                              rec_finbra_2015_2017_liquida$deducs,
                                              (-1)*rec_finbra_2015_2017_liquida$deducs)

rec_finbra_2015_2017_liquida<- mutate(rec_finbra_2015_2017_liquida, rec_liquida = Valor-deducs)

rm(rec_finbra_2015_2017, rec_finbra_2015_2017_brutas, rec_finbra_2015_2017_deducs, rec_finbra_2015_2017_deducsa)

rec_finbra_2015_2017_liquida[9] <- NULL
rec_finbra_2015_2017_liquida[7] <- NULL
rec_finbra_2015_2017_liquida[5] <- NULL
rec_finbra_2015_2017_liquida[3] <- NULL

rec_finbra_2015_2017 <- rec_finbra_2015_2017_liquida %>% pivot_wider(names_from = "Conta", values_from = c("rec_liquida", "deducs"))
rm(rec_finbra_2015_2017_liquida)

rec_finbra_2015_2017[41:74] <- NULL

names(rec_finbra_2015_2017)[1] <- "N_MUNIC"
names(rec_finbra_2015_2017)[5] <- "rec_total"
names(rec_finbra_2015_2017)[6] <- "rec_corr"
names(rec_finbra_2015_2017)[7] <- "rec_trib"
names(rec_finbra_2015_2017)[8] <- "IPTU_PRINC"
names(rec_finbra_2015_2017)[9] <- "ITBI_PRINC"
names(rec_finbra_2015_2017)[10] <- "ISSQN_PRINC"
names(rec_finbra_2015_2017)[11] <- "Contrib_soc"
names(rec_finbra_2015_2017)[12] <- "VAL_MOB"
names(rec_finbra_2015_2017)[13] <- "transf_corr"
names(rec_finbra_2015_2017)[14] <- "transf_corr_intergov"
names(rec_finbra_2015_2017)[15] <- "transf_corr_uniao"
names(rec_finbra_2015_2017)[16] <- "FPM"
names(rec_finbra_2015_2017)[17] <- "FPM_dez"
names(rec_finbra_2015_2017)[18] <- "FPM_jul"
names(rec_finbra_2015_2017)[19] <- "transf_corr_estados"
names(rec_finbra_2015_2017)[20] <- "ICMS"
names(rec_finbra_2015_2017)[21] <- "IPVA"
names(rec_finbra_2015_2017)[22] <- "IPTU_multas_e_juros"
names(rec_finbra_2015_2017)[23] <- "ISS_multas_e_juros"
names(rec_finbra_2015_2017)[24] <- "IPTU_divat_multas_e_juros"
names(rec_finbra_2015_2017)[25] <- "ISS_divat_multas_e_juros"
names(rec_finbra_2015_2017)[26] <- "IPTU_divat"
names(rec_finbra_2015_2017)[27] <- "ISS_divat"
names(rec_finbra_2015_2017)[28] <- "rec_capital"
names(rec_finbra_2015_2017)[29] <- "rec_emprestimos"
names(rec_finbra_2015_2017)[30] <- "transf_cap"
names(rec_finbra_2015_2017)[31] <- "rec_corr_intra"
names(rec_finbra_2015_2017)[32] <- "ITBI_multas_e_juros"
names(rec_finbra_2015_2017)[33] <- "IRRF"
names(rec_finbra_2015_2017)[34] <- "DIVIDENDOS"
names(rec_finbra_2015_2017)[35] <- "AMORT"
names(rec_finbra_2015_2017)[36] <- "ITBI_divat_multas_e_juros"
names(rec_finbra_2015_2017)[37] <- "ITBI_divat"
names(rec_finbra_2015_2017)[38] <- "rec_cap_intra"
names(rec_finbra_2015_2017)[39] <- "PARTICIPS"
names(rec_finbra_2015_2017)[40] <- "rec_deduc"

rec_finbra_2015_2017[is.na(rec_finbra_2015_2017)] <- 0

rec_finbra_2015_2017 <- mutate(rec_finbra_2015_2017, FPM = (FPM + FPM_jul + FPM_dez))

rec_finbra_2015_2017[17:18] <- NULL

rec_finbra_2015_2017$Cod.IBGE <- substring(rec_finbra_2015_2017$Cod.IBGE, 1, 6)


# -- -- Variáveis  2018  -- -- 
# Os códigos de interesse das variáveis extraídas em 2018 vão abaixo:

#Total Receitas
#1.0.0.0.00.0.0 - Receitas Correntes
#1.1.0.0.00.0.0 - Impostos, Taxas e Contribuições de Melhoria
#1.1.1.3.03.0.0 - Imposto sobre a Renda - Retido na Fonte
#1.1.1.8.01.1.0 - Imposto sobre a Propriedade Predial e Territorial Urbana
#1.1.1.8.01.4.0 - Imposto sobre Transmissão ¿Inter Vivos¿ de Bens Imóveis e de Direitos Reais sobre Imóveis
#1.1.1.8.02.3.0 - Imposto sobre Serviços de Qualquer Natureza
#1.2.1.0.00.0.0 - Contribuições Sociais
#1.3.2.0.00.0.0 - Valores Mobiliários
#1.3.2.2.00.0.0 - Dividendos
#1.3.2.3.00.0.0 - Participações
#1.7.0.0.00.0.0 - Transferências Correntes
#1.7.1.0.00.0.0 - Transferências da União e de suas Entidades
#1.7.1.8.01.2.0 - Cota-Parte do Fundo de Participação dos Municípios - Cota Mensal
#1.7.1.8.01.3.0 - Cota-Parte do Fundo de Participação do Municípios ¿ 1% Cota entregue no mês de dezembro
#1.7.1.8.01.4.0 - Cota-Parte do Fundo de Participação dos Municípios - 1% Cota entregue no mês de julho
#1.7.2.0.00.0.0 - Transferências dos Estados e do Distrito Federal e de suas Entidades
#1.7.2.8.01.1.0 - Cota-Parte do ICMS
#1.7.2.8.01.2.0 - Cota-Parte do IPVA
#2.0.0.0.00.0.0 - Receitas de Capital
#2.1.0.0.00.0.0 - Operações de Crédito
#2.3.0.0.00.0.0 - Amortização de Empréstimos
#2.4.0.0.00.0.0 - Transferências de Capital
#7.0.0.0.00.0.0 - Receitas Correntes - Intraorçamentárias
#8.0.0.0.00.0.0 - Receitas de Capital - Intraorçamentárias

rec_munic_2013_2021$codigo <- substring(rec_munic_2013_2021$Conta, 1, 14)

rec_finbra_2018 <- filter(rec_munic_2013_2021, 
                          codigo %in% c("Total Receitas", "1.0.0.0.00.0.0", "1.1.0.0.00.0.0", "1.1.1.8.01.1.0",
                                        "1.1.1.8.01.4.0", "1.1.1.8.02.3.0", "1.2.1.0.00.0.0", "1.3.2.0.00.0.0", "1.3.2.2.00.0.0",
                                        "1.3.2.3.00.0.0", "1.7.0.0.00.0.0", "1.7.1.0.00.0.0", "1.7.1.8.01.2.0", "1.7.1.8.01.3.0", 
                                        "1.7.1.8.01.4.0", "1.7.2.0.00.0.0", "1.7.2.8.01.1.0", "1.7.2.8.01.2.0", "2.0.0.0.00.0.0", 
                                        "2.1.0.0.00.0.0", "2.3.0.0.00.0.0", "2.4.0.0.00.0.0", "1.1.1.3.03.0.0", "7.0.0.0.00.0.0",
                                        "8.0.0.0.00.0.0"), ano == 2018)
                          

rec_finbra_2018_brutas <- filter(rec_finbra_2018, Coluna == "Receitas Brutas Realizadas")
rec_finbra_2018_deducs <- filter(rec_finbra_2018, Coluna != "Receitas Brutas Realizadas")

rec_finbra_2018_deducsa <- group_by(rec_finbra_2018_deducs, ano, Cod.IBGE, codigo)
rec_finbra_2018_deducsa <- summarise(rec_finbra_2018_deducsa, deducs = sum(Valor))

rec_finbra_2018_liquida <- left_join(rec_finbra_2018_brutas, rec_finbra_2018_deducsa)

rec_finbra_2018_liquida[is.na(rec_finbra_2018_liquida)] <- 0

rec_finbra_2018_liquida$deducs <- ifelse(rec_finbra_2018_liquida$deducs>=0, rec_finbra_2018_liquida$deducs,
                                         (-1)*rec_finbra_2018_liquida$deducs)

rec_finbra_2018_liquida<- mutate(rec_finbra_2018_liquida, rec_liquida = Valor-deducs)

rm(rec_finbra_2018, rec_finbra_2018_brutas, rec_finbra_2018_deducs, 
   rec_finbra_2018_deducsa)

rec_finbra_2018_liquida[9] <- NULL
rec_finbra_2018_liquida[7] <- NULL
rec_finbra_2018_liquida[5] <- NULL
rec_finbra_2018_liquida[3] <- NULL

rec_finbra_2018 <- rec_finbra_2018_liquida %>% pivot_wider(names_from = "Conta", values_from = c("rec_liquida", "deducs"))
rm(rec_finbra_2018_liquida)

rec_finbra_2018[31:54] <- NULL

names(rec_finbra_2018)[1] <- "N_MUNIC"
names(rec_finbra_2018)[5] <- "rec_total"
names(rec_finbra_2018)[6] <- "rec_corr"
names(rec_finbra_2018)[7] <- "rec_trib"
names(rec_finbra_2018)[8] <- "IRRF"
names(rec_finbra_2018)[9] <- "IPTU_TOTAL"
names(rec_finbra_2018)[10] <- "ITBI_TOTAL"
names(rec_finbra_2018)[11] <- "ISSQN_TOTAL"
names(rec_finbra_2018)[12] <- "VAL_MOB"
names(rec_finbra_2018)[13] <- "transf_corr"
names(rec_finbra_2018)[14] <- "transf_corr_uniao"
names(rec_finbra_2018)[15] <- "FPM"
names(rec_finbra_2018)[16] <- "transf_corr_estados"
names(rec_finbra_2018)[17] <- "ICMS"
names(rec_finbra_2018)[18] <- "IPVA"
names(rec_finbra_2018)[19] <- "rec_capital"
names(rec_finbra_2018)[20] <- "transf_cap"
names(rec_finbra_2018)[21] <- "rec_corr_intra"
names(rec_finbra_2018)[22] <- "Contrib_soc"
names(rec_finbra_2018)[23] <- "FPM_dez"
names(rec_finbra_2018)[24] <- "FPM_jul"
names(rec_finbra_2018)[25] <- "AMORT"
names(rec_finbra_2018)[26] <- "rec_emprestimos"
names(rec_finbra_2018)[27] <- "rec_cap_intra"
names(rec_finbra_2018)[28] <- "DIVIDENDOS"
names(rec_finbra_2018)[29] <- "PARTICIPS"
names(rec_finbra_2018)[30] <- "rec_deduc"

rec_finbra_2018[is.na(rec_finbra_2018)] <- 0

rec_finbra_2018 <- mutate(rec_finbra_2018, FPM = (FPM + FPM_jul + FPM_dez))

rec_finbra_2018[23:24] <- NULL

rec_finbra_2018$Cod.IBGE <- substring(rec_finbra_2018$Cod.IBGE, 1, 6)


# -- -- Variáveis  2019-2021  -- -- 
#RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)
# 1.0.0.0.00.0.0 - Receitas Correntes
#1.1.0.0.00.0.0 - Impostos, Taxas e Contribuições de Melhoria
#1.1.1.3.03.0.0 - Imposto sobre a Renda - Retido na Fonte
#1.1.1.8.01.1.0 Imposto sobre a Propriedade Predial e Territorial Urbana
#1.1.1.8.01.4.0 Imposto sobre Transmissão ¿Inter Vivos¿ de Bens Imóveis e de Direitos Reais sobre Imóveis
#1.1.1.8.02.3.0 Imposto sobre Serviços de Qualquer Natureza
#1.2.1.0.00.0.0 - Contribuições Sociais
#1.3.2.0.00.0.0 - Valores Mobiliários
#1.3.2.2.00.0.0 - Dividendos
#1.3.2.3.00.0.0 - Participações
#1.7.0.0.00.0.0 - Transferências Correntes
#1.7.1.0.00.0.0 - Transferências da União e de suas Entidades
#1.7.1.8.01.2.0 Cota-Parte do Fundo de Participação dos Municípios - Cota Mensal
#1.7.1.8.01.3.0 Cota-Parte do Fundo de Participação do Municípios ¿ 1% Cota entregue no mês de dezembro
#1.7.1.8.01.4.0 Cota-Parte do Fundo de Participação dos Municípios - 1% Cota entregue no mês de julho
#1.7.2.0.00.0.0 - Transferências dos Estados e do Distrito Federal e de suas Entidades
#1.7.2.8.01.1.0 Cota-Parte do ICMS
#1.7.2.8.01.2.0 Cota-Parte do IPVA
#2.0.0.0.00.0.0 - Receitas de Capital
#2.1.0.0.00.0.0 - Operações de Crédito
#2.3.0.0.00.0.0 - Amortização de Empréstimos
#2.4.0.0.00.0.0 - Transferências de Capital
#7.0.0.0.00.0.0 - Receitas Correntes (Intraorçamentárias)
#8.0.0.0.00.0.0 - Receitas de Capital (Intraorçamentárias)

rec_finbra_2019_2021 <- filter(rec_munic_2013_2021, codigo %in% c("RECEITAS (EXCE", "1.0.0.0.00.0.0", "1.1.0.0.00.0.0", "1.1.1.8.01.1.0",
                                                                  "1.1.1.8.01.4.0", "1.1.1.8.02.3.0", "1.2.1.0.00.0.0", "1.3.2.0.00.0.0",
                                                                  "1.3.2.2.00.0.0", "1.3.2.3.00.0.0", "1.7.0.0.00.0.0", "1.7.1.0.00.0.0",
                                                                  "1.7.1.8.01.2.0", "1.7.1.8.01.3.0", "1.7.1.8.01.4.0", "1.7.2.0.00.0.0",
                                                                  "1.7.2.8.01.1.0", "1.7.2.8.01.2.0", "2.0.0.0.00.0.0", "2.1.0.0.00.0.0", 
                                                                  "2.3.0.0.00.0.0", "2.4.0.0.00.0.0", "1.1.1.3.03.0.0", "7.0.0.0.00.0.0",
                                                                  "8.0.0.0.00.0.0")) 

rec_finbra_2019_2021_brutas <- filter(rec_finbra_2019_2021, Coluna == "Receitas Brutas Realizadas"|Coluna == "Receitas Realizadas")
rec_finbra_2019_2021_deducs <- filter(rec_finbra_2019_2021, Coluna != "Receitas Brutas Realizadas"& Coluna != "Receitas Realizadas")

rec_finbra_2019_2021_deducsa <- group_by(rec_finbra_2019_2021_deducs, ano, Cod.IBGE, codigo)
rec_finbra_2019_2021_deducsa <- summarise(rec_finbra_2019_2021_deducsa, deducs = sum(Valor))

rec_finbra_2019_2021_liquida <- left_join(rec_finbra_2019_2021_brutas, rec_finbra_2019_2021_deducsa)

rec_finbra_2019_2021_liquida[is.na(rec_finbra_2019_2021_liquida)] <- 0
rec_finbra_2019_2021_liquida <- filter(rec_finbra_2019_2021_liquida, ano %in% c(2019, 2020, 2021))

rec_finbra_2019_2021_liquida$deducs <- ifelse(rec_finbra_2019_2021_liquida$deducs>=0,
                                              rec_finbra_2019_2021_liquida$deducs,
                                              (-1)*rec_finbra_2019_2021_liquida$deducs)

rec_finbra_2019_2021_liquida<- mutate(rec_finbra_2019_2021_liquida, rec_liquida = Valor-deducs)

rm(rec_finbra_2019_2021, rec_finbra_2019_2021_brutas, rec_finbra_2019_2021_deducs, 
   rec_finbra_2019_2021_deducsa)                            

rec_finbra_2019_2021_liquida[9] <-NULL
rec_finbra_2019_2021_liquida[7] <-NULL
rec_finbra_2019_2021_liquida[5] <-NULL
rec_finbra_2019_2021_liquida[3] <-NULL

rec_finbra_2019_2021 <- rec_finbra_2019_2021_liquida %>% pivot_wider(names_from = "Conta", values_from = c("rec_liquida", "deducs"))

rec_finbra_2019_2021[39:70] <- NULL


names(rec_finbra_2019_2021)[1] <- "N_MUNIC"
names(rec_finbra_2019_2021)[5] <- "rec_total"
names(rec_finbra_2019_2021)[6] <- "rec_corr"
names(rec_finbra_2019_2021)[7] <- "rec_trib"
names(rec_finbra_2019_2021)[8] <- "IRRF"
names(rec_finbra_2019_2021)[9] <- "IPTU_TOTAL"
names(rec_finbra_2019_2021)[10] <- "ITBI_TOTAL"
names(rec_finbra_2019_2021)[11] <- "ISSQN_TOTAL"
names(rec_finbra_2019_2021)[12] <- "Contrib_soc"
names(rec_finbra_2019_2021)[13] <- "VAL_MOB"
names(rec_finbra_2019_2021)[14]<- "transf_corr"
names(rec_finbra_2019_2021)[15]<- "transf_corr_uniao"
names(rec_finbra_2019_2021)[16]<- "FPM"
names(rec_finbra_2019_2021)[17] <- "FPM_dez"
names(rec_finbra_2019_2021)[18]<- "FPM_jul"
names(rec_finbra_2019_2021)[19]<- "transf_corr_estados"
names(rec_finbra_2019_2021)[20]<- "ICMS"
names(rec_finbra_2019_2021)[21]<- "IPVA"
names(rec_finbra_2019_2021)[22] <- "rec_capital"
names(rec_finbra_2019_2021)[23] <- "AMORT"
names(rec_finbra_2019_2021)[24]<- "transf_cap"
names(rec_finbra_2019_2021)[25] <- "rec_corr_intra"
names(rec_finbra_2019_2021)[26] <- "rec_emprestimos"
names(rec_finbra_2019_2021)[27] <- "rec_cap_intra"
names(rec_finbra_2019_2021)[28] <- "DIVIDENDOS"
names(rec_finbra_2019_2021)[29] <- "PARTICIPS"
names(rec_finbra_2019_2021)[30] <- "IPTUb"
names(rec_finbra_2019_2021)[31]<- "ITBIb"
names(rec_finbra_2019_2021)[32]<- "ISSb"
names(rec_finbra_2019_2021)[33] <- "FPMb"
names(rec_finbra_2019_2021)[34] <- "FPM_deza"
names(rec_finbra_2019_2021)[35] <- "FPM_jula"
names(rec_finbra_2019_2021)[36]<- "ICMSb"
names(rec_finbra_2019_2021)[37]<- "IPVAb"
names(rec_finbra_2019_2021)[38]<- "rec_deduc"


rec_finbra_2019_2021[is.na(rec_finbra_2019_2021)] <- 0

rec_finbra_2019_2021$rec_total <- ifelse(rec_finbra_2019_2021$rec_total < 0,
                                         (-1)*rec_finbra_2019_2021$rec_total, 
                                         rec_finbra_2019_2021$rec_total)

rec_finbra_2019_2021$IPTU_TOTAL <- ifelse(rec_finbra_2019_2021$IPTU_TOTAL == 0,
                                          rec_finbra_2019_2021$IPTUb, 
                                          rec_finbra_2019_2021$IPTU_TOTAL)

rec_finbra_2019_2021[30] <- NULL

rec_finbra_2019_2021$ITBI_TOTAL <- ifelse(rec_finbra_2019_2021$ITBI_TOTAL == 0,
                                          rec_finbra_2019_2021$ITBIb, 
                                          rec_finbra_2019_2021$ITBI_TOTAL)

rec_finbra_2019_2021[30] <- NULL


rec_finbra_2019_2021$ISSQN_TOTAL <- ifelse(rec_finbra_2019_2021$ISSQN_TOTAL == 0,
                                           rec_finbra_2019_2021$ISSb, 
                                           rec_finbra_2019_2021$ISSQN_TOTAL)

rec_finbra_2019_2021[30] <- NULL

rec_finbra_2019_2021$ICMS <- ifelse(rec_finbra_2019_2021$ICMS == 0,
                                    rec_finbra_2019_2021$ICMSb, 
                                    rec_finbra_2019_2021$ICMS)

rec_finbra_2019_2021[33] <- NULL

rec_finbra_2019_2021$IPVA <- ifelse(rec_finbra_2019_2021$IPVA == 0,
                                    rec_finbra_2019_2021$IPVAb, 
                                    rec_finbra_2019_2021$IPVA)

rec_finbra_2019_2021[33] <- NULL

rec_finbra_2019_2021$FPM <- ifelse(rec_finbra_2019_2021$FPM == 0,
                                   rec_finbra_2019_2021$FPMb, 
                                   rec_finbra_2019_2021$FPM)

rec_finbra_2019_2021[30] <- NULL

rec_finbra_2019_2021$FPM_dez <- ifelse(rec_finbra_2019_2021$FPM_dez == 0,
                                       rec_finbra_2019_2021$FPM_deza, 
                                       rec_finbra_2019_2021$FPM_dez)

rec_finbra_2019_2021[30] <- NULL

rec_finbra_2019_2021$FPM_jul <- ifelse(rec_finbra_2019_2021$FPM_jul == 0,
                                       rec_finbra_2019_2021$FPM_jula, 
                                       rec_finbra_2019_2021$FPM_jul)

rec_finbra_2019_2021[30] <- NULL


rec_finbra_2019_2021 <- mutate(rec_finbra_2019_2021, FPM = (FPM + FPM_jul + FPM_dez))

rec_finbra_2019_2021[17:18] <- NULL

rec_finbra_2019_2021$Cod.IBGE <- substring(rec_finbra_2019_2021$Cod.IBGE, 1, 6)



## Outputs / Dados de saida

## Formato csv
write.csv2(rec_finbra_2013_2014, file = "rec_finbra_2013_2014.csv")
write.csv2(rec_finbra_2015_2017, file = "rec_finbra_2015_2017.csv")
write.csv2(rec_finbra_2018, file ="rec_finbra_2018.csv")
write.csv2(rec_finbra_2019_2021, file ="rec_finbra_2019_2021.csv")
