library(tidyverse)
library(readr)
library(dplyr)
library(readxl)

setwd("C:/Users/USUARIO/Documents/Bases_brutas/Finbra/Finbra_receitas")

rec_munic_2002 <-read_excel("finbra_rec_munic_2002.xlsx")
rec_munic_2002$Cod_IBGE <- ifelse(nchar(rec_munic_2002$CD_MUN)==1,
                                   paste(rec_munic_2002$CD_UF,"000",rec_munic_2002$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2002$CD_MUN)==2,
                                          paste(rec_munic_2002$CD_UF,"00",rec_munic_2002$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2002$CD_MUN)==3,
                                                 paste(rec_munic_2002$CD_UF,"0",rec_munic_2002$CD_MUN, sep=""),
                                                 paste(rec_munic_2002$CD_UF,rec_munic_2002$CD_MUN, sep=""))))

rec_munic_2002[1:2] <- NULL

rec_munic_2003 <-read_excel("finbra_rec_munic_2003.xlsx")
rec_munic_2003$Cod_IBGE <- ifelse(nchar(rec_munic_2003$CD_MUN)==1,
                                   paste(rec_munic_2003$CD_UF,"000",rec_munic_2003$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2003$CD_MUN)==2,
                                          paste(rec_munic_2003$CD_UF,"00",rec_munic_2003$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2003$CD_MUN)==3,
                                                 paste(rec_munic_2003$CD_UF,"0",rec_munic_2003$CD_MUN, sep=""),
                                                 paste(rec_munic_2003$CD_UF,rec_munic_2003$CD_MUN, sep=""))))
rec_munic_2003[1:2] <- NULL

rec_munic_2004 <-read_excel("finbra_rec_munic_2004.xlsx")
rec_munic_2004$Cod_IBGE <- ifelse(nchar(rec_munic_2004$CD_MUN)==1,
                                   paste(rec_munic_2004$CD_UF,"000",rec_munic_2004$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2004$CD_MUN)==2,
                                          paste(rec_munic_2004$CD_UF,"00",rec_munic_2004$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2004$CD_MUN)==3,
                                                 paste(rec_munic_2004$CD_UF,"0",rec_munic_2004$CD_MUN, sep=""),
                                                 paste(rec_munic_2004$CD_UF,rec_munic_2004$CD_MUN, sep=""))))

rec_munic_2004[1:2] <- NULL

rec_munic_2005 <-read_excel("finbra_rec_munic_2005.xlsx")
rec_munic_2005$Cod_IBGE <- ifelse(nchar(rec_munic_2005$CD_MUN)==1,
                                   paste(rec_munic_2005$CD_UF,"000",rec_munic_2005$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2005$CD_MUN)==2,
                                          paste(rec_munic_2005$CD_UF,"00",rec_munic_2005$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2005$CD_MUN)==3,
                                                 paste(rec_munic_2005$CD_UF,"0",rec_munic_2005$CD_MUN, sep=""),
                                                 paste(rec_munic_2005$CD_UF,rec_munic_2005$CD_MUN, sep=""))))

rec_munic_2005[1:2] <- NULL

rec_munic_2006 <-read_excel("finbra_rec_munic_2006.xlsx")

rec_munic_2006$Cod_IBGE <- ifelse(nchar(rec_munic_2006$CD_MUN)==1,
                                   paste(rec_munic_2006$CD_UF,"000",rec_munic_2006$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2006$CD_MUN)==2,
                                          paste(rec_munic_2006$CD_UF,"00",rec_munic_2006$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2006$CD_MUN)==3,
                                                 paste(rec_munic_2006$CD_UF,"0",rec_munic_2006$CD_MUN, sep=""),
                                                 paste(rec_munic_2006$CD_UF,rec_munic_2006$CD_MUN, sep=""))))

rec_munic_2006[1:2] <- NULL


rec_munic_2007 <-read_excel("finbra_rec_munic_2007.xlsx")
rec_munic_2007$Cod_IBGE <- ifelse(nchar(rec_munic_2007$CD_MUN)==1,
                                   paste(rec_munic_2007$CD_UF,"000",rec_munic_2007$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2007$CD_MUN)==2,
                                          paste(rec_munic_2007$CD_UF,"00",rec_munic_2007$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2007$CD_MUN)==3,
                                                 paste(rec_munic_2007$CD_UF,"0",rec_munic_2007$CD_MUN, sep=""),
                                                 paste(rec_munic_2007$CD_UF,rec_munic_2007$CD_MUN, sep=""))))

rec_munic_2007[1:2] <- NULL

rec_munic_2008 <-read_excel("finbra_rec_munic_2008.xlsx")
rec_munic_2008$Cod_IBGE <- ifelse(nchar(rec_munic_2008$CD_MUN)==1,
                                   paste(rec_munic_2008$CD_UF,"000",rec_munic_2008$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2008$CD_MUN)==2,
                                          paste(rec_munic_2008$CD_UF,"00",rec_munic_2008$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2008$CD_MUN)==3,
                                                 paste(rec_munic_2008$CD_UF,"0",rec_munic_2008$CD_MUN, sep=""),
                                                 paste(rec_munic_2008$CD_UF,rec_munic_2008$CD_MUN, sep=""))))

rec_munic_2008[1:2] <- NULL

rec_munic_2009 <-read_excel("finbra_rec_munic_2009.xlsx")
rec_munic_2009$Cod_IBGE <- ifelse(nchar(rec_munic_2009$CD_MUN)==1,
                                   paste(rec_munic_2009$CD_UF,"000",rec_munic_2009$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2009$CD_MUN)==2,
                                          paste(rec_munic_2009$CD_UF,"00",rec_munic_2009$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2009$CD_MUN)==3,
                                                 paste(rec_munic_2009$CD_UF,"0",rec_munic_2009$CD_MUN, sep=""),
                                                 paste(rec_munic_2009$CD_UF,rec_munic_2009$CD_MUN, sep=""))))

rec_munic_2009[1:2] <- NULL

rec_munic_2010 <-read_excel("finbra_rec_munic_2010.xlsx")
rec_munic_2010$Cod_IBGE <- ifelse(nchar(rec_munic_2010$CD_MUN)==1,
                                   paste(rec_munic_2010$CD_UF,"000",rec_munic_2010$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2010$CD_MUN)==2,
                                          paste(rec_munic_2010$CD_UF,"00",rec_munic_2010$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2010$CD_MUN)==3,
                                                 paste(rec_munic_2010$CD_UF,"0",rec_munic_2010$CD_MUN, sep=""),
                                                 paste(rec_munic_2010$CD_UF,rec_munic_2010$CD_MUN, sep=""))))

rec_munic_2010[1:2] <- NULL

rec_munic_2011 <-read_excel("finbra_rec_munic_2011.xlsx")
rec_munic_2011$Cod_IBGE <- ifelse(nchar(rec_munic_2011$CD_MUN)==1,
                                   paste(rec_munic_2011$CD_UF,"000",rec_munic_2011$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2011$CD_MUN)==2,
                                          paste(rec_munic_2011$CD_UF,"00",rec_munic_2011$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2011$CD_MUN)==3,
                                                 paste(rec_munic_2011$CD_UF,"0",rec_munic_2011$CD_MUN, sep=""),
                                                 paste(rec_munic_2011$CD_UF,rec_munic_2011$CD_MUN, sep=""))))

rec_munic_2011[1:2] <- NULL

rec_munic_2012 <-read_excel("finbra_rec_munic_2012.xlsx")

rec_munic_2012$Cod_IBGE <- ifelse(nchar(rec_munic_2012$CD_MUN)==1,
                                   paste(rec_munic_2012$CD_UF,"000",rec_munic_2012$CD_MUN, sep=""),
                                   ifelse(nchar(rec_munic_2012$CD_MUN)==2,
                                          paste(rec_munic_2012$CD_UF,"00",rec_munic_2012$CD_MUN, sep=""),
                                          ifelse(nchar(rec_munic_2012$CD_MUN)==3,
                                                 paste(rec_munic_2012$CD_UF,"0",rec_munic_2012$CD_MUN, sep=""),
                                                 paste(rec_munic_2012$CD_UF,rec_munic_2012$CD_MUN, sep=""))))

rec_munic_2012[1:2] <- NULL


#=============Dados anuais ==================================================

#2002

rec_munic_2002[13:16] <- NULL
rec_munic_2002[14:16] <- NULL
rec_munic_2002[15:19] <- NULL
rec_munic_2002[19:23] <- NULL
rec_munic_2002[20:22] <- NULL
rec_munic_2002[23:41] <- NULL
rec_munic_2002[24] <- NULL
rec_munic_2002[25] <- NULL
rec_munic_2002[27:31] <- NULL
rec_munic_2002[29:42] <- NULL
rec_munic_2002[30:33] <- NULL
rec_munic_2002[7] <- NULL

names(rec_munic_2002)[1] <- "N_MUNIC"
names(rec_munic_2002)[4] <- "rec_total"
names(rec_munic_2002)[5] <- "rec_corr"
names(rec_munic_2002)[6] <- "rec_trib"
names(rec_munic_2002)[7] <- "IPTU_PRINC"
names(rec_munic_2002)[9] <- "ITBI_PRINC"
names(rec_munic_2002)[10] <- "ISSQN_PRINC"
names(rec_munic_2002)[12] <- "Contrib_soc"
names(rec_munic_2002)[13] <- "VAL_MOB"
names(rec_munic_2002)[14] <- "transf_corr"
names(rec_munic_2002)[15] <- "transf_corr_intergov"
names(rec_munic_2002)[16] <- "transf_corr_uniao"
names(rec_munic_2002)[17] <- "FPM"
names(rec_munic_2002)[18] <- "SUS"
names(rec_munic_2002)[19] <- "transf_corr_estados"
names(rec_munic_2002)[20] <- "ICMS"
names(rec_munic_2002)[21] <- "IPVA"
names(rec_munic_2002)[22] <- "multas_e_juros_mora"
names(rec_munic_2002)[23] <- "rec_div_ativa"
names(rec_munic_2002)[24] <- "rec_capital"
names(rec_munic_2002)[25] <- "rec_emprestimos"
names(rec_munic_2002)[26] <- "AMORT"
names(rec_munic_2002)[27] <- "transf_cap"
names(rec_munic_2002)[28] <- "rec_deduc"


rec_munic_2002 <- mutate(rec_munic_2002, rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB,
         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
         test1 = rec_total-rec_corr-rec_capital+rec_deduc)

#A variÃ¡vel teste1 ser zero significa que a variÃ¡vel rec_total (mas nÃ£o as variÃ¡veis rec corrente e rec capital)
#Ã© (sÃ£o) lÃ­quida(s) de deduÃ§Ãµes. Por isso as deduÃ§Ãµes nÃ£o entram no cÃ¡lculo das receitas primÃ¡rias.
#Mas importa notar que todas as variÃ¡veis de receita exclusive receitas totais sÃ£o BRUTAS de deduÃ§Ãµes.
# As Ãºnicas variÃ¡veis que podem ser calculadas  lÃ­quida de deduÃ§Ãµes sÃ£o as receitas correntes, ICMS e FPM
#porque hÃ¡ variÃ¡veis de deduÃ§Ã£o desta receitas.

rec_munic_2002$test1 <- as.numeric(format(rec_munic_2002$test1, scientific = F))
rec_munic_2002$test1 <- round(rec_munic_2002$test1, digits=4)
rec_munic_2002$ano <- 2002


#2003

rec_munic_2003[13:16] <- NULL
rec_munic_2003[14:16] <- NULL
rec_munic_2003[15:19] <- NULL
rec_munic_2003[19:23] <- NULL
rec_munic_2003[20:22] <- NULL
rec_munic_2003[23:41] <- NULL
rec_munic_2003[24] <- NULL
rec_munic_2003[25] <- NULL
rec_munic_2003[27:31] <- NULL
rec_munic_2003[29:42] <- NULL
rec_munic_2003[30:33] <- NULL
rec_munic_2003[7] <- NULL

names(rec_munic_2003)[1] <- "N_MUNIC"
names(rec_munic_2003)[4] <- "rec_total"
names(rec_munic_2003)[5] <- "rec_corr"
names(rec_munic_2003)[6] <- "rec_trib"
names(rec_munic_2003)[7] <- "IPTU_PRINC"
names(rec_munic_2003)[9] <- "ITBI_PRINC"
names(rec_munic_2003)[10] <- "ISSQN_PRINC"
names(rec_munic_2003)[12] <- "Contrib_soc"
names(rec_munic_2003)[13] <- "VAL_MOB"
names(rec_munic_2003)[14] <- "transf_corr"
names(rec_munic_2003)[15] <- "transf_corr_intergov"
names(rec_munic_2003)[16] <- "transf_corr_uniao"
names(rec_munic_2003)[17] <- "FPM"
names(rec_munic_2003)[18] <- "SUS"
names(rec_munic_2003)[19] <- "transf_corr_estados"
names(rec_munic_2003)[20] <- "ICMS"
names(rec_munic_2003)[21] <- "IPVA"
names(rec_munic_2003)[22] <- "multas_e_juros_mora"
names(rec_munic_2003)[23] <- "rec_div_ativa"
names(rec_munic_2003)[24] <- "rec_capital"
names(rec_munic_2003)[25] <- "rec_emprestimos"
names(rec_munic_2003)[26] <- "AMORT"
names(rec_munic_2003)[27] <- "transf_cap"
names(rec_munic_2003)[28] <- "rec_deduc"


rec_munic_2003 <- mutate(rec_munic_2003, rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB,
                         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
                         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
                         test1 = rec_total-rec_corr-rec_capital+rec_deduc)

rec_munic_2003$test1 <- as.numeric(format(rec_munic_2003$test1, scientific = F))
rec_munic_2003$test1 <- round(rec_munic_2003$test1, digits=4)
rec_munic_2003$ano <- 2003

#Nenhuma mudanÃ§a na questÃ£o das deduÃ§Ãµes de receita.

#2004
rec_munic_2004[7:8] <- NULL
rec_munic_2004[8] <- NULL
rec_munic_2004[10] <- NULL
rec_munic_2004[12:15] <- NULL
rec_munic_2004[13:15] <- NULL
rec_munic_2004[14:18] <- NULL
rec_munic_2004[17] <- NULL
rec_munic_2004[18:28] <- NULL
rec_munic_2004[19:20] <- NULL
rec_munic_2004[20] <- NULL
rec_munic_2004[22:64] <- NULL
rec_munic_2004[23] <- NULL
rec_munic_2004[24] <- NULL
rec_munic_2004[26:30] <- NULL
rec_munic_2004[28:63] <- NULL
rec_munic_2004[29:32] <- NULL

names(rec_munic_2004)[1] <- "N_MUNIC"
names(rec_munic_2004)[4] <- "rec_total"
names(rec_munic_2004)[5] <- "rec_corr"
names(rec_munic_2004)[6] <- "rec_trib"
names(rec_munic_2004)[7] <- "IPTU_PRINC"
names(rec_munic_2004)[9] <- "ITBI_PRINC"
names(rec_munic_2004)[10] <- "ISSQN_PRINC"
names(rec_munic_2004)[12] <- "Contrib_soc"
names(rec_munic_2004)[13] <- "VAL_MOB"
names(rec_munic_2004)[14] <- "transf_corr"
names(rec_munic_2004)[15] <- "transf_corr_intergov"
names(rec_munic_2004)[16] <- "transf_corr_uniao"
names(rec_munic_2004)[17] <- "FPM"
names(rec_munic_2004)[18] <- "SUS"
names(rec_munic_2004)[19] <- "transf_corr_estados"
names(rec_munic_2004)[20] <- "ICMS"
names(rec_munic_2004)[21] <- "IPVA"
names(rec_munic_2004)[22] <- "multas_e_juros_mora"
names(rec_munic_2004)[23] <- "rec_div_ativa"
names(rec_munic_2004)[24] <- "rec_capital"
names(rec_munic_2004)[25] <- "rec_emprestimos"
names(rec_munic_2004)[26] <- "AMORT"
names(rec_munic_2004)[27] <- "transf_cap"
names(rec_munic_2004)[28] <- "rec_deduc"


rec_munic_2004 <- mutate(rec_munic_2004, rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB,
                         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
                         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
                         test1 = rec_total-rec_corr-rec_capital+rec_deduc)

rec_munic_2004$test1 <- as.numeric(format(rec_munic_2004$test1, scientific = F))
rec_munic_2004$test1 <- round(rec_munic_2004$test1, digits=4)
rec_munic_2004$ano <- 2004

#Nenhuma mudanÃ§a na questÃ£o das deduÃ§Ãµes.

#2005

rec_munic_2005[7:8] <- NULL
rec_munic_2005[8] <- NULL
rec_munic_2005[10] <- NULL
rec_munic_2005[12:15] <- NULL
rec_munic_2005[13:15] <- NULL
rec_munic_2005[14:18] <- NULL
rec_munic_2005[17] <- NULL
rec_munic_2005[18:27] <- NULL
rec_munic_2005[19:20] <- NULL
rec_munic_2005[20] <- NULL
rec_munic_2005[22:64] <- NULL
rec_munic_2005[23] <- NULL
rec_munic_2005[24] <- NULL
rec_munic_2005[26:30] <- NULL
rec_munic_2005[28:69] <- NULL
rec_munic_2005[29:32] <- NULL

names(rec_munic_2005)[1] <- "UF"
names(rec_munic_2005)[2] <- "N_MUNIC"
names(rec_munic_2005)[4] <- "rec_total"
names(rec_munic_2005)[5] <- "rec_corr"
names(rec_munic_2005)[6] <- "rec_trib"
names(rec_munic_2005)[7] <- "IPTU_PRINC"
names(rec_munic_2005)[9] <- "ITBI_PRINC"
names(rec_munic_2005)[10] <- "ISSQN_PRINC"
names(rec_munic_2005)[12] <- "Contrib_soc"
names(rec_munic_2005)[13] <- "VAL_MOB"
names(rec_munic_2005)[14] <- "transf_corr"
names(rec_munic_2005)[15] <- "transf_corr_intergov"
names(rec_munic_2005)[16] <- "transf_corr_uniao"
names(rec_munic_2005)[17] <- "FPM"
names(rec_munic_2005)[18] <- "SUS"
names(rec_munic_2005)[19] <- "transf_corr_estados"
names(rec_munic_2005)[20] <- "ICMS"
names(rec_munic_2005)[21] <- "IPVA"
names(rec_munic_2005)[22] <- "multas_e_juros_mora"
names(rec_munic_2005)[23] <- "rec_div_ativa"
names(rec_munic_2005)[24] <- "rec_capital"
names(rec_munic_2005)[25] <- "rec_emprestimos"
names(rec_munic_2005)[26] <- "AMORT"
names(rec_munic_2005)[27] <- "transf_cap"
names(rec_munic_2005)[28] <- "rec_deduc"


rec_munic_2005 <- mutate(rec_munic_2005, rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB,
                         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
                         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
                         test1 = rec_total-rec_corr-rec_capital+rec_deduc)

rec_munic_2005$test1 <- as.numeric(format(rec_munic_2005$test1, scientific = F))
rec_munic_2005$test1 <- round(rec_munic_2005$test1, digits=4)
rec_munic_2005$ano <- 2005

# Nenhuma mudanÃ§a na questÃ£o das deduÃ§Ãµes.

#2006

rec_munic_2006[7:8] <- NULL
rec_munic_2006[8] <- NULL
rec_munic_2006[10] <- NULL
rec_munic_2006[12:15] <- NULL
rec_munic_2006[13:15] <- NULL
rec_munic_2006[14:19] <- NULL
rec_munic_2006[17] <- NULL
rec_munic_2006[18:27] <- NULL
rec_munic_2006[19:20] <- NULL
rec_munic_2006[20] <- NULL
rec_munic_2006[22:65] <- NULL
rec_munic_2006[23] <- NULL
rec_munic_2006[24] <- NULL
rec_munic_2006[26:30] <- NULL
rec_munic_2006[28:71] <- NULL
rec_munic_2006[29:32] <- NULL

names(rec_munic_2006)[1] <- "UF"
names(rec_munic_2006)[2] <- "N_MUNIC"
names(rec_munic_2006)[4] <- "rec_total"
names(rec_munic_2006)[5] <- "rec_corr"
names(rec_munic_2006)[6] <- "rec_trib"
names(rec_munic_2006)[7] <- "IPTU_PRINC"
names(rec_munic_2006)[9] <- "ITBI_PRINC"
names(rec_munic_2006)[10] <- "ISSQN_PRINC"
names(rec_munic_2006)[12] <- "Contrib_soc"
names(rec_munic_2006)[13] <- "VAL_MOB"
names(rec_munic_2006)[14] <- "transf_corr"
names(rec_munic_2006)[15] <- "transf_corr_intergov"
names(rec_munic_2006)[16] <- "transf_corr_uniao"
names(rec_munic_2006)[17] <- "FPM"
names(rec_munic_2006)[18] <- "SUS"
names(rec_munic_2006)[19] <- "transf_corr_estados"
names(rec_munic_2006)[20] <- "ICMS"
names(rec_munic_2006)[21] <- "IPVA"
names(rec_munic_2006)[22] <- "multas_e_juros_mora"
names(rec_munic_2006)[23] <- "rec_div_ativa"
names(rec_munic_2006)[24] <- "rec_capital"
names(rec_munic_2006)[25] <- "rec_emprestimos"
names(rec_munic_2006)[26] <- "AMORT"
names(rec_munic_2006)[27] <- "transf_cap"
names(rec_munic_2006)[28] <- "rec_deduc"


rec_munic_2006 <- mutate(rec_munic_2006, rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB,
                         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
                         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
                         test1 = rec_total-rec_corr-rec_capital+rec_deduc)

rec_munic_2006$test1 <- as.numeric(format(rec_munic_2006$test1, scientific = F))
rec_munic_2006$test1 <- round(rec_munic_2006$test1, digits=4)
rec_munic_2006$ano <- 2006

#Nenhuma mudanÃ§a  na questÃ£o das deduÃ§Ãµes

#2007 (as contas passam a possibilitar excluir as receitas intraorÃ§amentÃ¡rias )

rec_munic_2007[7:8] <- NULL
rec_munic_2007[9:10] <- NULL
rec_munic_2007[10] <- NULL
rec_munic_2007[12:15] <- NULL
rec_munic_2007[13:15] <- NULL
rec_munic_2007[14:19] <- NULL
rec_munic_2007[17] <- NULL
rec_munic_2007[18:27] <- NULL
rec_munic_2007[19:23] <- NULL
rec_munic_2007[20] <- NULL
rec_munic_2007[22:67] <- NULL
rec_munic_2007[23] <- NULL
rec_munic_2007[24] <- NULL
rec_munic_2007[26:30] <- NULL
rec_munic_2007[28:74] <- NULL
rec_munic_2007[29:36] <- NULL

names(rec_munic_2007)[1] <- "UF"
names(rec_munic_2007)[2] <- "N_MUNIC"
names(rec_munic_2007)[4] <- "rec_total"
names(rec_munic_2007)[5] <- "rec_corr"
names(rec_munic_2007)[6] <- "rec_trib"
names(rec_munic_2007)[7] <- "IPTU_PRINC"
names(rec_munic_2007)[8] <- "IRRF"
names(rec_munic_2007)[9] <- "ITBI_PRINC"
names(rec_munic_2007)[10] <- "ISSQN_PRINC"
names(rec_munic_2007)[12] <- "Contrib_soc"
names(rec_munic_2007)[13] <- "VAL_MOB"
names(rec_munic_2007)[14] <- "transf_corr"
names(rec_munic_2007)[15] <- "transf_corr_intergov"
names(rec_munic_2007)[16] <- "transf_corr_uniao"
names(rec_munic_2007)[17] <- "FPM"
names(rec_munic_2007)[18] <- "SUS"
names(rec_munic_2007)[19] <- "transf_corr_estados"
names(rec_munic_2007)[20] <- "ICMS"
names(rec_munic_2007)[21] <- "IPVA"
names(rec_munic_2007)[22] <- "multas_e_juros_mora"
names(rec_munic_2007)[23] <- "rec_div_ativa"
names(rec_munic_2007)[24] <- "rec_capital"
names(rec_munic_2007)[25] <- "rec_emprestimos"
names(rec_munic_2007)[26] <- "AMORT"
names(rec_munic_2007)[27] <- "transf_cap"
names(rec_munic_2007)[28] <- "rec_deduc"
names(rec_munic_2007)[29] <- "rec_corr_intra"
names(rec_munic_2007)[30] <- "rec_cap_intra"

rec_munic_2007 <- mutate(rec_munic_2007, rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB - rec_corr_intra - rec_cap_intra,
                         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
                         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
                         test1 = rec_total-rec_corr-rec_capital+rec_deduc - rec_corr_intra - rec_cap_intra)

rec_munic_2007$test1 <- as.numeric(format(rec_munic_2007$test1, scientific = F))
rec_munic_2007$test1 <- round(rec_munic_2007$test1, digits=4)
rec_munic_2007$ano <- 2007

# A partir de 2007 as receitas intraorÃ§amentÃ¡rias de capital e corrente passam a ser explicitadas.
# A Ãºnica possibilidade de cÃ¡lculo de receitas lÃ­quidas de deduÃ§Ãµes Ã© a da receita corrente. 
# As Ãºnicas possibilidades de cÃ¡lculo de receitas lÃ­quidas de intraorÃ§amentÃ¡rias sÃ£o receitas correntes 
# lÃ­quidas de intraorÃ§amentÃ¡rias e receitas de capital lÃ­quidas de intraorÃ§amentÃ¡rias.
# AlÃ©m das receitas lÃ­quidas de deduÃ§Ãµes antes disponÃ­veis (rec_corr, FMP, ICMS), a partir de 2007 
# hÃ¡ tambÃ©m a possibilidade de calcular nÃºmeros para o IPVA, para as transf_corr_uniao e 
# transf_corr_estados.

#2008

rec_munic_2008[7:8] <- NULL
rec_munic_2008[9:10] <- NULL
rec_munic_2008[10] <- NULL
rec_munic_2008[12:15] <- NULL
rec_munic_2008[13:15] <- NULL
rec_munic_2008[14:19] <- NULL
rec_munic_2008[17] <- NULL
rec_munic_2008[18:27] <- NULL
rec_munic_2008[19:23] <- NULL
rec_munic_2008[20] <- NULL
rec_munic_2008[22:67] <- NULL
rec_munic_2008[23] <- NULL
rec_munic_2008[24] <- NULL
rec_munic_2008[26:30] <- NULL
rec_munic_2008[28:74] <- NULL
rec_munic_2008[29:36] <- NULL

names(rec_munic_2008)[1] <- "UF"
names(rec_munic_2008)[2] <- "N_MUNIC"
names(rec_munic_2008)[4] <- "rec_total"
names(rec_munic_2008)[5] <- "rec_corr"
names(rec_munic_2008)[6] <- "rec_trib"
names(rec_munic_2008)[7] <- "IPTU_PRINC"
names(rec_munic_2008)[8] <- "IRRF"
names(rec_munic_2008)[9] <- "ITBI_PRINC"
names(rec_munic_2008)[10] <- "ISSQN_PRINC"
names(rec_munic_2008)[12] <- "Contrib_soc"
names(rec_munic_2008)[13] <- "VAL_MOB"
names(rec_munic_2008)[14] <- "transf_corr"
names(rec_munic_2008)[15] <- "transf_corr_intergov"
names(rec_munic_2008)[16] <- "transf_corr_uniao"
names(rec_munic_2008)[17] <- "FPM"
names(rec_munic_2008)[18] <- "SUS"
names(rec_munic_2008)[19] <- "transf_corr_estados"
names(rec_munic_2008)[20] <- "ICMS"
names(rec_munic_2008)[21] <- "IPVA"
names(rec_munic_2008)[22] <- "multas_e_juros_mora"
names(rec_munic_2008)[23] <- "rec_div_ativa"
names(rec_munic_2008)[24] <- "rec_capital"
names(rec_munic_2008)[25] <- "rec_emprestimos"
names(rec_munic_2008)[26] <- "AMORT"
names(rec_munic_2008)[27] <- "transf_cap"
names(rec_munic_2008)[28] <- "rec_deduc"
names(rec_munic_2008)[29] <- "rec_corr_intra"
names(rec_munic_2008)[30] <- "rec_cap_intra"

rec_munic_2008 <- mutate(rec_munic_2008, rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB - rec_corr_intra - rec_cap_intra,
                         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
                         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
                         test1 = rec_total-rec_corr-rec_capital+rec_deduc - rec_corr_intra - rec_cap_intra)

rec_munic_2008$test1 <- as.numeric(format(rec_munic_2008$test1, scientific = F))
rec_munic_2008$test1 <- round(rec_munic_2008$test1, digits=4)
rec_munic_2008$ano <- 2008


# Nenhuma mudanÃ§a na questÃ£o das deduÃ§Ãµes de receitas em relaÃ§Ã£o a 2007.


# Em 2009 (os dados permitem separar receita da dÃ­vida ativa tributÃ¡ria da dÃ­vida ativa total. 
# Permitem ainda separar dividendos e participaÃ§Ãµes do total das receitas mobiliÃ¡rias. 
# HÃ¡ tambÃ©m uma pequena mudanÃ§a em relaÃ§Ã£o a deduÃ§Ãµes das receitas correntes
# que passaram a ser contabilizadas - como "outras deduÃ§Ãµes" - mesmo que nao relacionadas ao FUNDEB.

rec_munic_2009[7:8] <- NULL
rec_munic_2009[9:10] <- NULL
rec_munic_2009[10] <- NULL
rec_munic_2009[12:15] <- NULL
rec_munic_2009[13:32] <- NULL
rec_munic_2009[14] <- NULL
rec_munic_2009[16:26] <- NULL
rec_munic_2009[19] <- NULL
rec_munic_2009[20:29] <- NULL
rec_munic_2009[21:25] <- NULL
rec_munic_2009[22] <- NULL
rec_munic_2009[24:69] <- NULL
rec_munic_2009[25:26] <- NULL
rec_munic_2009[26:27] <- NULL
rec_munic_2009[28:32] <- NULL
rec_munic_2009[30:76] <- NULL
rec_munic_2009[31:39] <- NULL

names(rec_munic_2009)[1] <- "UF"
names(rec_munic_2009)[2] <- "N_MUNIC"
names(rec_munic_2009)[4] <- "rec_total"
names(rec_munic_2009)[5] <- "rec_corr"
names(rec_munic_2009)[6] <- "rec_trib"
names(rec_munic_2009)[7] <- "IPTU_PRINC"
names(rec_munic_2009)[8] <- "IRRF"
names(rec_munic_2009)[9] <- "ITBI_PRINC"
names(rec_munic_2009)[10] <- "ISSQN_PRINC"
names(rec_munic_2009)[12] <- "Contrib_soc"
names(rec_munic_2009)[13] <- "VAL_MOB"
names(rec_munic_2009)[14] <- "DIVIDENDOS"
names(rec_munic_2009)[15] <- "PARTICIPS"
names(rec_munic_2009)[16] <- "transf_corr"
names(rec_munic_2009)[17] <- "transf_corr_intergov"
names(rec_munic_2009)[18] <- "transf_corr_uniao"
names(rec_munic_2009)[19] <- "FPM"
names(rec_munic_2009)[20] <- "SUS"
names(rec_munic_2009)[21] <- "transf_corr_estados"
names(rec_munic_2009)[22] <- "ICMS"
names(rec_munic_2009)[23] <- "IPVA"
names(rec_munic_2009)[24] <- "multas_e_juros_mora"
names(rec_munic_2009)[25] <- "rec_div_ativa"
names(rec_munic_2009)[26] <- "rec_capital"
names(rec_munic_2009)[27] <- "rec_emprestimos"
names(rec_munic_2009)[28] <- "AMORT"
names(rec_munic_2009)[29] <- "transf_cap"
names(rec_munic_2009)[30] <- "rec_deduc"
names(rec_munic_2009)[31] <- "rec_corr_intra"
names(rec_munic_2009)[32] <- "rec_cap_intra"

rec_munic_2009 <- mutate(rec_munic_2009, VAL_MOB_liq = VAL_MOB - DIVIDENDOS - PARTICIPS,
                         rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB_liq - rec_corr_intra - rec_cap_intra,
                         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
                         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
                         test1 = rec_total-rec_corr-rec_capital+rec_deduc - rec_corr_intra - rec_cap_intra)

rec_munic_2009$test1 <- as.numeric(format(rec_munic_2009$test1, scientific = F))
rec_munic_2009$test1 <- round(rec_munic_2009$test1, digits=4)
rec_munic_2009$ano <- 2009


#2010 (nenhuma mudanÃ§a em relaÃ§Ã£o Ã s deduÃ§Ãµes de receitas)

rec_munic_2010[7:8] <- NULL
rec_munic_2010[9:10] <- NULL
rec_munic_2010[10] <- NULL
rec_munic_2010[12:15] <- NULL
rec_munic_2010[13:38] <- NULL
rec_munic_2010[14] <- NULL
rec_munic_2010[16:34] <- NULL
rec_munic_2010[19] <- NULL
rec_munic_2010[20:29] <- NULL
rec_munic_2010[21:25] <- NULL
rec_munic_2010[22] <- NULL
rec_munic_2010[24:69] <- NULL
rec_munic_2010[25:32] <- NULL
rec_munic_2010[25] <- NULL
rec_munic_2010[26:27] <- NULL
rec_munic_2010[28:32] <- NULL
rec_munic_2010[30:76] <- NULL
rec_munic_2010[31:39] <- NULL

names(rec_munic_2010)[1] <- "UF"
names(rec_munic_2010)[2] <- "N_MUNIC"
names(rec_munic_2010)[4] <- "rec_total"
names(rec_munic_2010)[5] <- "rec_corr"
names(rec_munic_2010)[6] <- "rec_trib"
names(rec_munic_2010)[7] <- "IPTU_PRINC"
names(rec_munic_2010)[8] <- "IRRF"
names(rec_munic_2010)[9] <- "ITBI_PRINC"
names(rec_munic_2010)[10] <- "ISSQN_PRINC"
names(rec_munic_2010)[12] <- "Contrib_soc"
names(rec_munic_2010)[13] <- "VAL_MOB"
names(rec_munic_2010)[14] <- "DIVIDENDOS"
names(rec_munic_2010)[15] <- "PARTICIPS"
names(rec_munic_2010)[16] <- "transf_corr"
names(rec_munic_2010)[17] <- "transf_corr_intergov"
names(rec_munic_2010)[18] <- "transf_corr_uniao"
names(rec_munic_2010)[19] <- "FPM"
names(rec_munic_2010)[20] <- "SUS"
names(rec_munic_2010)[21] <- "transf_corr_estados"
names(rec_munic_2010)[22] <- "ICMS"
names(rec_munic_2010)[23] <- "IPVA"
names(rec_munic_2010)[24] <- "multas_e_juros_mora"
names(rec_munic_2010)[25] <- "rec_div_ativa"
names(rec_munic_2010)[26] <- "rec_capital"
names(rec_munic_2010)[27] <- "rec_emprestimos"
names(rec_munic_2010)[28] <- "AMORT"
names(rec_munic_2010)[29] <- "transf_cap"
names(rec_munic_2010)[30] <- "rec_deduc"
names(rec_munic_2010)[31] <- "rec_corr_intra"
names(rec_munic_2010)[32] <- "rec_cap_intra"

rec_munic_2010 <- mutate(rec_munic_2010, VAL_MOB_liq = VAL_MOB - DIVIDENDOS - PARTICIPS,
                         rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB_liq - rec_corr_intra - rec_cap_intra,
                         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
                         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
                         test1 = rec_total-rec_corr-rec_capital+rec_deduc - rec_corr_intra - rec_cap_intra)

rec_munic_2010$test1 <- as.numeric(format(rec_munic_2010$test1, scientific = F))
rec_munic_2010$test1 <- round(rec_munic_2010$test1, digits=4)
rec_munic_2010$ano <- 2010


#2011

rec_munic_2011[7:8] <- NULL
rec_munic_2011[9:10] <- NULL
rec_munic_2011[10] <- NULL
rec_munic_2011[12:15] <- NULL
rec_munic_2011[13:38] <- NULL
rec_munic_2011[14] <- NULL
rec_munic_2011[16:34] <- NULL
rec_munic_2011[19] <- NULL
rec_munic_2011[20:29] <- NULL
rec_munic_2011[21:25] <- NULL
rec_munic_2011[22] <- NULL
rec_munic_2011[24:69] <- NULL
rec_munic_2011[25:32] <- NULL
rec_munic_2011[25] <- NULL
rec_munic_2011[26:27] <- NULL
rec_munic_2011[28:32] <- NULL
rec_munic_2011[30:76] <- NULL
rec_munic_2011[31:39] <- NULL

names(rec_munic_2011)[1] <- "UF"
names(rec_munic_2011)[2] <- "N_MUNIC"
names(rec_munic_2011)[4] <- "rec_total"
names(rec_munic_2011)[5] <- "rec_corr"
names(rec_munic_2011)[6] <- "rec_trib"
names(rec_munic_2011)[7] <- "IPTU_PRINC"
names(rec_munic_2011)[8] <- "IRRF"
names(rec_munic_2011)[9] <- "ITBI_PRINC"
names(rec_munic_2011)[10] <- "ISSQN_PRINC"
names(rec_munic_2011)[12] <- "Contrib_soc"
names(rec_munic_2011)[13] <- "VAL_MOB"
names(rec_munic_2011)[14] <- "DIVIDENDOS"
names(rec_munic_2011)[15] <- "PARTICIPS"
names(rec_munic_2011)[16] <- "transf_corr"
names(rec_munic_2011)[17] <- "transf_corr_intergov"
names(rec_munic_2011)[18] <- "transf_corr_uniao"
names(rec_munic_2011)[19] <- "FPM"
names(rec_munic_2011)[20] <- "SUS"
names(rec_munic_2011)[21] <- "transf_corr_estados"
names(rec_munic_2011)[22] <- "ICMS"
names(rec_munic_2011)[23] <- "IPVA"
names(rec_munic_2011)[24] <- "multas_e_juros_mora"
names(rec_munic_2011)[25] <- "rec_div_ativa"
names(rec_munic_2011)[26] <- "rec_capital"
names(rec_munic_2011)[27] <- "rec_emprestimos"
names(rec_munic_2011)[28] <- "AMORT"
names(rec_munic_2011)[29] <- "transf_cap"
names(rec_munic_2011)[30] <- "rec_deduc"
names(rec_munic_2011)[31] <- "rec_corr_intra"
names(rec_munic_2011)[32] <- "rec_cap_intra"

rec_munic_2011 <- mutate(rec_munic_2011, VAL_MOB_liq = VAL_MOB - DIVIDENDOS - PARTICIPS,
                         rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB_liq - rec_corr_intra - rec_cap_intra,
                         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
                         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
                         test1 = rec_total-rec_corr-rec_capital+rec_deduc - rec_corr_intra - rec_cap_intra)

rec_munic_2011$test1 <- as.numeric(format(rec_munic_2011$test1, scientific = F))
rec_munic_2011$test1 <- round(rec_munic_2011$test1, digits=4)
rec_munic_2011$ano <- 2011




#2012 - Nenhuma mudanÃ§a na questÃ£o das deduÃ§Ãµes de receitas

rec_munic_2012[7:8] <- NULL
rec_munic_2012[9:10] <- NULL
rec_munic_2012[10] <- NULL
rec_munic_2012[12:15] <- NULL
rec_munic_2012[13:38] <- NULL
rec_munic_2012[14] <- NULL
rec_munic_2012[16:34] <- NULL
rec_munic_2012[19] <- NULL
rec_munic_2012[20:29] <- NULL
rec_munic_2012[21:25] <- NULL
rec_munic_2012[22] <- NULL
rec_munic_2012[24:69] <- NULL
rec_munic_2012[25:32] <- NULL
rec_munic_2012[25] <- NULL
rec_munic_2012[26:27] <- NULL
rec_munic_2012[28:32] <- NULL
rec_munic_2012[30:76] <- NULL
rec_munic_2012[31:39] <- NULL

names(rec_munic_2012)[1] <- "UF"
names(rec_munic_2012)[2] <- "N_MUNIC"
names(rec_munic_2012)[4] <- "rec_total"
names(rec_munic_2012)[5] <- "rec_corr"
names(rec_munic_2012)[6] <- "rec_trib"
names(rec_munic_2012)[7] <- "IPTU_PRINC"
names(rec_munic_2012)[8] <- "IRRF"
names(rec_munic_2012)[9] <- "ITBI_PRINC"
names(rec_munic_2012)[10] <- "ISSQN_PRINC"
names(rec_munic_2012)[12] <- "Contrib_soc"
names(rec_munic_2012)[13] <- "VAL_MOB"
names(rec_munic_2012)[14] <- "DIVIDENDOS"
names(rec_munic_2012)[15] <- "PARTICIPS"
names(rec_munic_2012)[16] <- "transf_corr"
names(rec_munic_2012)[17] <- "transf_corr_intergov"
names(rec_munic_2012)[18] <- "transf_corr_uniao"
names(rec_munic_2012)[19] <- "FPM"
names(rec_munic_2012)[20] <- "SUS"
names(rec_munic_2012)[21] <- "transf_corr_estados"
names(rec_munic_2012)[22] <- "ICMS"
names(rec_munic_2012)[23] <- "IPVA"
names(rec_munic_2012)[24] <- "multas_e_juros_mora"
names(rec_munic_2012)[25] <- "rec_div_ativa"
names(rec_munic_2012)[26] <- "rec_capital"
names(rec_munic_2012)[27] <- "rec_emprestimos"
names(rec_munic_2012)[28] <- "AMORT"
names(rec_munic_2012)[29] <- "transf_cap"
names(rec_munic_2012)[30] <- "rec_deduc"
names(rec_munic_2012)[31] <- "rec_corr_intra"
names(rec_munic_2012)[32] <- "rec_cap_intra"

rec_munic_2012 <- mutate(rec_munic_2012, VAL_MOB_liq = VAL_MOB - DIVIDENDOS - PARTICIPS,
                         rec_prim = rec_total - rec_emprestimos - AMORT - VAL_MOB_liq - rec_corr_intra - rec_cap_intra,
                         IPTU_total = IPTU_PRINC+(multas_e_juros_mora+rec_div_ativa)*(IPTU_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ITBI_total = ITBI_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         ISSQN_total = ISSQN_PRINC+(multas_e_juros_mora+rec_div_ativa)*(ITBI_PRINC/(IPTU_PRINC+ITBI_PRINC+ISSQN_PRINC)),
                         rec_proprias1 = ITBI_total+ISSQN_total+IPTU_total, 
                         rec_proprias2 = rec_total+rec_deduc - transf_corr - transf_cap,
                         test1 = rec_total-rec_corr-rec_capital+rec_deduc - rec_corr_intra - rec_cap_intra)

rec_munic_2012$test1 <- as.numeric(format(rec_munic_2012$test1, scientific = F))
rec_munic_2012$test1 <- round(rec_munic_2012$test1, digits=4)
rec_munic_2012$ano <- 2012

#Ajustamento das receitas totais para brutas de deduÃ§Ãµes e lÃ­quidas de receitas intra-orÃ§amentÃ¡rias

rec_munic_2002 <- mutate(rec_munic_2002, rec_bruta_total = rec_total+rec_deduc)
rec_munic_2003 <- mutate(rec_munic_2003, rec_bruta_total = rec_total+rec_deduc)
rec_munic_2004 <- mutate(rec_munic_2004, rec_bruta_total = rec_total+rec_deduc)
rec_munic_2005 <- mutate(rec_munic_2005, rec_bruta_total = rec_total+rec_deduc)
rec_munic_2006 <- mutate(rec_munic_2006, rec_bruta_total = rec_total+rec_deduc)
rec_munic_2007 <- mutate(rec_munic_2007, rec_bruta_total = rec_total+rec_deduc - rec_corr_intra - rec_cap_intra)
rec_munic_2008 <- mutate(rec_munic_2008, rec_bruta_total = rec_total+rec_deduc - rec_corr_intra - rec_cap_intra)
rec_munic_2009 <- mutate(rec_munic_2009, rec_bruta_total = rec_total+rec_deduc - rec_corr_intra - rec_cap_intra)
rec_munic_2010 <- mutate(rec_munic_2010, rec_bruta_total = rec_total+rec_deduc - rec_corr_intra - rec_cap_intra)
rec_munic_2011 <- mutate(rec_munic_2011, rec_bruta_total = rec_total+rec_deduc - rec_corr_intra - rec_cap_intra)
rec_munic_2012 <- mutate(rec_munic_2012, rec_bruta_total = rec_total+rec_deduc - rec_corr_intra - rec_cap_intra)
  

# JUntando tudo em um painel

#AtÃ© 2006 os dados brutos anuais tÃªm 37 colunas. Em 2007 atÃ© 2008 eles tÃªm 39 colunas porque as receitas de capital e 
#correntes intraorÃ§amentÃ¡rias sÃ£o adicionadas. A partir de 2009 eles tÃªm 42 colunas porque as receitas com dividendos
# e participaÃ§Ãµes sÃ£o adicionadas e uma nova variÃ¡vel de juros sobre valores mobiliÃ¡rios foi criada.

rec_munic_2002$rec_corr_intra <- 0 
rec_munic_2002$rec_cap_intra <- 0
rec_munic_2002$DIVIDENDOS <- 0 
rec_munic_2002$PARTICIPS <- 0 
rec_munic_2002$VAL_MOB_liq <- rec_munic_2002$VAL_MOB

rec_munic_2003$rec_corr_intra <- 0 
rec_munic_2003$rec_cap_intra <- 0
rec_munic_2003$DIVIDENDOS <- 0 
rec_munic_2003$PARTICIPS <- 0 
rec_munic_2003$VAL_MOB_liq <- rec_munic_2003$VAL_MOB

rec_munic_2004$rec_corr_intra <- 0 
rec_munic_2004$rec_cap_intra <- 0
rec_munic_2004$DIVIDENDOS <- 0 
rec_munic_2004$PARTICIPS <- 0 
rec_munic_2004$VAL_MOB_liq <- rec_munic_2004$VAL_MOB

rec_munic_2005$rec_corr_intra <- 0 
rec_munic_2005$rec_cap_intra <- 0
rec_munic_2005$DIVIDENDOS <- 0 
rec_munic_2005$PARTICIPS <- 0 
rec_munic_2005$VAL_MOB_liq <- rec_munic_2005$VAL_MOB

rec_munic_2006$rec_corr_intra <- 0 
rec_munic_2006$rec_cap_intra <- 0
rec_munic_2006$DIVIDENDOS <- 0 
rec_munic_2006$PARTICIPS <- 0 
rec_munic_2006$VAL_MOB_liq <- rec_munic_2006$VAL_MOB

rec_munic_2007$DIVIDENDOS <- 0 
rec_munic_2007$PARTICIPS <- 0 
rec_munic_2007$VAL_MOB_liq <- rec_munic_2007$VAL_MOB

rec_munic_2008$DIVIDENDOS <- 0 
rec_munic_2008$PARTICIPS <- 0 
rec_munic_2008$VAL_MOB_liq <- rec_munic_2008$VAL_MOB

painel_finbra_rec_munics_2002_2012 <- rbind(rec_munic_2002, rec_munic_2003)
painel_finbra_rec_munics_2002_2012 <- rbind(painel_finbra_rec_munics_2002_2012, rec_munic_2004)
painel_finbra_rec_munics_2002_2012 <- rbind(painel_finbra_rec_munics_2002_2012, rec_munic_2005)
painel_finbra_rec_munics_2002_2012 <- rbind(painel_finbra_rec_munics_2002_2012, rec_munic_2006)
painel_finbra_rec_munics_2002_2012 <- rbind(painel_finbra_rec_munics_2002_2012, rec_munic_2007)
painel_finbra_rec_munics_2002_2012 <- rbind(painel_finbra_rec_munics_2002_2012, rec_munic_2008)
painel_finbra_rec_munics_2002_2012 <- rbind(painel_finbra_rec_munics_2002_2012, rec_munic_2009)
painel_finbra_rec_munics_2002_2012 <- rbind(painel_finbra_rec_munics_2002_2012, rec_munic_2010)
painel_finbra_rec_munics_2002_2012 <- rbind(painel_finbra_rec_munics_2002_2012, rec_munic_2011)
painel_finbra_rec_munics_2002_2012 <- rbind(painel_finbra_rec_munics_2002_2012, rec_munic_2012)

str(painel_finbra_rec_munics_2002_2012)



write.csv2(painel_finbra_rec_munics_2002_2012, "C:/Users/USUARIO/Documents/TD_munics/rec_finbra_2002_2012_mai2023.csv")






