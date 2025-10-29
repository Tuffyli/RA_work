# ---------------------------------------------------------------------------- #
# Data Extraction
# DataBase adjustment
# Last edited by: Tuffy Licciardi Issa
# Date: 29/10/2025
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(readxl)
library(writexl)
library(lmtest)
library(fixest)
library(xtable)
library(data.table)
library(stargazer)
library(AER)
library(sf)
library(janitor)
library(geobr)
library(RColorBrewer)
library(ggnewscale)
library(cobalt)
library(did)
library(MatchIt)
library(fastDummies)
library(broom)
library(rdrobust)
library(knitr)
library(kableExtra)
library(scales)
library(httr2)
library(glue)
library(ggbreak)
library(ggtext)
library(jsonlite)
library(haven)
library(ggplot2)

#Desativando a notação científica
options(scipen = 999)

# ---------------------------------------------------------------------------- #
# 1. School data ----
# ---------------------------------------------------------------------------- #

#' ***************************************************************************
#' In this sectio I will extract information regarding school characteristics from
#' Brazil's School Census. With the data collect in this code I plan to investi-
#' gate the violation of the pretrends in the SAEB regressions. Hopefully it is
#' possible to uncover the improvent in SAEB scores.
#' ***************************************************************************


path_list <- c("Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2005/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2006/DADOS/DADOS_CENSOESC.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2007/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2008/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2009/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2010/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2011/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2012/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2013/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2014/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2015/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2016/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2017/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2018/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2019/DADOS/ESCOLAS.dta",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2020/DADOS/escolas.CSV",
               "Z:/Arquivos IFB/Censo Escolar/MicrodCenso Escolar2021/2021/dados/microdados_ed_basica_2021.csv"
)

#Loop for creating a database regarding each schools characteristics

for(i in c(2005:2019)){
  
  gc()
  message("Year: ",i)
  
  ini <- Sys.time()
  
  j <- i - 2004 #path index
  
  ##1.1 2005-2006 years ----
  if (i <= 2006){
    
    temp <- read_dta(path_list[j]) %>%
      select(c(1:24,
               SAL_DE_P, #Teacher's room
               PERMANEN, #Classroom
               LAB_INFO, #Computer Lab
               LAB_CIEN, #Science Lab
               LAB_OUTR, #Other Lab
               BIBLIO,   #Library
               FUNCION,  #Employee
               PROFESS,  #Teachers
               MERE_ESC, #Lunch
               AUX_CRECHE, #Auxiliar at Kindergarden
               AUX_PRE,  #Auxiliar at Kindergarden
               228:241,  #Teacher's especiality
               PARQ_INF, #Playground
               QUAD_DES, #Court
               QUAD_COB #Covered court
               )
      ) %>% 
      filter(DEP == "Municipal", # Removes Federal and Private Schools
             CODFUNC == "Ativo") %>%  # Removes deactivated schools
      mutate(
        CODMUNIC = as.numeric(str_c( #concatenates
          str_sub(as.character(CODMUNIC), 1, 2), #only first two strings
          str_sub(as.character(CODMUNIC), -5))),
        
        #School characteristics variables
        classroom = as.numeric(PERMANEN),
        teach_room = ifelse(SAL_DE_P == "s", 1, 0),
        lab_dummy = ifelse(LAB_INFO == "s" | LAB_CIEN == "s" | LAB_OUTR == "s",
                           1, 0),
        lib_dummy = ifelse(BIBLIO == "s", 1, 0),
        play_area = ifelse(PARQ_INF == "s"| QUAD_DES == "s" | QUAD_COB == "s",
                           1, 0),
        lunch = ifelse(MERE_ESC == "s", 1, 0),
        employee = as.numeric(FUNCION),
        teacher = as.numeric(PROFESS),
        
        #Education levels
        kinder = ifelse(NIVELCRE == "s" | NIVELPRE == "s" , 1, 0),
        elementary = ifelse(NIV_F1A4_8 == "s" | NIV_F5A8_8 == "s" | NIV_F9FIM == "s" |
                              NIV_F9INI == "s", 1, 0),
        high = ifelse(NIVELMED == "s", 1, 0),
        inclusion = ifelse(ESP_EXCL == "s" | ESP_T_ES == "s" | ENS_INCL == "s",
                           1, 0)
      ) %>% 
      select(c(1:9,53:62)) %>% 
        rename(
          ano = ANO,
          school = MASCARA,
          codmun = CODMUNIC
        ) %>% 
        select(-c(UF, SIGLA, DEP, LOC, CODFUNC, MUNIC, teacher)) %>% 
        mutate( uf = codmun %/% 100000)
      

    # 1.2 2007-2018 years ----
  } else if(i %in% c(2007:2018)) {
    
    temp <- read_dta(path_list[j]) #%>%
      filter(desc_situacao_funcionamento == "EM ATIVIDADE") %>% #Active Schools
      select(fk_cod_estado,
             fk_cod_municipio,
             ano_censo,
             id_dependencia_adm,
             pk_cod_entidade,
             
             #School characteristics
             id_sala_professor,          #Teacher's room
             id_laboratorio_ciencias,    #Science Lab
             id_laboratorio_informatica, #Computer Lab
             id_quadra_esportes,         #Court
             id_parque_infantil,         #Playground
             id_biblioteca,              #Library
             num_salas_existentes,       #Classrooms
             num_salas_utilizadas,
             num_funcionarios,           #Employees
             id_alimentacao,             #Lunch
             
             #Education levels
             id_reg_infantil_creche,     #Kindergarden
             id_reg_infantil_preescola,  #Kindergarden
             id_reg_fund_8_anos,         #Elementary
             id_reg_fund_9_anos,         #Elementary
             
             id_reg_medio_integrado,
             id_reg_medio_medio,
             id_reg_medio_normal,        #Highschool
             id_reg_medio_prof,
             id_mod_ens_esp              #Special
             
      ) %>% 
      filter(id_dependencia_adm == 3)
    
    message("Opened database for year: ", i)
    
    
    temp <- temp %>% 
      mutate(
        
        #School characteristics variables
        classroom = ifelse(!is.na(num_salas_existentes),
                           as.numeric(num_salas_existentes), NA), 
        
        teach_room = as.numeric(id_sala_professor),
        
        lab_dummy = ifelse(id_laboratorio_ciencias == 1 |
                             id_laboratorio_informatica == 1, 1, 0),
        
        lib_dummy = as.numeric(id_biblioteca),
        
        play_area = ifelse(id_parque_infantil == 1 |
                             id_quadra_esportes == 1, 1, 0),
        
        lunch = as.numeric(id_alimentacao),
        
        employee = ifelse(!is.na(num_funcionarios),
                          as.numeric(num_funcionarios), NA),
        
        #teacher = as.numeric(PROFESS), #Not present in this data frame.
        
        #Education levels
        kinder = ifelse(id_reg_infantil_creche == 1 |
                          id_reg_infantil_preescola == 1, 1, 0),
        
        elementary = ifelse(id_reg_fund_8_anos == 1 | id_reg_fund_9_anos == 1, 1, 0),
        
        high = ifelse(id_reg_medio_integrado == 1 | id_reg_medio_medio == 1 |
                        id_reg_medio_normal == 1, 1, 0),
        
        inclusion = ifelse(id_mod_ens_esp == 1, 1, 0)
      ) %>% 
      rename(
        uf = fk_cod_estado,
        codmun = fk_cod_municipio,
        ano = ano_censo,
        school = no_entidade
      ) %>% 
      select( c(uf, codmun, ano, school, 25:35) ) #Final datafilter
    
    message("Finishing data preparation for: ", i)
    
    
    ## 1.3 2021 year ----
  } else if(i == 2021) {
    
    temp <- read.csv2(path_list[j]) #%>% 
     test <- temp %>%  select(NU_ANO_CENSO,
             CO_UF,
             CO_MUNICIPIO,
             CO_ENTIDADE,
             TP_DEPENDENCIA,
             
             IN_SALA_PROFESSOR,           #Teacher's room
             IN_LABORATORIO_CIENCIAS,     #Science Lab
             IN_LABORATORIO_INFORMATICA, #Computer Lab
             IN_QUADRA_ESPORTES,          #Court
             IN_PARQUE_INFANTIL,          #Playground
             IN_BIBLIOTECA,               #Libary
             QT_SALAS_EXISTENTES,         #Classrooms
             QT_SALAS_UTILIZADAS,
             QT_FUNCIONARIOS,             #Employee
             IN_ALIMENTACAO,              #Lunch
             
             IN_INF,                      #Kindergarden
             IN_FUND,                     #Elementary
             IN_MED,                      #Highschool
             IN_ESP                       #Special
             
             ) %>% 
       filter(TP_DEPENDENCIA == 3) %>% 
       mutate(
         
         #School characteristics variables
         classroom = ifelse(!is.na(QT_SALAS_UTILIZADAS),
                            as.numeric(QT_SALAS_UTILIZADAS), NA), 
         
         teach_room = as.numeric(IN_SALA_PROFESSOR),
         
         lab_dummy = ifelse(IN_LABORATORIO_INFORMATICA == 1 |
                              IN_LABORATORIO_CIENCIAS == 1, 1, 0),
         
         lib_dummy = as.numeric(IN_BIBLIOTECA),
         
         play_area = ifelse(IN_PARQUE_INFANTIL == 1 |
                              IN_QUADRA_ESPORTES == 1, 1, 0),
         
         lunch = as.numeric(IN_ALIMENTACAO),
         
         employee = ifelse(!is.na(QT_FUNCIONARIOS),
                           as.numeric(QT_FUNCIONARIOS), NA),
         
         #teacher = as.numeric(PROFESS), #Not present in this data frame.
         
         #Education levels
         kinder = as.numeric(IN_INF),
         
         elementary = as.numeric(IN_FUND),
         
         high = as.numeric(IN_MED),
         
         inclusion = as.numeric(IN_ESP)
       ) %>% 
       rename(
         uf = CO_UF,
         codmun = CO_MUNICIPIO,
         ano = NU_ANO_CENSO,
         school = CO_ENTIDADE
       ) %>% 
       select( c(uf, codmun, ano, school, 20:30) ) #Final datafilter
    
    
    
    message("Finishing data preparation for ", i)
    
  }
  
  #Binding itno a single dataframe
  if(i == 2005){
    data <- temp
    
    message("Successfully created reference dataframe")
  } else {
    data <- rbind(data, temp) %>% 
      arrange(codmun,ano)
    
    message("Successfull binding")
  }
  
  fim <- Sys.time()
  
  
  delta <- difftime(fim, ini, units = "secs")
  mins <- floor(as.numeric(delta) / 60)
  secs <- round(as.numeric(delta) %% 60)
  
  message("---------------------------------------------")
  message("Total time elapsed: ",mins," mins e ", secs, " s")
  message("---------------------------------------------")
  
  rm(temp, delta, ini, fim, temp, mins, secs, i, j)
}
rm(path_list)

# 2. Saving data ----
saveRDS(data, "Z:/Tuffy/Paper - Educ/Dados/censo_escolar_base.rds")

