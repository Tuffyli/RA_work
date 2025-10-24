# ---------------------------------------------------------------------------- #
# Regressions
# Last edited by: Tuffy Licciardi Issa
# Date: 24/10/2025
# ---------------------------------------------------------------------------- #

#' *************************************************************************** #
#' The main objective of this code is to recreate the specifications utilized in
#' the reference work of Rocha and others. Thus, regarding the SAEB exam scores 
#' I will estimate the effects on the stacked cross-section of students acording
#' to each exposure to treatment measurement.
#' ********************************************************************** #



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


#Deactivating scientific notation
options(scipen = 999)

# ---------------------------------------------------------------------------- #
# 1. Data ----
# ---------------------------------------------------------------------------- #

# ------------------- #
## 1.1 Extraction ----
#Saeb dataframe
df_saeb <- readRDS( "Z:/Tuffy/Paper - Educ/Dados/saeb_nvl_aluno.rds") %>%
mutate(codmun = ifelse(ano >= 2007, as.numeric(codmun) %/% 10, codmun),
       codmun = as.character(codmun))  #Arranging for the older municipal codes




#Main regression dataframe
df_reg <- readRDS("Z:/Tuffy/Paper - Educ/Dados/regdf.rds") %>% 
  select(codigo_ibge, ano, uf, nome, dif_coef_pp, dosage, aluno_dosage, PIBpc) %>% 
  mutate(across(c(codigo_ibge, uf), as.character))
  
# ------------------ #
# 1.2 Merge ----

#Combining both dataframes
df <- df_saeb %>% 
  left_join(df_reg %>%
              filter(ano %in% c(2005:2017)) %>%
              mutate(codigo_ibge = as.character(codigo_ibge)),
           by = c("codmun" = "codigo_ibge", "ano" = "ano")) %>% 
  mutate(
    sexo = as.numeric(sexo),
    raca = as.numeric(raca),
    mae_educ = as.numeric(mae_educ)
  ) %>% 
  mutate(age_exp = ifelse(treat_exp > 1, 1, treat_exp), #Based on Carrillo
         anos_exp = ifelse(ano < 2007, 0, ano - 2007),
         anos_exp = ifelse(grade == 9 & anos_exp > 0, anos_exp - 2, anos_exp),
         peso = as.numeric(peso),
         
         age_dist = ifelse(idade_aux == 0, 1, 0)
         )



# ---------------------------------------------------------------------------- #
# 2. Main Regression ----
# ---------------------------------------------------------------------------- #
## 2.1 Dosage ----
# ------------------ #
### 2.1.1 Age exp ----

main_mat <- feols(as.numeric(profic_mat) ~ dosage : age_exp
                              + sexo + raca + mae_educ + PIBpc + idade #Controls
                              | codmun + ano + uf^ano, #FE
                              data = df %>% filter(grade == 5), #Only 5h grade
                              #weights = df_saeb$peso_5,
                              vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ dosage : age_exp
                  + sexo + raca + mae_educ + PIBpc + idade #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

etable(main_mat, main_pot)


etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1),
                      file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage/nvl_individuo_age_exp_agregado.tex", replace = TRUE))



# ----------------- #
### 2.1.2 Years exp ----

main_mat <- feols(as.numeric(profic_mat) ~ dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")


sec_mat <- feols(as.numeric(profic_mat) ~ dosage : i(anos_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc 
                 | codmun + ano + uf^ano,
                 data = df %>% filter(grade == 9),
                 #weights = df$peso,
                 vcov = "hetero")



sec_pot <- feols(as.numeric(profic_port) ~ dosage : i(anos_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc 
                 | codmun + ano + uf^ano,
                 data = df %>% filter(grade == 9),
                 #weights = df$peso,
                 vcov = "hetero")

etable(main_mat, main_pot,
  sec_mat, sec_pot)



etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1),
       file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/nvl_individuo_anos_exp.tex", replace = TRUE))


rm(main_mat, main_pot, sec_mat, sec_pot)

# -------------------------------- #
## 2.2 Aluno Dosage ----
# -------------------------------- #

# ------------------ #
### 2.2.1 Age exp ----

main_mat <- feols(as.numeric(profic_mat) ~ aluno_dosage : age_exp
                  + sexo + raca + mae_educ + PIBpc + idade #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ aluno_dosage : age_exp
                  + sexo + raca + mae_educ + PIBpc + idade #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df_saeb$peso_5,
                  vcov = "hetero")

etable(main_mat, main_pot)


etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1),
                      file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/nvl_individuo_age_exp_agregado.tex", replace = TRUE))



# ----------------- #
### 2.2.2 Years exp ----

main_mat <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")

main_pot <- feols(as.numeric(profic_port) ~ aluno_dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")


sec_mat <- feols(as.numeric(profic_mat) ~ aluno_dosage : i(anos_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc 
                 | codmun + ano + uf^ano,
                 data = df %>% filter(grade == 9),
                 #weights = df$peso,
                 vcov = "hetero")



sec_pot <- feols(as.numeric(profic_port) ~ aluno_dosage : i(anos_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc 
                 | codmun + ano + uf^ano,
                 data = df %>% filter(grade == 9),
                 #weights = df$peso,
                 vcov = "hetero")

etable(main_mat, main_pot,
       sec_mat, sec_pot)



etable(main_mat, main_pot,
       vcov = "hetero",
       headers = list(":_:" = list("Matemática" = 1,"Português" = 1),
                      file = "Z:/Tuffy/Paper - Educ/Resultados/Tabelas/Dosage_aluno/nvl_individuo_anos_exp.tex", replace = TRUE))


rm(main_mat, main_pot, sec_mat, sec_pot)

# ---------------------------------------------------------------------------- #
# 3. Age vs. Grade Distortion ----
# ---------------------------------------------------------------------------- #

#' Here I will investigate the effect of the FUNDEB policy in the grade-age dis-
#' tortion dummy variable. Where the ideal age-grade equals 0, while the distor-
#' tion is equal 1

# ---------------- #
## 3.1 Dosage ----

main_dist <- feols(as.numeric(age_dist) ~ dosage : i(anos_exp, ref = 0)
                  + sexo + raca + mae_educ + idade + PIBpc #Controls
                  | codmun + ano + uf^ano, #FE
                  data = df %>% filter(grade == 5), #Only 5h grade
                  #weights = df %>% filter(grade == 5) %>% select(peso),
                  vcov = "hetero")


sec_dist <- feols(as.numeric(age_dist) ~ dosage : i(anos_exp, ref = 0)
                 + sexo + raca + mae_educ + idade + PIBpc 
                 | codmun + ano + uf^ano,
                 data = df %>% filter(grade == 9),
                 #weights = df$peso,
                 vcov = "hetero")

etable(main_dist, sec_dist)
