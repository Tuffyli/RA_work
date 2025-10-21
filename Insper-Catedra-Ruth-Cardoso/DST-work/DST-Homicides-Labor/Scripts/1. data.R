

# Library ----

library(tidyverse)
library(data.table)
library(lubridate)
library(sf)
library(haven)
library(readxl)



# ---------------------------------------------------------------------------- #
# Bases preliminares -----
# ---------------------------------------------------------------------------- #


#Capturando a população

censopop <- readxl::read_xlsx("Z:/Tuffy/Paper - HVSUS/Bases/pop_anual.xlsx")


# Base de distâncias
mun_hv <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/mun_hv.RDS")

conv <- read_dta("Z:/Tuffy/Paper - Brasil/amcs.dta") %>% 
  select(codmun, munic, nome)



# Coordenadas
coordenadas <- mun_hv$centroid %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(
    lon = X,
    lat = Y
  )

mun_hv <- mun_hv %>%
  bind_cols(coordenadas) %>%
  st_drop_geometry() %>%
  select(co_municipio, lon, lat, dist_hv_border, seg) %>% 
  left_join(conv, by = c("co_municipio" = "codmun"))
  
  
rm(coordenadas, conv)





# ---------------------------------------------------------------------------- #
# SIH Database ----
# ---------------------------------------------------------------------------- #

# 1. variable creation ----

#temp <- readRDS("Z:/Arquivos IFB/DATASUS/SIH/R/SIH_2019.rds")

# for (ano in 2013:2019) {
#   
#   
#   #Abertura da Base unificada
#   temp <- readRDS(paste0(
#     "Z:/Arquivos IFB/DATASUS/SIH/R/SIH_",ano,".rds"
#   ))
#   
#   #Alocando as classificações CID-10
#   temp <- temp %>% 
#     mutate(
#       diag_prefix = substr(DIAG_PRINC, 1, 3),
#       
#       #classificando a CID10
#       cid10 = case_when(
#         grepl("^A", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 1, #Doenças infecciosas e parasitárias
#         grepl("^B", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 1, #Doenças infecciosas e parasitárias
#         
#         grepl("^C", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 2, #Neoplasias
#         grepl("^D", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 48 ~ 2, #Neoplasias
#         
#         grepl("^D", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(50,89) ~ 3, #Doenças do sangue 
#         
#         grepl("^E", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 4, #Doenças Endócrinas
#         
#         grepl("^F", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 5, #Doenças Comportamentais
#         
#         grepl("^G", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 6, #Doenças do sistema nervoso
#         
#         grepl("^H", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 59 ~ 7, #Doenças do olho
#         
#         grepl("^H", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(60,95) ~ 8, #Doenças do ouvido
#         
#         grepl("^I", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 9, #Doenças do sistema circulatório
#         
#         grepl("^J", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 10, #Doenças do aparelho respiratório
#         
#         grepl("^K", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 11, #Doenças do aprelho digestivo
#         
#         grepl("^L", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 12, #Doenças de pele
#         
#         grepl("^M", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 13, #Doenças Osteomusculares
#         
#         grepl("^N", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 14, #Doenças do aparelho geniturinário
#         
#         grepl("^O", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 15, #Gravidez, parto e puepério
#         
#         grepl("^P", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 16, #Doenças do período pré-natal
#         
#         grepl("^Q", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 17, #Maformação congênita
#         
#         grepl("^R", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 18, #Achados anormais
#       
#         grepl("^S", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
#           grepl("^T", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 19, #Lesões
#         
#         grepl("^V", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
#           grepl("^W", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
#           grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
#           grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 20, #Causas externas
#         
#         grepl("^Z", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 21, #Fatores que influenciam a saúde
#         
#         grepl("^U", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 22, #Propósitos especiais
#         
#         TRUE ~ NA_real_
#         ),
#       
#       
#       
#       cid_transito = ifelse(
#         grepl("^V", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99,
#         1, 0),
#       
#       cid_violencia = ifelse(
#         grepl("^W", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(26,32,33,34,49,50) |
#           grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(89:99) |
#           grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(0:9),
#         1, 0
#       ),
#       
#       cid_cardio = ifelse(
#         grepl("^I", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) == 46,
#         1, 0),
#       
#     )
#   
#   
#   
#   saveRDS(temp,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_",ano,".rds"))
#   
#   
# }
# 
# rm()
# gc()


# ---------------------------------------------------------------------------- #

#2. 2018 - 2019 HV ----

for (ano in 2013:2019) {
  
  message("Reading ", ano," =)")
  
  
  #Abertura da Base unificada
  temp <- readRDS(paste0(
    "Z:/Arquivos IFB/DATASUS/SIH/R/SIH_",ano,".rds"
  ))
  
  
  temp_pop <- censopop %>% 
    select(codmun, as.character(ano)) %>% 
    mutate(codmun = as.numeric(codmun))
  
  
  
  #Alocando as classificações CID-10
  temp <- temp %>%
#    filter(DT_INTER != 12356283) %>% 
    mutate(
      
      MUNIC_MOV = as.numeric(MUNIC_MOV),
      MUNIC_RES = as.numeric(MUNIC_RES),
      DT_INTER = as.numeric(DT_INTER),
      
      data = ymd(DT_INTER),
      
      semana = isoweek(data),
      
      dia_semana = wday(data),
      
      year = ano,
      
      uf = MUNIC_MOV %/% 10000,
      

      
      diag_prefix = substr(DIAG_PRINC, 1, 3),
      
      #classificando a CID10
      cid10 = case_when(
        grepl("^A", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 1, #Doenças infecciosas e parasitárias
        grepl("^B", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 1, #Doenças infecciosas e parasitárias
        
        grepl("^C", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 2, #Neoplasias
        grepl("^D", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 48 ~ 2, #Neoplasias
        
        grepl("^D", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(50,89) ~ 3, #Doenças do sangue
        
        grepl("^E", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 4, #Doenças Endócrinas
        
        grepl("^F", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 5, #Doenças Comportamentais
        
        grepl("^G", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 6, #Doenças do sistema nervoso
        
        grepl("^H", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 59 ~ 7, #Doenças do olho
        
        grepl("^H", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(60,95) ~ 8, #Doenças do ouvido
        
        grepl("^I", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 9, #Doenças do sistema circulatório
        
        grepl("^J", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 10, #Doenças do aparelho respiratório
        
        grepl("^K", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 11, #Doenças do aprelho digestivo
        
        grepl("^L", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 12, #Doenças de pele
        
        grepl("^M", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 13, #Doenças Osteomusculares
        
        grepl("^N", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 14, #Doenças do aparelho geniturinário
        
        grepl("^O", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 15, #Gravidez, parto e puepério
        
        grepl("^P", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 16, #Doenças do período pré-natal
        
        grepl("^Q", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 17, #Maformação congênita
        
        grepl("^R", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 18, #Achados anormais
        
        grepl("^S", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^T", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 19, #Lesões
        
        grepl("^V", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^W", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 20, #Causas externas
        
        grepl("^Z", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 21, #Fatores que influenciam a saúde
        
        grepl("^U", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 22, #Propósitos especiais
        
        TRUE ~ NA_real_
      ),
      
      
      
      cid_transito = ifelse(
        grepl("^V", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99,
        1, 0),
      
      cid_violencia = ifelse(
        grepl("^W", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(26,32,33,34,49,50) |
          grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(89:99) |
          grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(0:9),
        1, 0
      ),
      
      cid_cardio = ifelse(
        grepl("^I", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) == 46,
        1, 0),
      
      cid_cardio_tot = ifelse(cid10 == 10, 1, 0),
      
      cid_lesao = ifelse(cid10 == 19, 1, 0),
      
      cid_ext = ifelse(cid10 == 20, 1, 0),
      
      cid_lsxt = ifelse(cid10 == 19 | cid10 == 20, 1, 0),
      
      new_homicidio = ifelse(
        grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(85:99) |
          grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in%c(0:9,35,36),
        1,0
      )
    ) 
  
  
  if (ano == 2019) {
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-11-03")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
  } else if (ano %in% c(2018)) {
    
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-11-04")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
    
  } else if (ano == 2017) {
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-10-15")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
  } else if (ano == 2016) {
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-10-16")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
  } else if (ano == 2015) {
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-10-18")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
  } else if (ano == 2014) {
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-10-19")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
  } else if(ano == 2013) { # 2013
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-10-20")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
  }
  
  
  for (d in c(1,7, 30, 45, 60)) {
    
    
    
    if (d == 1) {
      
      dados <- temp %>% 
        filter(days_to_hv == 1) %>% 
        setDT()
      
    } else if (d == 7) {
      dados <- temp %>% 
        filter(days_to_hv > 0 &
                 days_to_hv < 8) %>% 
        setDT()
    } else if (d == 30) {
      dados <- temp %>% 
        filter(days_to_hv > 0 &
                 days_to_hv < 31) %>% 
        setDT()
    } else if (d == 45){
      dados <- temp %>% 
        filter(days_to_hv > 0 &
                 days_to_hv < 46) %>%  
        setDT()
    } else {
      dados <- temp %>% 
        filter(days_to_hv > 0 &
                 days_to_hv < 61) %>% 
        setDT()
    }
  
        temp_df <- mun_hv %>% 
          left_join(dados,
                    by = c(
                      "munic" =   "MUNIC_MOV"
                    )) %>% 
          select(-nome) %>% 
          setDT() 
  
  
  
  temp_dt <- temp_df[,.(#homicidio = sum(homicidio, na.rm = T),
                        transito = sum(cid_transito, na.rm = T),
                        cardio = sum(cid_cardio, na.rm = T),
                        cardio_tot = sum(cid_cardio_tot, na.rm = T),
                        lesao = sum(cid_lesao, na.rm = T),
                        externos = sum(cid_ext, na.rm = T),
                        les_ext = sum(cid_lsxt, na.rm = T),
                        new_homicidio = sum(new_homicidio, na.rm = T)
  ),
  by = .(co_municipio,dist_hv_border,seg,lat,lon)] 
  
  
  
  temp_dt <- temp_dt %>% 
    mutate(
      co_municipio = as.numeric(co_municipio)
    ) %>% 
    left_join(temp_pop,
              by = c("co_municipio" = "codmun")) %>% 
    rename(
      pop = as.character(ano)
    ) %>% 
    mutate(
      #hom_pop = homicidio/(pop/100000),
      tra_pop = transito/(pop/100000),
      crd_pop = cardio/(pop/100000),
      crt_pop = cardio_tot/(pop/100000),
      les_pop = lesao/(pop/100000),
      ext_pop = externos/(pop/100000),
      lxt_pop = les_ext/(pop/100000),
      nhm_pop = new_homicidio/(pop/100000),
      
      year = ano
    )
  
  
  
  message("Saving ",ano,"... Mun_Ocur")
  saveRDS(temp_dt,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIH/",d,"SIH_hv_oco_",ano,".rds"))
  message("Saved ",ano," Mun_Ocur")
  
  message("Mun residêncial: ",ano)
  
  
  
  
  temp_df <- mun_hv %>% 
    left_join(dados,
              by = c(
                "munic" =   "MUNIC_RES"
              )) %>% 
    select(-nome) %>% 
    setDT() 
  
  
  
  temp_dt <- temp_df[,.(#homicidio = sum(homicidio, na.rm = T),
                        transito = sum(cid_transito, na.rm = T),
                        cardio = sum(cid_cardio, na.rm = T),
                        cardio_tot = sum(cid_cardio_tot, na.rm = T),
                        lesao = sum(cid_lesao, na.rm = T),
                        externos = sum(cid_ext, na.rm = T),
                        les_ext = sum(cid_lsxt, na.rm = T),
                        new_homicidio = sum(new_homicidio, na.rm = T)
  ),
  by = .(co_municipio,dist_hv_border,seg,lat,lon)] 
  
  
  
  temp_dt <- temp_dt %>% 
    mutate(
      co_municipio = as.numeric(co_municipio)
    ) %>% 
    left_join(temp_pop,
              by = c("co_municipio" = "codmun")) %>% 
    rename(
      pop = as.character(ano)
    ) %>% 
    mutate(
      #hom_pop = homicidio/(pop/100000),
      tra_pop = transito/(pop/100000),
      crd_pop = cardio/(pop/100000),
      crt_pop = cardio_tot/(pop/100000),
      les_pop = lesao/(pop/100000),
      ext_pop = externos/(pop/100000),
      lxt_pop = les_ext/(pop/100000),
      nhm_pop = new_homicidio/(pop/100000),
      
      year = ano
    )
  
  message("Saving ",ano," ",d,"... Mun_Res")
  saveRDS(temp_dt,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIH/",d,"SIH_hv_res_",ano,".rds"))
  message("Saved ",ano," Mun_Res")
  
  
  rm(d, dados)
  }  
  rm(temp, temp_df)
  
}





#2. Casos ----
#Para extrair a população
com_pop <- censopop %>% 
  select(codmun, as.character(2018), as.character(2019)) %>% 
  left_join(mun_hv %>% 
              mutate(co_municipio = as.character(co_municipio)) %>% 
              select(co_municipio, lon, lat, dist_hv_border, seg, munic),
            by = c("codmun" = "co_municipio")) %>% 
  rename(
    pop18 = as.character(2018),
    pop19 = as.character(2019)
  )

## 2.1 Homicidio ----

base_hm <- read.csv2("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_2018_2019_hom.csv")

base_hm <- base_hm %>% 
  select(Município,X2018.Nov,X2018.Dez,X2019.Nov,X2019.Dez) %>% 
  mutate(
    cod_mun = substr(Município, 1, 6)
  ) %>% 
  rename(
    nov18 = X2018.Nov,
    dez18 = X2018.Dez,
    nov19 = X2019.Nov,
    dez19 = X2019.Dez
  ) %>% 
  group_by(cod_mun) %>% 
  mutate(
    nov18 = ifelse(nov18 != as.character("-"),
                   as.numeric(nov18), 0),
    
    dez18 = ifelse(dez18 != as.character("-"),
                   as.numeric(dez18), 0),
    
    nov19 = ifelse(nov19 != as.character("-"),
                   as.numeric(nov19), 0),
    
    dez19 = ifelse(dez19 != as.character("-"),
                   as.numeric(dez19), 0),
    
    tot18 = nov18 + dez18,
    tot19 = nov19 + dez19
    
  ) %>% 
  ungroup() %>% 
    left_join(com_pop %>% 
                mutate(munic = as.character(munic)),
              by = c("cod_mun" = "munic")) %>% 
    select(-c(
      nov18, nov19, dez18, dez19
    )) %>% 
    group_by(codmun) %>% 
    mutate(
      hom18 = tot18/(pop18/100000),
      hom19 = tot19/(pop19/100000),
      
      hv = ifelse(as.numeric(codmun) %/% 100000 > 30, 1, 0),
       dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)      
    ) %>% 
    select(codmun, lon,lat,dist_hv_border,seg,hom18,hom19,pop18,pop19)

  

  
  saveRDS(base_hm,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_hom_oco.rds"))
  rm(base_hm)
  
  
  
## 2.2 Externas ----
  
  base_hm <- read.csv2("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_2018_2019_cext.csv")
  
  base_hm <- base_hm %>% 
    select(Município,X2018.Nov,X2018.Dez,X2019.Nov,X2019.Dez) %>% 
    mutate(
      cod_mun = substr(Município, 1, 6)
    ) %>% 
    rename(
      nov18 = X2018.Nov,
      dez18 = X2018.Dez,
      nov19 = X2019.Nov,
      dez19 = X2019.Dez
    ) %>% 
    group_by(cod_mun) %>% 
    mutate(
      nov18 = ifelse(nov18 != as.character("-"),
                     as.numeric(nov18), 0),
      
      dez18 = ifelse(dez18 != as.character("-"),
                     as.numeric(dez18), 0),
      
      nov19 = ifelse(nov19 != as.character("-"),
                     as.numeric(nov19), 0),
      
      dez19 = ifelse(dez19 != as.character("-"),
                     as.numeric(dez19), 0),
      
      tot18 = nov18 + dez18,
      tot19 = nov19 + dez19
      
    ) %>% 
    ungroup() %>% 
    left_join(com_pop %>% 
                mutate(munic = as.character(munic)),
              by = c("cod_mun" = "munic")) %>% 
    select(-c(
      nov18, nov19, dez18, dez19
    )) %>% 
    group_by(codmun) %>% 
    mutate(
      ext18 = tot18/(pop18/100000),
      ext19 = tot19/(pop19/100000),
      
      hv = ifelse(as.numeric(codmun) %/% 100000 > 30, 1, 0),
      dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)      
    ) %>% 
    select(codmun, lon,lat,dist_hv_border,seg,ext18,ext19, pop18,pop19)
  
  
  
  
  saveRDS(base_hm,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_ext_oco.rds"))
  rm(com_pop, base_hm)
  
  

  
  
# ---------------------------------------------------------------------------- #  
#3. Casos outros anos:
  
  
  #2. Casos ----
  #Para extrair a população
  com_pop <- censopop %>% 
    select(codmun, as.character(2018), as.character(2019),
           as.character(2017), as.character(2016), as.character(2015),
           as.character(2014), as.character(2013)) %>% 
    left_join(mun_hv %>% 
                mutate(co_municipio = as.character(co_municipio)) %>% 
                select(co_municipio, lon, lat, dist_hv_border, seg, munic),
              by = c("codmun" = "co_municipio")) %>% 
    rename(
      pop18 = as.character(2018),
      pop19 = as.character(2019),
      pop17 = as.character(2017),
      pop16 = as.character(2016),
      pop15 = as.character(2015),
      pop14 = as.character(2014),
      pop13 = as.character(2013)
    )
  
  ## 2.1 Homicidio ----
  
  base_hm <- read.csv2("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_Hom_1319.csv")
  
  base_hm <- base_hm %>% 
    select(Município,X2013.Nov,X2013.Dez ,X2014.Nov,X2014.Dez,X2015.Nov,X2015.Dez,
           X2016.Nov,X2016.Dez,X2017.Nov,X2017.Dez,
           X2018.Nov,X2018.Dez,X2019.Nov,X2019.Dez) %>% 
    mutate(
      cod_mun = substr(Município, 1, 6)
    ) %>% 
    rename(
      nov13 = X2013.Nov,
      dez13 = X2013.Dez,
      nov14 = X2014.Nov,
      dez14 = X2014.Dez,
      nov15 = X2015.Nov,
      dez15 = X2015.Dez,
      nov16 = X2016.Nov,
      dez16 = X2016.Dez,
      nov17 = X2017.Nov,
      dez17 = X2017.Dez,
      nov18 = X2018.Nov,
      dez18 = X2018.Dez,
      nov19 = X2019.Nov,
      dez19 = X2019.Dez
    ) %>% 
    group_by(cod_mun) %>% 
    mutate(
      
      nov13 = ifelse(nov13 != as.character("-"),
                     as.numeric(nov13), 0),
      
      dez13 = ifelse(dez13 != as.character("-"),
                     as.numeric(dez13), 0),
      nov14 = ifelse(nov14 != as.character("-"),
                     as.numeric(nov14), 0),
      
      dez14 = ifelse(dez14 != as.character("-"),
                     as.numeric(dez14), 0),
      
      nov15 = ifelse(nov15 != as.character("-"),
                     as.numeric(nov15), 0),
      dez15 = ifelse(dez15 != as.character("-"),
                     as.numeric(dez15), 0),
      
      nov17 = ifelse(nov17 != as.character("-"),
                     as.numeric(nov17), 0),
      
      dez17 = ifelse(dez17 != as.character("-"),
                     as.numeric(dez17), 0),
      
      nov16 = ifelse(nov16 != as.character("-"),
                     as.numeric(nov16), 0),
      
      dez16 = ifelse(dez16 != as.character("-"),
                     as.numeric(dez16), 0),
      nov18 = ifelse(nov18 != as.character("-"),
                     as.numeric(nov18), 0),
      
      dez18 = ifelse(dez18 != as.character("-"),
                     as.numeric(dez18), 0),
      
      nov19 = ifelse(nov19 != as.character("-"),
                     as.numeric(nov19), 0),
      
      dez19 = ifelse(dez19 != as.character("-"),
                     as.numeric(dez19), 0),
      
      tot13 = nov13 + dez13,
      tot14 = nov14 + dez14,
      tot15 = nov15 + dez15,
      tot16 = nov16 + dez16,
      tot17 = nov17 + dez17,
      tot18 = nov18 + dez18,
      tot19 = nov19 + dez19
      
      
      
    ) %>% 
    ungroup() %>% 
    left_join(com_pop %>% 
                mutate(munic = as.character(munic)),
              by = c("cod_mun" = "munic")) %>% 
    group_by(codmun) %>% 
    mutate(
      
      hom13 = tot13/(pop13/100000),
      hom14 = tot14/(pop14/100000),
      hom15 = tot15/(pop15/100000),
      hom16 = tot16/(pop16/100000),
      hom17 = tot17/(pop17/100000),
      hom18 = tot18/(pop18/100000),
      hom19 = tot19/(pop19/100000),
      
      hv = ifelse(as.numeric(codmun) %/% 100000 > 30, 1, 0),
      dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)      
    ) %>% 
    mutate(
      
      nov13 = nov13/(pop13/100000),
      dez13 = dez13/(pop13/100000),
      nov14 = nov14/(pop14/100000),
      dez14 = dez14/(pop14/100000),
      nov15 = nov15/(pop15/100000),
      dez15 = dez15/(pop15/100000),
      nov16 = nov16/(pop16/100000),
      dez16 = dez16/(pop16/100000),
      nov17 = nov17/(pop17/100000),
      dez17 = dez17/(pop17/100000),
      nov18 = nov18/(pop18/100000),
      dez18 = dez18/(pop18/100000),
      nov19 = tot19/(pop19/100000),
      dez19 = tot19/(pop19/100000)
      
    ) %>%
    ungroup()
  
  
  
  
  saveRDS(base_hm,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_hom_1319.rds"))
  rm(base_hm)
  
  
  
  ## 2.2 Externas ----
  
  base_hm <- read.csv2("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_Total_1319.csv")
  
  base_hm <- base_hm %>% 
    select(Município,X2013.Nov,X2013.Dez ,X2014.Nov,X2014.Dez,X2015.Nov,X2015.Dez,
           X2016.Nov,X2016.Dez,X2017.Nov,X2017.Dez,
           X2018.Nov,X2018.Dez,X2019.Nov,X2019.Dez) %>% 
    mutate(
      cod_mun = substr(Município, 1, 6)
    ) %>% 
    rename(
      nov13 = X2013.Nov,
      dez13 = X2013.Dez,
      nov14 = X2014.Nov,
      dez14 = X2014.Dez,
      nov15 = X2015.Nov,
      dez15 = X2015.Dez,
      nov16 = X2016.Nov,
      dez16 = X2016.Dez,
      nov17 = X2017.Nov,
      dez17 = X2017.Dez,
      nov18 = X2018.Nov,
      dez18 = X2018.Dez,
      nov19 = X2019.Nov,
      dez19 = X2019.Dez
    ) %>% 
    group_by(cod_mun) %>% 
    mutate(
      
      nov13 = ifelse(nov13 != as.character("-"),
                     as.numeric(nov13), 0),
      
      dez13 = ifelse(dez13 != as.character("-"),
                     as.numeric(dez13), 0),
      nov14 = ifelse(nov14 != as.character("-"),
                     as.numeric(nov14), 0),
      
      dez14 = ifelse(dez14 != as.character("-"),
                     as.numeric(dez14), 0),
      
      nov15 = ifelse(nov15 != as.character("-"),
                     as.numeric(nov15), 0),
      dez15 = ifelse(dez15 != as.character("-"),
                     as.numeric(dez15), 0),
      
      nov17 = ifelse(nov17 != as.character("-"),
                     as.numeric(nov17), 0),
      
      dez17 = ifelse(dez17 != as.character("-"),
                     as.numeric(dez17), 0),
      
      nov16 = ifelse(nov16 != as.character("-"),
                     as.numeric(nov16), 0),
      
      dez16 = ifelse(dez16 != as.character("-"),
                     as.numeric(dez16), 0),
      nov18 = ifelse(nov18 != as.character("-"),
                     as.numeric(nov18), 0),
      
      dez18 = ifelse(dez18 != as.character("-"),
                     as.numeric(dez18), 0),
      
      nov19 = ifelse(nov19 != as.character("-"),
                     as.numeric(nov19), 0),
      
      dez19 = ifelse(dez19 != as.character("-"),
                     as.numeric(dez19), 0),
      
      tot13 = nov13 + dez13,
      tot14 = nov14 + dez14,
      tot15 = nov15 + dez15,
      tot16 = nov16 + dez16,
      tot17 = nov17 + dez17,
      tot18 = nov18 + dez18,
      tot19 = nov19 + dez19
      
      
      
    ) %>% 
    ungroup() %>% 
    left_join(com_pop %>% 
                mutate(munic = as.character(munic)),
              by = c("cod_mun" = "munic")) %>% 
    group_by(codmun) %>% 
    mutate(
      
      ext13 = tot13/(pop13/100000),
      ext14 = tot14/(pop14/100000),
      ext15 = tot15/(pop15/100000),
      ext16 = tot16/(pop16/100000),
      ext17 = tot17/(pop17/100000),
      ext18 = tot18/(pop18/100000),
      ext19 = tot19/(pop19/100000),
      
      hv = ifelse(as.numeric(codmun) %/% 100000 > 30, 1, 0),
      dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)      
    ) %>% 
    mutate(
      
      nov13 = nov13/(pop13/100000),
      dez13 = dez13/(pop13/100000),
      nov14 = nov14/(pop14/100000),
      dez14 = dez14/(pop14/100000),
      nov15 = nov15/(pop15/100000),
      dez15 = dez15/(pop15/100000),
      nov16 = nov16/(pop16/100000),
      dez16 = dez16/(pop16/100000),
      nov17 = nov17/(pop17/100000),
      dez17 = dez17/(pop17/100000),
      nov18 = nov18/(pop18/100000),
      dez18 = dez18/(pop18/100000),
      nov19 = tot19/(pop19/100000),
      dez19 = tot19/(pop19/100000)
      
    ) %>%
    ungroup()
  
  
  summary(base_hm)
  
  saveRDS(base_hm,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_ext_1319.rds"))
  rm(base_hm)
  
  
  
  
  # ---------------------------------------------------------------------------- #  
  
  
  
  ## 2.3 Transito ----
  
  base_hm <- read.csv2("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_Tran_1319.csv")
  
  base_hm <- base_hm %>% 
    select(Município,X2013.Nov,X2013.Dez ,X2014.Nov,X2014.Dez,X2015.Nov,X2015.Dez,
           X2016.Nov,X2016.Dez,X2017.Nov,X2017.Dez,
           X2018.Nov,X2018.Dez,X2019.Nov,X2019.Dez) %>% 
    mutate(
      cod_mun = substr(Município, 1, 6)
    ) %>% 
    rename(
      nov13 = X2013.Nov,
      dez13 = X2013.Dez,
      nov14 = X2014.Nov,
      dez14 = X2014.Dez,
      nov15 = X2015.Nov,
      dez15 = X2015.Dez,
      nov16 = X2016.Nov,
      dez16 = X2016.Dez,
      nov17 = X2017.Nov,
      dez17 = X2017.Dez,
      nov18 = X2018.Nov,
      dez18 = X2018.Dez,
      nov19 = X2019.Nov,
      dez19 = X2019.Dez
    ) %>% 
    group_by(cod_mun) %>% 
    mutate(
      
      nov13 = ifelse(nov13 != as.character("-"),
                     as.numeric(nov13), 0),
      
      dez13 = ifelse(dez13 != as.character("-"),
                     as.numeric(dez13), 0),
      nov14 = ifelse(nov14 != as.character("-"),
                     as.numeric(nov14), 0),
      
      dez14 = ifelse(dez14 != as.character("-"),
                     as.numeric(dez14), 0),
      
      nov15 = ifelse(nov15 != as.character("-"),
                     as.numeric(nov15), 0),
      dez15 = ifelse(dez15 != as.character("-"),
                     as.numeric(dez15), 0),
      
      nov17 = ifelse(nov17 != as.character("-"),
                     as.numeric(nov17), 0),
      
      dez17 = ifelse(dez17 != as.character("-"),
                     as.numeric(dez17), 0),
      
      nov16 = ifelse(nov16 != as.character("-"),
                     as.numeric(nov16), 0),
      
      dez16 = ifelse(dez16 != as.character("-"),
                     as.numeric(dez16), 0),
      nov18 = ifelse(nov18 != as.character("-"),
                     as.numeric(nov18), 0),
      
      dez18 = ifelse(dez18 != as.character("-"),
                     as.numeric(dez18), 0),
      
      nov19 = ifelse(nov19 != as.character("-"),
                     as.numeric(nov19), 0),
      
      dez19 = ifelse(dez19 != as.character("-"),
                     as.numeric(dez19), 0),
      
      tot13 = nov13 + dez13,
      tot14 = nov14 + dez14,
      tot15 = nov15 + dez15,
      tot16 = nov16 + dez16,
      tot17 = nov17 + dez17,
      tot18 = nov18 + dez18,
      tot19 = nov19 + dez19
      
      
      
    ) %>% 
    ungroup() %>% 
    left_join(com_pop %>% 
                mutate(munic = as.character(munic)),
              by = c("cod_mun" = "munic")) %>% 
    group_by(codmun) %>% 
    mutate(
      
      tra13 = tot13/(pop13/100000),
      tra14 = tot14/(pop14/100000),
      tra15 = tot15/(pop15/100000),
      tra16 = tot16/(pop16/100000),
      tra17 = tot17/(pop17/100000),
      tra18 = tot18/(pop18/100000),
      tra19 = tot19/(pop19/100000),
      
      hv = ifelse(as.numeric(codmun) %/% 100000 > 30, 1, 0),
      dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)      
    ) %>% 
    mutate(
      
      nov13 = nov13/(pop13/100000),
      dez13 = dez13/(pop13/100000),
      nov14 = nov14/(pop14/100000),
      dez14 = dez14/(pop14/100000),
      nov15 = nov15/(pop15/100000),
      dez15 = dez15/(pop15/100000),
      nov16 = nov16/(pop16/100000),
      dez16 = dez16/(pop16/100000),
      nov17 = nov17/(pop17/100000),
      dez17 = dez17/(pop17/100000),
      nov18 = nov18/(pop18/100000),
      dez18 = dez18/(pop18/100000),
      nov19 = tot19/(pop19/100000),
      dez19 = tot19/(pop19/100000)
      
    ) %>%
    ungroup()
  
  
  summary(base_hm)
  
  saveRDS(base_hm,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_tra_1319.rds"))
  rm(base_hm)
  
  
  
  
  # ---------------------------------------------------------------------------- #  
  
  
  
# SIM Database ---------
# ---------------------------------------------------------------------------- #

#temp <- read.csv2("Z:/Arquivos IFB/DATASUS/SIM/OpenDataSUS/Mortalidade_Geral_2019.csv")
## 1.1 Geral ----


for (ano in 2018:2019) {
  
  message("Reading ", ano," =)")
  
  
  #Abertura da Base unificada
  temp <- read.csv2(paste0(
    "Z:/Arquivos IFB/DATASUS/SIM/OpenDataSUS/Mortalidade_Geral_",ano,".csv"
  ))
  

  temp_pop <- censopop %>% 
    select(codmun, as.character(ano)) %>% 
    mutate(codmun = as.numeric(codmun))
  
  
  
  #Alocando as classificações CID-10
  temp <- temp %>% 
    mutate(
      
      CODMUNOCOR = as.numeric(CODMUNOCOR),
      
      data = dmy(sprintf("%08d",as.numeric(DTOBITO))),
      
      semana = isoweek(data),
      
      dia_semana = wday(data),
      
      year = ano,
      
      uf = CODMUNOCOR %/% 10000,
      
      homicidio = ifelse(CIRCOBITO == 3, 1, 0),
      
      
       diag_prefix = substr(CAUSABAS, 1, 3),
       
       #classificando a CID10
      cid10 = case_when(
        grepl("^A", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 1, #Doenças infecciosas e parasitárias
        grepl("^B", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 1, #Doenças infecciosas e parasitárias

        grepl("^C", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 2, #Neoplasias
        grepl("^D", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 48 ~ 2, #Neoplasias

        grepl("^D", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(50,89) ~ 3, #Doenças do sangue

        grepl("^E", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 4, #Doenças Endócrinas

        grepl("^F", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 5, #Doenças Comportamentais

        grepl("^G", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 6, #Doenças do sistema nervoso

        grepl("^H", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 59 ~ 7, #Doenças do olho

        grepl("^H", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(60,95) ~ 8, #Doenças do ouvido

        grepl("^I", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 9, #Doenças do sistema circulatório

        grepl("^J", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 10, #Doenças do aparelho respiratório

        grepl("^K", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 11, #Doenças do aprelho digestivo

        grepl("^L", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 12, #Doenças de pele

        grepl("^M", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 13, #Doenças Osteomusculares

        grepl("^N", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 14, #Doenças do aparelho geniturinário

        grepl("^O", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 15, #Gravidez, parto e puepério

        grepl("^P", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 16, #Doenças do período pré-natal

        grepl("^Q", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 17, #Maformação congênita

        grepl("^R", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 18, #Achados anormais

        grepl("^S", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^T", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 19, #Lesões

        grepl("^V", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^W", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 20, #Causas externas

        grepl("^Z", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 21, #Fatores que influenciam a saúde

        grepl("^U", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 22, #Propósitos especiais

        TRUE ~ NA_real_
      ),



      
      cid_transito = ifelse(
        grepl("^V", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99,
        1, 0),
      
      cid_violencia = ifelse(
        grepl("^W", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(26,32,33,34,49,50) |
          grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(89:99) |
          grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(0:9),
        1, 0
      ),
      
      cid_cardio = ifelse(
        grepl("^I", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) == 46,
        1, 0),
      
      cid_cardio_tot = ifelse(cid10 == 10, 1, 0),
      
      cid_lesao = ifelse(cid10 == 19, 1, 0),
      
      cid_ext = ifelse(cid10 == 20, 1, 0),
      
      cid_lsxt = ifelse(cid10 == 19 | cid10 == 20, 1, 0),
      
      new_homicidio = ifelse(
        grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(85:99) |
          grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in%c(0:9,35,36),
        1,0
      )
    ) %>% 
    filter(between(data,
                   ymd(paste0(ano,"-09-08")),
                   ymd(paste0(ano,"-12-31"))
                   )
    )
  
  

    temp_df <- temp %>%  left_join(mun_hv,
              by = c("CODMUNOCOR" = "munic"
              )) %>%
    select(-nome) %>%
    setDT()
  # 
  # 
  # 
  # temp_dt <- temp_df[,.(homicidio = sum(homicidio, na.rm = T),
  #                    transito = sum(cid_transito, na.rm = T),
  #                    cardio = sum(cid_cardio, na.rm = T)),
  #                 by = .(co_municipio,dist_hv_border,seg,lat,lon)] 
  # 
  
  
  # 
  # temp_dt <- temp_dt %>% 
  #   mutate(
  #     co_municipio = as.numeric(co_municipio)
  #   ) %>% 
  #   left_join(temp_pop,
  #             by = c("co_municipio" = "codmun")) %>% 
  #   rename(
  #     pop = as.character(ano)
  #   ) %>% 
  #   mutate(
  #     hom_pop = homicidio/(pop/100000),
  #     tra_pop = transito/(pop/100000),
  #     crd_pop = cardio/(pop/100000)
  #   )
  # 
  
  
  message("Saving ",ano,"...")
  saveRDS(temp_df,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_60d_",ano,".rds"))
  
  rm(temp, temp_df, ano)
}

##1.2 2013 - 2019 HV ----

for (ano in 2013:2019) {
  
  message("Reading ", ano," =)")
  
  
  #Abertura da Base unificada
  temp <- read.csv2(paste0(
    "Z:/Arquivos IFB/DATASUS/SIM/OpenDataSUS/Mortalidade_Geral_",ano,".csv"
  ))
  
  
  temp_pop <- censopop %>% 
    select(codmun, as.character(ano)) %>% 
    mutate(codmun = as.numeric(codmun))
  
  
  
  #Alocando as classificações CID-10
  temp <- temp %>% 
    mutate(
      
      CODMUNOCOR = as.numeric(CODMUNOCOR),
      
      data = dmy(sprintf("%08d",as.numeric(DTOBITO))),
      
      semana = isoweek(data),
      
      dia_semana = wday(data),
      
      year = ano,
      
      uf = CODMUNOCOR %/% 10000,
      
      homicidio = ifelse(CIRCOBITO == 3, 1, 0),
      
      
      diag_prefix = substr(CAUSABAS, 1, 3),
      
      #classificando a CID10
      cid10 = case_when(
        grepl("^A", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 1, #Doenças infecciosas e parasitárias
        grepl("^B", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 1, #Doenças infecciosas e parasitárias
        
        grepl("^C", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 2, #Neoplasias
        grepl("^D", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 48 ~ 2, #Neoplasias
        
        grepl("^D", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(50,89) ~ 3, #Doenças do sangue
        
        grepl("^E", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 4, #Doenças Endócrinas
        
        grepl("^F", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 5, #Doenças Comportamentais
        
        grepl("^G", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 6, #Doenças do sistema nervoso
        
        grepl("^H", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 59 ~ 7, #Doenças do olho
        
        grepl("^H", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(60,95) ~ 8, #Doenças do ouvido
        
        grepl("^I", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 9, #Doenças do sistema circulatório
        
        grepl("^J", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 10, #Doenças do aparelho respiratório
        
        grepl("^K", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 11, #Doenças do aprelho digestivo
        
        grepl("^L", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 12, #Doenças de pele
        
        grepl("^M", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 13, #Doenças Osteomusculares
        
        grepl("^N", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 14, #Doenças do aparelho geniturinário
        
        grepl("^O", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 15, #Gravidez, parto e puepério
        
        grepl("^P", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 16, #Doenças do período pré-natal
        
        grepl("^Q", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 17, #Maformação congênita
        
        grepl("^R", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 18, #Achados anormais
        
        grepl("^S", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^T", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 19, #Lesões
        
        grepl("^V", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^W", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 |
          grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 20, #Causas externas
        
        grepl("^Z", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 21, #Fatores que influenciam a saúde
        
        grepl("^U", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99 ~ 22, #Propósitos especiais
        
        TRUE ~ NA_real_
      ),
      
      
      
      cid_transito = ifelse(
        grepl("^V", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) <= 99,
        1, 0),
      
      cid_violencia = ifelse(
        grepl("^W", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(26,32,33,34,49,50) |
          grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(89:99) |
          grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(0:9),
        1, 0
      ),
      
      cid_cardio = ifelse(
        grepl("^I", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) == 46,
        1, 0),
      
      cid_cardio_tot = ifelse(cid10 == 10, 1, 0),
      
      cid_lesao = ifelse(cid10 == 19, 1, 0),
      
      cid_ext = ifelse(cid10 == 20, 1, 0),
      
      cid_lsxt = ifelse(cid10 == 19 | cid10 == 20, 1, 0),
      
      new_homicidio = ifelse(
        grepl("^X", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in% c(85:99) |
          grepl("^Y", diag_prefix) & as.numeric(substr(diag_prefix, 2,3)) %in%c(0:9,35,36),
        1,0
      )
    )
  
  
  
  #Time to HV

  
  if (ano == 2019) {
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-11-03")),
        days_to_hv = as.numeric(data - ref_date)
      )
  
  } else if (ano %in% c(2018)) {
    
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-11-04")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
    
  } else if (ano == 2017) {
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-10-15")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
  } else if (ano == 2016) {
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-10-16")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
  } else if (ano == 2015) {
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-10-18")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
  } else if (ano == 2014) {
    
    temp <- temp %>% 
      mutate(
        ref_date = ymd(paste0(ano, "-10-19")),
        days_to_hv = as.numeric(data - ref_date)
      )
    
  } else if(ano == 2013) { # 2013
    
      temp <- temp %>% 
        mutate(
          ref_date = ymd(paste0(ano, "-10-20")),
          days_to_hv = as.numeric(data - ref_date)
        )
    
  }
    
  
  
  for (d in c(1,7, 30, 45, 60)) {
    
    
  
  if (d == 1) {
    
    dados <- temp %>% 
      filter(days_to_hv == 1) %>% 
      setDT()
    
  } else if (d == 7) {
    dados <- temp %>% 
      filter(days_to_hv > 0 &
               days_to_hv < 8) %>% 
      setDT()
  } else if (d == 30) {
    dados <- temp %>% 
      filter(days_to_hv > 0 &
               days_to_hv < 31) %>% 
      setDT()
  } else if (d == 45){
    dados <- temp %>% 
      filter(days_to_hv > 0 &
               days_to_hv < 46) %>%  
      setDT()
  } else {
    dados <- temp %>% 
      filter(days_to_hv > 0 &
               days_to_hv < 61) %>% 
      setDT()
  }
  
  
    
    message("Ocorrência ",d," anos", ano)
  
    temp_df <- mun_hv %>% 
      left_join(dados,
                by = c(
                  "munic" =   "CODMUNOCOR"
                )) %>% 
      select(-nome) %>% 
      setDT()
    
    
    
  temp_dt <- temp_df[,.(homicidio = sum(homicidio, na.rm = T),
                     transito = sum(cid_transito, na.rm = T),
                     cardio = sum(cid_cardio, na.rm = T),
                     cardio_tot = sum(cid_cardio_tot, na.rm = T),
                     lesao = sum(cid_lesao, na.rm = T),
                     externos = sum(cid_ext, na.rm = T),
                     les_ext = sum(cid_lsxt, na.rm = T),
                     new_homicidio = sum(new_homicidio, na.rm = T)
                     ),
                  by = .(co_municipio,dist_hv_border,seg,lat,lon)] 
  
  
  
  temp_dt <- temp_dt %>% 
    mutate(
      co_municipio = as.numeric(co_municipio)
    ) %>% 
    left_join(temp_pop,
              by = c("co_municipio" = "codmun")) %>% 
    rename(
      pop = as.character(ano)
    ) %>% 
    mutate(
      hom_pop = homicidio/(pop/100000),
      tra_pop = transito/(pop/100000),
      crd_pop = cardio/(pop/100000),
      crt_pop = cardio_tot/(pop/100000),
      les_pop = lesao/(pop/100000),
      ext_pop = externos/(pop/100000),
      lxt_pop = les_ext/(pop/100000),
      nhm_pop = new_homicidio/(pop/100000),
      
      year = ano
    )
  
  
  
  message("Saving ",ano," ",d,"... Mun_Ocur")
  saveRDS(temp_dt,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano,".rds"))
  message("Saved ",ano," ",d," Mun_Ocur")
  
  message("Mun residêncial: ",ano, " ", d)
  
  
  
  
  temp_df <- mun_hv %>% 
    left_join(dados,
              by = c(
                "munic" =   "CODMUNRES"
              )) %>% 
    select(-nome) %>% 
    setDT()
  
  
  
  temp_dt <- temp_df[,.(homicidio = sum(homicidio, na.rm = T),
                        transito = sum(cid_transito, na.rm = T),
                        cardio = sum(cid_cardio, na.rm = T),
                        cardio_tot = sum(cid_cardio_tot, na.rm = T),
                        lesao = sum(cid_lesao, na.rm = T),
                        externos = sum(cid_ext, na.rm = T),
                        les_ext = sum(cid_lsxt, na.rm = T),
                        new_homicidio = sum(new_homicidio, na.rm = T)
  ),
  by = .(co_municipio,dist_hv_border,seg,lat,lon)] 
  
  
  
  temp_dt <- temp_dt %>% 
    mutate(
      co_municipio = as.numeric(co_municipio)
    ) %>% 
    left_join(temp_pop,
              by = c("co_municipio" = "codmun")) %>% 
    rename(
      pop = as.character(ano)
    ) %>% 
    mutate(
      hom_pop = homicidio/(pop/100000),
      tra_pop = transito/(pop/100000),
      crd_pop = cardio/(pop/100000),
      crt_pop = cardio_tot/(pop/100000),
      les_pop = lesao/(pop/100000),
      ext_pop = externos/(pop/100000),
      lxt_pop = les_ext/(pop/100000),
      nhm_pop = new_homicidio/(pop/100000),
      
      year = ano
    )
  
  message("Saving ",ano," ",d,"... Mun_Res")
  saveRDS(temp_dt,paste0( "Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano,".rds"))
  message("Saved ",ano," ",d," Mun_Res")
  
  
  
  rm(d, dados)
  }
  
  
  rm(temp, temp_df)
}

