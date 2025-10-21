#Library ----
library(tidyverse)
library(data.table)
library(sf)
library(haven)
library(labelled)
library(rdrobust)
library(fastDummies)
library(janitor)
library(xtable)
library(viridis)
library(rdd)
library(readstata13)
library(stringr)
library(RColorBrewer)
library(fixest)
library(rddensity)
library(tidyr)
library(stringi)
library(readxl)
library(knitr)
library(stargazer)


# % -------------------------------------------------------------------------- %
# 1. Base ----




for( year in c(2018,2019)) {
  
  
  temp <- readRDS(paste0("Z:/Tuffy/Paper - HVSUS/Bases/CAGED/caged_",year,"_consolidado.rds"))
  
  
  temp <- temp %>% 
    rename(
      codmun = Município,
      mov = `Tipo Mov Desagregado`
      ) %>%
    mutate(
      codmun = as.character(codmun),
      
      mov = as.numeric(mov),
      
      dem_aux = ifelse( mov %in% c(4,5,6,8,90,99), 1, NA),
      new_dem = ifelse( mov %in% c(4,5), 1, NA)
    ) %>% 
    setDT()
  
  agr_df <- temp[, .(

    dem_nov   = sum(dem_aux[mes == 11], na.rm = TRUE),
    dem_dez   = sum(dem_aux[mes == 12], na.rm = TRUE),
    rdem_nov = sum(new_dem[mes == 11], na.rm = TRUE),
    rdem_dez = sum(new_dem[mes == 12], na.rm = TRUE),
    obs_nov   = sum(mes == 11),      # count of November rows
    obs_dez   = sum(mes == 12),      # count of December rows
    obs = .N                   # total rows per codmun
  ), by = .(codmun, ano)]
  

  #Salvando
  saveRDS("Z:/Tuffy/Paper - HVSUS/Bases/CAGED/caged_",year,"_agregado.rds")
rm(year)    
}

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
  left_join(conv, by = c("co_municipio" = "codmun")) %>% 
  mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
         dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
  setDT()


#Unindo as bases de dados:

base <- readRDS("Z:/Tuffy/Paper - HVSUS/Bases/CAGED/caged_2018_agregado.rds") %>% 
  bind_rows(readRDS("Z:/Tuffy/Paper - HVSUS/Bases/CAGED/caged_2019_agregado.rds")) %>% 
  mutate(codmun = as.character(codmun)) %>% 
  left_join(mun_hv %>% 
              select(co_municipio, lon, lat, dist_hv_border, seg, munic) %>% 
              mutate(munic = as.character(munic)),
            by = c("codmun" = "munic")) %>% 
  arrange(co_municipio, ano)

base <- base %>% 
  mutate(
    tx_dem = (dem_dez + dem_nov)/obs,
    tx_d.nov = dem_nov/obs_nov,
    tx_d.dez = dem_dez/obs_dez,
    
    
    ntx_dem = (rdem_dez + rdem_nov)/obs,
    ntx_d.nov = rdem_nov/obs_nov,
    ntx_d.dez = rdem_dez/obs_dez
  )


## 1.1 Base RAIS ----

rais <- readRDS("Z:/Tuffy/Paper - HVSUS/Bases/RAIS/2019/rais_estab_pub.rds")

colnames(rais)

rais <- rais %>%
  setDT() %>%
  setnames(make.names(names(.), unique = TRUE)) %>% 
rename(
    vinc   = Qtd.Vínculos.Ativos,
    codmun = Município
  ) %>% 
  mutate(
    ano = 2019,
    vinc = as.numeric(vinc)
  ) %>% 
  group_by(codmun, ano) %>% 
  summarise(
    v_total = sum(vinc, na.rm = T)
  ) %>%
  ungroup()



### 1.1.1 Caged 2018 ----


vpath <- c("Z:/Arquivos IFB/CAGED/2018/CAGEDEST_012018/CAGEDEST_012018.txt", #Jan
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_022018/CAGEDEST_022018.txt", #Fev
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_032018/CAGEDEST_032018.txt", #Mar
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_042018/CAGEDEST_042018.txt", #Abr
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_052018/CAGEDEST_052018.txt", #Mai
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_062018/CAGEDEST_062018.txt", #Jun
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_072018/CAGEDEST_072018.txt", #Jul
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_082018/CAGEDEST_082018.txt", #Ago
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_092018/CAGEDEST_092018.txt", #Sep
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_102018/CAGEDEST_102018.txt", #Out
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_112018/CAGEDEST_112018.txt", #Nov
           "Z:/Arquivos IFB/CAGED/2018/CAGEDEST_122018/CAGEDEST_122018.txt" #Dez
           )



#Crie o Data Frame antes de rodar este loop
df_final <- NULL
for (path in vpath) {
  
  month <- str_extract(path, "(?<=CAGEDEST_)\\d{2}(?=2018)")
  
  
  message("Abrindo os dados do CAGED para o mês de: ", month,"...")
  
          dt <- fread(path,
                      sep = ";",              
                      encoding = "Latin-1",    
                      na.strings = c("", "NA"),
                      showProgress = TRUE)

          
  message("Sucesso! =)")
  
          dt <- dt %>% 
            rename(
              saldo = `Saldo Mov`,
              codmun = Município
            ) %>% 
            mutate(
              mes = month,
              codmun = as.character(codmun),
              saldo = as.numeric(saldo)
            ) %>% 
            setDT() %>% 
            group_by(codmun) %>% 
            summarise(
              !!paste0("sa_total_",month) := sum(saldo, na.rm = T)
            )
          
          #Salvando o Dataframe
          if (is.null(df_final)) {
            
            
            df_final <- rais %>% 
              select(codmun) %>% 
              mutate(codmun = as.character(codmun))
            
            message("Dataframe final criado!")
            
            
            df_final <- df_final %>% 
              left_join(dt,
                        by = c("codmun" = "codmun")
                        )
            
            
          } else {
            
            df_final <- df_final %>% 
              left_join(dt,
                        by = c("codmun" = "codmun"))
            
            
            message("Merge com sucesso! Vamos para o próximo mês...")
          }
          
          rm(dt, month, path)

}

#O saldo de todas as movimentações municipais calulamos o valor da RAIS de vínculos
#ativos para o ano de 2018


df_final <- df_final %>% 
  replace(is.na(.), 0) %>% 
  group_by(codmun) %>% 
  mutate(
    saldo_final = sa_total_01 + sa_total_02 + sa_total_03 + sa_total_04 + sa_total_05 +
      sa_total_06 + sa_total_07 + sa_total_08 + sa_total_08 + sa_total_09 + 
      sa_total_10 + sa_total_11 + sa_total_12
  ) 



rais <- rais  %>% 
  mutate(codmun = as.character(codmun)) %>% 
  left_join(df_final %>% select(codmun, saldo_final),
            by = c("codmun" = "codmun")) %>% 
  rename(saldo_2018 = saldo_final)

rm(df_final, vpath)

### 1.1.2 Caged 2017 ----


vpath <- c("Z:/Arquivos IFB/CAGED/2017/CAGEDEST_012017.txt", #Jan
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_022017.txt", #Fev
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_032017.txt", #Mar
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_042017.txt", #Abr
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_052017.txt", #Mai
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_062017.txt", #Jun
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_072017.txt", #Jul
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_082017.txt", #Ago
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_092017.txt", #Sep
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_102017.txt", #Out
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_112017.txt", #Nov
           "Z:/Arquivos IFB/CAGED/2017/CAGEDEST_122017.txt" #Dez
)



#Crie o Data Frame antes de rodar este loop
df_final <- NULL
for (path in vpath) {
  
  month <- str_extract(path, "(?<=CAGEDEST_)\\d{2}(?=2017)")
  
  
  message("Abrindo os dados do CAGED para o mês de: ", month,"...")
  
  dt <- fread(path,
              sep = ";",              
              encoding = "Latin-1",    
              na.strings = c("", "NA"),
              showProgress = TRUE)
  
  
  message("Sucesso! =)")
  
  dt <- dt %>% 
    rename(
      saldo = `Saldo Mov`,
      codmun = Município
    ) %>% 
    mutate(
      mes = month,
      codmun = as.character(codmun),
      saldo = as.numeric(saldo)
    ) %>% 
    setDT() %>% 
    group_by(codmun) %>% 
    summarise(
      !!paste0("sa_total_",month) := sum(saldo, na.rm = T)
    )
  
  #Salvando o Dataframe
  if (is.null(df_final)) {
    
    
    df_final <- rais %>% 
      select(codmun) %>% 
      mutate(codmun = as.character(codmun))
    
    message("Dataframe final criado!")
    
    
    df_final <- df_final %>% 
      left_join(dt,
                by = c("codmun" = "codmun")
      )
    
    
  } else {
    
    df_final <- df_final %>% 
      left_join(dt,
                by = c("codmun" = "codmun"))
    
    
    message("Merge com sucesso! Vamos para o próximo mês...")
  }
  
  rm(dt, month, path)
  
}

#O saldo de todas as movimentações municipais calulamos o valor da RAIS de vínculos
#ativos para o ano de 2018


df_final <- df_final %>% 
  replace(is.na(.), 0) %>% 
  group_by(codmun) %>% 
  mutate(
    saldo_final = sa_total_01 + sa_total_02 + sa_total_03 + sa_total_04 + sa_total_05 +
      sa_total_06 + sa_total_07 + sa_total_08 + sa_total_08 + sa_total_09 + 
      sa_total_10 + sa_total_11 + sa_total_12
  ) 



rais <- rais  %>% 
  mutate(codmun = as.character(codmun)) %>% 
  left_join(df_final %>% select(codmun, saldo_final),
            by = c("codmun" = "codmun")) %>% 
  rename(saldo_2017 = saldo_final)




##1.2 Vinculos final ----
rais <- rais %>% 
  group_by(codmun) %>% 
  mutate(
    vinc_2018 = v_total + saldo_2018,
    vinc_2017 = v_total + saldo_2018 + saldo_2017
  )



#Salvando a base de dados

saveRDS(rais,"Z:/Tuffy/Paper - HVSUS/Bases/RAIS/rais_vinculos_19-17.rds")

##1.3 RAIS ----

rais <- readRDS("Z:/Tuffy/Paper - HVSUS/Bases/RAIS/rais_vinculos_19-17.rds")



#Juntando as duas bases

base <- base %>% 
  left_join(rais %>%  select(-ano),
            by = c("codmun" = "codmun"))

#Ajustando os pesos
base <- base %>% 
  group_by(codmun) %>% 
  mutate(
    vinc = case_when(
      
      ano == 2018 ~ vinc_2017,
      ano == 2019 ~ vinc_2018,
      T ~ NA
      
    )
  ) %>% 
  ungroup() %>% 
  select(
    -c(saldo_2018, saldo_2017, v_total, vinc_2017, vinc_2018)
  )



###1.3.1 Variáveis ----

base <- base %>% 
  group_by(codmun, ano) %>% 
  mutate(
    
    obs = as.numeric(obs),
    obs_nov = as.numeric(obs_nov),
    obs_dez = as.numeric(obs_dez),
    
    tx_vnc_dem = (dem_nov + dem_dez)/vinc,
    new_tx_nv_dem = (rdem_nov + rdem_nov)/vinc,
    
    ln_dem = log(dem_nov + dem_dez),
    ln_new_dem = log(rdem_nov + rdem_dez)
    

    
    ) %>% 
  ungroup() 






# ---------------------------------------------------------------------------- #
#2. Regressão ----
# ---------------------------------------------------------------------------- #
for(var in c("tx_dem", "ntx_dem", "tx_vnc_dem", "new_tx_nv_dem", "ln_dem", "ln_new_dem")) {
  
  
  message("Iniciando as regressões para:", var)
  
  #Criando lista de resultados
  if (var == "tx_dem") {
    list <- list()
    
    message("Lista Criada")
  }
  
  
  ini <- Sys.time()
  
  
  
  #BASE
      temp <- base %>% 
        arrange(codmun,ano) %>%
        group_by(codmun) %>%
        filter(!is.na(var)) %>% 
        mutate(
          dup1 = 1,
          dup2 = sum(dup1),
          
          
          v1 = ifelse(ano == 2018, as.numeric(get(var)), NA),
          v2 = max(v1, na.rm = T),
          d.media = as.numeric(get(var)) - v2,
        ) %>%
        ungroup() %>% 
        filter(dup2 == 2) %>% 
        select(-c(dup2, dup1,
                  v1, v2))
      
      
      
      
      if(var %in% c("ln_dem", "ln_new_dem")) {
        
        # Find which codmun have Inf or -Inf in d.media
        bad_codmun <- temp %>%
          filter(!is.finite(d.media)) %>%
          distinct(codmun) %>%
          pull(codmun)
        
        # Filter out those codmun
        if(length(bad_codmun) > 0) {
          #message("Removing codmun(s) with Inf d.media for var = ", var, ": ", 
                  #paste(bad_codmun, collapse = ", "))
          temp <- temp %>% filter(!(codmun %in% bad_codmun))
        }
      
      }
  # Variáveis
  yv <- temp %>%
    filter(ano == 2019) %>% 
    select(d.media) %>% 
    rename(vd = 1)
  
  # Running variable
  xv <- temp %>%
    filter(ano == 2018) %>% 
    select(dist_hv_border)
  
  # Clusters
  clu <- temp %>% 
    filter(ano == 2018) %>% 
    select(seg)
  
  # Latitude
  latv <- temp %>%
    filter(ano == 2018) %>% 
    select(lat)
  
  
  #Peso
  if (var == "tx_dem") {
    
    weiv <- temp %>% 
      filter(ano == 2018) %>% 
      select(obs)
    
    } else {
    
          weiv <- temp %>% 
            filter(ano == 2018) %>% 
            select(vinc)
          }
  # Longitude
  lonv <- temp %>% 
    filter(ano == 2018) %>% 
    select(lon)
  
  ef <- dummy_cols(clu$seg)
  ef <- ef %>% select(-1,-2)
  
  
  ## Principal ----
  
  list[[as.character(paste0(var,"|TOT"))]] <- rdrobust(
    y = yv$vd,
    x = xv$dist_hv_border,
    c = 0,
    weight = weiv,
    cluster = clu$seg,
    vce = "hc0",
    covs = cbind(
      ef,
      lonv,
      latv
    )
  )
  
  
  rm(clu, latv, lonv, ef, xv, yv, weiv )
  
  
  
  #Iniciando as regressões
  if (var == "tx_dem") {
    
    message("Calculando para os diferentes meses...")
    
    for (var2 in c("tx_d.nov", "tx_d.dez", "ntx_d.nov", "ntx_d.dez")) {
      
      
      
      
      temp <- base %>% 
        arrange(codmun,ano) %>%
        filter(!is.na(var2)) %>% 
        group_by(codmun) %>%
        mutate(
          
          dup1 = 1,
          dup2 = sum(dup1),
          
          
          v1 = ifelse(ano == 2018, as.numeric(get(var2)), NA),
          v2 = max(v1, na.rm = T),
          d.media = as.numeric(get(var2)) - v2,
          
          
        ) %>%
        ungroup() %>% 
        filter(dup2 == 2) %>% 
        select(-c(dup2, dup1,
                  v1, v2)) 
      
      
      
      # Variáveis
      yv <- temp %>%
        filter(ano == 2019) %>% 
        select(d.media) %>% 
        rename(vd = 1)
      
      # Running variable
      xv <- temp %>%
        filter(ano == 2018) %>% 
        select(dist_hv_border)
      
      # Clusters
      clu <- temp %>% 
        filter(ano == 2018) %>% 
        select(seg)
      
      # Latitude
      latv <- temp %>%
        filter(ano == 2018) %>% 
        select(lat)
      
      # Longitude
      lonv <- temp %>% 
        filter(ano == 2018) %>% 
        select(lon)
      
      ef <- dummy_cols(clu$seg)
      ef <- ef %>% select(-1,-2)
      
      
      
      if (var2 %in% c("tx_d.nov", "ntx_d.nov")) {
        
        weiv <- temp %>% 
          filter(ano == 2018) %>% 
          select(obs_nov)
        
      } else {
        
        weiv <- temp %>% 
          filter(ano == 2018) %>% 
          select(obs_dez)
        
      }
      
      ## Meses ----
      
      list[[as.character(paste0(var2,"|MNT"))]] <- rdrobust(
        y = yv$vd,
        x = xv$dist_hv_border,
        c = 0,
        weight = weiv,
        cluster = clu$seg,
        vce = "hc0",
        covs = cbind(
          ef,
          lonv,
          latv
        )
      )
      
      
      message("Finalizado para: ", var2)
      rm(var2)
    }
    rm(temp)
  }
  
  
  fim <- Sys.time()
  delta <- fim - ini
  message("Finalizado para __ ",var, " __ Duração de: ", delta)
  
  rm(var, temp, delta, ini, fim, bad_codmun)
}



#3. Tabela ----

tab <- data.frame(
  coef = do.call(rbind,lapply(list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list, FUN = function(x){x$pv[3]})),
  n = do.call(rbind,lapply(list, FUN = function(x){x$N_h})),
  bw = do.call(rbind,lapply(list, FUN = function(x){x$bws[1]})))

print(tab)


tab <- tab %>%
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "***", 
                         ifelse(pv < 0.05, "**", 
                                ifelse(pv < 0.1, "*", "")
                         ))),
    se =  paste0("(", formatC(x = se, digits = 2, format = "f"), ")"),
    N = paste0("[N = ", n.1 + n.2, "]"),
    bw = as.numeric(formatC(x = bw, digits = )),
    bw = bw/1000
  ) %>%
  select( coef, se, N, bw ) %>%
  setDT()


names <- c("Demissões",
           " ", " ",
           "Com/Sem JC",
           " ", " ")

result <- data.frame(
  var = names,
  dem_total = rep(NA, times = length(names)),
  dem_nov = rep(NA, times = length(names)),
  dem_dez = rep(NA, times = length(names)),
  tx_vinc = rep(NA, times = length(names)),
  ln_nvl = rep(NA, times = length(names))
  
)

#Dia 1
#Dem_Total W_OBS
result$dem_total[1] <- tab$coef[[1]]
result$dem_total[2] <- tab$se[[1]]
result$dem_total[3] <- tab$N[[1]]
result$dem_total[4] <- tab$coef[[6]]
result$dem_total[5] <- tab$se[[6]]
result$dem_total[6] <- tab$N[[6]]

##DemNov
result$dem_nov[1] <- tab$coef[[2]]
result$dem_nov[2] <- tab$se[[2]]
result$dem_nov[3] <- tab$N[[2]]
result$dem_nov[4] <- tab$coef[[4]]
result$dem_nov[5] <- tab$se[[4]]
result$dem_nov[6] <- tab$N[[4]]

##DemDez
result$dem_dez[1] <- tab$coef[[3]]
result$dem_dez[2] <- tab$se[[3]]
result$dem_dez[3] <- tab$N[[3]]
result$dem_dez[4] <- tab$coef[[5]]
result$dem_dez[5] <- tab$se[[5]]
result$dem_dez[6] <- tab$N[[5]]

#TXVinc
result$tx_vinc[1] <- tab$coef[[7]]
result$tx_vinc[2] <- tab$se[[7]]
result$tx_vinc[3] <- tab$N[[7]]
result$tx_vinc[4] <- tab$coef[[8]]
result$tx_vinc[5] <- tab$se[[8]]
result$tx_vinc[6] <- tab$N[[8]]

#LNDem
result$ln_nvl[1] <- tab$coef[[9]]
result$ln_nvl[2] <- tab$se[[9]]
result$ln_nvl[3] <- tab$N[[9]]
result$ln_nvl[4] <- tab$coef[[10]]
result$ln_nvl[5] <- tab$se[[10]]
result$ln_nvl[6] <- tab$N[[10]]

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Vinculos.tex")
rm( list, result, tab, latex_table)



#ag_19 <- readRDS("Z:/Tuffy/Paper - HVSUS/Bases/CAGED/caged_2019_agregado.rds")



