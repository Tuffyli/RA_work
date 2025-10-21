# ---------------------------------------------------------------------------- #
# Library ----- 

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

# ****************** ----
#SIM ----

# ---------------------------------------------------------------------------- #
# 1. Regression ---- 
# ---------------------------------------------------------------------------- #
## ***OCO**** ----
## 1.1 Base ----

list <- list()

for ( d in c(1, 7, 30, 45, 60)) {
  
  
  base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_2018.rds")) %>%
    bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_2019.rds"))) 
  
  base <- base %>% 
    arrange(co_municipio,year) %>%
    group_by(co_municipio) %>%
    mutate(
      dup1 = 1,
      dup2 = sum(dup1),
      
      
      v1_nhm = ifelse(year == 2018, nhm_pop, NA),
      v2_nhm = max(v1_nhm, na.rm = T),
      d.media = nhm_pop - v2_nhm,
      
      
    ) %>%
    ungroup() %>% 
    filter(dup2 == 2) %>% 
    select(-c(dup2, dup1,
              v1_nhm, v2_nhm)) %>% 
    mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
           dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
    setDT()
  
  
  ###Regressão -----
  
  yv <- base %>%
    filter(year == 2019) %>% 
    select(d.media) %>% 
    rename(vd = 1)
  
  # Running variable
  xv <- base %>%
    filter(year == 2018) %>% 
    select(dist_hv_border)
  
  # Clusters
  clu <- base %>% 
    filter(year == 2018) %>% 
    select(seg)
  
  # Latitude
  latv <- base %>%
    filter(year == 2018) %>% 
    select(lat)
  
  # Longitude
  lonv <- base %>% 
    filter(year == 2018) %>% 
    select(lon)
  
  ef <- dummy_cols(clu$seg)
  ef <- ef %>% select(-1,-2)
  

  ## 1.1.1 Regression ----
  
  list[[as.character(paste0(2019,"-",2018,"|",d))]] <- rdrobust(
    y = yv$vd,
    x = xv$dist_hv_border,
    c = 0,
    weight = base$pop[base$year == 2018],
    cluster = clu$seg,
    vce = "hc0",
    covs = cbind(
      ef,
      lonv,
      latv
    )
  )
  
  
  rm(clu, latv, lonv, ef, xv, yv, d )
  
}
  
  
  
  # ---------------------------------------------------------------------------- #
  #Extração da banda ótima
  
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
  
  
  
  ##1.2 RESULTADOS -----
  names <- c("2019 - 2018",
             " "," ",
             "Bandwidth")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  result$d1[1] <- tab$coef[[1]]
  result$d1[2] <- tab$se[[1]]
  result$d1[3] <- tab$N[[1]]
  result$d1[4] <- tab$bw[[1]]
  
  
  #Mun de Ocorrência
  result$d7[1] <- tab$coef[[2]]
  result$d7[2] <- tab$se[[2]]
  result$d7[3] <- tab$N[[2]]
  result$d7[4] <- tab$bw[[2]]
  
  
  result$d30[1] <- tab$coef[[3]]
  result$d30[2] <- tab$se[[3]]
  result$d30[3] <- tab$N[[3]]
  result$d30[4] <- tab$bw[[3]]
  
  #Mun de res
  result$d45[1] <- tab$coef[[4]]
  result$d45[2] <- tab$se[[4]]
  result$d45[3] <- tab$N[[4]]
  result$d45[4] <- tab$bw[[4]]
  
  result$d60[1] <- tab$coef[[5]]
  result$d60[2] <- tab$se[[5]]
  result$d60[3] <- tab$N[[5]]
  result$d60[4] <- tab$bw[[5]]
  
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  bw_60main <- list[["2019-2018|60"]]$bws[1]  
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Principal_dias_oco_v2.tex")
  rm( list, result, tab, latex_table)
  
  
  
  




## ***RES*** ----

  
  
  
  
  
  list <- list()
  
  for ( d in c(1,7, 30, 45, 60)) {
    
    
    base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_2018.rds")) %>%
      bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_2019.rds"))) 
    
    base <- base %>% 
      arrange(co_municipio,year) %>%
      group_by(co_municipio) %>%
      mutate(
        dup1 = 1,
        dup2 = sum(dup1),
        
        
        v1_nhm = ifelse(year == 2018, nhm_pop, NA),
        v2_nhm = max(v1_nhm, na.rm = T),
        d.media = nhm_pop - v2_nhm,
        
        
      ) %>%
      ungroup() %>% 
      filter(dup2 == 2) %>% 
      select(-c(dup2, dup1,
                v1_nhm, v2_nhm)) %>% 
      mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
             dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
      setDT()
    
    
    #2. Regressão -----
    
    yv <- base %>%
      filter(year == 2019) %>% 
      select(d.media) %>% 
      rename(vd = 1)
    
    # Running variable
    xv <- base %>%
      filter(year == 2018) %>% 
      select(dist_hv_border)
    
    # Clusters
    clu <- base %>% 
      filter(year == 2018) %>% 
      select(seg)
    
    # Latitude
    latv <- base %>%
      filter(year == 2018) %>% 
      select(lat)
    
    # Longitude
    lonv <- base %>% 
      filter(year == 2018) %>% 
      select(lon)
    
    ef <- dummy_cols(clu$seg)
    ef <- ef %>% select(-1,-2)
    
    
    ## 2.1 Regression ----
    
    list[[as.character(paste0(2019,"-",2018,"|",d))]] <- rdrobust(
      y = yv$vd,
      x = xv$dist_hv_border,
      c = 0,
      weight = base$pop[base$year == 2018],
      cluster = clu$seg,
      vce = "hc0",
      covs = cbind(
        ef,
        lonv,
        latv
      )
    )
    
    
    rm(clu, latv, lonv, ef, xv, yv, d )
    
  }
  
  
  
  # ---------------------------------------------------------------------------- #
  #Extração da banda ótima
  
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
  
  
  
  ##2.2 RESULTADOS -----
  names <- c("2019 - 2018",
             " "," ",
             "Bandwidth")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  
  result$d1[1] <- tab$coef[[1]]
  result$d1[2] <- tab$se[[1]]
  result$d1[3] <- tab$N[[1]]
  result$d1[4] <- tab$bw[[1]]
  
  
  #Mun de Ocorrência
  result$d7[1] <- tab$coef[[2]]
  result$d7[2] <- tab$se[[2]]
  result$d7[3] <- tab$N[[2]]
  result$d7[4] <- tab$bw[[2]]
  
  
  result$d30[1] <- tab$coef[[3]]
  result$d30[2] <- tab$se[[3]]
  result$d30[3] <- tab$N[[3]]
  result$d30[4] <- tab$bw[[3]]
  
  #Mun de res
  result$d45[1] <- tab$coef[[4]]
  result$d45[2] <- tab$se[[4]]
  result$d45[3] <- tab$N[[4]]
  result$d45[4] <- tab$bw[[4]]
  
  result$d60[1] <- tab$coef[[5]]
  result$d60[2] <- tab$se[[5]]
  result$d60[3] <- tab$N[[5]]
  result$d60[4] <- tab$bw[[5]]
  
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lcccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Principal_dias_res_v2.tex")
  rm( list, result, tab, latex_table)
  
  
  
  
  
# ****************** ----  
#Outras variáveis ----
  # ****************** ----
  
# ***OCO*** ----
# 3. Loop  -----
  
  list <- list()
  
  for (d in c(1,7,30,45,60)){
  
    base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_2018.rds")) %>%
      bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_2019.rds"))) 
    
  base <- base %>% 
    arrange(co_municipio,year) %>%
    group_by(co_municipio) %>%
    mutate(
      dup1 = 1,
      dup2 = sum(dup1),
      
      v1_tra = ifelse(year == 2018, tra_pop, NA),
      v2_tra = max(v1_tra, na.rm = T),
      d.tra = tra_pop - v2_tra
    ) %>%
    mutate(
      
      v1_crd = ifelse(year == 2018, crd_pop, NA),
      v2_crd = max(v1_crd, na.rm = T),
      d.crd = crd_pop - v2_crd,
      
      v1_crt = ifelse(year == 2018, crt_pop, NA),
      v2_crt = max(v1_crt, na.rm = T),
      d.crt = crt_pop - v2_crt,
      
      v1_les = ifelse(year == 2018, les_pop, NA),
      v2_les = max(v1_les, na.rm = T),
      d.les = les_pop - v2_les,
      
      v1_ext = ifelse(year == 2018, ext_pop, NA),
      v2_ext = max(v1_ext, na.rm = T),
      d.ext = ext_pop - v2_ext,
      
      v1_lxt = ifelse(year == 2018, lxt_pop, NA),
      v2_lxt = max(v1_lxt, na.rm = T),
      d.lxt = lxt_pop - v2_lxt
      
    ) %>% 
    ungroup() %>% 
    filter(dup2 == 2) %>% 
    select(-c(dup2, dup1, v1_tra, v2_tra,
              v1_crd, v2_crd, v1_crt, v2_crt,
              v1_les, v2_les, v1_lxt, v2_lxt)) %>% 
    mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
           dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
    setDT()
  
  
  var_list <- c("d.tra", "d.crd", "d.crt","d.ext", "d.lxt")
  

  for (var in var_list){
    
    ef <- dummy_cols(base$seg[base$year == 2018])
    ef <- ef %>% select(-1,-2)
    
    
    
    
    list[[as.character(paste0(var,"|",d))]] <-
      rdrobust(
        y = base[[var]][base$year == 2019],
        x = base$dist_hv_border[base$year == 2018],
        c = 0,
        cluster = base$seg[base$year == 2018],
        weights = base$pop[base$year == 2018],
        vce = "hc0",
        covs = cbind(
          ef, 
          base$lat[base$year == 2018], 
          base$lon[base$year == 2018]
        )
      )
    
    message("Finalizado para", var," ",d)
    
  }
  rm(ef,var)
  
  
  
  }
  
  
  
  ## 3.1 Tab ----
  
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
  
  
  names <- c("Trânsito",
             " ", " ",
             "Infato",
             " ", " ",
             "Prob. Cir.",
             " ", " ",
             "C. Externas",
             " ", " ",
             "C.E. + Lesões",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #Dia 1
  #Transito
  result$d1[1] <- tab$coef[[1]]
  result$d1[2] <- tab$se[[1]]
  result$d1[3] <- tab$N[[1]]
  #Inf
  result$d1[4] <- tab$coef[[2]]
  result$d1[5] <- tab$se[[2]]
  result$d1[6] <- tab$N[[2]]
  #PC
  result$d1[7] <- tab$coef[[3]]
  result$d1[8] <- tab$se[[3]]
  result$d1[9] <- tab$N[[3]]
  #ce
  result$d1[10] <- tab$coef[[4]]
  result$d1[11] <- tab$se[[4]]
  result$d1[12] <- tab$N[[4]]
  #CEL
  result$d1[13] <- tab$coef[[5]]
  result$d1[14] <- tab$se[[5]]
  result$d1[15] <- tab$N[[5]]
  
  
  #Dia 7
  #Transito
  result$d7[1] <- tab$coef[[6]]
  result$d7[2] <- tab$se[[6]]
  result$d7[3] <- tab$N[[6]]
  #Inf
  result$d7[4] <- tab$coef[[7]]
  result$d7[5] <- tab$se[[7]]
  result$d7[6] <- tab$N[[7]]
  #PC
  result$d7[7] <- tab$coef[[8]]
  result$d7[8] <- tab$se[[8]]
  result$d7[9] <- tab$N[[8]]
  #ce
  result$d7[10] <- tab$coef[[9]]
  result$d7[11] <- tab$se[[9]]
  result$d7[12] <- tab$N[[9]]
  #CEL
  result$d7[13] <- tab$coef[[10]]
  result$d7[14] <- tab$se[[10]]
  result$d7[15] <- tab$N[[10]]
  
  
  #Dia 30
  #Transito
  result$d30[1] <- tab$coef[[11]]
  result$d30[2] <- tab$se[[11]]
  result$d30[3] <- tab$N[[11]]
  #Inf
  result$d30[4] <- tab$coef[[12]]
  result$d30[5] <- tab$se[[12]]
  result$d30[6] <- tab$N[[12]]
  #PC
  result$d30[7] <- tab$coef[[13]]
  result$d30[8] <- tab$se[[13]]
  result$d30[9] <- tab$N[[13]]
  #ce
  result$d30[10] <- tab$coef[[14]]
  result$d30[11] <- tab$se[[14]]
  result$d30[12] <- tab$N[[14]]
  #CEL
  result$d30[13] <- tab$coef[[15]]
  result$d30[14] <- tab$se[[15]]
  result$d30[15] <- tab$N[[15]]
  
  
  
  #Dia 45
  #Transito
  result$d45[1] <- tab$coef[[16]]
  result$d45[2] <- tab$se[[16]]
  result$d45[3] <- tab$N[[16]]
  #Inf
  result$d45[4] <- tab$coef[[17]]
  result$d45[5] <- tab$se[[17]]
  result$d45[6] <- tab$N[[17]]
  #PC
  result$d45[7] <- tab$coef[[18]]
  result$d45[8] <- tab$se[[18]]
  result$d45[9] <- tab$N[[18]]
  #ce
  result$d45[10] <- tab$coef[[19]]
  result$d45[11] <- tab$se[[19]]
  result$d45[12] <- tab$N[[19]]
  #CEL
  result$d45[13] <- tab$coef[[20]]
  result$d45[14] <- tab$se[[20]]
  result$d45[15] <- tab$N[[20]]
  
  #Dia 60
  #Transito
  result$d60[1] <- tab$coef[[21]]
  result$d60[2] <- tab$se[[21]]
  result$d60[3] <- tab$N[[21]]
  #Inf
  result$d60[4] <- tab$coef[[22]]
  result$d60[5] <- tab$se[[22]]
  result$d60[6] <- tab$N[[22]]
  #PC
  result$d60[7] <- tab$coef[[23]]
  result$d60[8] <- tab$se[[23]]
  result$d60[9] <- tab$N[[23]]
  #ce
  result$d60[10] <- tab$coef[[24]]
  result$d60[11] <- tab$se[[24]]
  result$d60[12] <- tab$N[[24]]
  #CEL
  result$d60[13] <- tab$coef[[25]]
  result$d60[14] <- tab$se[[25]]
  result$d60[15] <- tab$N[[25]]
  
  
  

  
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lcccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Vars_dias_oco.tex")
  rm( list, result, tab, latex_table)
  
  
  
  
  
  
  
  
  
  
  
  # ***RES*** ----
  # 3. Loop  -----
  
  list <- list()
  
  for (d in c(1,7,30,45,60)){
    
    base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_2018.rds")) %>%
      bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_2019.rds"))) 
    
    base <- base %>% 
      arrange(co_municipio,year) %>%
      group_by(co_municipio) %>%
      mutate(
        dup1 = 1,
        dup2 = sum(dup1),
        
        v1_tra = ifelse(year == 2018, tra_pop, NA),
        v2_tra = max(v1_tra, na.rm = T),
        d.tra = tra_pop - v2_tra
      ) %>%
      mutate(
        
        v1_crd = ifelse(year == 2018, crd_pop, NA),
        v2_crd = max(v1_crd, na.rm = T),
        d.crd = crd_pop - v2_crd,
        
        v1_crt = ifelse(year == 2018, crt_pop, NA),
        v2_crt = max(v1_crt, na.rm = T),
        d.crt = crt_pop - v2_crt,
        
        v1_les = ifelse(year == 2018, les_pop, NA),
        v2_les = max(v1_les, na.rm = T),
        d.les = les_pop - v2_les,
        
        v1_ext = ifelse(year == 2018, ext_pop, NA),
        v2_ext = max(v1_ext, na.rm = T),
        d.ext = ext_pop - v2_ext,
        
        v1_lxt = ifelse(year == 2018, lxt_pop, NA),
        v2_lxt = max(v1_lxt, na.rm = T),
        d.lxt = lxt_pop - v2_lxt
        
      ) %>% 
      ungroup() %>% 
      filter(dup2 == 2) %>% 
      select(-c(dup2, dup1, v1_tra, v2_tra,
                v1_crd, v2_crd, v1_crt, v2_crt,
                v1_les, v2_les, v1_lxt, v2_lxt)) %>% 
      mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
             dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
      setDT()
    
    
    var_list <- c("d.tra", "d.crd", "d.crt","d.ext", "d.lxt")
    
    
    for (var in var_list){
      
      ef <- dummy_cols(base$seg[base$year == 2018])
      ef <- ef %>% select(-1,-2)
      
      
      
      
      list[[as.character(paste0(var,"|",d))]] <-
        rdrobust(
          y = base[[var]][base$year == 2019],
          x = base$dist_hv_border[base$year == 2018],
          c = 0,
          cluster = base$seg[base$year == 2018],
          weights = base$pop[base$year == 2018],
          vce = "hc0",
          covs = cbind(
            ef, 
            base$lat[base$year == 2018], 
            base$lon[base$year == 2018]
          )
        )
      
      message("Finalizado para", var," ",d)
      
    }
    rm(ef,var)
    
    
    
  }
  
  
  
  ## 3.1 Tab ----
  
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
  
  
  names <- c("Trânsito",
             " ", " ",
             "Infato",
             " ", " ",
             "Prob. Cir.",
             " ", " ",
             "C. Externas",
             " ", " ",
             "C.E. + Lesões",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #Dia 1
  #Transito
  result$d1[1] <- tab$coef[[1]]
  result$d1[2] <- tab$se[[1]]
  result$d1[3] <- tab$N[[1]]
  #Inf
  result$d1[4] <- tab$coef[[2]]
  result$d1[5] <- tab$se[[2]]
  result$d1[6] <- tab$N[[2]]
  #PC
  result$d1[7] <- tab$coef[[3]]
  result$d1[8] <- tab$se[[3]]
  result$d1[9] <- tab$N[[3]]
  #ce
  result$d1[10] <- tab$coef[[4]]
  result$d1[11] <- tab$se[[4]]
  result$d1[12] <- tab$N[[4]]
  #CEL
  result$d1[13] <- tab$coef[[5]]
  result$d1[14] <- tab$se[[5]]
  result$d1[15] <- tab$N[[5]]
  
  
  #Dia 7
  #Transito
  result$d7[1] <- tab$coef[[6]]
  result$d7[2] <- tab$se[[6]]
  result$d7[3] <- tab$N[[6]]
  #Inf
  result$d7[4] <- tab$coef[[7]]
  result$d7[5] <- tab$se[[7]]
  result$d7[6] <- tab$N[[7]]
  #PC
  result$d7[7] <- tab$coef[[8]]
  result$d7[8] <- tab$se[[8]]
  result$d7[9] <- tab$N[[8]]
  #ce
  result$d7[10] <- tab$coef[[9]]
  result$d7[11] <- tab$se[[9]]
  result$d7[12] <- tab$N[[9]]
  #CEL
  result$d7[13] <- tab$coef[[10]]
  result$d7[14] <- tab$se[[10]]
  result$d7[15] <- tab$N[[10]]
  
  
  #Dia 30
  #Transito
  result$d30[1] <- tab$coef[[11]]
  result$d30[2] <- tab$se[[11]]
  result$d30[3] <- tab$N[[11]]
  #Inf
  result$d30[4] <- tab$coef[[12]]
  result$d30[5] <- tab$se[[12]]
  result$d30[6] <- tab$N[[12]]
  #PC
  result$d30[7] <- tab$coef[[13]]
  result$d30[8] <- tab$se[[13]]
  result$d30[9] <- tab$N[[13]]
  #ce
  result$d30[10] <- tab$coef[[14]]
  result$d30[11] <- tab$se[[14]]
  result$d30[12] <- tab$N[[14]]
  #CEL
  result$d30[13] <- tab$coef[[15]]
  result$d30[14] <- tab$se[[15]]
  result$d30[15] <- tab$N[[15]]
  
  
  
  #Dia 45
  #Transito
  result$d45[1] <- tab$coef[[16]]
  result$d45[2] <- tab$se[[16]]
  result$d45[3] <- tab$N[[16]]
  #Inf
  result$d45[4] <- tab$coef[[17]]
  result$d45[5] <- tab$se[[17]]
  result$d45[6] <- tab$N[[17]]
  #PC
  result$d45[7] <- tab$coef[[18]]
  result$d45[8] <- tab$se[[18]]
  result$d45[9] <- tab$N[[18]]
  #ce
  result$d45[10] <- tab$coef[[19]]
  result$d45[11] <- tab$se[[19]]
  result$d45[12] <- tab$N[[19]]
  #CEL
  result$d45[13] <- tab$coef[[20]]
  result$d45[14] <- tab$se[[20]]
  result$d45[15] <- tab$N[[20]]
  
  #Dia 60
  #Transito
  result$d60[1] <- tab$coef[[21]]
  result$d60[2] <- tab$se[[21]]
  result$d60[3] <- tab$N[[21]]
  #Inf
  result$d60[4] <- tab$coef[[22]]
  result$d60[5] <- tab$se[[22]]
  result$d60[6] <- tab$N[[22]]
  #PC
  result$d60[7] <- tab$coef[[23]]
  result$d60[8] <- tab$se[[23]]
  result$d60[9] <- tab$N[[23]]
  #ce
  result$d60[10] <- tab$coef[[24]]
  result$d60[11] <- tab$se[[24]]
  result$d60[12] <- tab$N[[24]]
  #CEL
  result$d60[13] <- tab$coef[[25]]
  result$d60[14] <- tab$se[[25]]
  result$d60[15] <- tab$N[[25]]
  
  
  
  
  
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lcccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Vars_dias_res.tex")
  rm( list, result, tab, latex_table)
  
  
  
  
  
  
  

# 4. MAPA ---- 

line <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/line.RDS")

bw_main  <- bw_60main

mun_hv <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/mun_hv.RDS") %>% 
  mutate(
    in_band = ifelse(abs(dist_hv_border) <= bw_main, 1, 0),
    final = as.factor(in_band)
  ) 


mun_hv <- mun_hv %>%
  mutate(
    final = factor(final,
                   levels = c("0", "1"),
                   labels = c("Out", "In")
    )
  )


# Mapa de clusters
map <- ggplot(mun_hv) +
  geom_sf(aes(fill = factor(final)) ) +
  geom_sf(data = line, color = "blue") +
  scale_fill_manual(
    name = "Groups",
    values = c(
      "Out"           = "#66c2a5",     # soft green
      "In"  = "#CB4C4E"      # muted red
    ),
    drop = FALSE
  ) +
  guides(fill = guide_legend(
    direction   = "vertical",
    keywidth    = unit(0.8, "cm"),
    keyheight   = unit(0.8, "cm"),
    override.aes = list(alpha = 1)
  )) +
  theme_bw() +
  theme(
    legend.position    = c(0.005, 0.005),        # top-right corner
    legend.justification = c("left", "bottom"),  # align legend box corner
    legend.background  = element_rect(fill = "white", color = "black", size = 0.2),
    legend.key         = element_rect(color = NA),
    axis.text          = element_text(size = 16),
    axis.title         = element_text(size = 18, face = "bold"),
    legend.text        = element_text(size = 16),
    legend.title       = element_text(size= 18)
  ) 


map


ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/map_band_60.png"),plot = map,device = "png", dpi = 300)

rm(line, map, mun_hv, bw_main)

# ****************** ----
#5. Anos ----
# ***OCO*** ----

##5.1 Loop
###5.1.1 Hom ----
list <- list()

for (ano in 2019:2014) {
  
  ano_ant <- ano - 1
  ano_ant <- as.character(ano_ant)
  ano <- as.character(ano)
  
  
  for ( d in c(1, 7, 30, 45, 60)) {
    
    message("Abrindo Bases: ", ano, "e ", ano_ant," para ", d, "dias...")
    
    
    base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano,".rds")) %>%
      bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano_ant,".rds"))) 
    
    base <- base %>% 
      arrange(co_municipio,year) %>%
      group_by(co_municipio) %>%
      mutate(
        dup1 = 1,
        dup2 = sum(dup1),
        
        
        v1_nhm = ifelse(year == ano_ant, nhm_pop, NA),
        v2_nhm = max(v1_nhm, na.rm = T),
        d.media = nhm_pop - v2_nhm
        
        
      ) %>%
      ungroup() %>% 
      filter(dup2 == 2) %>% 
      select(-c(dup2, dup1,
                v1_nhm, v2_nhm)) %>% 
      mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
             dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
      setDT()
    
    
    ###Regressão -----
    
    ef <- dummy_cols(base$seg[base$year == ano_ant])
    ef <- ef %>% select(-1,-2)
    
    
    list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
      rdrobust(
        y = base$d.media[base$year == ano],
        x = base$dist_hv_border[base$year == ano_ant],
        c = 0,
        cluster = base$seg[base$year == ano_ant],
        weights = base$pop[base$year == ano_ant],
        vce = "hc0",
        covs = cbind(
          ef, 
          base$lat[base$year == ano_ant], 
          base$lon[base$year == ano_ant]
        )
      )
    
    
    
    if (ano == 2019) {
      
      
      list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
        rdrobust(
          y = base$d.media[base$year == ano],
          x = base$dist_hv_border[base$year == ano_ant],
          c = 0,
          cluster = base$seg[base$year == ano_ant],
          weights = base$pop[base$year == ano_ant],
          vce = "hc0",
          covs = cbind(
            ef, 
            base$lat[base$year == ano_ant], 
            base$lon[base$year == ano_ant]
          )
        )
      
      if (d == 1) {
          bw_main1  <- list[["2019-2018|1"]]$bws[1]
          bw_bias1  <- list[["2019-2018|1"]]$bws[2]
      } else if (d == 7) {
        bw_main7  <- list[["2019-2018|7"]]$bws[1]
        bw_bias7  <- list[["2019-2018|7"]]$bws[2]
      } else if (d == 30) {
        bw_main30  <- list[["2019-2018|30"]]$bws[1]
        bw_bias30  <- list[["2019-2018|30"]]$bws[2]
      } else if (d == 45) {
        bw_main45  <- list[["2019-2018|45"]]$bws[1]
        bw_bias45  <- list[["2019-2018|45"]]$bws[2]
      } else if (d == 60) {
        bw_main60  <- list[["2019-2018|60"]]$bws[1]
        bw_bias60  <- list[["2019-2018|60"]]$bws[2]
      }
    } else {
      
      
      
      
      list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
        rdrobust(
          y = base$d.media[base$year == ano],
          x = base$dist_hv_border[base$year == ano_ant],
          c = 0,
          h = get(paste0("bw_main",d)),
          b = get(paste0("bw_bias",d)),
          cluster = base$seg[base$year == ano_ant],
          weights = base$pop[base$year == ano_ant],
          vce = "hc0",
          covs = cbind(
            ef, 
            base$lat[base$year == ano_ant], 
            base$lon[base$year == ano_ant]
          )
        )
      
      
    }
    
    
    message("Finalizado para", ano,"e ", ano_ant,"no dia", d)
    
    
  } 
  
  rm(d,ano, ano_ant)
  }

rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
   bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2014 - 2013",
             " "," ",
             "2015 - 2014",
             " "," ",
             "2016 - 2015",
             " "," ",
             "2017 - 2016",
             " "," ",
             "2018 - 2017",
             " "," ",
             "2019 - 2018",
             " "," ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #14-13
  result$d1[1] <- tab$coef[[26]]
  result$d1[2] <- tab$se[[26]]
  result$d1[3] <- tab$N[[26]]
  result$d7[1] <- tab$coef[[27]]
  result$d7[2] <- tab$se[[27]]
  result$d7[3] <- tab$N[[27]]
  result$d30[1] <- tab$coef[[28]]
  result$d30[2] <- tab$se[[28]]
  result$d30[3] <- tab$N[[28]]
  result$d45[1] <- tab$coef[[29]]
  result$d45[2] <- tab$se[[29]]
  result$d45[3] <- tab$N[[29]]
  result$d60[1] <- tab$coef[[30]]
  result$d60[2] <- tab$se[[30]]
  result$d60[3] <- tab$N[[30]]

  
  #15-14
  result$d1[4] <- tab$coef[[21]]
  result$d1[5] <- tab$se[[21]]
  result$d1[6] <- tab$N[[21]]
  result$d7[4] <- tab$coef[[22]]
  result$d7[5] <- tab$se[[22]]
  result$d7[6] <- tab$N[[22]]
  result$d30[4] <- tab$coef[[23]]
  result$d30[5] <- tab$se[[23]]
  result$d30[6] <- tab$N[[23]]
  result$d45[4] <- tab$coef[[24]]
  result$d45[5] <- tab$se[[24]]
  result$d45[6] <- tab$N[[24]]
  result$d60[4] <- tab$coef[[25]]
  result$d60[5] <- tab$se[[25]]
  result$d60[6] <- tab$N[[25]]

  #16-15
  result$d1[7] <- tab$coef[[16]]
  result$d1[8] <- tab$se[[16]]
  result$d1[9] <- tab$N[[16]]
  result$d7[7] <- tab$coef[[17]]
  result$d7[8] <- tab$se[[17]]
  result$d7[9] <- tab$N[[17]]
  result$d30[7] <- tab$coef[[18]]
  result$d30[8] <- tab$se[[18]]
  result$d30[9] <- tab$N[[18]]
  result$d45[7] <- tab$coef[[19]]
  result$d45[8] <- tab$se[[19]]
  result$d45[9] <- tab$N[[19]]
  result$d60[7] <- tab$coef[[20]]
  result$d60[8] <- tab$se[[20]]
  result$d60[9] <- tab$N[[20]]

  #17-16
  result$d1[10] <- tab$coef[[11]]
  result$d1[11] <- tab$se[[11]]
  result$d1[12] <- tab$N[[11]]
  result$d7[10] <- tab$coef[[12]]
  result$d7[11] <- tab$se[[12]]
  result$d7[12] <- tab$N[[12]]
  result$d30[10] <- tab$coef[[13]]
  result$d30[11] <- tab$se[[13]]
  result$d30[12] <- tab$N[[13]]
  result$d45[10] <- tab$coef[[14]]
  result$d45[11] <- tab$se[[14]]
  result$d45[12] <- tab$N[[14]]
  result$d60[10] <- tab$coef[[15]]
  result$d60[11] <- tab$se[[15]]
  result$d60[12] <- tab$N[[15]]
  
  #18-17
  result$d1[13] <- tab$coef[[6]]
  result$d1[14] <- tab$se[[6]]
  result$d1[15] <- tab$N[[6]]
  result$d7[13] <- tab$coef[[7]]
  result$d7[14] <- tab$se[[7]]
  result$d7[15] <- tab$N[[7]]
  result$d30[13] <- tab$coef[[8]]
  result$d30[14] <- tab$se[[8]]
  result$d30[15] <- tab$N[[8]]
  result$d45[13] <- tab$coef[[9]]
  result$d45[14] <- tab$se[[9]]
  result$d45[15] <- tab$N[[9]]
  result$d60[13] <- tab$coef[[10]]
  result$d60[14] <- tab$se[[10]]
  result$d60[15] <- tab$N[[10]]

  # 19-18
  result$d1[16] <- tab$coef[[1]]
  result$d1[17] <- tab$se[[1]]
  result$d1[18] <- tab$N[[1]]
  result$d7[16] <- tab$coef[[2]]
  result$d7[17] <- tab$se[[2]]
  result$d7[18] <- tab$N[[2]]
  result$d30[16] <- tab$coef[[3]]
  result$d30[17] <- tab$se[[3]]
  result$d30[18] <- tab$N[[3]]
  result$d45[16] <- tab$coef[[4]]
  result$d45[17] <- tab$se[[4]]
  result$d45[18] <- tab$N[[4]]
  result$d60[16] <- tab$coef[[5]]
  result$d60[17] <- tab$se[[5]]
  result$d60[18] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Anos_dias_oco.tex")
  rm(list, result, tab, latex_table)
  
  

###5.1.2 Tra ----
  list <- list()
  
  for (ano in 2019:2014) {
    
    ano_ant <- ano - 1
    ano_ant <- as.character(ano_ant)
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, "e ", ano_ant," para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano,".rds")) %>%
        bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(
          dup1 = 1,
          dup2 = sum(dup1),
          
          
          v1_tra = ifelse(year == ano_ant, tra_pop, NA),
          v2_tra = max(v1_tra, na.rm = T),
          d.tra = tra_pop - v2_tra
          
          
        ) %>%
        ungroup() %>% 
        filter(dup2 == 2) %>% 
        select(-c(dup2, dup1,
                  v1_tra, v2_tra)) %>% 
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano_ant])
      ef <- ef %>% select(-1,-2)
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.tra[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019-2018|1"]]$bws[1]
          bw_bias1  <- list[["2019-2018|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019-2018|7"]]$bws[1]
          bw_bias7  <- list[["2019-2018|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019-2018|30"]]$bws[1]
          bw_bias30  <- list[["2019-2018|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019-2018|45"]]$bws[1]
          bw_bias45  <- list[["2019-2018|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019-2018|60"]]$bws[1]
          bw_bias60  <- list[["2019-2018|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.tra[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        
      }
      
      message("Finalizado para", ano,"e ", ano_ant,"no dia", d)
      
      
    } 
    
    rm(d,ano, ano_ant)
  }
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  
  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2014 - 2013",
             " "," ",
             "2015 - 2014",
             " "," ",
             "2016 - 2015",
             " "," ",
             "2017 - 2016",
             " "," ",
             "2018 - 2017",
             " "," ",
             "2019 - 2018",
             " "," ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  #14-13
  result$d1[1] <- tab$coef[[26]]
  result$d1[2] <- tab$se[[26]]
  result$d1[3] <- tab$N[[26]]
  result$d7[1] <- tab$coef[[27]]
  result$d7[2] <- tab$se[[27]]
  result$d7[3] <- tab$N[[27]]
  result$d30[1] <- tab$coef[[28]]
  result$d30[2] <- tab$se[[28]]
  result$d30[3] <- tab$N[[28]]
  result$d45[1] <- tab$coef[[29]]
  result$d45[2] <- tab$se[[29]]
  result$d45[3] <- tab$N[[29]]
  result$d60[1] <- tab$coef[[30]]
  result$d60[2] <- tab$se[[30]]
  result$d60[3] <- tab$N[[30]]
  
  
  #15-14
  result$d1[4] <- tab$coef[[21]]
  result$d1[5] <- tab$se[[21]]
  result$d1[6] <- tab$N[[21]]
  result$d7[4] <- tab$coef[[22]]
  result$d7[5] <- tab$se[[22]]
  result$d7[6] <- tab$N[[22]]
  result$d30[4] <- tab$coef[[23]]
  result$d30[5] <- tab$se[[23]]
  result$d30[6] <- tab$N[[23]]
  result$d45[4] <- tab$coef[[24]]
  result$d45[5] <- tab$se[[24]]
  result$d45[6] <- tab$N[[24]]
  result$d60[4] <- tab$coef[[25]]
  result$d60[5] <- tab$se[[25]]
  result$d60[6] <- tab$N[[25]]
  
  #16-15
  result$d1[7] <- tab$coef[[16]]
  result$d1[8] <- tab$se[[16]]
  result$d1[9] <- tab$N[[16]]
  result$d7[7] <- tab$coef[[17]]
  result$d7[8] <- tab$se[[17]]
  result$d7[9] <- tab$N[[17]]
  result$d30[7] <- tab$coef[[18]]
  result$d30[8] <- tab$se[[18]]
  result$d30[9] <- tab$N[[18]]
  result$d45[7] <- tab$coef[[19]]
  result$d45[8] <- tab$se[[19]]
  result$d45[9] <- tab$N[[19]]
  result$d60[7] <- tab$coef[[20]]
  result$d60[8] <- tab$se[[20]]
  result$d60[9] <- tab$N[[20]]
  
  #17-16
  result$d1[10] <- tab$coef[[11]]
  result$d1[11] <- tab$se[[11]]
  result$d1[12] <- tab$N[[11]]
  result$d7[10] <- tab$coef[[12]]
  result$d7[11] <- tab$se[[12]]
  result$d7[12] <- tab$N[[12]]
  result$d30[10] <- tab$coef[[13]]
  result$d30[11] <- tab$se[[13]]
  result$d30[12] <- tab$N[[13]]
  result$d45[10] <- tab$coef[[14]]
  result$d45[11] <- tab$se[[14]]
  result$d45[12] <- tab$N[[14]]
  result$d60[10] <- tab$coef[[15]]
  result$d60[11] <- tab$se[[15]]
  result$d60[12] <- tab$N[[15]]
  
  #18-17
  result$d1[13] <- tab$coef[[6]]
  result$d1[14] <- tab$se[[6]]
  result$d1[15] <- tab$N[[6]]
  result$d7[13] <- tab$coef[[7]]
  result$d7[14] <- tab$se[[7]]
  result$d7[15] <- tab$N[[7]]
  result$d30[13] <- tab$coef[[8]]
  result$d30[14] <- tab$se[[8]]
  result$d30[15] <- tab$N[[8]]
  result$d45[13] <- tab$coef[[9]]
  result$d45[14] <- tab$se[[9]]
  result$d45[15] <- tab$N[[9]]
  result$d60[13] <- tab$coef[[10]]
  result$d60[14] <- tab$se[[10]]
  result$d60[15] <- tab$N[[10]]
  
  # 19-18
  result$d1[16] <- tab$coef[[1]]
  result$d1[17] <- tab$se[[1]]
  result$d1[18] <- tab$N[[1]]
  result$d7[16] <- tab$coef[[2]]
  result$d7[17] <- tab$se[[2]]
  result$d7[18] <- tab$N[[2]]
  result$d30[16] <- tab$coef[[3]]
  result$d30[17] <- tab$se[[3]]
  result$d30[18] <- tab$N[[3]]
  result$d45[16] <- tab$coef[[4]]
  result$d45[17] <- tab$se[[4]]
  result$d45[18] <- tab$N[[4]]
  result$d60[16] <- tab$coef[[5]]
  result$d60[17] <- tab$se[[5]]
  result$d60[18] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Anos_dias_Tra_oco.tex")
  rm(list, result, tab, latex_table)
  
  
  
###5.1.3 Crt ----
  list <- list()
  
  for (ano in 2019:2014) {
    
    ano_ant <- ano - 1
    ano_ant <- as.character(ano_ant)
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, "e ", ano_ant," para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano,".rds")) %>%
        bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(
          dup1 = 1,
          dup2 = sum(dup1),
          
          
          v1_crt = ifelse(year == ano_ant, crt_pop, NA),
          v2_crt = max(v1_crt, na.rm = T),
          d.crt = crt_pop - v2_crt
          
          
        ) %>%
        ungroup() %>% 
        filter(dup2 == 2) %>% 
        select(-c(dup2, dup1,
                  v1_crt, v2_crt)) %>% 
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano_ant])
      ef <- ef %>% select(-1,-2)
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.crt[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019-2018|1"]]$bws[1]
          bw_bias1  <- list[["2019-2018|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019-2018|7"]]$bws[1]
          bw_bias7  <- list[["2019-2018|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019-2018|30"]]$bws[1]
          bw_bias30  <- list[["2019-2018|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019-2018|45"]]$bws[1]
          bw_bias45  <- list[["2019-2018|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019-2018|60"]]$bws[1]
          bw_bias60  <- list[["2019-2018|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.crt[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        
      }
      
      message("Finalizado para", ano,"e ", ano_ant,"no dia", d)
      
      
    } 
    
    rm(d,ano, ano_ant)
  }
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2014 - 2013",
             " "," ",
             "2015 - 2014",
             " "," ",
             "2016 - 2015",
             " "," ",
             "2017 - 2016",
             " "," ",
             "2018 - 2017",
             " "," ",
             "2019 - 2018",
             " "," ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #14-13
  result$d1[1] <- tab$coef[[26]]
  result$d1[2] <- tab$se[[26]]
  result$d1[3] <- tab$N[[26]]
  result$d7[1] <- tab$coef[[27]]
  result$d7[2] <- tab$se[[27]]
  result$d7[3] <- tab$N[[27]]
  result$d30[1] <- tab$coef[[28]]
  result$d30[2] <- tab$se[[28]]
  result$d30[3] <- tab$N[[28]]
  result$d45[1] <- tab$coef[[29]]
  result$d45[2] <- tab$se[[29]]
  result$d45[3] <- tab$N[[29]]
  result$d60[1] <- tab$coef[[30]]
  result$d60[2] <- tab$se[[30]]
  result$d60[3] <- tab$N[[30]]
  
  
  #15-14
  result$d1[4] <- tab$coef[[21]]
  result$d1[5] <- tab$se[[21]]
  result$d1[6] <- tab$N[[21]]
  result$d7[4] <- tab$coef[[22]]
  result$d7[5] <- tab$se[[22]]
  result$d7[6] <- tab$N[[22]]
  result$d30[4] <- tab$coef[[23]]
  result$d30[5] <- tab$se[[23]]
  result$d30[6] <- tab$N[[23]]
  result$d45[4] <- tab$coef[[24]]
  result$d45[5] <- tab$se[[24]]
  result$d45[6] <- tab$N[[24]]
  result$d60[4] <- tab$coef[[25]]
  result$d60[5] <- tab$se[[25]]
  result$d60[6] <- tab$N[[25]]
  
  #16-15
  result$d1[7] <- tab$coef[[16]]
  result$d1[8] <- tab$se[[16]]
  result$d1[9] <- tab$N[[16]]
  result$d7[7] <- tab$coef[[17]]
  result$d7[8] <- tab$se[[17]]
  result$d7[9] <- tab$N[[17]]
  result$d30[7] <- tab$coef[[18]]
  result$d30[8] <- tab$se[[18]]
  result$d30[9] <- tab$N[[18]]
  result$d45[7] <- tab$coef[[19]]
  result$d45[8] <- tab$se[[19]]
  result$d45[9] <- tab$N[[19]]
  result$d60[7] <- tab$coef[[20]]
  result$d60[8] <- tab$se[[20]]
  result$d60[9] <- tab$N[[20]]
  
  #17-16
  result$d1[10] <- tab$coef[[11]]
  result$d1[11] <- tab$se[[11]]
  result$d1[12] <- tab$N[[11]]
  result$d7[10] <- tab$coef[[12]]
  result$d7[11] <- tab$se[[12]]
  result$d7[12] <- tab$N[[12]]
  result$d30[10] <- tab$coef[[13]]
  result$d30[11] <- tab$se[[13]]
  result$d30[12] <- tab$N[[13]]
  result$d45[10] <- tab$coef[[14]]
  result$d45[11] <- tab$se[[14]]
  result$d45[12] <- tab$N[[14]]
  result$d60[10] <- tab$coef[[15]]
  result$d60[11] <- tab$se[[15]]
  result$d60[12] <- tab$N[[15]]
  
  #18-17
  result$d1[13] <- tab$coef[[6]]
  result$d1[14] <- tab$se[[6]]
  result$d1[15] <- tab$N[[6]]
  result$d7[13] <- tab$coef[[7]]
  result$d7[14] <- tab$se[[7]]
  result$d7[15] <- tab$N[[7]]
  result$d30[13] <- tab$coef[[8]]
  result$d30[14] <- tab$se[[8]]
  result$d30[15] <- tab$N[[8]]
  result$d45[13] <- tab$coef[[9]]
  result$d45[14] <- tab$se[[9]]
  result$d45[15] <- tab$N[[9]]
  result$d60[13] <- tab$coef[[10]]
  result$d60[14] <- tab$se[[10]]
  result$d60[15] <- tab$N[[10]]
  
  # 19-18
  result$d1[16] <- tab$coef[[1]]
  result$d1[17] <- tab$se[[1]]
  result$d1[18] <- tab$N[[1]]
  result$d7[16] <- tab$coef[[2]]
  result$d7[17] <- tab$se[[2]]
  result$d7[18] <- tab$N[[2]]
  result$d30[16] <- tab$coef[[3]]
  result$d30[17] <- tab$se[[3]]
  result$d30[18] <- tab$N[[3]]
  result$d45[16] <- tab$coef[[4]]
  result$d45[17] <- tab$se[[4]]
  result$d45[18] <- tab$N[[4]]
  result$d60[16] <- tab$coef[[5]]
  result$d60[17] <- tab$se[[5]]
  result$d60[18] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Anos_dias_Crt_oco.tex")
  rm(list, result, tab, latex_table)
  
  
  
  ###5.1.4 Lxt ----
  list <- list()
  
  for (ano in 2019:2014) {
    
    ano_ant <- ano - 1
    ano_ant <- as.character(ano_ant)
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, "e ", ano_ant," para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano,".rds")) %>%
        bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(
          dup1 = 1,
          dup2 = sum(dup1),
          
          
          v1_lxt = ifelse(year == ano_ant, lxt_pop, NA),
          v2_lxt = max(v1_lxt, na.rm = T),
          d.lxt = lxt_pop - v2_lxt
          
          
        ) %>%
        ungroup() %>% 
        filter(dup2 == 2) %>% 
        select(-c(dup2, dup1,
                  v1_lxt, v2_lxt)) %>% 
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano_ant])
      ef <- ef %>% select(-1,-2)
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.lxt[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019-2018|1"]]$bws[1]
          bw_bias1  <- list[["2019-2018|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019-2018|7"]]$bws[1]
          bw_bias7  <- list[["2019-2018|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019-2018|30"]]$bws[1]
          bw_bias30  <- list[["2019-2018|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019-2018|45"]]$bws[1]
          bw_bias45  <- list[["2019-2018|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019-2018|60"]]$bws[1]
          bw_bias60  <- list[["2019-2018|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.lxt[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        
      }
      
      message("Finalizado para", ano,"e ", ano_ant,"no dia", d)
      
      
    } 
    
    rm(d,ano, ano_ant)
  }
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60) 
  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2014 - 2013",
             " "," ",
             "2015 - 2014",
             " "," ",
             "2016 - 2015",
             " "," ",
             "2017 - 2016",
             " "," ",
             "2018 - 2017",
             " "," ",
             "2019 - 2018",
             " "," ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #14-13
  result$d1[1] <- tab$coef[[26]]
  result$d1[2] <- tab$se[[26]]
  result$d1[3] <- tab$N[[26]]
  result$d7[1] <- tab$coef[[27]]
  result$d7[2] <- tab$se[[27]]
  result$d7[3] <- tab$N[[27]]
  result$d30[1] <- tab$coef[[28]]
  result$d30[2] <- tab$se[[28]]
  result$d30[3] <- tab$N[[28]]
  result$d45[1] <- tab$coef[[29]]
  result$d45[2] <- tab$se[[29]]
  result$d45[3] <- tab$N[[29]]
  result$d60[1] <- tab$coef[[30]]
  result$d60[2] <- tab$se[[30]]
  result$d60[3] <- tab$N[[30]]
  
  
  #15-14
  result$d1[4] <- tab$coef[[21]]
  result$d1[5] <- tab$se[[21]]
  result$d1[6] <- tab$N[[21]]
  result$d7[4] <- tab$coef[[22]]
  result$d7[5] <- tab$se[[22]]
  result$d7[6] <- tab$N[[22]]
  result$d30[4] <- tab$coef[[23]]
  result$d30[5] <- tab$se[[23]]
  result$d30[6] <- tab$N[[23]]
  result$d45[4] <- tab$coef[[24]]
  result$d45[5] <- tab$se[[24]]
  result$d45[6] <- tab$N[[24]]
  result$d60[4] <- tab$coef[[25]]
  result$d60[5] <- tab$se[[25]]
  result$d60[6] <- tab$N[[25]]
  
  #16-15
  result$d1[7] <- tab$coef[[16]]
  result$d1[8] <- tab$se[[16]]
  result$d1[9] <- tab$N[[16]]
  result$d7[7] <- tab$coef[[17]]
  result$d7[8] <- tab$se[[17]]
  result$d7[9] <- tab$N[[17]]
  result$d30[7] <- tab$coef[[18]]
  result$d30[8] <- tab$se[[18]]
  result$d30[9] <- tab$N[[18]]
  result$d45[7] <- tab$coef[[19]]
  result$d45[8] <- tab$se[[19]]
  result$d45[9] <- tab$N[[19]]
  result$d60[7] <- tab$coef[[20]]
  result$d60[8] <- tab$se[[20]]
  result$d60[9] <- tab$N[[20]]
  
  #17-16
  result$d1[10] <- tab$coef[[11]]
  result$d1[11] <- tab$se[[11]]
  result$d1[12] <- tab$N[[11]]
  result$d7[10] <- tab$coef[[12]]
  result$d7[11] <- tab$se[[12]]
  result$d7[12] <- tab$N[[12]]
  result$d30[10] <- tab$coef[[13]]
  result$d30[11] <- tab$se[[13]]
  result$d30[12] <- tab$N[[13]]
  result$d45[10] <- tab$coef[[14]]
  result$d45[11] <- tab$se[[14]]
  result$d45[12] <- tab$N[[14]]
  result$d60[10] <- tab$coef[[15]]
  result$d60[11] <- tab$se[[15]]
  result$d60[12] <- tab$N[[15]]
  
  #18-17
  result$d1[13] <- tab$coef[[6]]
  result$d1[14] <- tab$se[[6]]
  result$d1[15] <- tab$N[[6]]
  result$d7[13] <- tab$coef[[7]]
  result$d7[14] <- tab$se[[7]]
  result$d7[15] <- tab$N[[7]]
  result$d30[13] <- tab$coef[[8]]
  result$d30[14] <- tab$se[[8]]
  result$d30[15] <- tab$N[[8]]
  result$d45[13] <- tab$coef[[9]]
  result$d45[14] <- tab$se[[9]]
  result$d45[15] <- tab$N[[9]]
  result$d60[13] <- tab$coef[[10]]
  result$d60[14] <- tab$se[[10]]
  result$d60[15] <- tab$N[[10]]
  
  # 19-18
  result$d1[16] <- tab$coef[[1]]
  result$d1[17] <- tab$se[[1]]
  result$d1[18] <- tab$N[[1]]
  result$d7[16] <- tab$coef[[2]]
  result$d7[17] <- tab$se[[2]]
  result$d7[18] <- tab$N[[2]]
  result$d30[16] <- tab$coef[[3]]
  result$d30[17] <- tab$se[[3]]
  result$d30[18] <- tab$N[[3]]
  result$d45[16] <- tab$coef[[4]]
  result$d45[17] <- tab$se[[4]]
  result$d45[18] <- tab$N[[4]]
  result$d60[16] <- tab$coef[[5]]
  result$d60[17] <- tab$se[[5]]
  result$d60[18] <- tab$N[[5]]
  
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Anos_dias_Lxt_oco.tex")
  rm(list, result, tab, latex_table)
  
  
  
  
  
  #5.2 Nível ----
  
  ###5.2.1 Hom ----

  for (ano in 2019:2013) {
    
    if (ano == 2019) {
      
      list <- list()
      
    }
    
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, " para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano,".rds")) #%>%
        #bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        ungroup() %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano])
      ef <- ef %>% select(-1,-2)
      
      # 
      # list[[as.character(paste0(ano,"|",d))]] <-
      #   rdrobust(
      #     y = base$hom_pop[base$year == ano],
      #     x = base$dist_hv_border[base$year == ano],
      #     c = 0,
      #     cluster = base$seg[base$year == ano],
      #     weights = base$pop[base$year == ano],
      #     vce = "hc0",
      #     covs = cbind(
      #       ef, 
      #       base$lat[base$year == ano], 
      #       base$lon[base$year == ano]
      #     )
      #   )
      # 
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$hom_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019|1"]]$bws[1]
          bw_bias1  <- list[["2019|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019|7"]]$bws[1]
          bw_bias7  <- list[["2019|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019|30"]]$bws[1]
          bw_bias30  <- list[["2019|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019|45"]]$bws[1]
          bw_bias45  <- list[["2019|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019|60"]]$bws[1]
          bw_bias60  <- list[["2019|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$hom_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        
      }
      
      
      message("Finalizado para", ano,"no dia", d)
      
      
    } 
    
    rm(d,ano)
  }
  
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2013",
             " "," ",
             "2014",
             " "," ",
             "2015",
             " "," ",
             "2016",
             " "," ",
             "2017",
             " "," ",
             "2018",
             " "," ",
             "2019",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #13
  result$d1[1] <- tab$coef[[31]]
  result$d1[2] <- tab$se[[31]]
  result$d1[3] <- tab$N[[31]]
  result$d7[1] <- tab$coef[[32]]
  result$d7[2] <- tab$se[[32]]
  result$d7[3] <- tab$N[[32]]
  result$d30[1] <- tab$coef[[33]]
  result$d30[2] <- tab$se[[33]]
  result$d30[3] <- tab$N[[33]]
  result$d45[1] <- tab$coef[[34]]
  result$d45[2] <- tab$se[[34]]
  result$d45[3] <- tab$N[[34]]
  result$d60[1] <- tab$coef[[35]]
  result$d60[2] <- tab$se[[35]]
  result$d60[3] <- tab$N[[35]]
  
  #14
  result$d1[4] <- tab$coef[[26]]
  result$d1[5] <- tab$se[[26]]
  result$d1[6] <- tab$N[[26]]
  result$d7[4] <- tab$coef[[27]]
  result$d7[5] <- tab$se[[27]]
  result$d7[6] <- tab$N[[27]]
  result$d30[4] <- tab$coef[[28]]
  result$d30[5] <- tab$se[[28]]
  result$d30[6] <- tab$N[[28]]
  result$d45[4] <- tab$coef[[29]]
  result$d45[5] <- tab$se[[29]]
  result$d45[6] <- tab$N[[29]]
  result$d60[4] <- tab$coef[[30]]
  result$d60[5] <- tab$se[[30]]
  result$d60[6] <- tab$N[[30]]
  
  #15
  result$d1[7] <- tab$coef[[21]]
  result$d1[8] <- tab$se[[21]]
  result$d1[9] <- tab$N[[21]]
  result$d7[7] <- tab$coef[[22]]
  result$d7[8] <- tab$se[[22]]
  result$d7[9] <- tab$N[[22]]
  result$d30[7] <- tab$coef[[23]]
  result$d30[8] <- tab$se[[23]]
  result$d30[9] <- tab$N[[23]]
  result$d45[7] <- tab$coef[[24]]
  result$d45[8] <- tab$se[[24]]
  result$d45[9] <- tab$N[[24]]
  result$d60[7] <- tab$coef[[25]]
  result$d60[8] <- tab$se[[25]]
  result$d60[9] <- tab$N[[25]]
  
  #16
  result$d1[10] <- tab$coef[[16]]
  result$d1[11] <- tab$se[[16]]
  result$d1[12] <- tab$N[[16]]
  result$d7[10] <- tab$coef[[17]]
  result$d7[11] <- tab$se[[17]]
  result$d7[12] <- tab$N[[17]]
  result$d30[10] <- tab$coef[[18]]
  result$d30[11] <- tab$se[[18]]
  result$d30[12] <- tab$N[[18]]
  result$d45[10] <- tab$coef[[19]]
  result$d45[11] <- tab$se[[19]]
  result$d45[12] <- tab$N[[19]]
  result$d60[10] <- tab$coef[[20]]
  result$d60[11] <- tab$se[[20]]
  result$d60[12] <- tab$N[[20]]
  
  #17
  result$d1[13] <- tab$coef[[11]]
  result$d1[14] <- tab$se[[11]]
  result$d1[15] <- tab$N[[11]]
  result$d7[13] <- tab$coef[[12]]
  result$d7[14] <- tab$se[[12]]
  result$d7[15] <- tab$N[[12]]
  result$d30[13] <- tab$coef[[13]]
  result$d30[14] <- tab$se[[13]]
  result$d30[15] <- tab$N[[13]]
  result$d45[13] <- tab$coef[[14]]
  result$d45[14] <- tab$se[[14]]
  result$d45[15] <- tab$N[[14]]
  result$d60[13] <- tab$coef[[15]]
  result$d60[14] <- tab$se[[15]]
  result$d60[15] <- tab$N[[15]]
  
  #18
  result$d1[16] <- tab$coef[[6]]
  result$d1[17] <- tab$se[[6]]
  result$d1[18] <- tab$N[[6]]
  result$d7[16] <- tab$coef[[7]]
  result$d7[17] <- tab$se[[7]]
  result$d7[18] <- tab$N[[7]]
  result$d30[16] <- tab$coef[[8]]
  result$d30[17] <- tab$se[[8]]
  result$d30[18] <- tab$N[[8]]
  result$d45[16] <- tab$coef[[9]]
  result$d45[17] <- tab$se[[9]]
  result$d45[18] <- tab$N[[9]]
  result$d60[16] <- tab$coef[[10]]
  result$d60[17] <- tab$se[[10]]
  result$d60[18] <- tab$N[[10]]
  
  # 19
  result$d1[19] <- tab$coef[[1]]
  result$d1[20] <- tab$se[[1]]
  result$d1[21] <- tab$N[[1]]
  result$d7[19] <- tab$coef[[2]]
  result$d7[20] <- tab$se[[2]]
  result$d7[21] <- tab$N[[2]]
  result$d30[19] <- tab$coef[[3]]
  result$d30[20] <- tab$se[[3]]
  result$d30[21] <- tab$N[[3]]
  result$d45[19] <- tab$coef[[4]]
  result$d45[20] <- tab$se[[4]]
  result$d45[21] <- tab$N[[4]]
  result$d60[19] <- tab$coef[[5]]
  result$d60[20] <- tab$se[[5]]
  result$d60[21] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Nivel_anos_dias_oco.tex")
  rm(list, result, tab, latex_table)
  
  
  
  ###5.2.2 TRA ----
  
  for (ano in 2019:2013) {
    
    if (ano == 2019) {
      
      list <- list()
      
    }
    
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, " para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano,".rds")) #%>%
      #bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        ungroup() %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano])
      ef <- ef %>% select(-1,-2)
      
      # 
      # list[[as.character(paste0(ano,"|",d))]] <-
      #   rdrobust(
      #     y = base$hom_pop[base$year == ano],
      #     x = base$dist_hv_border[base$year == ano],
      #     c = 0,
      #     cluster = base$seg[base$year == ano],
      #     weights = base$pop[base$year == ano],
      #     vce = "hc0",
      #     covs = cbind(
      #       ef, 
      #       base$lat[base$year == ano], 
      #       base$lon[base$year == ano]
      #     )
      #   )
      # 
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$tra_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019|1"]]$bws[1]
          bw_bias1  <- list[["2019|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019|7"]]$bws[1]
          bw_bias7  <- list[["2019|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019|30"]]$bws[1]
          bw_bias30  <- list[["2019|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019|45"]]$bws[1]
          bw_bias45  <- list[["2019|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019|60"]]$bws[1]
          bw_bias60  <- list[["2019|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$tra_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        
      }
      
      
      message("Finalizado para ", ano," no dia ", d)
      
      
    } 
    
    rm(d,ano)
  }
  
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2013",
             " "," ",
             "2014",
             " "," ",
             "2015",
             " "," ",
             "2016",
             " "," ",
             "2017",
             " "," ",
             "2018",
             " "," ",
             "2019",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #13
  result$d1[1] <- tab$coef[[31]]
  result$d1[2] <- tab$se[[31]]
  result$d1[3] <- tab$N[[31]]
  result$d7[1] <- tab$coef[[32]]
  result$d7[2] <- tab$se[[32]]
  result$d7[3] <- tab$N[[32]]
  result$d30[1] <- tab$coef[[33]]
  result$d30[2] <- tab$se[[33]]
  result$d30[3] <- tab$N[[33]]
  result$d45[1] <- tab$coef[[34]]
  result$d45[2] <- tab$se[[34]]
  result$d45[3] <- tab$N[[34]]
  result$d60[1] <- tab$coef[[35]]
  result$d60[2] <- tab$se[[35]]
  result$d60[3] <- tab$N[[35]]
  
  #14
  result$d1[4] <- tab$coef[[26]]
  result$d1[5] <- tab$se[[26]]
  result$d1[6] <- tab$N[[26]]
  result$d7[4] <- tab$coef[[27]]
  result$d7[5] <- tab$se[[27]]
  result$d7[6] <- tab$N[[27]]
  result$d30[4] <- tab$coef[[28]]
  result$d30[5] <- tab$se[[28]]
  result$d30[6] <- tab$N[[28]]
  result$d45[4] <- tab$coef[[29]]
  result$d45[5] <- tab$se[[29]]
  result$d45[6] <- tab$N[[29]]
  result$d60[4] <- tab$coef[[30]]
  result$d60[5] <- tab$se[[30]]
  result$d60[6] <- tab$N[[30]]
  
  #15
  result$d1[7] <- tab$coef[[21]]
  result$d1[8] <- tab$se[[21]]
  result$d1[9] <- tab$N[[21]]
  result$d7[7] <- tab$coef[[22]]
  result$d7[8] <- tab$se[[22]]
  result$d7[9] <- tab$N[[22]]
  result$d30[7] <- tab$coef[[23]]
  result$d30[8] <- tab$se[[23]]
  result$d30[9] <- tab$N[[23]]
  result$d45[7] <- tab$coef[[24]]
  result$d45[8] <- tab$se[[24]]
  result$d45[9] <- tab$N[[24]]
  result$d60[7] <- tab$coef[[25]]
  result$d60[8] <- tab$se[[25]]
  result$d60[9] <- tab$N[[25]]
  
  #16
  result$d1[10] <- tab$coef[[16]]
  result$d1[11] <- tab$se[[16]]
  result$d1[12] <- tab$N[[16]]
  result$d7[10] <- tab$coef[[17]]
  result$d7[11] <- tab$se[[17]]
  result$d7[12] <- tab$N[[17]]
  result$d30[10] <- tab$coef[[18]]
  result$d30[11] <- tab$se[[18]]
  result$d30[12] <- tab$N[[18]]
  result$d45[10] <- tab$coef[[19]]
  result$d45[11] <- tab$se[[19]]
  result$d45[12] <- tab$N[[19]]
  result$d60[10] <- tab$coef[[20]]
  result$d60[11] <- tab$se[[20]]
  result$d60[12] <- tab$N[[20]]
  
  #17
  result$d1[13] <- tab$coef[[11]]
  result$d1[14] <- tab$se[[11]]
  result$d1[15] <- tab$N[[11]]
  result$d7[13] <- tab$coef[[12]]
  result$d7[14] <- tab$se[[12]]
  result$d7[15] <- tab$N[[12]]
  result$d30[13] <- tab$coef[[13]]
  result$d30[14] <- tab$se[[13]]
  result$d30[15] <- tab$N[[13]]
  result$d45[13] <- tab$coef[[14]]
  result$d45[14] <- tab$se[[14]]
  result$d45[15] <- tab$N[[14]]
  result$d60[13] <- tab$coef[[15]]
  result$d60[14] <- tab$se[[15]]
  result$d60[15] <- tab$N[[15]]
  
  #18
  result$d1[16] <- tab$coef[[6]]
  result$d1[17] <- tab$se[[6]]
  result$d1[18] <- tab$N[[6]]
  result$d7[16] <- tab$coef[[7]]
  result$d7[17] <- tab$se[[7]]
  result$d7[18] <- tab$N[[7]]
  result$d30[16] <- tab$coef[[8]]
  result$d30[17] <- tab$se[[8]]
  result$d30[18] <- tab$N[[8]]
  result$d45[16] <- tab$coef[[9]]
  result$d45[17] <- tab$se[[9]]
  result$d45[18] <- tab$N[[9]]
  result$d60[16] <- tab$coef[[10]]
  result$d60[17] <- tab$se[[10]]
  result$d60[18] <- tab$N[[10]]
  
  # 19
  result$d1[19] <- tab$coef[[1]]
  result$d1[20] <- tab$se[[1]]
  result$d1[21] <- tab$N[[1]]
  result$d7[19] <- tab$coef[[2]]
  result$d7[20] <- tab$se[[2]]
  result$d7[21] <- tab$N[[2]]
  result$d30[19] <- tab$coef[[3]]
  result$d30[20] <- tab$se[[3]]
  result$d30[21] <- tab$N[[3]]
  result$d45[19] <- tab$coef[[4]]
  result$d45[20] <- tab$se[[4]]
  result$d45[21] <- tab$N[[4]]
  result$d60[19] <- tab$coef[[5]]
  result$d60[20] <- tab$se[[5]]
  result$d60[21] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Nivel_tra_anos_oco.tex")
  rm(list, result, tab, latex_table)
  
  
  
  
  
  ###5.2.3 CRT ----
  
  for (ano in 2019:2013) {
    
    if (ano == 2019) {
      
      list <- list()
      
    }
    
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, " para ", d, " dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano,".rds")) #%>%
      #bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        ungroup() %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano])
      ef <- ef %>% select(-1,-2)
      
      # 
      # list[[as.character(paste0(ano,"|",d))]] <-
      #   rdrobust(
      #     y = base$hom_pop[base$year == ano],
      #     x = base$dist_hv_border[base$year == ano],
      #     c = 0,
      #     cluster = base$seg[base$year == ano],
      #     weights = base$pop[base$year == ano],
      #     vce = "hc0",
      #     covs = cbind(
      #       ef, 
      #       base$lat[base$year == ano], 
      #       base$lon[base$year == ano]
      #     )
      #   )
      # 
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$crt_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019|1"]]$bws[1]
          bw_bias1  <- list[["2019|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019|7"]]$bws[1]
          bw_bias7  <- list[["2019|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019|30"]]$bws[1]
          bw_bias30  <- list[["2019|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019|45"]]$bws[1]
          bw_bias45  <- list[["2019|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019|60"]]$bws[1]
          bw_bias60  <- list[["2019|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$crt_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        
      }
      
      
      message("Finalizado para ", ano," no dia ", d)
      
      
    } 
    
    rm(d,ano)
  }
  
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2013",
             " "," ",
             "2014",
             " "," ",
             "2015",
             " "," ",
             "2016",
             " "," ",
             "2017",
             " "," ",
             "2018",
             " "," ",
             "2019",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #13
  result$d1[1] <- tab$coef[[31]]
  result$d1[2] <- tab$se[[31]]
  result$d1[3] <- tab$N[[31]]
  result$d7[1] <- tab$coef[[32]]
  result$d7[2] <- tab$se[[32]]
  result$d7[3] <- tab$N[[32]]
  result$d30[1] <- tab$coef[[33]]
  result$d30[2] <- tab$se[[33]]
  result$d30[3] <- tab$N[[33]]
  result$d45[1] <- tab$coef[[34]]
  result$d45[2] <- tab$se[[34]]
  result$d45[3] <- tab$N[[34]]
  result$d60[1] <- tab$coef[[35]]
  result$d60[2] <- tab$se[[35]]
  result$d60[3] <- tab$N[[35]]
  
  #14
  result$d1[4] <- tab$coef[[26]]
  result$d1[5] <- tab$se[[26]]
  result$d1[6] <- tab$N[[26]]
  result$d7[4] <- tab$coef[[27]]
  result$d7[5] <- tab$se[[27]]
  result$d7[6] <- tab$N[[27]]
  result$d30[4] <- tab$coef[[28]]
  result$d30[5] <- tab$se[[28]]
  result$d30[6] <- tab$N[[28]]
  result$d45[4] <- tab$coef[[29]]
  result$d45[5] <- tab$se[[29]]
  result$d45[6] <- tab$N[[29]]
  result$d60[4] <- tab$coef[[30]]
  result$d60[5] <- tab$se[[30]]
  result$d60[6] <- tab$N[[30]]
  
  #15
  result$d1[7] <- tab$coef[[21]]
  result$d1[8] <- tab$se[[21]]
  result$d1[9] <- tab$N[[21]]
  result$d7[7] <- tab$coef[[22]]
  result$d7[8] <- tab$se[[22]]
  result$d7[9] <- tab$N[[22]]
  result$d30[7] <- tab$coef[[23]]
  result$d30[8] <- tab$se[[23]]
  result$d30[9] <- tab$N[[23]]
  result$d45[7] <- tab$coef[[24]]
  result$d45[8] <- tab$se[[24]]
  result$d45[9] <- tab$N[[24]]
  result$d60[7] <- tab$coef[[25]]
  result$d60[8] <- tab$se[[25]]
  result$d60[9] <- tab$N[[25]]
  
  #16
  result$d1[10] <- tab$coef[[16]]
  result$d1[11] <- tab$se[[16]]
  result$d1[12] <- tab$N[[16]]
  result$d7[10] <- tab$coef[[17]]
  result$d7[11] <- tab$se[[17]]
  result$d7[12] <- tab$N[[17]]
  result$d30[10] <- tab$coef[[18]]
  result$d30[11] <- tab$se[[18]]
  result$d30[12] <- tab$N[[18]]
  result$d45[10] <- tab$coef[[19]]
  result$d45[11] <- tab$se[[19]]
  result$d45[12] <- tab$N[[19]]
  result$d60[10] <- tab$coef[[20]]
  result$d60[11] <- tab$se[[20]]
  result$d60[12] <- tab$N[[20]]
  
  #17
  result$d1[13] <- tab$coef[[11]]
  result$d1[14] <- tab$se[[11]]
  result$d1[15] <- tab$N[[11]]
  result$d7[13] <- tab$coef[[12]]
  result$d7[14] <- tab$se[[12]]
  result$d7[15] <- tab$N[[12]]
  result$d30[13] <- tab$coef[[13]]
  result$d30[14] <- tab$se[[13]]
  result$d30[15] <- tab$N[[13]]
  result$d45[13] <- tab$coef[[14]]
  result$d45[14] <- tab$se[[14]]
  result$d45[15] <- tab$N[[14]]
  result$d60[13] <- tab$coef[[15]]
  result$d60[14] <- tab$se[[15]]
  result$d60[15] <- tab$N[[15]]
  
  #18
  result$d1[16] <- tab$coef[[6]]
  result$d1[17] <- tab$se[[6]]
  result$d1[18] <- tab$N[[6]]
  result$d7[16] <- tab$coef[[7]]
  result$d7[17] <- tab$se[[7]]
  result$d7[18] <- tab$N[[7]]
  result$d30[16] <- tab$coef[[8]]
  result$d30[17] <- tab$se[[8]]
  result$d30[18] <- tab$N[[8]]
  result$d45[16] <- tab$coef[[9]]
  result$d45[17] <- tab$se[[9]]
  result$d45[18] <- tab$N[[9]]
  result$d60[16] <- tab$coef[[10]]
  result$d60[17] <- tab$se[[10]]
  result$d60[18] <- tab$N[[10]]
  
  # 19
  result$d1[19] <- tab$coef[[1]]
  result$d1[20] <- tab$se[[1]]
  result$d1[21] <- tab$N[[1]]
  result$d7[19] <- tab$coef[[2]]
  result$d7[20] <- tab$se[[2]]
  result$d7[21] <- tab$N[[2]]
  result$d30[19] <- tab$coef[[3]]
  result$d30[20] <- tab$se[[3]]
  result$d30[21] <- tab$N[[3]]
  result$d45[19] <- tab$coef[[4]]
  result$d45[20] <- tab$se[[4]]
  result$d45[21] <- tab$N[[4]]
  result$d60[19] <- tab$coef[[5]]
  result$d60[20] <- tab$se[[5]]
  result$d60[21] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Nivel_crt_anos_oco.tex")
  rm(list, result, tab, latex_table)
  
  
  
  
  
  ###5.2.5 LXT ----
  
  for (ano in 2019:2013) {
    
    if (ano == 2019) {
      
      list <- list()
      
    }
    
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, " para ", d, " dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano,".rds")) #%>%
      #bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_oco_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        ungroup() %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano])
      ef <- ef %>% select(-1,-2)
      
      # 
      # list[[as.character(paste0(ano,"|",d))]] <-
      #   rdrobust(
      #     y = base$hom_pop[base$year == ano],
      #     x = base$dist_hv_border[base$year == ano],
      #     c = 0,
      #     cluster = base$seg[base$year == ano],
      #     weights = base$pop[base$year == ano],
      #     vce = "hc0",
      #     covs = cbind(
      #       ef, 
      #       base$lat[base$year == ano], 
      #       base$lon[base$year == ano]
      #     )
      #   )
      # 
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$lxt_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019|1"]]$bws[1]
          bw_bias1  <- list[["2019|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019|7"]]$bws[1]
          bw_bias7  <- list[["2019|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019|30"]]$bws[1]
          bw_bias30  <- list[["2019|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019|45"]]$bws[1]
          bw_bias45  <- list[["2019|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019|60"]]$bws[1]
          bw_bias60  <- list[["2019|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$lxt_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        
      }
      
      
      message("Finalizado para ", ano," no dia ", d)
      
      
    } 
    
    rm(d,ano)
  }
  
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2013",
             " "," ",
             "2014",
             " "," ",
             "2015",
             " "," ",
             "2016",
             " "," ",
             "2017",
             " "," ",
             "2018",
             " "," ",
             "2019",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #13
  result$d1[1] <- tab$coef[[31]]
  result$d1[2] <- tab$se[[31]]
  result$d1[3] <- tab$N[[31]]
  result$d7[1] <- tab$coef[[32]]
  result$d7[2] <- tab$se[[32]]
  result$d7[3] <- tab$N[[32]]
  result$d30[1] <- tab$coef[[33]]
  result$d30[2] <- tab$se[[33]]
  result$d30[3] <- tab$N[[33]]
  result$d45[1] <- tab$coef[[34]]
  result$d45[2] <- tab$se[[34]]
  result$d45[3] <- tab$N[[34]]
  result$d60[1] <- tab$coef[[35]]
  result$d60[2] <- tab$se[[35]]
  result$d60[3] <- tab$N[[35]]
  
  #14
  result$d1[4] <- tab$coef[[26]]
  result$d1[5] <- tab$se[[26]]
  result$d1[6] <- tab$N[[26]]
  result$d7[4] <- tab$coef[[27]]
  result$d7[5] <- tab$se[[27]]
  result$d7[6] <- tab$N[[27]]
  result$d30[4] <- tab$coef[[28]]
  result$d30[5] <- tab$se[[28]]
  result$d30[6] <- tab$N[[28]]
  result$d45[4] <- tab$coef[[29]]
  result$d45[5] <- tab$se[[29]]
  result$d45[6] <- tab$N[[29]]
  result$d60[4] <- tab$coef[[30]]
  result$d60[5] <- tab$se[[30]]
  result$d60[6] <- tab$N[[30]]
  
  #15
  result$d1[7] <- tab$coef[[21]]
  result$d1[8] <- tab$se[[21]]
  result$d1[9] <- tab$N[[21]]
  result$d7[7] <- tab$coef[[22]]
  result$d7[8] <- tab$se[[22]]
  result$d7[9] <- tab$N[[22]]
  result$d30[7] <- tab$coef[[23]]
  result$d30[8] <- tab$se[[23]]
  result$d30[9] <- tab$N[[23]]
  result$d45[7] <- tab$coef[[24]]
  result$d45[8] <- tab$se[[24]]
  result$d45[9] <- tab$N[[24]]
  result$d60[7] <- tab$coef[[25]]
  result$d60[8] <- tab$se[[25]]
  result$d60[9] <- tab$N[[25]]
  
  #16
  result$d1[10] <- tab$coef[[16]]
  result$d1[11] <- tab$se[[16]]
  result$d1[12] <- tab$N[[16]]
  result$d7[10] <- tab$coef[[17]]
  result$d7[11] <- tab$se[[17]]
  result$d7[12] <- tab$N[[17]]
  result$d30[10] <- tab$coef[[18]]
  result$d30[11] <- tab$se[[18]]
  result$d30[12] <- tab$N[[18]]
  result$d45[10] <- tab$coef[[19]]
  result$d45[11] <- tab$se[[19]]
  result$d45[12] <- tab$N[[19]]
  result$d60[10] <- tab$coef[[20]]
  result$d60[11] <- tab$se[[20]]
  result$d60[12] <- tab$N[[20]]
  
  #17
  result$d1[13] <- tab$coef[[11]]
  result$d1[14] <- tab$se[[11]]
  result$d1[15] <- tab$N[[11]]
  result$d7[13] <- tab$coef[[12]]
  result$d7[14] <- tab$se[[12]]
  result$d7[15] <- tab$N[[12]]
  result$d30[13] <- tab$coef[[13]]
  result$d30[14] <- tab$se[[13]]
  result$d30[15] <- tab$N[[13]]
  result$d45[13] <- tab$coef[[14]]
  result$d45[14] <- tab$se[[14]]
  result$d45[15] <- tab$N[[14]]
  result$d60[13] <- tab$coef[[15]]
  result$d60[14] <- tab$se[[15]]
  result$d60[15] <- tab$N[[15]]
  
  #18
  result$d1[16] <- tab$coef[[6]]
  result$d1[17] <- tab$se[[6]]
  result$d1[18] <- tab$N[[6]]
  result$d7[16] <- tab$coef[[7]]
  result$d7[17] <- tab$se[[7]]
  result$d7[18] <- tab$N[[7]]
  result$d30[16] <- tab$coef[[8]]
  result$d30[17] <- tab$se[[8]]
  result$d30[18] <- tab$N[[8]]
  result$d45[16] <- tab$coef[[9]]
  result$d45[17] <- tab$se[[9]]
  result$d45[18] <- tab$N[[9]]
  result$d60[16] <- tab$coef[[10]]
  result$d60[17] <- tab$se[[10]]
  result$d60[18] <- tab$N[[10]]
  
  # 19
  result$d1[19] <- tab$coef[[1]]
  result$d1[20] <- tab$se[[1]]
  result$d1[21] <- tab$N[[1]]
  result$d7[19] <- tab$coef[[2]]
  result$d7[20] <- tab$se[[2]]
  result$d7[21] <- tab$N[[2]]
  result$d30[19] <- tab$coef[[3]]
  result$d30[20] <- tab$se[[3]]
  result$d30[21] <- tab$N[[3]]
  result$d45[19] <- tab$coef[[4]]
  result$d45[20] <- tab$se[[4]]
  result$d45[21] <- tab$N[[4]]
  result$d60[19] <- tab$coef[[5]]
  result$d60[20] <- tab$se[[5]]
  result$d60[21] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Nivel_lxt_anos_oco.tex")
  rm(list, result, tab, latex_table)
  
  
  
  
  
  # ***RES*** ----
  
  ##5.3 Loop
  ###5.3.1 Hom ----
  list <- list()
  
  for (ano in 2019:2014) {
    
    ano_ant <- ano - 1
    ano_ant <- as.character(ano_ant)
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, "e ", ano_ant," para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano,".rds")) %>%
        bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(
          dup1 = 1,
          dup2 = sum(dup1),
          
          
          v1_nhm = ifelse(year == ano_ant, nhm_pop, NA),
          v2_nhm = max(v1_nhm, na.rm = T),
          d.media = nhm_pop - v2_nhm
          
          
        ) %>%
        ungroup() %>% 
        filter(dup2 == 2) %>% 
        select(-c(dup2, dup1,
                  v1_nhm, v2_nhm)) %>% 
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano_ant])
      ef <- ef %>% select(-1,-2)
      
      
      list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
        rdrobust(
          y = base$d.media[base$year == ano],
          x = base$dist_hv_border[base$year == ano_ant],
          c = 0,
          cluster = base$seg[base$year == ano_ant],
          weights = base$pop[base$year == ano_ant],
          vce = "hc0",
          covs = cbind(
            ef, 
            base$lat[base$year == ano_ant], 
            base$lon[base$year == ano_ant]
          )
        )
      
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.media[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019-2018|1"]]$bws[1]
          bw_bias1  <- list[["2019-2018|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019-2018|7"]]$bws[1]
          bw_bias7  <- list[["2019-2018|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019-2018|30"]]$bws[1]
          bw_bias30  <- list[["2019-2018|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019-2018|45"]]$bws[1]
          bw_bias45  <- list[["2019-2018|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019-2018|60"]]$bws[1]
          bw_bias60  <- list[["2019-2018|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.media[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        
      }
      
      
      message("Finalizado para", ano,"e ", ano_ant,"no dia", d)
      
      
    } 
    
    rm(d,ano, ano_ant)
  }
  
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2014 - 2013",
             " "," ",
             "2015 - 2014",
             " "," ",
             "2016 - 2015",
             " "," ",
             "2017 - 2016",
             " "," ",
             "2018 - 2017",
             " "," ",
             "2019 - 2018",
             " "," ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #14-13
  result$d1[1] <- tab$coef[[26]]
  result$d1[2] <- tab$se[[26]]
  result$d1[3] <- tab$N[[26]]
  result$d7[1] <- tab$coef[[27]]
  result$d7[2] <- tab$se[[27]]
  result$d7[3] <- tab$N[[27]]
  result$d30[1] <- tab$coef[[28]]
  result$d30[2] <- tab$se[[28]]
  result$d30[3] <- tab$N[[28]]
  result$d45[1] <- tab$coef[[29]]
  result$d45[2] <- tab$se[[29]]
  result$d45[3] <- tab$N[[29]]
  result$d60[1] <- tab$coef[[30]]
  result$d60[2] <- tab$se[[30]]
  result$d60[3] <- tab$N[[30]]
  
  
  #15-14
  result$d1[4] <- tab$coef[[21]]
  result$d1[5] <- tab$se[[21]]
  result$d1[6] <- tab$N[[21]]
  result$d7[4] <- tab$coef[[22]]
  result$d7[5] <- tab$se[[22]]
  result$d7[6] <- tab$N[[22]]
  result$d30[4] <- tab$coef[[23]]
  result$d30[5] <- tab$se[[23]]
  result$d30[6] <- tab$N[[23]]
  result$d45[4] <- tab$coef[[24]]
  result$d45[5] <- tab$se[[24]]
  result$d45[6] <- tab$N[[24]]
  result$d60[4] <- tab$coef[[25]]
  result$d60[5] <- tab$se[[25]]
  result$d60[6] <- tab$N[[25]]
  
  #16-15
  result$d1[7] <- tab$coef[[16]]
  result$d1[8] <- tab$se[[16]]
  result$d1[9] <- tab$N[[16]]
  result$d7[7] <- tab$coef[[17]]
  result$d7[8] <- tab$se[[17]]
  result$d7[9] <- tab$N[[17]]
  result$d30[7] <- tab$coef[[18]]
  result$d30[8] <- tab$se[[18]]
  result$d30[9] <- tab$N[[18]]
  result$d45[7] <- tab$coef[[19]]
  result$d45[8] <- tab$se[[19]]
  result$d45[9] <- tab$N[[19]]
  result$d60[7] <- tab$coef[[20]]
  result$d60[8] <- tab$se[[20]]
  result$d60[9] <- tab$N[[20]]
  
  #17-16
  result$d1[10] <- tab$coef[[11]]
  result$d1[11] <- tab$se[[11]]
  result$d1[12] <- tab$N[[11]]
  result$d7[10] <- tab$coef[[12]]
  result$d7[11] <- tab$se[[12]]
  result$d7[12] <- tab$N[[12]]
  result$d30[10] <- tab$coef[[13]]
  result$d30[11] <- tab$se[[13]]
  result$d30[12] <- tab$N[[13]]
  result$d45[10] <- tab$coef[[14]]
  result$d45[11] <- tab$se[[14]]
  result$d45[12] <- tab$N[[14]]
  result$d60[10] <- tab$coef[[15]]
  result$d60[11] <- tab$se[[15]]
  result$d60[12] <- tab$N[[15]]
  
  #18-17
  result$d1[13] <- tab$coef[[6]]
  result$d1[14] <- tab$se[[6]]
  result$d1[15] <- tab$N[[6]]
  result$d7[13] <- tab$coef[[7]]
  result$d7[14] <- tab$se[[7]]
  result$d7[15] <- tab$N[[7]]
  result$d30[13] <- tab$coef[[8]]
  result$d30[14] <- tab$se[[8]]
  result$d30[15] <- tab$N[[8]]
  result$d45[13] <- tab$coef[[9]]
  result$d45[14] <- tab$se[[9]]
  result$d45[15] <- tab$N[[9]]
  result$d60[13] <- tab$coef[[10]]
  result$d60[14] <- tab$se[[10]]
  result$d60[15] <- tab$N[[10]]
  
  # 19-18
  result$d1[16] <- tab$coef[[1]]
  result$d1[17] <- tab$se[[1]]
  result$d1[18] <- tab$N[[1]]
  result$d7[16] <- tab$coef[[2]]
  result$d7[17] <- tab$se[[2]]
  result$d7[18] <- tab$N[[2]]
  result$d30[16] <- tab$coef[[3]]
  result$d30[17] <- tab$se[[3]]
  result$d30[18] <- tab$N[[3]]
  result$d45[16] <- tab$coef[[4]]
  result$d45[17] <- tab$se[[4]]
  result$d45[18] <- tab$N[[4]]
  result$d60[16] <- tab$coef[[5]]
  result$d60[17] <- tab$se[[5]]
  result$d60[18] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Anos_dias_res.tex")
  rm(list, result, tab, latex_table)
  
  
  
  ###5.3.2 Tra ----
  list <- list()
  
  for (ano in 2019:2014) {
    
    ano_ant <- ano - 1
    ano_ant <- as.character(ano_ant)
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, "e ", ano_ant," para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano,".rds")) %>%
        bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(
          dup1 = 1,
          dup2 = sum(dup1),
          
          
          v1_tra = ifelse(year == ano_ant, tra_pop, NA),
          v2_tra = max(v1_tra, na.rm = T),
          d.tra = tra_pop - v2_tra
          
          
        ) %>%
        ungroup() %>% 
        filter(dup2 == 2) %>% 
        select(-c(dup2, dup1,
                  v1_tra, v2_tra)) %>% 
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano_ant])
      ef <- ef %>% select(-1,-2)
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.tra[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019-2018|1"]]$bws[1]
          bw_bias1  <- list[["2019-2018|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019-2018|7"]]$bws[1]
          bw_bias7  <- list[["2019-2018|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019-2018|30"]]$bws[1]
          bw_bias30  <- list[["2019-2018|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019-2018|45"]]$bws[1]
          bw_bias45  <- list[["2019-2018|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019-2018|60"]]$bws[1]
          bw_bias60  <- list[["2019-2018|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.tra[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        
      }
      
      message("Finalizado para", ano,"e ", ano_ant,"no dia", d)
      
      
    } 
    
    rm(d,ano, ano_ant)
  }
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  
  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2014 - 2013",
             " "," ",
             "2015 - 2014",
             " "," ",
             "2016 - 2015",
             " "," ",
             "2017 - 2016",
             " "," ",
             "2018 - 2017",
             " "," ",
             "2019 - 2018",
             " "," ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  #14-13
  result$d1[1] <- tab$coef[[26]]
  result$d1[2] <- tab$se[[26]]
  result$d1[3] <- tab$N[[26]]
  result$d7[1] <- tab$coef[[27]]
  result$d7[2] <- tab$se[[27]]
  result$d7[3] <- tab$N[[27]]
  result$d30[1] <- tab$coef[[28]]
  result$d30[2] <- tab$se[[28]]
  result$d30[3] <- tab$N[[28]]
  result$d45[1] <- tab$coef[[29]]
  result$d45[2] <- tab$se[[29]]
  result$d45[3] <- tab$N[[29]]
  result$d60[1] <- tab$coef[[30]]
  result$d60[2] <- tab$se[[30]]
  result$d60[3] <- tab$N[[30]]
  
  
  #15-14
  result$d1[4] <- tab$coef[[21]]
  result$d1[5] <- tab$se[[21]]
  result$d1[6] <- tab$N[[21]]
  result$d7[4] <- tab$coef[[22]]
  result$d7[5] <- tab$se[[22]]
  result$d7[6] <- tab$N[[22]]
  result$d30[4] <- tab$coef[[23]]
  result$d30[5] <- tab$se[[23]]
  result$d30[6] <- tab$N[[23]]
  result$d45[4] <- tab$coef[[24]]
  result$d45[5] <- tab$se[[24]]
  result$d45[6] <- tab$N[[24]]
  result$d60[4] <- tab$coef[[25]]
  result$d60[5] <- tab$se[[25]]
  result$d60[6] <- tab$N[[25]]
  
  #16-15
  result$d1[7] <- tab$coef[[16]]
  result$d1[8] <- tab$se[[16]]
  result$d1[9] <- tab$N[[16]]
  result$d7[7] <- tab$coef[[17]]
  result$d7[8] <- tab$se[[17]]
  result$d7[9] <- tab$N[[17]]
  result$d30[7] <- tab$coef[[18]]
  result$d30[8] <- tab$se[[18]]
  result$d30[9] <- tab$N[[18]]
  result$d45[7] <- tab$coef[[19]]
  result$d45[8] <- tab$se[[19]]
  result$d45[9] <- tab$N[[19]]
  result$d60[7] <- tab$coef[[20]]
  result$d60[8] <- tab$se[[20]]
  result$d60[9] <- tab$N[[20]]
  
  #17-16
  result$d1[10] <- tab$coef[[11]]
  result$d1[11] <- tab$se[[11]]
  result$d1[12] <- tab$N[[11]]
  result$d7[10] <- tab$coef[[12]]
  result$d7[11] <- tab$se[[12]]
  result$d7[12] <- tab$N[[12]]
  result$d30[10] <- tab$coef[[13]]
  result$d30[11] <- tab$se[[13]]
  result$d30[12] <- tab$N[[13]]
  result$d45[10] <- tab$coef[[14]]
  result$d45[11] <- tab$se[[14]]
  result$d45[12] <- tab$N[[14]]
  result$d60[10] <- tab$coef[[15]]
  result$d60[11] <- tab$se[[15]]
  result$d60[12] <- tab$N[[15]]
  
  #18-17
  result$d1[13] <- tab$coef[[6]]
  result$d1[14] <- tab$se[[6]]
  result$d1[15] <- tab$N[[6]]
  result$d7[13] <- tab$coef[[7]]
  result$d7[14] <- tab$se[[7]]
  result$d7[15] <- tab$N[[7]]
  result$d30[13] <- tab$coef[[8]]
  result$d30[14] <- tab$se[[8]]
  result$d30[15] <- tab$N[[8]]
  result$d45[13] <- tab$coef[[9]]
  result$d45[14] <- tab$se[[9]]
  result$d45[15] <- tab$N[[9]]
  result$d60[13] <- tab$coef[[10]]
  result$d60[14] <- tab$se[[10]]
  result$d60[15] <- tab$N[[10]]
  
  # 19-18
  result$d1[16] <- tab$coef[[1]]
  result$d1[17] <- tab$se[[1]]
  result$d1[18] <- tab$N[[1]]
  result$d7[16] <- tab$coef[[2]]
  result$d7[17] <- tab$se[[2]]
  result$d7[18] <- tab$N[[2]]
  result$d30[16] <- tab$coef[[3]]
  result$d30[17] <- tab$se[[3]]
  result$d30[18] <- tab$N[[3]]
  result$d45[16] <- tab$coef[[4]]
  result$d45[17] <- tab$se[[4]]
  result$d45[18] <- tab$N[[4]]
  result$d60[16] <- tab$coef[[5]]
  result$d60[17] <- tab$se[[5]]
  result$d60[18] <- tab$N[[5]]
  
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Anos_dias_Tra_res.tex")
  rm(list, result, tab, latex_table)
  
  
  
  ###5.3.3 Crt ----
  list <- list()
  
  for (ano in 2019:2014) {
    
    ano_ant <- ano - 1
    ano_ant <- as.character(ano_ant)
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, "e ", ano_ant," para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano,".rds")) %>%
        bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano_ant,".rds"))) 
      
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(
          dup1 = 1,
          dup2 = sum(dup1),
          
          
          v1_crt = ifelse(year == ano_ant, crt_pop, NA),
          v2_crt = max(v1_crt, na.rm = T),
          d.crt = crt_pop - v2_crt
          
          
        ) %>%
        ungroup() %>% 
        filter(dup2 == 2) %>% 
        select(-c(dup2, dup1,
                  v1_crt, v2_crt)) %>% 
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano_ant])
      ef <- ef %>% select(-1,-2)
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.crt[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019-2018|1"]]$bws[1]
          bw_bias1  <- list[["2019-2018|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019-2018|7"]]$bws[1]
          bw_bias7  <- list[["2019-2018|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019-2018|30"]]$bws[1]
          bw_bias30  <- list[["2019-2018|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019-2018|45"]]$bws[1]
          bw_bias45  <- list[["2019-2018|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019-2018|60"]]$bws[1]
          bw_bias60  <- list[["2019-2018|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.crt[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        
      }
      
      message("Finalizado para", ano,"e ", ano_ant,"no dia", d)
      
      
    } 
    
    rm(d,ano, ano_ant)
  }
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2014 - 2013",
             " "," ",
             "2015 - 2014",
             " "," ",
             "2016 - 2015",
             " "," ",
             "2017 - 2016",
             " "," ",
             "2018 - 2017",
             " "," ",
             "2019 - 2018",
             " "," ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #14-13
  result$d1[1] <- tab$coef[[26]]
  result$d1[2] <- tab$se[[26]]
  result$d1[3] <- tab$N[[26]]
  result$d7[1] <- tab$coef[[27]]
  result$d7[2] <- tab$se[[27]]
  result$d7[3] <- tab$N[[27]]
  result$d30[1] <- tab$coef[[28]]
  result$d30[2] <- tab$se[[28]]
  result$d30[3] <- tab$N[[28]]
  result$d45[1] <- tab$coef[[29]]
  result$d45[2] <- tab$se[[29]]
  result$d45[3] <- tab$N[[29]]
  result$d60[1] <- tab$coef[[30]]
  result$d60[2] <- tab$se[[30]]
  result$d60[3] <- tab$N[[30]]
  
  
  #15-14
  result$d1[4] <- tab$coef[[21]]
  result$d1[5] <- tab$se[[21]]
  result$d1[6] <- tab$N[[21]]
  result$d7[4] <- tab$coef[[22]]
  result$d7[5] <- tab$se[[22]]
  result$d7[6] <- tab$N[[22]]
  result$d30[4] <- tab$coef[[23]]
  result$d30[5] <- tab$se[[23]]
  result$d30[6] <- tab$N[[23]]
  result$d45[4] <- tab$coef[[24]]
  result$d45[5] <- tab$se[[24]]
  result$d45[6] <- tab$N[[24]]
  result$d60[4] <- tab$coef[[25]]
  result$d60[5] <- tab$se[[25]]
  result$d60[6] <- tab$N[[25]]
  
  #16-15
  result$d1[7] <- tab$coef[[16]]
  result$d1[8] <- tab$se[[16]]
  result$d1[9] <- tab$N[[16]]
  result$d7[7] <- tab$coef[[17]]
  result$d7[8] <- tab$se[[17]]
  result$d7[9] <- tab$N[[17]]
  result$d30[7] <- tab$coef[[18]]
  result$d30[8] <- tab$se[[18]]
  result$d30[9] <- tab$N[[18]]
  result$d45[7] <- tab$coef[[19]]
  result$d45[8] <- tab$se[[19]]
  result$d45[9] <- tab$N[[19]]
  result$d60[7] <- tab$coef[[20]]
  result$d60[8] <- tab$se[[20]]
  result$d60[9] <- tab$N[[20]]
  
  #17-16
  result$d1[10] <- tab$coef[[11]]
  result$d1[11] <- tab$se[[11]]
  result$d1[12] <- tab$N[[11]]
  result$d7[10] <- tab$coef[[12]]
  result$d7[11] <- tab$se[[12]]
  result$d7[12] <- tab$N[[12]]
  result$d30[10] <- tab$coef[[13]]
  result$d30[11] <- tab$se[[13]]
  result$d30[12] <- tab$N[[13]]
  result$d45[10] <- tab$coef[[14]]
  result$d45[11] <- tab$se[[14]]
  result$d45[12] <- tab$N[[14]]
  result$d60[10] <- tab$coef[[15]]
  result$d60[11] <- tab$se[[15]]
  result$d60[12] <- tab$N[[15]]
  
  #18-17
  result$d1[13] <- tab$coef[[6]]
  result$d1[14] <- tab$se[[6]]
  result$d1[15] <- tab$N[[6]]
  result$d7[13] <- tab$coef[[7]]
  result$d7[14] <- tab$se[[7]]
  result$d7[15] <- tab$N[[7]]
  result$d30[13] <- tab$coef[[8]]
  result$d30[14] <- tab$se[[8]]
  result$d30[15] <- tab$N[[8]]
  result$d45[13] <- tab$coef[[9]]
  result$d45[14] <- tab$se[[9]]
  result$d45[15] <- tab$N[[9]]
  result$d60[13] <- tab$coef[[10]]
  result$d60[14] <- tab$se[[10]]
  result$d60[15] <- tab$N[[10]]
  
  # 19-18
  result$d1[16] <- tab$coef[[1]]
  result$d1[17] <- tab$se[[1]]
  result$d1[18] <- tab$N[[1]]
  result$d7[16] <- tab$coef[[2]]
  result$d7[17] <- tab$se[[2]]
  result$d7[18] <- tab$N[[2]]
  result$d30[16] <- tab$coef[[3]]
  result$d30[17] <- tab$se[[3]]
  result$d30[18] <- tab$N[[3]]
  result$d45[16] <- tab$coef[[4]]
  result$d45[17] <- tab$se[[4]]
  result$d45[18] <- tab$N[[4]]
  result$d60[16] <- tab$coef[[5]]
  result$d60[17] <- tab$se[[5]]
  result$d60[18] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Anos_dias_Crt_res.tex")
  rm(list, result, tab, latex_table)
  
  
  
  ###5.3.4 Lxt ----
  list <- list()
  
  for (ano in 2019:2014) {
    
    ano_ant <- ano - 1
    ano_ant <- as.character(ano_ant)
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, "e ", ano_ant," para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano,".rds")) %>%
        bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano_ant,".rds"))) 
      
      
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(
          dup1 = 1,
          dup2 = sum(dup1),
          
          
          v1_lxt = ifelse(year == ano_ant, lxt_pop, NA),
          v2_lxt = max(v1_lxt, na.rm = T),
          d.lxt = lxt_pop - v2_lxt
          
          
        ) %>%
        ungroup() %>% 
        filter(dup2 == 2) %>% 
        select(-c(dup2, dup1,
                  v1_lxt, v2_lxt)) %>% 
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano_ant])
      ef <- ef %>% select(-1,-2)
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.lxt[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019-2018|1"]]$bws[1]
          bw_bias1  <- list[["2019-2018|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019-2018|7"]]$bws[1]
          bw_bias7  <- list[["2019-2018|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019-2018|30"]]$bws[1]
          bw_bias30  <- list[["2019-2018|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019-2018|45"]]$bws[1]
          bw_bias45  <- list[["2019-2018|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019-2018|60"]]$bws[1]
          bw_bias60  <- list[["2019-2018|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"-",ano_ant,"|",d))]] <-
          rdrobust(
            y = base$d.lxt[base$year == ano],
            x = base$dist_hv_border[base$year == ano_ant],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano_ant],
            weights = base$pop[base$year == ano_ant],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano_ant], 
              base$lon[base$year == ano_ant]
            )
          )
        
        
      }
      
      message("Finalizado para", ano,"e ", ano_ant,"no dia", d)
      
      
    } 
    
    rm(d,ano, ano_ant)
  }
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60) 
  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2014 - 2013",
             " "," ",
             "2015 - 2014",
             " "," ",
             "2016 - 2015",
             " "," ",
             "2017 - 2016",
             " "," ",
             "2018 - 2017",
             " "," ",
             "2019 - 2018",
             " "," ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #14-13
  result$d1[1] <- tab$coef[[26]]
  result$d1[2] <- tab$se[[26]]
  result$d1[3] <- tab$N[[26]]
  result$d7[1] <- tab$coef[[27]]
  result$d7[2] <- tab$se[[27]]
  result$d7[3] <- tab$N[[27]]
  result$d30[1] <- tab$coef[[28]]
  result$d30[2] <- tab$se[[28]]
  result$d30[3] <- tab$N[[28]]
  result$d45[1] <- tab$coef[[29]]
  result$d45[2] <- tab$se[[29]]
  result$d45[3] <- tab$N[[29]]
  result$d60[1] <- tab$coef[[30]]
  result$d60[2] <- tab$se[[30]]
  result$d60[3] <- tab$N[[30]]
  
  
  #15-14
  result$d1[4] <- tab$coef[[21]]
  result$d1[5] <- tab$se[[21]]
  result$d1[6] <- tab$N[[21]]
  result$d7[4] <- tab$coef[[22]]
  result$d7[5] <- tab$se[[22]]
  result$d7[6] <- tab$N[[22]]
  result$d30[4] <- tab$coef[[23]]
  result$d30[5] <- tab$se[[23]]
  result$d30[6] <- tab$N[[23]]
  result$d45[4] <- tab$coef[[24]]
  result$d45[5] <- tab$se[[24]]
  result$d45[6] <- tab$N[[24]]
  result$d60[4] <- tab$coef[[25]]
  result$d60[5] <- tab$se[[25]]
  result$d60[6] <- tab$N[[25]]
  
  #16-15
  result$d1[7] <- tab$coef[[16]]
  result$d1[8] <- tab$se[[16]]
  result$d1[9] <- tab$N[[16]]
  result$d7[7] <- tab$coef[[17]]
  result$d7[8] <- tab$se[[17]]
  result$d7[9] <- tab$N[[17]]
  result$d30[7] <- tab$coef[[18]]
  result$d30[8] <- tab$se[[18]]
  result$d30[9] <- tab$N[[18]]
  result$d45[7] <- tab$coef[[19]]
  result$d45[8] <- tab$se[[19]]
  result$d45[9] <- tab$N[[19]]
  result$d60[7] <- tab$coef[[20]]
  result$d60[8] <- tab$se[[20]]
  result$d60[9] <- tab$N[[20]]
  
  #17-16
  result$d1[10] <- tab$coef[[11]]
  result$d1[11] <- tab$se[[11]]
  result$d1[12] <- tab$N[[11]]
  result$d7[10] <- tab$coef[[12]]
  result$d7[11] <- tab$se[[12]]
  result$d7[12] <- tab$N[[12]]
  result$d30[10] <- tab$coef[[13]]
  result$d30[11] <- tab$se[[13]]
  result$d30[12] <- tab$N[[13]]
  result$d45[10] <- tab$coef[[14]]
  result$d45[11] <- tab$se[[14]]
  result$d45[12] <- tab$N[[14]]
  result$d60[10] <- tab$coef[[15]]
  result$d60[11] <- tab$se[[15]]
  result$d60[12] <- tab$N[[15]]
  
  #18-17
  result$d1[13] <- tab$coef[[6]]
  result$d1[14] <- tab$se[[6]]
  result$d1[15] <- tab$N[[6]]
  result$d7[13] <- tab$coef[[7]]
  result$d7[14] <- tab$se[[7]]
  result$d7[15] <- tab$N[[7]]
  result$d30[13] <- tab$coef[[8]]
  result$d30[14] <- tab$se[[8]]
  result$d30[15] <- tab$N[[8]]
  result$d45[13] <- tab$coef[[9]]
  result$d45[14] <- tab$se[[9]]
  result$d45[15] <- tab$N[[9]]
  result$d60[13] <- tab$coef[[10]]
  result$d60[14] <- tab$se[[10]]
  result$d60[15] <- tab$N[[10]]
  
  # 19-18
  result$d1[16] <- tab$coef[[1]]
  result$d1[17] <- tab$se[[1]]
  result$d1[18] <- tab$N[[1]]
  result$d7[16] <- tab$coef[[2]]
  result$d7[17] <- tab$se[[2]]
  result$d7[18] <- tab$N[[2]]
  result$d30[16] <- tab$coef[[3]]
  result$d30[17] <- tab$se[[3]]
  result$d30[18] <- tab$N[[3]]
  result$d45[16] <- tab$coef[[4]]
  result$d45[17] <- tab$se[[4]]
  result$d45[18] <- tab$N[[4]]
  result$d60[16] <- tab$coef[[5]]
  result$d60[17] <- tab$se[[5]]
  result$d60[18] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Anos_dias_Lxt_res.tex")
  rm(list, result, tab, latex_table)

  
  
  #5.4 Nível ----
  
  ###5.4.1 Hom ----
  
  for (ano in 2019:2013) {
    
    if (ano == 2019) {
      
      list <- list()
      
    }
    
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, " para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano,".rds")) #%>%
      #bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        ungroup() %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano])
      ef <- ef %>% select(-1,-2)
      
      # 
      # list[[as.character(paste0(ano,"|",d))]] <-
      #   rdrobust(
      #     y = base$hom_pop[base$year == ano],
      #     x = base$dist_hv_border[base$year == ano],
      #     c = 0,
      #     cluster = base$seg[base$year == ano],
      #     weights = base$pop[base$year == ano],
      #     vce = "hc0",
      #     covs = cbind(
      #       ef, 
      #       base$lat[base$year == ano], 
      #       base$lon[base$year == ano]
      #     )
      #   )
      # 
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$hom_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019|1"]]$bws[1]
          bw_bias1  <- list[["2019|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019|7"]]$bws[1]
          bw_bias7  <- list[["2019|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019|30"]]$bws[1]
          bw_bias30  <- list[["2019|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019|45"]]$bws[1]
          bw_bias45  <- list[["2019|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019|60"]]$bws[1]
          bw_bias60  <- list[["2019|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$hom_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        
      }
      
      
      message("Finalizado para", ano,"no dia", d)
      
      
    } 
    
    rm(d,ano)
  }
  
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2013",
             " "," ",
             "2014",
             " "," ",
             "2015",
             " "," ",
             "2016",
             " "," ",
             "2017",
             " "," ",
             "2018",
             " "," ",
             "2019",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #13
  result$d1[1] <- tab$coef[[31]]
  result$d1[2] <- tab$se[[31]]
  result$d1[3] <- tab$N[[31]]
  result$d7[1] <- tab$coef[[32]]
  result$d7[2] <- tab$se[[32]]
  result$d7[3] <- tab$N[[32]]
  result$d30[1] <- tab$coef[[33]]
  result$d30[2] <- tab$se[[33]]
  result$d30[3] <- tab$N[[33]]
  result$d45[1] <- tab$coef[[34]]
  result$d45[2] <- tab$se[[34]]
  result$d45[3] <- tab$N[[34]]
  result$d60[1] <- tab$coef[[35]]
  result$d60[2] <- tab$se[[35]]
  result$d60[3] <- tab$N[[35]]
  
  #14
  result$d1[4] <- tab$coef[[26]]
  result$d1[5] <- tab$se[[26]]
  result$d1[6] <- tab$N[[26]]
  result$d7[4] <- tab$coef[[27]]
  result$d7[5] <- tab$se[[27]]
  result$d7[6] <- tab$N[[27]]
  result$d30[4] <- tab$coef[[28]]
  result$d30[5] <- tab$se[[28]]
  result$d30[6] <- tab$N[[28]]
  result$d45[4] <- tab$coef[[29]]
  result$d45[5] <- tab$se[[29]]
  result$d45[6] <- tab$N[[29]]
  result$d60[4] <- tab$coef[[30]]
  result$d60[5] <- tab$se[[30]]
  result$d60[6] <- tab$N[[30]]
  
  #15
  result$d1[7] <- tab$coef[[21]]
  result$d1[8] <- tab$se[[21]]
  result$d1[9] <- tab$N[[21]]
  result$d7[7] <- tab$coef[[22]]
  result$d7[8] <- tab$se[[22]]
  result$d7[9] <- tab$N[[22]]
  result$d30[7] <- tab$coef[[23]]
  result$d30[8] <- tab$se[[23]]
  result$d30[9] <- tab$N[[23]]
  result$d45[7] <- tab$coef[[24]]
  result$d45[8] <- tab$se[[24]]
  result$d45[9] <- tab$N[[24]]
  result$d60[7] <- tab$coef[[25]]
  result$d60[8] <- tab$se[[25]]
  result$d60[9] <- tab$N[[25]]
  
  #16
  result$d1[10] <- tab$coef[[16]]
  result$d1[11] <- tab$se[[16]]
  result$d1[12] <- tab$N[[16]]
  result$d7[10] <- tab$coef[[17]]
  result$d7[11] <- tab$se[[17]]
  result$d7[12] <- tab$N[[17]]
  result$d30[10] <- tab$coef[[18]]
  result$d30[11] <- tab$se[[18]]
  result$d30[12] <- tab$N[[18]]
  result$d45[10] <- tab$coef[[19]]
  result$d45[11] <- tab$se[[19]]
  result$d45[12] <- tab$N[[19]]
  result$d60[10] <- tab$coef[[20]]
  result$d60[11] <- tab$se[[20]]
  result$d60[12] <- tab$N[[20]]
  
  #17
  result$d1[13] <- tab$coef[[11]]
  result$d1[14] <- tab$se[[11]]
  result$d1[15] <- tab$N[[11]]
  result$d7[13] <- tab$coef[[12]]
  result$d7[14] <- tab$se[[12]]
  result$d7[15] <- tab$N[[12]]
  result$d30[13] <- tab$coef[[13]]
  result$d30[14] <- tab$se[[13]]
  result$d30[15] <- tab$N[[13]]
  result$d45[13] <- tab$coef[[14]]
  result$d45[14] <- tab$se[[14]]
  result$d45[15] <- tab$N[[14]]
  result$d60[13] <- tab$coef[[15]]
  result$d60[14] <- tab$se[[15]]
  result$d60[15] <- tab$N[[15]]
  
  #18
  result$d1[16] <- tab$coef[[6]]
  result$d1[17] <- tab$se[[6]]
  result$d1[18] <- tab$N[[6]]
  result$d7[16] <- tab$coef[[7]]
  result$d7[17] <- tab$se[[7]]
  result$d7[18] <- tab$N[[7]]
  result$d30[16] <- tab$coef[[8]]
  result$d30[17] <- tab$se[[8]]
  result$d30[18] <- tab$N[[8]]
  result$d45[16] <- tab$coef[[9]]
  result$d45[17] <- tab$se[[9]]
  result$d45[18] <- tab$N[[9]]
  result$d60[16] <- tab$coef[[10]]
  result$d60[17] <- tab$se[[10]]
  result$d60[18] <- tab$N[[10]]
  
  # 19
  result$d1[19] <- tab$coef[[1]]
  result$d1[20] <- tab$se[[1]]
  result$d1[21] <- tab$N[[1]]
  result$d7[19] <- tab$coef[[2]]
  result$d7[20] <- tab$se[[2]]
  result$d7[21] <- tab$N[[2]]
  result$d30[19] <- tab$coef[[3]]
  result$d30[20] <- tab$se[[3]]
  result$d30[21] <- tab$N[[3]]
  result$d45[19] <- tab$coef[[4]]
  result$d45[20] <- tab$se[[4]]
  result$d45[21] <- tab$N[[4]]
  result$d60[19] <- tab$coef[[5]]
  result$d60[20] <- tab$se[[5]]
  result$d60[21] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Nivel_anos_dias_res.tex")
  rm(list, result, tab, latex_table)
  
  
  
  ###5.4.2 TRA ----
  
  for (ano in 2019:2013) {
    
    if (ano == 2019) {
      
      list <- list()
      
    }
    
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, " para ", d, "dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano,".rds")) #%>%
      #bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        ungroup() %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano])
      ef <- ef %>% select(-1,-2)
      
      # 
      # list[[as.character(paste0(ano,"|",d))]] <-
      #   rdrobust(
      #     y = base$hom_pop[base$year == ano],
      #     x = base$dist_hv_border[base$year == ano],
      #     c = 0,
      #     cluster = base$seg[base$year == ano],
      #     weights = base$pop[base$year == ano],
      #     vce = "hc0",
      #     covs = cbind(
      #       ef, 
      #       base$lat[base$year == ano], 
      #       base$lon[base$year == ano]
      #     )
      #   )
      # 
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$tra_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019|1"]]$bws[1]
          bw_bias1  <- list[["2019|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019|7"]]$bws[1]
          bw_bias7  <- list[["2019|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019|30"]]$bws[1]
          bw_bias30  <- list[["2019|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019|45"]]$bws[1]
          bw_bias45  <- list[["2019|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019|60"]]$bws[1]
          bw_bias60  <- list[["2019|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$tra_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        
      }
      
      
      message("Finalizado para ", ano," no dia ", d)
      
      
    } 
    
    rm(d,ano)
  }
  
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2013",
             " "," ",
             "2014",
             " "," ",
             "2015",
             " "," ",
             "2016",
             " "," ",
             "2017",
             " "," ",
             "2018",
             " "," ",
             "2019",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #13
  result$d1[1] <- tab$coef[[31]]
  result$d1[2] <- tab$se[[31]]
  result$d1[3] <- tab$N[[31]]
  result$d7[1] <- tab$coef[[32]]
  result$d7[2] <- tab$se[[32]]
  result$d7[3] <- tab$N[[32]]
  result$d30[1] <- tab$coef[[33]]
  result$d30[2] <- tab$se[[33]]
  result$d30[3] <- tab$N[[33]]
  result$d45[1] <- tab$coef[[34]]
  result$d45[2] <- tab$se[[34]]
  result$d45[3] <- tab$N[[34]]
  result$d60[1] <- tab$coef[[35]]
  result$d60[2] <- tab$se[[35]]
  result$d60[3] <- tab$N[[35]]
  
  #14
  result$d1[4] <- tab$coef[[26]]
  result$d1[5] <- tab$se[[26]]
  result$d1[6] <- tab$N[[26]]
  result$d7[4] <- tab$coef[[27]]
  result$d7[5] <- tab$se[[27]]
  result$d7[6] <- tab$N[[27]]
  result$d30[4] <- tab$coef[[28]]
  result$d30[5] <- tab$se[[28]]
  result$d30[6] <- tab$N[[28]]
  result$d45[4] <- tab$coef[[29]]
  result$d45[5] <- tab$se[[29]]
  result$d45[6] <- tab$N[[29]]
  result$d60[4] <- tab$coef[[30]]
  result$d60[5] <- tab$se[[30]]
  result$d60[6] <- tab$N[[30]]
  
  #15
  result$d1[7] <- tab$coef[[21]]
  result$d1[8] <- tab$se[[21]]
  result$d1[9] <- tab$N[[21]]
  result$d7[7] <- tab$coef[[22]]
  result$d7[8] <- tab$se[[22]]
  result$d7[9] <- tab$N[[22]]
  result$d30[7] <- tab$coef[[23]]
  result$d30[8] <- tab$se[[23]]
  result$d30[9] <- tab$N[[23]]
  result$d45[7] <- tab$coef[[24]]
  result$d45[8] <- tab$se[[24]]
  result$d45[9] <- tab$N[[24]]
  result$d60[7] <- tab$coef[[25]]
  result$d60[8] <- tab$se[[25]]
  result$d60[9] <- tab$N[[25]]
  
  #16
  result$d1[10] <- tab$coef[[16]]
  result$d1[11] <- tab$se[[16]]
  result$d1[12] <- tab$N[[16]]
  result$d7[10] <- tab$coef[[17]]
  result$d7[11] <- tab$se[[17]]
  result$d7[12] <- tab$N[[17]]
  result$d30[10] <- tab$coef[[18]]
  result$d30[11] <- tab$se[[18]]
  result$d30[12] <- tab$N[[18]]
  result$d45[10] <- tab$coef[[19]]
  result$d45[11] <- tab$se[[19]]
  result$d45[12] <- tab$N[[19]]
  result$d60[10] <- tab$coef[[20]]
  result$d60[11] <- tab$se[[20]]
  result$d60[12] <- tab$N[[20]]
  
  #17
  result$d1[13] <- tab$coef[[11]]
  result$d1[14] <- tab$se[[11]]
  result$d1[15] <- tab$N[[11]]
  result$d7[13] <- tab$coef[[12]]
  result$d7[14] <- tab$se[[12]]
  result$d7[15] <- tab$N[[12]]
  result$d30[13] <- tab$coef[[13]]
  result$d30[14] <- tab$se[[13]]
  result$d30[15] <- tab$N[[13]]
  result$d45[13] <- tab$coef[[14]]
  result$d45[14] <- tab$se[[14]]
  result$d45[15] <- tab$N[[14]]
  result$d60[13] <- tab$coef[[15]]
  result$d60[14] <- tab$se[[15]]
  result$d60[15] <- tab$N[[15]]
  
  #18
  result$d1[16] <- tab$coef[[6]]
  result$d1[17] <- tab$se[[6]]
  result$d1[18] <- tab$N[[6]]
  result$d7[16] <- tab$coef[[7]]
  result$d7[17] <- tab$se[[7]]
  result$d7[18] <- tab$N[[7]]
  result$d30[16] <- tab$coef[[8]]
  result$d30[17] <- tab$se[[8]]
  result$d30[18] <- tab$N[[8]]
  result$d45[16] <- tab$coef[[9]]
  result$d45[17] <- tab$se[[9]]
  result$d45[18] <- tab$N[[9]]
  result$d60[16] <- tab$coef[[10]]
  result$d60[17] <- tab$se[[10]]
  result$d60[18] <- tab$N[[10]]
  
  # 19
  result$d1[19] <- tab$coef[[1]]
  result$d1[20] <- tab$se[[1]]
  result$d1[21] <- tab$N[[1]]
  result$d7[19] <- tab$coef[[2]]
  result$d7[20] <- tab$se[[2]]
  result$d7[21] <- tab$N[[2]]
  result$d30[19] <- tab$coef[[3]]
  result$d30[20] <- tab$se[[3]]
  result$d30[21] <- tab$N[[3]]
  result$d45[19] <- tab$coef[[4]]
  result$d45[20] <- tab$se[[4]]
  result$d45[21] <- tab$N[[4]]
  result$d60[19] <- tab$coef[[5]]
  result$d60[20] <- tab$se[[5]]
  result$d60[21] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Nivel_tra_anos_res.tex")
  rm(list, result, tab, latex_table)
  
  
  
  
  
  ###5.4.3 CRT ----
  
  for (ano in 2019:2013) {
    
    if (ano == 2019) {
      
      list <- list()
      
    }
    
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, " para ", d, " dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano,".rds")) #%>%
      #bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        ungroup() %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano])
      ef <- ef %>% select(-1,-2)
      
      # 
      # list[[as.character(paste0(ano,"|",d))]] <-
      #   rdrobust(
      #     y = base$hom_pop[base$year == ano],
      #     x = base$dist_hv_border[base$year == ano],
      #     c = 0,
      #     cluster = base$seg[base$year == ano],
      #     weights = base$pop[base$year == ano],
      #     vce = "hc0",
      #     covs = cbind(
      #       ef, 
      #       base$lat[base$year == ano], 
      #       base$lon[base$year == ano]
      #     )
      #   )
      # 
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$crt_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019|1"]]$bws[1]
          bw_bias1  <- list[["2019|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019|7"]]$bws[1]
          bw_bias7  <- list[["2019|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019|30"]]$bws[1]
          bw_bias30  <- list[["2019|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019|45"]]$bws[1]
          bw_bias45  <- list[["2019|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019|60"]]$bws[1]
          bw_bias60  <- list[["2019|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$crt_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        
      }
      
      
      message("Finalizado para ", ano," no dia ", d)
      
      
    } 
    
    rm(d,ano)
  }
  
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2013",
             " "," ",
             "2014",
             " "," ",
             "2015",
             " "," ",
             "2016",
             " "," ",
             "2017",
             " "," ",
             "2018",
             " "," ",
             "2019",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #13
  result$d1[1] <- tab$coef[[31]]
  result$d1[2] <- tab$se[[31]]
  result$d1[3] <- tab$N[[31]]
  result$d7[1] <- tab$coef[[32]]
  result$d7[2] <- tab$se[[32]]
  result$d7[3] <- tab$N[[32]]
  result$d30[1] <- tab$coef[[33]]
  result$d30[2] <- tab$se[[33]]
  result$d30[3] <- tab$N[[33]]
  result$d45[1] <- tab$coef[[34]]
  result$d45[2] <- tab$se[[34]]
  result$d45[3] <- tab$N[[34]]
  result$d60[1] <- tab$coef[[35]]
  result$d60[2] <- tab$se[[35]]
  result$d60[3] <- tab$N[[35]]
  
  #14
  result$d1[4] <- tab$coef[[26]]
  result$d1[5] <- tab$se[[26]]
  result$d1[6] <- tab$N[[26]]
  result$d7[4] <- tab$coef[[27]]
  result$d7[5] <- tab$se[[27]]
  result$d7[6] <- tab$N[[27]]
  result$d30[4] <- tab$coef[[28]]
  result$d30[5] <- tab$se[[28]]
  result$d30[6] <- tab$N[[28]]
  result$d45[4] <- tab$coef[[29]]
  result$d45[5] <- tab$se[[29]]
  result$d45[6] <- tab$N[[29]]
  result$d60[4] <- tab$coef[[30]]
  result$d60[5] <- tab$se[[30]]
  result$d60[6] <- tab$N[[30]]
  
  #15
  result$d1[7] <- tab$coef[[21]]
  result$d1[8] <- tab$se[[21]]
  result$d1[9] <- tab$N[[21]]
  result$d7[7] <- tab$coef[[22]]
  result$d7[8] <- tab$se[[22]]
  result$d7[9] <- tab$N[[22]]
  result$d30[7] <- tab$coef[[23]]
  result$d30[8] <- tab$se[[23]]
  result$d30[9] <- tab$N[[23]]
  result$d45[7] <- tab$coef[[24]]
  result$d45[8] <- tab$se[[24]]
  result$d45[9] <- tab$N[[24]]
  result$d60[7] <- tab$coef[[25]]
  result$d60[8] <- tab$se[[25]]
  result$d60[9] <- tab$N[[25]]
  
  #16
  result$d1[10] <- tab$coef[[16]]
  result$d1[11] <- tab$se[[16]]
  result$d1[12] <- tab$N[[16]]
  result$d7[10] <- tab$coef[[17]]
  result$d7[11] <- tab$se[[17]]
  result$d7[12] <- tab$N[[17]]
  result$d30[10] <- tab$coef[[18]]
  result$d30[11] <- tab$se[[18]]
  result$d30[12] <- tab$N[[18]]
  result$d45[10] <- tab$coef[[19]]
  result$d45[11] <- tab$se[[19]]
  result$d45[12] <- tab$N[[19]]
  result$d60[10] <- tab$coef[[20]]
  result$d60[11] <- tab$se[[20]]
  result$d60[12] <- tab$N[[20]]
  
  #17
  result$d1[13] <- tab$coef[[11]]
  result$d1[14] <- tab$se[[11]]
  result$d1[15] <- tab$N[[11]]
  result$d7[13] <- tab$coef[[12]]
  result$d7[14] <- tab$se[[12]]
  result$d7[15] <- tab$N[[12]]
  result$d30[13] <- tab$coef[[13]]
  result$d30[14] <- tab$se[[13]]
  result$d30[15] <- tab$N[[13]]
  result$d45[13] <- tab$coef[[14]]
  result$d45[14] <- tab$se[[14]]
  result$d45[15] <- tab$N[[14]]
  result$d60[13] <- tab$coef[[15]]
  result$d60[14] <- tab$se[[15]]
  result$d60[15] <- tab$N[[15]]
  
  #18
  result$d1[16] <- tab$coef[[6]]
  result$d1[17] <- tab$se[[6]]
  result$d1[18] <- tab$N[[6]]
  result$d7[16] <- tab$coef[[7]]
  result$d7[17] <- tab$se[[7]]
  result$d7[18] <- tab$N[[7]]
  result$d30[16] <- tab$coef[[8]]
  result$d30[17] <- tab$se[[8]]
  result$d30[18] <- tab$N[[8]]
  result$d45[16] <- tab$coef[[9]]
  result$d45[17] <- tab$se[[9]]
  result$d45[18] <- tab$N[[9]]
  result$d60[16] <- tab$coef[[10]]
  result$d60[17] <- tab$se[[10]]
  result$d60[18] <- tab$N[[10]]
  
  # 19
  result$d1[19] <- tab$coef[[1]]
  result$d1[20] <- tab$se[[1]]
  result$d1[21] <- tab$N[[1]]
  result$d7[19] <- tab$coef[[2]]
  result$d7[20] <- tab$se[[2]]
  result$d7[21] <- tab$N[[2]]
  result$d30[19] <- tab$coef[[3]]
  result$d30[20] <- tab$se[[3]]
  result$d30[21] <- tab$N[[3]]
  result$d45[19] <- tab$coef[[4]]
  result$d45[20] <- tab$se[[4]]
  result$d45[21] <- tab$N[[4]]
  result$d60[19] <- tab$coef[[5]]
  result$d60[20] <- tab$se[[5]]
  result$d60[21] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Nivel_crt_anos_res.tex")
  rm(list, result, tab, latex_table)
  
  
  
  
  
  ###5.4.5 LXT ----
  
  for (ano in 2019:2013) {
    
    if (ano == 2019) {
      
      list <- list()
      
    }
    
    ano <- as.character(ano)
    
    
    for ( d in c(1, 7, 30, 45, 60)) {
      
      message("Abrindo Bases: ", ano, " para ", d, " dias...")
      
      
      base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano,".rds")) #%>%
      #bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_",ano_ant,".rds"))) 
      
      base <- base %>% 
        arrange(co_municipio,year) %>%
        group_by(co_municipio) %>%
        mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
               dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
        ungroup() %>%
        setDT()
      
      
      ###Regressão -----
      
      ef <- dummy_cols(base$seg[base$year == ano])
      ef <- ef %>% select(-1,-2)
      
      # 
      # list[[as.character(paste0(ano,"|",d))]] <-
      #   rdrobust(
      #     y = base$hom_pop[base$year == ano],
      #     x = base$dist_hv_border[base$year == ano],
      #     c = 0,
      #     cluster = base$seg[base$year == ano],
      #     weights = base$pop[base$year == ano],
      #     vce = "hc0",
      #     covs = cbind(
      #       ef, 
      #       base$lat[base$year == ano], 
      #       base$lon[base$year == ano]
      #     )
      #   )
      # 
      
      
      if (ano == 2019) {
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$lxt_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        if (d == 1) {
          bw_main1  <- list[["2019|1"]]$bws[1]
          bw_bias1  <- list[["2019|1"]]$bws[2]
        } else if (d == 7) {
          bw_main7  <- list[["2019|7"]]$bws[1]
          bw_bias7  <- list[["2019|7"]]$bws[2]
        } else if (d == 30) {
          bw_main30  <- list[["2019|30"]]$bws[1]
          bw_bias30  <- list[["2019|30"]]$bws[2]
        } else if (d == 45) {
          bw_main45  <- list[["2019|45"]]$bws[1]
          bw_bias45  <- list[["2019|45"]]$bws[2]
        } else if (d == 60) {
          bw_main60  <- list[["2019|60"]]$bws[1]
          bw_bias60  <- list[["2019|60"]]$bws[2]
        }
      } else {
        
        
        
        
        list[[as.character(paste0(ano,"|",d))]] <-
          rdrobust(
            y = base$lxt_pop[base$year == ano],
            x = base$dist_hv_border[base$year == ano],
            c = 0,
            h = get(paste0("bw_main",d)),
            b = get(paste0("bw_bias",d)),
            cluster = base$seg[base$year == ano],
            weights = base$pop[base$year == ano],
            vce = "hc0",
            covs = cbind(
              ef, 
              base$lat[base$year == ano], 
              base$lon[base$year == ano]
            )
          )
        
        
      }
      
      
      message("Finalizado para ", ano," no dia ", d)
      
      
    } 
    
    rm(d,ano)
  }
  
  rm(bw_bias1, bw_bias7, bw_bias30, bw_bias45, bw_bias60,
     bw_main1, bw_main7, bw_main30, bw_main45, bw_main60)  
  ####Tabela ----
  
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
  
  
  
  #### RESULTADOS -----
  names <- c("2013",
             " "," ",
             "2014",
             " "," ",
             "2015",
             " "," ",
             "2016",
             " "," ",
             "2017",
             " "," ",
             "2018",
             " "," ",
             "2019",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  #13
  result$d1[1] <- tab$coef[[31]]
  result$d1[2] <- tab$se[[31]]
  result$d1[3] <- tab$N[[31]]
  result$d7[1] <- tab$coef[[32]]
  result$d7[2] <- tab$se[[32]]
  result$d7[3] <- tab$N[[32]]
  result$d30[1] <- tab$coef[[33]]
  result$d30[2] <- tab$se[[33]]
  result$d30[3] <- tab$N[[33]]
  result$d45[1] <- tab$coef[[34]]
  result$d45[2] <- tab$se[[34]]
  result$d45[3] <- tab$N[[34]]
  result$d60[1] <- tab$coef[[35]]
  result$d60[2] <- tab$se[[35]]
  result$d60[3] <- tab$N[[35]]
  
  #14
  result$d1[4] <- tab$coef[[26]]
  result$d1[5] <- tab$se[[26]]
  result$d1[6] <- tab$N[[26]]
  result$d7[4] <- tab$coef[[27]]
  result$d7[5] <- tab$se[[27]]
  result$d7[6] <- tab$N[[27]]
  result$d30[4] <- tab$coef[[28]]
  result$d30[5] <- tab$se[[28]]
  result$d30[6] <- tab$N[[28]]
  result$d45[4] <- tab$coef[[29]]
  result$d45[5] <- tab$se[[29]]
  result$d45[6] <- tab$N[[29]]
  result$d60[4] <- tab$coef[[30]]
  result$d60[5] <- tab$se[[30]]
  result$d60[6] <- tab$N[[30]]
  
  #15
  result$d1[7] <- tab$coef[[21]]
  result$d1[8] <- tab$se[[21]]
  result$d1[9] <- tab$N[[21]]
  result$d7[7] <- tab$coef[[22]]
  result$d7[8] <- tab$se[[22]]
  result$d7[9] <- tab$N[[22]]
  result$d30[7] <- tab$coef[[23]]
  result$d30[8] <- tab$se[[23]]
  result$d30[9] <- tab$N[[23]]
  result$d45[7] <- tab$coef[[24]]
  result$d45[8] <- tab$se[[24]]
  result$d45[9] <- tab$N[[24]]
  result$d60[7] <- tab$coef[[25]]
  result$d60[8] <- tab$se[[25]]
  result$d60[9] <- tab$N[[25]]
  
  #16
  result$d1[10] <- tab$coef[[16]]
  result$d1[11] <- tab$se[[16]]
  result$d1[12] <- tab$N[[16]]
  result$d7[10] <- tab$coef[[17]]
  result$d7[11] <- tab$se[[17]]
  result$d7[12] <- tab$N[[17]]
  result$d30[10] <- tab$coef[[18]]
  result$d30[11] <- tab$se[[18]]
  result$d30[12] <- tab$N[[18]]
  result$d45[10] <- tab$coef[[19]]
  result$d45[11] <- tab$se[[19]]
  result$d45[12] <- tab$N[[19]]
  result$d60[10] <- tab$coef[[20]]
  result$d60[11] <- tab$se[[20]]
  result$d60[12] <- tab$N[[20]]
  
  #17
  result$d1[13] <- tab$coef[[11]]
  result$d1[14] <- tab$se[[11]]
  result$d1[15] <- tab$N[[11]]
  result$d7[13] <- tab$coef[[12]]
  result$d7[14] <- tab$se[[12]]
  result$d7[15] <- tab$N[[12]]
  result$d30[13] <- tab$coef[[13]]
  result$d30[14] <- tab$se[[13]]
  result$d30[15] <- tab$N[[13]]
  result$d45[13] <- tab$coef[[14]]
  result$d45[14] <- tab$se[[14]]
  result$d45[15] <- tab$N[[14]]
  result$d60[13] <- tab$coef[[15]]
  result$d60[14] <- tab$se[[15]]
  result$d60[15] <- tab$N[[15]]
  
  #18
  result$d1[16] <- tab$coef[[6]]
  result$d1[17] <- tab$se[[6]]
  result$d1[18] <- tab$N[[6]]
  result$d7[16] <- tab$coef[[7]]
  result$d7[17] <- tab$se[[7]]
  result$d7[18] <- tab$N[[7]]
  result$d30[16] <- tab$coef[[8]]
  result$d30[17] <- tab$se[[8]]
  result$d30[18] <- tab$N[[8]]
  result$d45[16] <- tab$coef[[9]]
  result$d45[17] <- tab$se[[9]]
  result$d45[18] <- tab$N[[9]]
  result$d60[16] <- tab$coef[[10]]
  result$d60[17] <- tab$se[[10]]
  result$d60[18] <- tab$N[[10]]
  
  # 19
  result$d1[19] <- tab$coef[[1]]
  result$d1[20] <- tab$se[[1]]
  result$d1[21] <- tab$N[[1]]
  result$d7[19] <- tab$coef[[2]]
  result$d7[20] <- tab$se[[2]]
  result$d7[21] <- tab$N[[2]]
  result$d30[19] <- tab$coef[[3]]
  result$d30[20] <- tab$se[[3]]
  result$d30[21] <- tab$N[[3]]
  result$d45[19] <- tab$coef[[4]]
  result$d45[20] <- tab$se[[4]]
  result$d45[21] <- tab$N[[4]]
  result$d60[19] <- tab$coef[[5]]
  result$d60[20] <- tab$se[[5]]
  result$d60[21] <- tab$N[[5]]
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Nivel_lxt_anos_res.tex")
  rm(list, result, tab, latex_table)
  
  
  
  # ******************* ------
  # -------------------------------------------------------------------------- #
  #SIH ----
  # -------------------------------------------------------------------------- #
  
  ## ***OCO**** ----
  ## 1.1 Base ----
  
  list <- list()
  
  for ( d in c(1, 7, 30, 45, 60)) {
    
    
    base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIH/",d,"SIH_hv_oco_2018.rds")) %>%
      bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIH/",d,"SIH_hv_oco_2019.rds"))) 
    
    base <- base %>% 
      arrange(co_municipio,year) %>%
      group_by(co_municipio) %>%
      mutate(
        dup1 = 1,
        dup2 = sum(dup1),
        
        
        v1_nhm = ifelse(year == 2018, nhm_pop, NA),
        v2_nhm = max(v1_nhm, na.rm = T),
        d.media = nhm_pop - v2_nhm,
        
        
      ) %>%
      ungroup() %>% 
      filter(dup2 == 2) %>% 
      select(-c(dup2, dup1,
                v1_nhm, v2_nhm)) %>% 
      mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
             dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
      setDT()
    
    
    ###Regressão -----
    
    yv <- base %>%
      filter(year == 2019) %>% 
      select(d.media) %>% 
      rename(vd = 1)
    
    # Running variable
    xv <- base %>%
      filter(year == 2018) %>% 
      select(dist_hv_border)
    
    # Clusters
    clu <- base %>% 
      filter(year == 2018) %>% 
      select(seg)
    
    # Latitude
    latv <- base %>%
      filter(year == 2018) %>% 
      select(lat)
    
    # Longitude
    lonv <- base %>% 
      filter(year == 2018) %>% 
      select(lon)
    
    ef <- dummy_cols(clu$seg)
    ef <- ef %>% select(-1,-2)
    
    
    ## 1.1.1 Regression ----
    
    list[[as.character(paste0(2019,"-",2018,"|",d))]] <- rdrobust(
      y = yv$vd,
      x = xv$dist_hv_border,
      c = 0,
      weight = base$pop[base$year == 2018],
      cluster = clu$seg,
      vce = "hc0",
      covs = cbind(
        ef,
        lonv,
        latv
      )
    )
    
    
    rm(clu, latv, lonv, ef, xv, yv, d )
    
  }
  
  
  
  # ---------------------------------------------------------------------------- #
  #Extração da banda ótima
  
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
  
  
  
  ##1.2 RESULTADOS -----
  names <- c("2019 - 2018",
             " "," ",
             "Bandwidth")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  result$d1[1] <- tab$coef[[1]]
  result$d1[2] <- tab$se[[1]]
  result$d1[3] <- tab$N[[1]]
  result$d1[4] <- tab$bw[[1]]
  
  
  #Mun de Ocorrência
  result$d7[1] <- tab$coef[[2]]
  result$d7[2] <- tab$se[[2]]
  result$d7[3] <- tab$N[[2]]
  result$d7[4] <- tab$bw[[2]]
  
  
  result$d30[1] <- tab$coef[[3]]
  result$d30[2] <- tab$se[[3]]
  result$d30[3] <- tab$N[[3]]
  result$d30[4] <- tab$bw[[3]]
  
  #Mun de res
  result$d45[1] <- tab$coef[[4]]
  result$d45[2] <- tab$se[[4]]
  result$d45[3] <- tab$N[[4]]
  result$d45[4] <- tab$bw[[4]]
  
  result$d60[1] <- tab$coef[[5]]
  result$d60[2] <- tab$se[[5]]
  result$d60[3] <- tab$N[[5]]
  result$d60[4] <- tab$bw[[5]]
  
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lccccc",
    linesep = ""
  )
  
  
  bw_60main <- list[["2019-2018|60"]]$bws[1]  
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Principal_dias_oco_v2.tex")
  rm( list, result, tab, latex_table)
  
  
  
  
  
  
  
  
  ## ***RES*** ----
  
  
  
  
  
  
  list <- list()
  
  for ( d in c(1,7, 30, 45, 60)) {
    
    
    base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_2018.rds")) %>%
      bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/",d,"SIM_hv_res_2019.rds"))) 
    
    base <- base %>% 
      arrange(co_municipio,year) %>%
      group_by(co_municipio) %>%
      mutate(
        dup1 = 1,
        dup2 = sum(dup1),
        
        
        v1_nhm = ifelse(year == 2018, nhm_pop, NA),
        v2_nhm = max(v1_nhm, na.rm = T),
        d.media = nhm_pop - v2_nhm,
        
        
      ) %>%
      ungroup() %>% 
      filter(dup2 == 2) %>% 
      select(-c(dup2, dup1,
                v1_nhm, v2_nhm)) %>% 
      mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
             dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
      setDT()
    
    
    #2. Regressão -----
    
    yv <- base %>%
      filter(year == 2019) %>% 
      select(d.media) %>% 
      rename(vd = 1)
    
    # Running variable
    xv <- base %>%
      filter(year == 2018) %>% 
      select(dist_hv_border)
    
    # Clusters
    clu <- base %>% 
      filter(year == 2018) %>% 
      select(seg)
    
    # Latitude
    latv <- base %>%
      filter(year == 2018) %>% 
      select(lat)
    
    # Longitude
    lonv <- base %>% 
      filter(year == 2018) %>% 
      select(lon)
    
    ef <- dummy_cols(clu$seg)
    ef <- ef %>% select(-1,-2)
    
    
    ## 2.1 Regression ----
    
    list[[as.character(paste0(2019,"-",2018,"|",d))]] <- rdrobust(
      y = yv$vd,
      x = xv$dist_hv_border,
      c = 0,
      weight = base$pop[base$year == 2018],
      cluster = clu$seg,
      vce = "hc0",
      covs = cbind(
        ef,
        lonv,
        latv
      )
    )
    
    
    rm(clu, latv, lonv, ef, xv, yv, d )
    
  }
  
  
  
  # ---------------------------------------------------------------------------- #
  #Extração da banda ótima
  
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
  
  
  
  ##2.2 RESULTADOS -----
  names <- c("2019 - 2018",
             " "," ",
             "Bandwidth")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  
  result$d1[1] <- tab$coef[[1]]
  result$d1[2] <- tab$se[[1]]
  result$d1[3] <- tab$N[[1]]
  result$d1[4] <- tab$bw[[1]]
  
  
  #Mun de Ocorrência
  result$d7[1] <- tab$coef[[2]]
  result$d7[2] <- tab$se[[2]]
  result$d7[3] <- tab$N[[2]]
  result$d7[4] <- tab$bw[[2]]
  
  
  result$d30[1] <- tab$coef[[3]]
  result$d30[2] <- tab$se[[3]]
  result$d30[3] <- tab$N[[3]]
  result$d30[4] <- tab$bw[[3]]
  
  #Mun de res
  result$d45[1] <- tab$coef[[4]]
  result$d45[2] <- tab$se[[4]]
  result$d45[3] <- tab$N[[4]]
  result$d45[4] <- tab$bw[[4]]
  
  result$d60[1] <- tab$coef[[5]]
  result$d60[2] <- tab$se[[5]]
  result$d60[3] <- tab$N[[5]]
  result$d60[4] <- tab$bw[[5]]
  
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lcccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Principal_dias_res_v2.tex")
  rm( list, result, tab, latex_table)
  
  
  
  
  
  # ****************** ----  
  #Outras variáveis ----
  # ****************** ----
  
  # ***OCO*** ----
  # 3. Loop  -----
  
  list <- list()
  
  for (d in c(1,7,30,45,60)){
    
    base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIH/",d,"SIH_hv_oco_2018.rds")) %>%
      bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIH/",d,"SIH_hv_oco_2019.rds"))) 
    
    summary(base)
    
    base <- base %>% 
      arrange(co_municipio,year) %>%
      group_by(co_municipio) %>%
      mutate(
        dup1 = 1,
        dup2 = sum(dup1),
        
        v1_tra = ifelse(year == 2018, tra_pop, NA),
        v2_tra = max(v1_tra, na.rm = T),
        d.tra = tra_pop - v2_tra
      ) %>%
      mutate(
        
        v1_crd = ifelse(year == 2018, crd_pop, NA),
        v2_crd = max(v1_crd, na.rm = T),
        d.crd = crd_pop - v2_crd,
        
        v1_crt = ifelse(year == 2018, crt_pop, NA),
        v2_crt = max(v1_crt, na.rm = T),
        d.crt = crt_pop - v2_crt,
        
        v1_les = ifelse(year == 2018, les_pop, NA),
        v2_les = max(v1_les, na.rm = T),
        d.les = les_pop - v2_les,
        
        v1_ext = ifelse(year == 2018, ext_pop, NA),
        v2_ext = max(v1_ext, na.rm = T),
        d.ext = ext_pop - v2_ext,
        
        v1_lxt = ifelse(year == 2018, lxt_pop, NA),
        v2_lxt = max(v1_lxt, na.rm = T),
        d.lxt = lxt_pop - v2_lxt
        
      ) %>% 
      ungroup() %>% 
      filter(dup2 == 2) %>% 
      select(-c(dup2, dup1, v1_tra, v2_tra,
                v1_crd, v2_crd, v1_crt, v2_crt,
                v1_les, v2_les, v1_lxt, v2_lxt)) %>% 
      mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
             dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
      setDT()
    
    
    var_list <- c(
      #"d.tra", 
      "d.crd", "d.crt",
      #"d.ext",
      "d.lxt")
    
    
    for (var in var_list){
      
      ef <- dummy_cols(base$seg[base$year == 2018])
      ef <- ef %>% select(-1,-2)
      
      
      
      
      list[[as.character(paste0(var,"|",d))]] <-
        rdrobust(
          y = base[[var]][base$year == 2019],
          x = base$dist_hv_border[base$year == 2018],
          c = 0,
          cluster = base$seg[base$year == 2018],
          weights = base$pop[base$year == 2018],
          vce = "hc0",
          covs = cbind(
            ef, 
            base$lat[base$year == 2018], 
            base$lon[base$year == 2018]
          )
        )
      
      message("Finalizado para", var," ",d)
      
    }
    rm(ef,var)
    
    
    
  }
  
  
  
  ## 3.1 Tab ----
  
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
  
  
  names <- c(
             "Infarto",
             " ", " ",
             "Prob. Cir.",
             " ", " ",
             "Lesões",
             " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  
  #Dia 1
  #Inf
  result$d1[1] <- tab$coef[[1]]
  result$d1[2] <- tab$se[[1]]
  result$d1[3] <- tab$N[[1]]
  #PC
  result$d1[4] <- tab$coef[[2]]
  result$d1[5] <- tab$se[[2]]
  result$d1[6] <- tab$N[[2]]
  #L
  result$d1[7] <- tab$coef[[3]]
  result$d1[8] <- tab$se[[3]]
  result$d1[9] <- tab$N[[3]]
  
  
  #Dia 7

  #Inf
  result$d7[1] <- tab$coef[[4]]
  result$d7[2] <- tab$se[[4]]
  result$d7[3] <- tab$N[[4]]
  #PC
  result$d7[4] <- tab$coef[[5]]
  result$d7[5] <- tab$se[[5]]
  result$d7[6] <- tab$N[[5]]
  #Le
  result$d7[7] <- tab$coef[[6]]
  result$d7[8] <- tab$se[[6]]
  result$d7[9] <- tab$N[[6]]
  
  
  #Dia 30
  #Inf
  result$d30[1] <- tab$coef[[7]]
  result$d30[2] <- tab$se[[7]]
  result$d30[3] <- tab$N[[7]]
  #PC
  result$d30[4] <- tab$coef[[8]]
  result$d30[5] <- tab$se[[8]]
  result$d30[6] <- tab$N[[8]]
  #Le
  result$d30[7] <- tab$coef[[9]]
  result$d30[8] <- tab$se[[9]]
  result$d30[9] <- tab$N[[9]]
  
  
  
  #Dia 45
  #Inf
  result$d45[1] <- tab$coef[[10]]
  result$d45[2] <- tab$se[[10]]
  result$d45[3] <- tab$N[[10]]
  #PC
  result$d45[4] <- tab$coef[[11]]
  result$d45[5] <- tab$se[[11]]
  result$d45[6] <- tab$N[[11]]
  #L
  result$d45[7] <- tab$coef[[12]]
  result$d45[8] <- tab$se[[12]]
  result$d45[9] <- tab$N[[12]]
  
  #Dia 60
  #Inf
  result$d60[1] <- tab$coef[[13]]
  result$d60[2] <- tab$se[[13]]
  result$d60[3] <- tab$N[[13]]
  #Crt
  result$d60[4] <- tab$coef[[14]]
  result$d60[5] <- tab$se[[14]]
  result$d60[6] <- tab$N[[14]]
  #Le
  result$d60[7] <- tab$coef[[15]]
  result$d60[8] <- tab$se[[15]]
  result$d60[9] <- tab$N[[15]]

  
  
  
  
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lcccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/SIH_Vars_dias_oco.tex")
  rm( list, result, tab, latex_table)
  
  
  
  
  
  
  
  
  
  
  
  # ***RES*** ----
  # 3. Loop  -----
  
  list <- list()
  
  for (d in c(1,7,30,45,60)){
    
    base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIH/",d,"SIH_hv_res_2018.rds")) %>%
      bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIH/",d,"SIH_hv_res_2019.rds"))) 
    
    summary(base)
    
    base <- base %>% 
      arrange(co_municipio,year) %>%
      group_by(co_municipio) %>%
      mutate(
        dup1 = 1,
        dup2 = sum(dup1),
        
        v1_tra = ifelse(year == 2018, tra_pop, NA),
        v2_tra = max(v1_tra, na.rm = T),
        d.tra = tra_pop - v2_tra
      ) %>%
      mutate(
        
        v1_crd = ifelse(year == 2018, crd_pop, NA),
        v2_crd = max(v1_crd, na.rm = T),
        d.crd = crd_pop - v2_crd,
        
        v1_crt = ifelse(year == 2018, crt_pop, NA),
        v2_crt = max(v1_crt, na.rm = T),
        d.crt = crt_pop - v2_crt,
        
        v1_les = ifelse(year == 2018, les_pop, NA),
        v2_les = max(v1_les, na.rm = T),
        d.les = les_pop - v2_les,
        
        v1_ext = ifelse(year == 2018, ext_pop, NA),
        v2_ext = max(v1_ext, na.rm = T),
        d.ext = ext_pop - v2_ext,
        
        v1_lxt = ifelse(year == 2018, lxt_pop, NA),
        v2_lxt = max(v1_lxt, na.rm = T),
        d.lxt = lxt_pop - v2_lxt
        
      ) %>% 
      ungroup() %>% 
      filter(dup2 == 2) %>% 
      select(-c(dup2, dup1, v1_tra, v2_tra,
                v1_crd, v2_crd, v1_crt, v2_crt,
                v1_les, v2_les, v1_lxt, v2_lxt)) %>% 
      mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
             dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
      setDT()
    
    
    var_list <- c(
      #"d.tra", 
      "d.crd", "d.crt",
      #"d.ext",
      "d.lxt")
    
    
    for (var in var_list){
      
      ef <- dummy_cols(base$seg[base$year == 2018])
      ef <- ef %>% select(-1,-2)
      
      
      
      
      list[[as.character(paste0(var,"|",d))]] <-
        rdrobust(
          y = base[[var]][base$year == 2019],
          x = base$dist_hv_border[base$year == 2018],
          c = 0,
          cluster = base$seg[base$year == 2018],
          weights = base$pop[base$year == 2018],
          vce = "hc0",
          covs = cbind(
            ef, 
            base$lat[base$year == 2018], 
            base$lon[base$year == 2018]
          )
        )
      
      message("Finalizado para", var," ",d)
      
    }
    rm(ef,var)
    
    
    
  }
  
  
  
  ## 3.1 Tab ----
  
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
  
  
  names <- c(
    "Infarto",
    " ", " ",
    "Prob. Cir.",
    " ", " ",
    "Lesões",
    " ", " ")
  
  result <- data.frame(
    var = names,
    d1 = rep(NA, times = length(names)),
    d7 = rep(NA, times = length(names)),
    d30 = rep(NA, times = length(names)),
    d45 = rep(NA, times = length(names)),
    d60 = rep(NA, times = length(names))
    
  )
  
  
  
  #Dia 1
  #Inf
  result$d1[1] <- tab$coef[[1]]
  result$d1[2] <- tab$se[[1]]
  result$d1[3] <- tab$N[[1]]
  #PC
  result$d1[4] <- tab$coef[[2]]
  result$d1[5] <- tab$se[[2]]
  result$d1[6] <- tab$N[[2]]
  #L
  result$d1[7] <- tab$coef[[3]]
  result$d1[8] <- tab$se[[3]]
  result$d1[9] <- tab$N[[3]]
  
  
  #Dia 7
  
  #Inf
  result$d7[1] <- tab$coef[[4]]
  result$d7[2] <- tab$se[[4]]
  result$d7[3] <- tab$N[[4]]
  #PC
  result$d7[4] <- tab$coef[[5]]
  result$d7[5] <- tab$se[[5]]
  result$d7[6] <- tab$N[[5]]
  #Le
  result$d7[7] <- tab$coef[[6]]
  result$d7[8] <- tab$se[[6]]
  result$d7[9] <- tab$N[[6]]
  
  
  #Dia 30
  #Inf
  result$d30[1] <- tab$coef[[7]]
  result$d30[2] <- tab$se[[7]]
  result$d30[3] <- tab$N[[7]]
  #PC
  result$d30[4] <- tab$coef[[8]]
  result$d30[5] <- tab$se[[8]]
  result$d30[6] <- tab$N[[8]]
  #Le
  result$d30[7] <- tab$coef[[9]]
  result$d30[8] <- tab$se[[9]]
  result$d30[9] <- tab$N[[9]]
  
  
  
  #Dia 45
  #Inf
  result$d45[1] <- tab$coef[[10]]
  result$d45[2] <- tab$se[[10]]
  result$d45[3] <- tab$N[[10]]
  #PC
  result$d45[4] <- tab$coef[[11]]
  result$d45[5] <- tab$se[[11]]
  result$d45[6] <- tab$N[[11]]
  #L
  result$d45[7] <- tab$coef[[12]]
  result$d45[8] <- tab$se[[12]]
  result$d45[9] <- tab$N[[12]]
  
  #Dia 60
  #Inf
  result$d60[1] <- tab$coef[[13]]
  result$d60[2] <- tab$se[[13]]
  result$d60[3] <- tab$N[[13]]
  #Crt
  result$d60[4] <- tab$coef[[14]]
  result$d60[5] <- tab$se[[14]]
  result$d60[6] <- tab$N[[14]]
  #Le
  result$d60[7] <- tab$coef[[15]]
  result$d60[8] <- tab$se[[15]]
  result$d60[9] <- tab$N[[15]]
  
  
  
  
  
  
  
  colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")
  
  # Cria a tabela LaTeX
  latex_table <- knitr::kable(
    result,
    format = "latex",
    booktabs = TRUE,
    align = "lcccc",
    linesep = ""
  )
  
  
  writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/SIH_Vars_dias_res.tex")
  rm( list, result, tab, latex_table)
  
  
  
  
  
  
  
  
  