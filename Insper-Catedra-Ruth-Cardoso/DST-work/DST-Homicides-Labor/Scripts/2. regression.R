# ---------------------------------------------------------------------------- #
# Main regressiom
# Last edited by: Tuffy Licciardi Issa
# Date: 03/09/2025
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Libraries -----
# ---------------------------------------------------------------------------- #


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
# ---------------------------------------------------------------------------- #
#' DISCLAIMER:
#' For all data regarding homicide and hospilatization in DATASUS there are two
#' different municipal code values:
#' 
#' - Municipality of Occurrence, labeled as [OCO]
#' - Municipality of Residence, labeled as [RES] 
#'
#' Therefore, all regressions made in this script are repeated for both municipal
#' codes available.
# ----------------------------------------------------------------------------- #

#SIM ----

# ---------------------------------------------------------------------------- #
# 1. Regression ---- 
# ---------------------------------------------------------------------------- #
## ***OCO**** ----
## 1.1 Base ----

base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_hv_oco_2018.rds")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_hv_oco_2019.rds"))) 

base <- base %>% 
arrange(co_municipio,year) %>%
  group_by(co_municipio) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_hom = ifelse(year == 2018, hom_pop, NA),
    v2_hom = max(v1_hom, na.rm = T),
    d.media = hom_pop - v2_hom,
    
    
    v1_nhm = ifelse(year == 2018, nhm_pop, NA),
    v2_nhm = max(v1_nhm, na.rm = T),
    d.new_media = nhm_pop - v2_nhm,
    
    
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_hom, v2_hom,
            v1_nhm, v2_nhm)) %>% 
  mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
         dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
  setDT()

## 1.2 Controls ----
# Dependent variable
yv <- base %>%
  filter(year == 2019) %>% 
  select(d.media) %>% 
  rename(vd = 1)

yv2 <- base %>%
  filter(year == 2019) %>% 
  select(d.new_media) %>% 
  rename(vd = 1)


#45696
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



list <- list()

## 1.3 Regression ----

list[[as.character(paste0(2019,"-",2018,"|H"))]] <- rdrobust(
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

list[[as.character(paste0(2019,"-",2018,"|NH"))]] <- rdrobust(
  y = yv2$vd,
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

rm(clu, latv, lonv, ef, xv, yv, yv2)
# ---------------------------------------------------------------------------- #
#Extração da banda ótima
#[English: Optimal bandwidth extraction]
bw_main_a  <- list[["2019-2018|H"]]$bws[1]
bw_bias_a  <- list[["2019-2018|H"]]$bws[2]

bw_main_b  <- list[["2019-2018|NH"]]$bws[1]
bw_bias_b  <- list[["2019-2018|NH"]]$bws[2]
# ---------------------------------------------------------------------------- #


## 1.4 Table ----
tab <- data.frame(
  coef = do.call(rbind,lapply(list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list, FUN = function(x){x$pv[3]})),
  n = do.call(rbind,lapply(list, FUN = function(x){x$N_h}))
)
print(tab)


tab <- tab %>%
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "***", 
                         ifelse(pv < 0.05, "**", 
                                ifelse(pv < 0.1, "*", "")
                         ))),
    se =  paste0("(", formatC(x = se, digits = 2, format = "f"), ")"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select( coef, se, N ) %>%
  setDT()


## ***RES*** ----


## 1.5 Base ----

base_res <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_hv_res_2018.rds")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_hv_res_2019.rds"))) 

base_res <- base_res %>% 
  arrange(co_municipio,year) %>%
  group_by(co_municipio) %>%
  mutate(
    dup1 = 1,
    dup2 = sum(dup1),
    v1_hom = ifelse(year == 2018, hom_pop, NA),
    v2_hom = max(v1_hom, na.rm = T),
    d.media = hom_pop - v2_hom,
    
    
    v1_nhm = ifelse(year == 2018, nhm_pop, NA),
    v2_nhm = max(v1_nhm, na.rm = T),
    d.new_media = nhm_pop - v2_nhm,
    
    
  ) %>%
  ungroup() %>% 
  filter(dup2 == 2) %>% 
  select(-c(dup2, dup1, v1_hom, v2_hom,
            v1_nhm, v2_nhm)) %>% 
  mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
         dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
  setDT()





## 1.6 Controles ----
# Dependent variable
yv <- base_res %>%
  filter(year == 2019) %>% 
  select(d.media) %>% 
  rename(vd = 1)

yv2 <- base_res %>%
  filter(year == 2019) %>% 
  select(d.new_media) %>% 
  rename(vd = 1)


#45696
# Running variable
xv <- base_res %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- base_res %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- base_res %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- base_res %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)



list_res <- list()

## 1.7 Regression ----

list_res[[as.character(paste0(2019,"-",2018,"|H"))]] <- rdrobust(
  y = yv$vd,
  x = xv$dist_hv_border,
  c = 0,
  weight = base_res$pop[base$year == 2018],
  cluster = clu$seg,
  vce = "hc0",
  covs = cbind(
    ef,
    lonv,
    latv
  )
)

list_res[[as.character(paste0(2019,"-",2018,"|NH"))]] <- rdrobust(
  y = yv2$vd,
  x = xv$dist_hv_border,
  c = 0,
  weight = base_res$pop[base$year == 2018],
  cluster = clu$seg,
  vce = "hc0",
  covs = cbind(
    ef,
    lonv,
    latv
  )
)

rm(clu, latv, lonv, ef, xv, yv, yv2)
# ---------------------------------------------------------------------------- #
#Extração da banda ótima
#[English: Optimal bandwidth extraction]
bw_main_c  <- list_res[["2019-2018|H"]]$bws[1]
bw_bias_c  <- list_res[["2019-2018|H"]]$bws[2]

bw_main_d  <- list_res[["2019-2018|NH"]]$bws[1]
bw_bias_d  <- list_res[["2019-2018|NH"]]$bws[2]
# ---------------------------------------------------------------------------- #


## 1.8 Table ----


tab2 <- data.frame(
  coef = do.call(rbind,lapply(list_res, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list_res, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list_res, FUN = function(x){x$pv[3]})),
  n = do.call(rbind,lapply(list_res, FUN = function(x){x$N_h}))
)
print(tab2)


tab2 <- tab2 %>%
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "***", 
                         ifelse(pv < 0.05, "**", 
                                ifelse(pv < 0.1, "*", "")
                         ))),
    se =  paste0("(", formatC(x = se, digits = 2, format = "f"), ")"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select( coef, se, N ) %>%
  setDT()




##1.9 RESULTS -----
names <- c("2019 - 2018",
           " "," ")

result <- data.frame(
  var = names,
  
  nhcor = rep(NA, times = length(names)),
  nhres = rep(NA, times = length(names)),
  hcor = rep(NA, times = length(names)),
  hres = rep(NA, times = length(names))
  
)


#Mun de Ocorrência
#[English: Occurence Municipality]
result$hcor[1] <- tab$coef[[1]]
result$hcor[2] <- tab$se[[1]]
result$hcor[3] <- tab$N[[1]]

result$nhcor[1] <- tab$coef[[2]]
result$nhcor[2] <- tab$se[[2]]
result$nhcor[3] <- tab$N[[2]]

#Mun de res
#[English: Residency municipality]
result$hres[1] <- tab2$coef[[1]]
result$hres[2] <- tab2$se[[1]]
result$hres[3] <- tab2$N[[1]]

result$nhres[1] <- tab2$coef[[2]]
result$nhres[2] <- tab2$se[[2]]
result$nhres[3] <- tab2$N[[2]]




colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)")

# Cria a tabela LaTeX
#[English: Creating a LaTeX table]
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lcccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Principal_v2.tex")
rm(ef, list, result, tab, latex_table, list_res, tab2)

# --------------------------------------------------------------------------- #

#2. Figure ----
## ***OCO***----

##2.1 Hom ---------
fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_a ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.media),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.media) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
#[English: Estimating each graph parameter]
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main_a,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = T,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
#[English: Vectors and auxiliatory values]
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-5*10^5,5*10^5,10^5)

# Graph
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Homicide rate \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_Principal_1918_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_Principal_1918_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)


##2.2 New Hom ----
#' In this section I utilize a new homicide variable



fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_b ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.new_media),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.media) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
#[English: Estimating the graph parameters]
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main_b,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = T,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
#[English: Vectors and auxiliatory variables]
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-5*10^5,5*10^5,10^5)

# Graph
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Homicide rate \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_Principal_newhm_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_Principal_newhm_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)



## ***RES*** -----
##2.3 Hom ---------
fig <- list()

temp <- base_res %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_c ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.media),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.media) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
#[English: Estimating the graph parameters]
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main_c,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = T,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
#[English: Vectors and auxiliatory variables]
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-5*10^5,5*10^5,10^5)

# Graph
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Homicide rate \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_Principal_hom_res_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_Principal_hom_res_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)


##2.4 New Hom ----
fig <- list()

temp <- base_res %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main_d ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.new_media),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.media) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
#[English: Estimating the graph parameters]
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main_d,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = T,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
#[English: Vectors and auxiliatory variables]
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-5*10^5,5*10^5,10^5)

# Graph
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Homicide rate \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_Principal_newhm_res_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_Principal_newhm_res_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips,
   bw_bias_a, bw_bias_b, bw_bias_c, bw_bias_d, bw_main_a, bw_main_b, bw_main_c,
   bw_main_d, base, base_res)



# ---------------------------------------------------------------------------- #
#3. Other Variable ----
## *** OCO *** ----

base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_hv_oco_2018.rds")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_hv_oco_2019.rds"))) 

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

list <- list()

for (var in var_list){
  
  ef <- dummy_cols(base$seg[base$year == 2018])
  ef <- ef %>% select(-1,-2)
  

  

    list[[as.character(paste0(var,"|C"))]] <-
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
  
    message("Finalizado para", var)
  
}
rm(ef,var)



##3.1 Table ----


tab <- data.frame(
  coef = do.call(rbind,lapply(list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list, FUN = function(x){x$pv[3]})),
  n.1 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[1]})),
  n.2 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[2]}))
)


tab <- tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "***", 
                         ifelse(pv < 0.05, "**", 
                                ifelse(pv < 0.1, "*", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select(-c(pv))
  

names <- c("2019 - 2018",
           " "," ")

result <- data.frame(
  var = names,
  tra = rep(NA, times = length(names)),
  crd = rep(NA, times = length(names)),
  crt = rep(NA, times = length(names)),
  ext = rep(NA, times = length(names)),
  lxt = rep(NA,time = length(names))
)


#Transito
#[English: Transit]
result$tra[1] <- tab$coef[[1]]
result$tra[2] <- tab$se[[1]]
result$tra[3] <- tab$N[[1]]
#Cardio
#[English: Cardiovasculatory]
result$crd[1] <- tab$coef[[2]]
result$crd[2] <- tab$se[[2]]
result$crd[3] <- tab$N[[2]]

#Cardio Total
#[English: Total Cardiovasculatory]
result$crt[1] <- tab$coef[[3]]
result$crt[2] <- tab$se[[3]]
result$crt[3] <- tab$N[[3]]

#Externas
#[English: External Causes]
result$ext[1] <- tab$coef[[4]]
result$ext[2] <- tab$se[[4]]
result$ext[3] <- tab$N[[4]]
#Lesões Externas
#[English: External Injuries]
result$lxt[1] <- tab$coef[[4]]
result$lxt[2] <- tab$se[[4]]
result$lxt[3] <- tab$N[[4]]




colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")

# Cria a tabela LaTeX
#[English: Crearing LaTeX Table]
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Outras_vars_oco_v1.tex")
rm(result, tab, latex_table)





## 3.2 Figure ----

###3.2.1 Transito ----


#Extração da banda ótima
bw_main  <- list[["d.tra|C"]]$bws[1]
bw_bias  <- list[["d.tra|C"]]$bws[2]

fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.tra),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.tra) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-5*10^5,5*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Transit mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_Transito_oco_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_Transito_oco_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, bw_bias_a, bw_main_a)



###3.2.2 Cardio ----


#Extração da banda ótima
bw_main <- list[["d.crd|C"]]$bws[1]
bw_bias  <- list[["d.crd|C"]]$bws[2]

fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.crd),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crd) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Cardiac mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_Cardio_oco_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_Cardio_oco_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)


###3.2.3  CarTotal ----


#Extração da banda ótima
bw_main <- list[["d.crt|C"]]$bws[1]
bw_bias  <- list[["d.crt|C"]]$bws[2]

fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.crt),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crt) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Cardiac mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_CardioTotal_oco_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_CardioTotal_oco_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

###3.2.4 Externas ----
  
  
  #Extração da banda ótima
bw_main <- list[["d.ext|C"]]$bws[1]
bw_bias  <- list[["d.ext|C"]]$bws[2]

fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.ext),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crd) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)

y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Foreign causes mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_Externas_oco_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_Externas_oco_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)






###3.2.5 L + Externas ----


#Extração da banda ótima
bw_main <- list[["d.lxt|C"]]$bws[1]
bw_bias  <- list[["d.lxt|C"]]$bws[2]

fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.lxt),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.lxt) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)

y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Foreign causes mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_LeExternas_oco_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_LeExternas_oco_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)




# 2. Dados SIH


## *** RES *** ----

base_res <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_hv_res_2018.rds")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_hv_res_2019.rds"))) 

base_res <- base_res %>% 
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

list <- list()

for (var in var_list){
  
  ef <- dummy_cols(base_res$seg[base_res$year == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  
  
  list[[as.character(paste0(var,"|C"))]] <-
    rdrobust(
      y = base_res[[var]][base_res$year == 2019],
      x = base_res$dist_hv_border[base_res$year == 2018],
      c = 0,
      cluster = base_res$seg[base_res$year == 2018],
      weights = base_res$pop[base_res$year == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_res$lat[base_res$year == 2018], 
        base_res$lon[base_res$year == 2018]
      )
    )
  
  message("Finalizado para", var)
  
}
rm(ef,i)



##3.1 Tab ----


tab <- data.frame(
  coef = do.call(rbind,lapply(list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list, FUN = function(x){x$pv[3]})),
  n.1 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[1]})),
  n.2 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[2]}))
)


tab <- tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "***", 
                         ifelse(pv < 0.05, "**", 
                                ifelse(pv < 0.1, "*", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select(-c(pv))


names <- c("2019 - 2018",
           " "," ")

result <- data.frame(
  var = names,
  tra = rep(NA, times = length(names)),
  crd = rep(NA, times = length(names)),
  crt = rep(NA, times = length(names)),
  ext = rep(NA, times = length(names)),
  lxt = rep(NA,time = length(names))
)


#Transito
result$tra[1] <- tab$coef[[1]]
result$tra[2] <- tab$se[[1]]
result$tra[3] <- tab$N[[1]]
#Cardio
result$crd[1] <- tab$coef[[2]]
result$crd[2] <- tab$se[[2]]
result$crd[3] <- tab$N[[2]]

#Cardio Total
result$crt[1] <- tab$coef[[3]]
result$crt[2] <- tab$se[[3]]
result$crt[3] <- tab$N[[3]]

#Externas
result$ext[1] <- tab$coef[[4]]
result$ext[2] <- tab$se[[4]]
result$ext[3] <- tab$N[[4]]
#Lesões Externas
result$lxt[1] <- tab$coef[[4]]
result$lxt[2] <- tab$se[[4]]
result$lxt[3] <- tab$N[[4]]




colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Outras_vars_res_v1.tex")
rm(ef, result, tab, latex_table)





## 3.2 Figure ----

###3.2.1 Transito ----


#Extração da banda ótima
bw_main  <- list[["d.tra|C"]]$bws[1]
bw_bias  <- list[["d.tra|C"]]$bws[2]

fig <- list()

temp <- base_res %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.tra),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.tra) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-5*10^5,5*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Transit mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_Transito_res_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_Transito_res_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, bw_bias_a, bw_main_a)



###3.2.2 Cardio ----


#Extração da banda ótima
bw_main <- list[["d.crd|C"]]$bws[1]
bw_bias  <- list[["d.crd|C"]]$bws[2]

fig <- list()

temp <- base_res %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.crd),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crd) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Cardiac mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_Cardio_res_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_Cardio_res_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)


###3.2.3  CarTotal ----


#Extração da banda ótima
bw_main <- list[["d.crt|C"]]$bws[1]
bw_bias  <- list[["d.crt|C"]]$bws[2]

fig <- list()

temp <- base_res %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.crt),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crt) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Cardiac mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_CardioTotal_res_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_CardioTotal_res_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

###3.2.4 Externas ----


#Extração da banda ótima
bw_main <- list[["d.ext|C"]]$bws[1]
bw_bias  <- list[["d.ext|C"]]$bws[2]

fig <- list()

temp <- base_res %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.ext),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crd) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)

y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Foreign causes mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_Externas_res_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_Externas_res_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)






###3.2.5 L + Externas ----


#Extração da banda ótima
bw_main <- list[["d.lxt|C"]]$bws[1]
bw_bias  <- list[["d.lxt|C"]]$bws[2]

fig <- list()

temp <- base_res %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.lxt),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.lxt) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)

y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Foreign causes mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_LeExternas_res_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_LeExternas_res_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)




# 2. Dados SIH




# ****************** ----
# SIH ----
# ---------------------------------------------------------------------------- #
# 4. SIH Regression ---- 
# ---------------------------------------------------------------------------- #
## *** OCO **** ----
## 4.1 Base ----


names <- c("2019 - 2018",
           " "," ")

base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_oco_2018.rds")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_oco_2019.rds"))) 

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
            v1_les, v2_les, v1_lxt, v2_lxt,
            v1_ext, v2_ext)) %>% 
  mutate(hv = ifelse(co_municipio %/% 100000 > 30, 1, 0),
         dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>%
  setDT()


var_list <- c( "d.crd", "d.crt", "d.lxt")

list <- list()

for (var in var_list){
  
  ef <- dummy_cols(base$seg[base$year == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  
  
  list[[as.character(paste0(var,"|C"))]] <-
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
  
  message("Finalizado para", var)
  
}
rm(ef,i)



##4.2 Tab ----


tab <- data.frame(
  coef = do.call(rbind,lapply(list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list, FUN = function(x){x$pv[3]})),
  n.1 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[1]})),
  n.2 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[2]}))
)


tab <- tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "***", 
                         ifelse(pv < 0.05, "**", 
                                ifelse(pv < 0.1, "*", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select(-c(pv))


names <- c("2019 - 2018",
           " "," ")

result <- data.frame(
  var = names,
  #tra = rep(NA, times = length(names)),
  crd = rep(NA, times = length(names)),
  crt = rep(NA, times = length(names)),
  #ext = rep(NA, times = length(names)),
  lxt = rep(NA,time = length(names))
)


# #Transito
# result$tra[1] <- tab$coef[[1]]
# result$tra[2] <- tab$se[[1]]
# result$tra[3] <- tab$N[[1]]
#Cardio
result$crd[1] <- tab$coef[[1]]
result$crd[2] <- tab$se[[1]]
result$crd[3] <- tab$N[[1]]

#Cardio Total
result$crt[1] <- tab$coef[[2]]
result$crt[2] <- tab$se[[2]]
result$crt[3] <- tab$N[[2]]

# #Externas
# result$ext[1] <- tab$coef[[4]]
# result$ext[2] <- tab$se[[4]]
# result$ext[3] <- tab$N[[4]]
#Lesões Externas
result$lxt[1] <- tab$coef[[3]]
result$lxt[2] <- tab$se[[3]]
result$lxt[3] <- tab$N[[3]]




colnames(result) <- c("", "(1)", "(2)", "(3)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/SIH_vars_oco_v1.tex")
rm(ef, result, tab, latex_table)





## 4.3 Figure ----

###4.3.1 Transito ----


#Extração da banda ótima
bw_main  <- list[["d.tra|C"]]$bws[1]
bw_bias  <- list[["d.tra|C"]]$bws[2]

fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.tra),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.tra) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-5*10^5,5*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Transit mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_SIH_Transito_oco_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_SIH_Transito_oco_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips, bw_bias_a, bw_main_a)



###4.3.2 Cardio ----


#Extração da banda ótima
bw_main <- list[["d.crd|C"]]$bws[1]
bw_bias  <- list[["d.crd|C"]]$bws[2]

fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.crd),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crd) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Cardiac mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_SIH_Cardio_oco_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_SIH_Cardio_oco_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)


###4.3.3  CarTotal ----


#Extração da banda ótima
bw_main <- list[["d.crt|C"]]$bws[1]
bw_bias  <- list[["d.crt|C"]]$bws[2]

fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.crt),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crt) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Cardiac mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_SIH_CardioTotal_oco_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_SIH_CardioTotal_oco_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

###4.3.4 Externas ----


#Extração da banda ótima
bw_main <- list[["d.ext|C"]]$bws[1]
bw_bias  <- list[["d.ext|C"]]$bws[2]

fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.ext),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crd) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)

y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Foreign causes mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_SIH_Externas_oco_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_SIH_Externas_oco_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)






###4.3.5 L + Externas ----


#Extração da banda ótima
bw_main <- list[["d.lxt|C"]]$bws[1]
bw_bias  <- list[["d.lxt|C"]]$bws[2]

fig <- list()

temp <- base %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.lxt),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.lxt) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)

y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Foreign causes mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_SIH_LeExternas_oco_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_SIH_LeExternas_oco_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips,list)



# 5. Homicidio ----
## 5.1 Reg ----

df <- readRDS("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_hom_oco.rds")

df <- df %>% 
  group_by(codmun) %>% 
  mutate(
    d.hom = hom19 - hom18
  )
  
#
list <- list()

ef <- dummy_cols(df$seg)
ef <- ef %>% select(-1,-2)

list[[as.character(paste0("hom|C"))]] <-
  rdrobust(
    y = df$d.hom,
    x = df$dist_hv_border,
    c = 0,
    cluster = df$seg,
    weights = df$pop18,
    vce = "hc0",
    covs = cbind(
      ef, 
      df$lat, 
      df$lon
    )
  )


#Repetindo para as causas externas
df <- readRDS("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_ext_oco.rds")

df <- df %>% 
  group_by(codmun) %>% 
  mutate(
    d.ext = ext19 - ext18
  )


ef <- dummy_cols(df$seg)
ef <- ef %>% select(-1,-2)

list[[as.character(paste0("ext|C"))]] <-
  rdrobust(
    y = df$d.ext,
    x = df$dist_hv_border,
    c = 0,
    cluster = df$seg,
    weights = df$pop18,
    vce = "hc0",
    covs = cbind(
      ef, 
      df$lat, 
      df$lon
    )
  )


## 5.2 Tabela ----
tab <- data.frame(
  coef = do.call(rbind,lapply(list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list, FUN = function(x){x$pv[3]})),
  n.1 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[1]})),
  n.2 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[2]}))
)


tab <- tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "***", 
                         ifelse(pv < 0.05, "**", 
                                ifelse(pv < 0.1, "*", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select(-c(pv))


names <- c("2019 - 2018",
           " "," ")

result <- data.frame(
  var = names,
  hom = rep(NA, times = length(names)),
  ext = rep(NA, times = length(names))
)



#Cardio
result$hom[1] <- tab$coef[[1]]
result$hom[2] <- tab$se[[1]]
result$hom[3] <- tab$N[[1]]

#Cardio Total
result$ext[1] <- tab$coef[[2]]
result$ext[2] <- tab$se[[2]]
result$ext[3] <- tab$N[[2]]


#colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/SIH_hom_v1.tex")
rm(df, result, tab, latex_table,list,ef)




## *** RES *** ----
# 6. Regression ----
base_res <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_res_2018.rds")) %>%
  bind_rows(readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIH/SIH_hv_res_2019.rds"))) 

base_res <- base_res %>% 
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


var_list <- c( "d.crd", "d.crt", "d.lxt")

list <- list()

for (var in var_list){
  
  ef <- dummy_cols(base_res$seg[base_res$year == 2018])
  ef <- ef %>% select(-1,-2)
  
  
  
  
  list[[as.character(paste0(var,"|C"))]] <-
    rdrobust(
      y = base_res[[var]][base_res$year == 2019],
      x = base_res$dist_hv_border[base_res$year == 2018],
      c = 0,
      cluster = base_res$seg[base_res$year == 2018],
      weights = base_res$pop[base_res$year == 2018],
      vce = "hc0",
      covs = cbind(
        ef, 
        base_res$lat[base_res$year == 2018], 
        base_res$lon[base_res$year == 2018]
      )
    )
  
  message("Finalizado para", var)
  
}
rm(ef,i)



##6.1 Tab ----


tab <- data.frame(
  coef = do.call(rbind,lapply(list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list, FUN = function(x){x$pv[3]})),
  n.1 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[1]})),
  n.2 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[2]}))
)


tab <- tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "***", 
                         ifelse(pv < 0.05, "**", 
                                ifelse(pv < 0.1, "*", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select(-c(pv))


names <- c("2019 - 2018",
           " "," ")

result <- data.frame(
  var = names,
  crd = rep(NA, times = length(names)),
  crt = rep(NA, times = length(names)),
  lxt = rep(NA,time = length(names))
)



#Cardio
result$crd[1] <- tab$coef[[1]]
result$crd[2] <- tab$se[[1]]
result$crd[3] <- tab$N[[1]]

#Cardio Total
result$crt[1] <- tab$coef[[2]]
result$crt[2] <- tab$se[[2]]
result$crt[3] <- tab$N[[2]]

#Lesões Externas
result$lxt[1] <- tab$coef[[3]]
result$lxt[2] <- tab$se[[3]]
result$lxt[3] <- tab$N[[3]]




#colnames(result) <- c("", "(1)", "(2)", "(3)", "(4)", "(5)")

# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccccc",
  linesep = ""
)


writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/SIH_vars_res_v1.tex")
rm(ef, result, tab, latex_table)





## 6.2 Figure ----

###6.2.1 Cardio ----


#Extração da banda ótima
bw_main <- list[["d.crd|C"]]$bws[1]
bw_bias  <- list[["d.crd|C"]]$bws[2]

fig <- list()

temp <- base_res %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.crd),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crd) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Cardiac mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_SIH_Cardio_res_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_SIH_Cardio_res_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)


###6.2.2  CarTotal ----


#Extração da banda ótima
bw_main <- list[["d.crt|C"]]$bws[1]
bw_bias  <- list[["d.crt|C"]]$bws[2]

fig <- list()

temp <- base_res %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.crt),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.crt) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)
y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Cardiac mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-15,15) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_SIH_CardioTotal_res_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_SIH_CardioTotal_res_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

###6.2.3 L + Externas ----


#Extração da banda ótima
bw_main <- list[["d.lxt|C"]]$bws[1]
bw_bias  <- list[["d.lxt|C"]]$bws[2]

fig <- list()

temp <- base_res %>% 
  mutate(subset = case_when(
    abs(dist_hv_border) < bw_main ~ 1,
    .default = 0
  )
  ) %>% 
  filter(
    !is.na(d.lxt),
    subset == 1
  )

# Dependent variable
yv <- temp %>%
  filter(year == 2019) %>% 
  select(d.lxt) %>% 
  rename(vd = 1)

#45696
# Running variable
xv <- temp %>%
  filter(year == 2018) %>% 
  select(dist_hv_border)

# Clusters
clu <- temp %>% 
  filter(year == 2018) %>% 
  select(seg)

# Latitude
latv <- temp %>%
  filter(year == 2018) %>% 
  select(lat)

# Longitude
lonv <- temp %>% 
  filter(year == 2018) %>% 
  select(lon)

ef <- dummy_cols(clu$seg)
ef <- ef %>% select(-1,-2)

# Estimando parâmetros do gráfico
fig <- rdplot(y = yv$vd,
              x = xv$dist_hv_border,
              c = 0,
              p = 1,
              #binselect = "esmv",
              kernel = "triangular",
              h = bw_main,
              #b = bw_bias_a,
              weights = temp$pop[temp$year == 2018],
              subset = temp$subset == 1,
              hide = F,
              masspoints= "adjust",
              covs = cbind(ef,latv,lonv)
)

rm(yv, xv, clu, latv, lonv, ef, temp)



# Vetores e valores auxiliares
fig$vars_bins$hv <- fig$vars_bins$rdplot_mean_x >= 1
fig$vars_poly$hv <- fig$vars_poly$rdplot_x >= 1

x_r_sta <- 0
x_r_end <- max(fig$vars_poly$rdplot_x)

y_r_sta <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))
y_r_end <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == max(fig$vars_poly$rdplot_x)]

x_l_sta <- min(fig$vars_poly$rdplot_x)
x_l_end <- 0

y_l_sta <- fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == min(fig$vars_poly$rdplot_x)]
y_l_end <- ifelse(fig$coef[1,1] > fig$coef[1,2], 
                  max(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]),
                  min(fig$vars_poly$rdplot_y[fig$vars_poly$rdplot_x == 0]))

xtips <- seq(-6*10^5,6*10^5,10^5)

# Gráfico
fig_gg <- ggplot() +
  geom_point(data = fig$vars_bins, aes(x = rdplot_mean_x, y = rdplot_mean_y, color = factor(hv)),
             alpha = 0.5, size = 2, show.legend = FALSE) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_color_brewer(palette = "Set1") + 
  geom_segment(aes(x = x_l_sta, 
                   xend = x_l_end, 
                   y = y_l_sta, 
                   yend = y_l_end),
               size = 1.5, color = "#E41A4C") + 
  geom_segment(aes(x = x_r_sta, 
                   xend = x_r_end, 
                   y = y_r_sta, 
                   yend = y_r_end), 
               size = 1.5, color = "#377EB8") + 
  labs(x = "Distance to DST Border (km)",
       y = "Foreign causes mortality \n per 100,000") + 
  theme_bw() + 
  scale_x_continuous(breaks = xtips,
                     labels = (xtips / 1000) %>% formatC(digits = 0,format = "f")) +
  ylim(-30,30) + 
  theme(axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        axis.text.x = element_text(size = 30,angle = 90,hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 30))


fig_gg

ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/RDD_SIH_LeExternas_res_v1.png"),plot = fig_gg, device = "png",dpi = 300, height = 6, width = 9)
ggsave(filename = paste0("Z:/Tuffy/Paper - HVSUS/Resultados/Imagens/pdf/RDD_SIH_LeExternas_res_v1.pdf"),plot = fig_gg, device = "pdf",height = 7, width = 10)

rm(fig,fig_gg,x_r_sta,x_r_end,x_l_sta,x_l_end,y_r_sta,y_r_end,y_l_sta,y_l_end,xtips)

rm(base, base_res, bw_main, bw_bias)

# ****************** ----




# RDD e DID ----
# ---------------------------------------------------------------------------- #
#7.Bandwidth para os dias ----
# ---------------------------------------------------------------------------- #
base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_60d_",2018,".rds"))

base <- base %>% 
  mutate(hv = ifelse(as.numeric(co_municipio) %/% 100000 > 30, 1, 0),
         dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border))

censopop <- readxl::read_xlsx("Z:/Tuffy/Paper - HVSUS/Bases/pop_anual.xlsx") %>% 
  select(codmun, as.character(2018))


#Temos já a dummy de homicidio (new_homicidio).
#Montamos então um RDD geográfico.


## 7.1 RDD ----



list <- list()

days <- c(30, 45, 60)

for (d in days ) {
  
  #Filtrando as datas
  if (d == 30) {
  
    temp <- base %>% 
      filter(
        between(data,
                ymd(paste0(2018,"-10-05")),
                ymd(paste0(2018,"-12-04"))
        ),
        !is.na(dist_hv_border)
      ) 
      
  } else if (d == 45) {
    
    temp <- base %>% 
      filter(
        between(data,
                ymd(paste0(2018,"-09-20")),
                ymd(paste0(2018,"-12-19"))
        ),
        !is.na(dist_hv_border)
      )
    
  } else {
    
    temp <- base %>% 
      filter(
        !is.na(dist_hv_border)
      )
    
  }
  
  
  ef <- dummy_cols(temp$seg)
  ef <- ef %>% select(-1,-2)
  
  
  list[[as.character(paste0(d,"|RDD"))]] <- rdrobust(
    y = temp$new_homicidio,
    x = temp$dist_hv_border,
    c = 0,
    #weight = base_res$pop[base$year == 2018],
    cluster = temp$seg,
    vce = "hc0",
    covs = cbind(
      ef,
      temp$lon,
      temp$lat
    )
  )
  
  rm(temp, d, ef)
  
}
### 7.1.1 Tab ----
tab <- data.frame(
  coef = do.call(rbind,lapply(list, FUN = function(x){x$coef[3]})),
  se = do.call(rbind,lapply(list, FUN = function(x){x$se[3]})),
  pv = do.call(rbind,lapply(list, FUN = function(x){x$pv[3]})),
  n.1 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[1]})),
  n.2 = do.call(rbind,lapply(list, FUN = function(x){x$N_h[2]})),
  bw_main = do.call(rbind,lapply(list, FUN = function(x){x$bws[1]}))
)


tab <- tab %>% 
  mutate(
    coef = paste0(formatC(x = coef, digits = 2, format = "f"),
                  ifelse(pv < 0.01, "***", 
                         ifelse(pv < 0.05, "**", 
                                ifelse(pv < 0.1, "*", "")
                         ))),
    se = paste0(" (",formatC(x = se, digits = 2, format = "f"),")"),
    pv = formatC(x = pv, digits = 3, format = "f"),
    N = paste0("[N = ", n.1 + n.2, "]")
  ) %>%
  select(-c(pv))


names <- c("2018",
           " "," ",
           "Bandwidth")

result <- data.frame(
  var = names,
  crd = rep(NA, times = length(names)),
  crt = rep(NA, times = length(names)),
  lxt = rep(NA,time = length(names))
)


#Cardio
result$crd[1] <- tab$coef[[1]]
result$crd[2] <- tab$se[[1]]
result$crd[3] <- tab$N[[1]]
result$crd[4] <- tab$bw_main[[1]]
#Cardio Total
result$crt[1] <- tab$coef[[2]]
result$crt[2] <- tab$se[[2]]
result$crt[3] <- tab$N[[2]]
result$crt[4] <- tab$bw_main[[2]]
#Lesões Externas
result$lxt[1] <- tab$coef[[3]]
result$lxt[2] <- tab$se[[3]]
result$lxt[3] <- tab$N[[3]]
result$lxt[4] <- tab$bw_main[[3]]

bw_30d <- list[["30|RDD"]]$bws[1]
bw_45d  <- list[["45|RDD"]]$bws[1]
bw_60d  <- list[["60|RDD"]]$bws[1]




# Cria a tabela LaTeX
latex_table <- knitr::kable(
  result,
  format = "latex",
  booktabs = TRUE,
  align = "lccccc",
  linesep = ""
)

writeLines(latex_table, "Z:/Tuffy/Paper - HVSUS/Resultados/Tabelas/Geo_Band.tex")

rm(result, latex_table, list, base)
### 7.1.2 MAPA ---- 

line <- readRDS(file = "Z:/Arquivos IFB/Paper - Horário de Verão e Educação/V2 Horário de Verão e ENEM/Bases de dados/revisao/line.RDS")

bw_main  <- bw_60d

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

# ---------------------------------------------------------------------------- #
# 8. DiD ----
# ---------------------------------------------------------------------------- #
library(fixest)

base <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_60d_",2018,".rds"))
temp <- readRDS(file = paste0("Z:/Tuffy/Paper - HVSUS/Bases/SIM/SIM_60d_",2019,".rds"))


base <- rbind(base, temp)
rm(temp)


##8.1 Variáveis ----

base <- base %>% 
  mutate(hv = ifelse(as.numeric(co_municipio) %/% 100000 > 30, 1, 0),
         dist_hv_border = ifelse(hv == 1, dist_hv_border, -dist_hv_border)) %>% 
  mutate(
    endhv = ifelse( between(data,ymd(paste0(2019,"-11-04")),
                             ymd(paste0(2019,"-12-31")))
                     , 1 , 0)
  ) %>% 
  filter(
    !is.na(dist_hv_border)
  )

base <- transform(base, fef = as.factor(paste(semana, dia_semana, year)))

base <- base %>% 
  mutate(
    seg = as.factor(seg)
  )

## 8.2 Regression ----
###8.2.1 60 Dias -----

df60 <- base %>% 
  filter(abs(dist_hv_border) <= bw_60d)

est1 <- feols(new_homicidio ~ endhv*hv | lon + lat + seg, df60, vcov = "hetero")
est2 <- feols(new_homicidio ~ endhv*hv | lon + lat + seg + fef, df60, vcov = "hetero")

print(est1)
print(est2)
