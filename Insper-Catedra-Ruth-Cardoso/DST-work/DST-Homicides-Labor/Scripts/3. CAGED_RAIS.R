# Library ------

library(tidyverse)

library(readr)


#Checando o separador
linhas <- readLines("Z:/Arquivos IFB/CAGED/2019/CAGEDEST_AJUSTES_112019/CAGEDEST_AJUSTES_112019.txt", n = 5)
linhas


dados <- read.table("Z:/Arquivos IFB/CAGED/2019/CAGEDEST_112019/CAGEDEST_112019.txt", header = F, sep = ";")


source('http://cemin.wikidot.com/local--files/raisr/rais.r')


#ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/Layout%20N%E3o-identificado%20Novo%20Caged%20Movimenta%E7%E3o.xlsx

library(dplyr)
library(readr)
library(purrr)

# Caminho da pasta principal onde estão as subpastas
base_dir <- "Z:/Arquivos IFB/CAGED/2019"  # ajuste esse caminho

# 1) detecta todas as subpastas com padrão CAGEDEST_*
subpastas <- list.dirs(path = base_dir, full.names = TRUE, recursive = FALSE)
subpastas <- subpastas[grepl("CAGEDEST_", basename(subpastas))]

# 2) função que lê cada pasta (procura TXT/CSV e retorna tibble)
ler_cagedest <- function(folder) {
  files <- list.files(folder, pattern = "\\.txt$|\\.csv$", full.names = TRUE)
  if (length(files) == 0) {
    warning("Nenhum arquivo de dados em ", folder)
    return(NULL)
  }
  # caso haja múltiplos, pega o primeiro
  file <- files[1]
  message("Lendo: ", file)
  if (grepl("\\.txt$", file, ignore.case = TRUE)) {
    read_delim(file, delim=";", locale = locale(encoding="latin1"))
  } else {
    read_csv(file, locale = locale(encoding="latin1"))
  }
}

# 3) lê todos e empilha
lista_df <- map(subpastas, ler_cagedest)
dados_totais <- bind_rows(lista_df, .id = "origem")

# 4) extrai ano/mês do nome da pasta e adiciona colunas
dados_totais <- dados_totais %>%
  mutate(
    pasta = names(lista_df),
    ano   = substr(pasta, nchar(pasta)-3, nchar(pasta)-2),  # ex: "19" de 2019
    mes   = substr(pasta, nchar(pasta)-1, nchar(pasta))     # ex: "01"
  ) %>%
  mutate(
    ano = as.integer(ifelse(nchar(ano)==2, paste0("20", ano), ano)),
    mes = as.integer(mes)
  )

# 5) Remova coluna 'origem' se quiser e veja os resultados
dados_totais <- select(dados_totais, -origem)
glimpse(dados_totais)

## Extração de docs---

library(curl)

# links for the PDFs (encode special characters if needed)
urls <- c(
  "ftp://ftp.mtps.gov.br/pdet/microdados/COMUNICADO_microdados.pdf",
  "ftp://ftp.mtps.gov.br/pdet/microdados/NOTA_TECNICA_microdados.pdf",
  "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/2019/Leia-me.txt"
  
  )

dests <- c(
  "COMUNICADO_microdados.pdf",
  "NOTA_TECNICA_microdados.pdf",
  "Leia-me.txt"
)

for (i in seq_along(urls)) {
  message("Downloading: ", urls[i])
  tryCatch(
    curl_download(urls[i], destfile = dests[i], mode = "wb"),
    error = function(e) message("Failed: ", e$message)
  )
}


### RAIS 2019 -----

library(archive)

# Define the FTP URL and local paths
url_estab <- "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/2019/Legado/RAIS_ESTAB_PUB.7z"
local_download <- "RAIS_ESTAB_PUB_2019.7z"
local_extract_dir <- "RAIS_ESTAB_2019_estab"

# Download
curl_download(url_estab, destfile = local_download, mode = "wb")

# Check if download succeeded
if (!file.exists(local_download)) stop("Download failed or file not found: ", local_download)

# Extract
archive_extract(local_download, dir = local_extract_dir)

# Find the .txt inside
txt_path <- list.files(local_extract_dir, pattern = "\\.txt$", full.names = TRUE)
if (length(txt_path) == 0) stop("No .txt file found in the extracted directory.")

# Read it (choose correct delimiter + encoding, often ";" and Latin1)
estab_dt <- fread(txt_path[1], sep = ";", encoding = "Latin-1", na.strings = c("", "NA"))

# View the top rows
head(estab_dt)

## RAIS 2018 -----

# Install & load needed packages
if (!requireNamespace("archive", quietly = TRUE)) install.packages("archive")
if (!requireNamespace("curl", quietly = TRUE)) install.packages("curl")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")

library(archive)
library(curl)
library(data.table)

# Function to try downloading & extracting for 2018
download_extract_estab_2018 <- function() {
  url_estab <- "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/2018/RAIS_ESTAB_PUB.7z"
  local_zip <- "RAIS_ESTAB_PUB_2018.7z"
  extract_dir <- "RAIS_ESTAB_2018_"
  
  # Download
  message("Downloading: ", url_estab)
  curl_download(url = url_estab, destfile = local_zip, mode = "wb")
  
  # Check success
  if (!file.exists(local_zip)) stop("Download failed or file not found: ", local_zip)
  if (file.info(local_zip)$size == 0) stop("Downloaded file has size 0: likely incomplete.")
  
  # Clean up existing folder to avoid overwrite problems
  if (dir.exists(extract_dir)) {
    message("Removing existing extraction directory: ", extract_dir)
    unlink(extract_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(extract_dir)
  
  # Try extracting
  message("Attempting archive_extract() …")
  tryCatch({
    archive_extract(local_zip, dir = extract_dir)
  }, error = function(e) {
    message("archive_extract error: ", e$message)
    # Fallback: try archive_read to stream into R, instead of extracting to file system
    message("Trying fallback via archive_read …")
    con <- archive_read(local_zip, file = 1)  # read first file inside
    dt <- tryCatch(
      fread(con, sep = ";", encoding = "Latin-1", na.strings = c("", "NA")),
      error = function(e2) stop("fread fallback failed: ", e2$message)
    )
    close(con)
    return(dt)
  }) -> extract_ok
  
  # If extraction succeeded, read the .txt
  if (is.logical(extract_ok) && extract_ok) {
    message("Extraction successful. Reading .txt …")
    txts <- list.files(extract_dir, pattern = "\\.txt$", full.names = TRUE)
    if (length(txts) == 0) stop("No .txt file found after extraction.")
    
    # Read it
    dt <- fread(txts[1], sep = ";", encoding = "Latin-1", na.strings = c("", "NA"))
    return(dt)
  }
  
  # If fallback returned already, dt will be returned above
}

# Try it
rais_estab_2018 <- download_extract_estab_2018()
head(rais_estab_2018)

file.info("RAIS_ESTAB_PUB_2018.7z")$size

### RAIS 2017 -----
library(curl)
library(archive)
library(data.table)

# Function to download + extract one UF file for 2017
extract_rais_uf_2017 <- function(uf, year = 2017) {
  
  # Build file name & URL
  fname <- paste0(toupper(uf), year, ".7z")  
  url <- sprintf("ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/%d/%s", year, fname)
  
  local_zip <- fname
  extract_dir <- paste0("RAIS_2017_", uf, "_extracted")
  
  message("Processing UF: ", uf, " / URL: ", url)
  
  # Download
  tryCatch({
    curl_download(url, destfile = local_zip, mode = "wb")
  }, error = function(e) {
    message("Download error for ", uf, ": ", e$message)
    return(NULL)
  })
  
  if (!file.exists(local_zip)) {
    message("File not downloaded for ", uf)
    return(NULL)
  }
  if (file.info(local_zip)$size == 0) {
    message("Downloaded file has zero size for ", uf)
    return(NULL)
  }
  
  # Clean extraction directory
  if (dir.exists(extract_dir)) {
    unlink(extract_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(extract_dir)
  
  # Try extract via archive
  extract_ok <- tryCatch({
    archive_extract(local_zip, dir = extract_dir)
    TRUE
  }, error = function(e) {
    message("archive_extract failed for ", uf, ": ", e$message)
    FALSE
  })
  
  # If archive_extract succeeded
  if (extract_ok) {
    txts <- list.files(extract_dir, pattern = "\\.txt$", full.names = TRUE)
    if (length(txts) == 0) {
      message("No .txt inside for ", uf)
      return(NULL)
    }
    dt <- tryCatch({
      fread(txts[1], sep = ";", encoding = "Latin-1", na.strings = c("", "NA"))
    }, error = function(e) {
      message("fread failed for ", uf, ": ", e$message)
      NULL
    })
    return(dt)
  }
  
  # Fallback: use system 7-Zip
  message("Trying 7-Zip system fallback for ", uf)
  cmd <- sprintf('"%s" x "%s" -o"%s"', 
                 "C:/Program Files/7-Zip/7z.exe", 
                 local_zip, 
                 extract_dir)
  status <- system(cmd)
  if (status == 0) {
    txts2 <- list.files(extract_dir, pattern = "\\.txt$", full.names = TRUE)
    if (length(txts2) > 0) {
      dt2 <- tryCatch({
        fread(txts2[1], sep = ";", encoding = "Latin-1", na.strings = c("", "NA"))
      }, error = function(e) {
        message("fread (fallback) failed for ", uf, ": ", e$message)
        NULL
      })
      return(dt2)
    } else {
      message("No .txt found via fallback for ", uf)
      return(NULL)
    }
  } else {
    message("7-Zip system call failed for ", uf)
    return(NULL)
  }
}

# Example: run for several UFs
ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")

# Loop and store results in a list
rais2017_by_uf <- lapply(ufs, extract_rais_uf_2017)

names(rais2017_by_uf) <- ufs

# Inspect which UFs succeeded
succeeded <- sapply(rais2017_by_uf, function(x) !is.null(x))
print(succeeded)
