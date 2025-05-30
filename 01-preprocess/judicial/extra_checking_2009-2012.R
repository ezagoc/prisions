###################################################
## Data Preprocessing
## Author: Eduardo Zago-Cuevas (all errors are my own)
## Run before: same folder, a number before
## Output: Judicial panel dataset 2009-2012
##
###################################################

# install.packages('pacman')

pacman::p_load(tidyverse, arrow, foreign)

rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Paths:

pf <- '../../../data/01-judicial/00-sentencing/raw/Judiciales_BD_Catalogos_2009_dbf/'

# First read sreg, preg, pdel

preg <- read.dbf(paste0(pf, 'TablasMicrodatos_2009/preg2009.DBF'))

sreg <- read.dbf(paste0(pf, 'TablasMicrodatos_2009/sreg2009.DBF'))

pdel <- read.dbf(paste0(pf, 'TablasMicrodatos_2009/pdel2009.DBF'))

# From CENTIDAD.dbf we know: if state > 32 then it is NA (other countries, NA)

entidad <-  read.dbf(paste0(pf, 'CatalogosMicrodatos_2009/CENTIDAD.DBF'))

# Check pdel. What to do, first keep the important crime 

pdel.c <- pdel |> filter(IMPORTANTE == T)

# Check the delitos codebook:

del <- read.dbf(paste0(pf, 'CatalogosMicrodatos_2009/CDEL2009.DBF'), as.is = T) 

del <- del |> mutate(
  DESC_DEL = DESC_DEL %>%
    str_replace_all("<a2>", "ó") %>%
    str_replace_all("<b4>", "ñ") %>%
    str_replace_all("<e1>", "á") %>%
    str_replace_all("<e9>", "é") %>%
    str_replace_all("<ed>", "í") %>%
    str_replace_all("<fa>", "ú")
) 

read.dbf("file.dbf", as.is = TRUE)
Encoding(del$DESC_DEL) <- "latin1"
del$DESC_DEL <- iconv(del$DESC_DEL, from = "latin1", to = "UTF-8")


  mutate(DESC_DEL = iconv(DESC_DEL, from = "latin1", to = "UTF-8"))
  mutate(
    DESC_DEL = DESC_DEL %>%
      str_replace_all("<a2>", "ó") %>%
      str_replace_all("<b4>", "ñ") %>%
      str_replace_all("<e1>", "á") %>%
      str_replace_all("<e9>", "é") %>%
      str_replace_all("<ed>", "í") %>%
      str_replace_all("<fa>", "ú")
  ) 

# It matches the preg data set: However, we do not want to lose info on these crimes.
# Lets keep the important one as crime_1_id, and the others as crime_1_id

# Pivot wider:
pdel.w <- pdel |> distinct(ID_PS, B_DELITO, .keep_all = T) |> group_by(ID_PS) |> 
  mutate(B_NUMDEL = row_number()) |> ungroup() |>
  mutate(B_NUMDEL = paste0('crime_', B_NUMDEL)) |>
  pivot_wider(
    id_cols = c(ID_PS),
    names_from = B_NUMDEL,                        
    values_from = B_DELITO                             
  )

pdel.wd <- pdel.w |> filter(is.na(crime_9) == F) # Seems correct

# Now merge back 
# Check for NAs on relevant variables: sentence, mun_rh, st_rh, date sentence.

preg.na <- preg |> mutate(across(c(B_MESREG:B_TOTDEL), ~as.numeric(.x))) |> filter(B_ENTRH<32)

# check total number of different code inegis (residence)
preg.unique <- preg.na |> mutate(code_inegi_rh = B_ENTRH*1000+B_MUNRH) |> 
  distinct(code_inegi_rh)

preg.auto <- preg |> filter(is.na(B_FAUTO) == F)
