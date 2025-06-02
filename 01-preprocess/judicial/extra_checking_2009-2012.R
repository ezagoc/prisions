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

# Import municipality panel

munpan <- read_parquet('../../../data/00-map/municipalities.parquet')

preg <- read.dbf(paste0(pf, 'TablasMicrodatos_2009/preg2009.DBF'), as.is = T)

sreg <- read.dbf(paste0(pf, 'TablasMicrodatos_2009/sreg2009.DBF'))

pdel <- read.dbf(paste0(pf, 'TablasMicrodatos_2009/pdel2009.DBF'), as.is = T)

# From CENTIDAD.dbf we know: if state > 32 then it is NA (other countries, NA)

entidad <-  read.dbf(paste0(pf, 'CatalogosMicrodatos_2009/CENTIDAD.DBF'))

# Check pdel. What to do, first keep the important crime 

# Matches the length of the other data set (processed one)


# It matches the preg data set: However, we do not want to lose info on these crimes.
# Lets keep the important one as crime_1_id, and the others as crime_1_id
# Import coded crimes 

coded <- readxl::read_excel(paste0(pf, 
                                   'CatalogosMicrodatos_2009/CDEL2009_coded.xlsx')) |>
  rename(B_DELITO = CVE_DEL)

# We take all the data from the important crime, that is our deduplicated dataset by year

pdel <- pdel |> left_join(coded, by = c('B_DELITO')) |> arrange(ID_PS)

pdel.ind <- pdel |> filter(IMPORTANTE == T)

# Pivot wider:
pdel.w <- pdel |> distinct(ID_PS, B_DELITO, .keep_all = T) |> group_by(ID_PS) |> 
  mutate(B_NUMDEL = row_number()) |> ungroup()|> arrange(ID_PS) 

pdel.w <- pdel.w |> mutate(snieg_code = paste0('crime_', snieg_code)) |>
  select(ID_PS, snieg_code) |>
  mutate(dummy = 1)
  
pdel.w <- pdel.w |> distinct(ID_PS, snieg_code, .keep_all = T) |> pivot_wider(
    id_cols = ID_PS,
    names_from = snieg_code,
    values_from = dummy,
    values_fill = 0
  )

# But we know which other crimes committed

pdel.ind <- pdel.ind |> left_join(pdel.w, by = ('ID_PS'))

# Now we clean it 

pdel.ind <- pdel.ind |> 
  mutate(date_ocu = dmy(B_FOCU),         # Convert to Date format
         day_ocu = day(date_ocu),
         month_ocu = month(date_ocu),
         year_ocu = year(date_ocu),
         CVE_ENT_ocu = ifelse(B_ENTOC != '99', as.numeric(B_ENTOC), NA),  
         CVE_MUN_ocu = ifelse(B_MUNOC != '999', as.numeric(B_MUNOC), NA), 
         code_inegi_ocu = CVE_ENT_ocu*1000 + CVE_MUN_ocu, 
         prison_immed = ifelse(B_AUTO == 1, 1, 0), 
         juicio = ifelse(B_AUTO == 2, 1, 0),
         preventive = ifelse(B_AUTO == 5, 1, 0),
         free = ifelse(B_AUTO %in% c(4, 6, 0), 1, 0), 
         petty = ifelse(B_CALIFICA == 2, 1, 0), 
         federal_imp = ifelse(B_FUERO == 2, 1, 0))

pdel.ind <- pdel.ind |> select(ID_PS, code_delito = B_DELITO, DESC_DEL:federal_imp)
# Now merge back 
# Check for NAs on relevant variables: sentence, mun_rh, st_rh, date sentence.

preg <- preg |> mutate(across(c(B_MESREG:B_TOTDEL), ~as.numeric(.x))) |> 
  mutate(federal = ifelse(B_CVEESTAD == 42, 1, 0),
         CVE_ENT_reg = ifelse(B_ENTREG > 32, B_ENTREG, NA),
         CVE_MUN_reg = B_MUNREG, 
         code_inegi_reg = CVE_ENT_reg*1000 + CVE_MUN_reg, 
         CVE_ENT_nac = ifelse(B_ENTNAC > 32, B_ENTNAC, NA),
         CVE_MUN_nac = ifelse(B_MUNNAC == 999, B_MUNNAC, NA), 
         code_inegi_nac = CVE_ENT_nac*1000 + CVE_MUN_nac, 
         CVE_ENT_rh = ifelse(B_ENTRH > 32, B_ENTRH, NA),
         CVE_MUN_rh = ifelse(B_MUNRH == 999, B_MUNRH, NA), 
         code_inegi_rh = CVE_ENT_rh*1000 + CVE_MUN_rh,
         )

### After: Generate a marginalized index: LI, Ocupation, Conalf, Nivel In, 

preg <- preg |> 
# check total number of different code inegis (residence)
preg.unique <- preg.na |> mutate(code_inegi_rh = B_ENTRH*1000+B_MUNRH) |> 
  distinct(code_inegi_rh)

preg.auto <- preg |> filter(is.na(B_FAUTO) == F)


# Check if we can merge sreg and preg
