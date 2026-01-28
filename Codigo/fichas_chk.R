# ============================================================
# Plantas Para Polinizadores - Asociación Abejas Silvetres
# Proyecto divulgación AAS, SGHN
# Script: Julia G. de Aledo
# Date: 16/01/2026
# ============================================================

# remotes::install_github("Pakillo/FloraIberica")

library(readxl)
library(labeleR)
library(tidyr)
library(dplyr)
library(googlesheets4)
library(gsheet)
library(rgbif)
library(FloraIberica)
library(colouR)
library(R.utils)
library(ggplot2)
library(stringr)
library(pdftools)
library(here)
here()
# Custom functions used by labeleR templates
source("Codigo/fichas_functions.R")


# ============================================================
# 1. Read species database from Google Sheets
# ============================================================

sheet_url <- "https://docs.google.com/spreadsheets/d/1MVFOgBXAuX9XTjhNQlLsHam-NVludLXLISpJOgJ0TQg/edit?usp=sharing"

df_sp <- read_sheet(sheet_url, sheet = 2)

# Keep original species name
SP <- df_sp$especie

# Split species into genus and species (first space)
df_sp <- df_sp %>%
  separate(
    col = especie,
    into = c("genus", "species"),
    sep = " ",
    remove = FALSE
  )

df_sp$especie <- SP


# ============================================================
# 2. Build image paths
# ============================================================

base_img <- "/Users/juliag.dealedo/ONE/Postdoc/Colaboraciones/beelab/images/"
base_icon <- "/Users/juliag.dealedo/ONE/Postdoc/Colaboraciones/beelab/icon/"

# Main species image
df_sp$imagen <- paste0(
  base_img,
  gsub(" ", "_", df_sp$especie),
  ".jpg"
)


# ============================================================
# 3. Pollinator icons (with fallbacks)
# ============================================================

df_sp$icon1 <- ifelse(
  is.na(df_sp$Abejas),
  paste0(base_icon, "Abejas_grey.png"),
  paste0(base_icon, df_sp$Abejas, ".png")
)

df_sp$icon2 <- ifelse(
  is.na(df_sp$Sirfidos),
  paste0(base_icon, "Sirfidos_grey.png"),
  paste0(base_icon, df_sp$Sirfidos, ".png")
)

df_sp$icon3 <- ifelse(
  is.na(df_sp$Mariposas),
  paste0(base_icon, "Mariposas_grey.png"),
  paste0(base_icon, df_sp$Mariposas, ".png")
)

df_sp$icon4 <- ifelse(
  is.na(df_sp$Escarabajos),
  paste0(base_icon, "Escarabajos_grey.png"),
  paste0(base_icon, df_sp$Escarabajos, ".png")
)


# ============================================================
# 5. Join with Flora Iberica presence data
# ============================================================

present_flora = data.frame (especie=df_sp$especie, 
                            present = is_present(genus = df_sp$genus, 
                                                 species = df_sp$species))

df_mod_original <- present_flora %>%
  left_join(df_sp, by = "especie") %>%
  filter(present != FALSE)

# Convert empty strings to NA
df_sp[df_sp == ""] <- NA

# Normalize selected text fields
df_mod_original <- df_mod_original %>%
  mutate(across(
    c(porte, flor, color_categorico, requisito_clima, requisito_suelo),
    tolower
  ))

df_mod_or <- df_mod_original

# ============================================================
# 6. Create PDF panels with labeleR
# ============================================================

crear_ficha(
  data = df_mod_or,
  path = "labeleR_output_5",
  filename = "fichas",
  subtitle = "Plantas para polinizadores",
  lpic = "/Users/juliag.dealedo/ONE/Postdoc/Colaboraciones/beelab/lpic.png",
  rpic = "/Users/juliag.dealedo/ONE/Postdoc/Colaboraciones/beelab/rpic.png",
  imagen.column = "imagen",
  credito.column = "author_contribution",
  family.column = "familia",
  taxon.column = "especie",
  species.column = "species",
  genus.column = "genus",
  vernaculo.column = "nombre_comun",
  color.column = "color_categorico",
  floracion.column = "meses_floracion",
  habito.column = "porte",
  clima.column = "requisito_clima",
  suelo.column = "requisito_suelo",
  icon1.column = "icon1",
  icon2.column = "icon2",
  icon3.column = "icon3",
  icon4.column = "icon4",
  template = "Codigo/fichas2.Rmd"
)


# ============================================================
# 7. Combine all generated PDFs into one
# ============================================================

output_dir <- "/Users/juliag.dealedo/ONE/Postdoc/Colaboraciones/beelab/labeleR_output_5"

pdf_files <- list.files(
  path = output_dir,
  pattern = "\\.pdf$",
  full.names = TRUE
)

pdf_combine(
  input  = pdf_files,
  output = file.path(output_dir, "archivo_final.pdf")
)


# gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -sOutputFile=archivo_comprimido2.pdf archivo_final.pdf
