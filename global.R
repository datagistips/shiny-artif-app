library(shiny)
library(tidyverse)
library(sf)
library(glue)
library(streamgraph)
library(rjson)
library(leaflet)
library(plotly)

# Flux sur PACA
flux <- read_csv("data/obs_artif_conso_com_2009_2020_V2.csv", na = c("", "NULL")) %>% 
  filter(idreg == "93")

# Contours de communes
comms <- readRDS("data/comms.rds") %>% 
  filter(INSEE_REG == 93)

# Liste communes
communes <- flux$idcom
names(communes) <- glue("{flux$idcomtxt} ({flux$idcom})")
communes <- communes[order(names(communes))]
communes2 <- c("", communes)
names(communes2) <- c("Aller vers une commune", names(communes))

# Palette 
colorBlue    <- hcl(h = 220, c = 50, l = 80, fixup = TRUE)
colorRed     <- hcl(h = 4, c = 50, l = 80, fixup = TRUE)
colorMagenta <- hcl(h = 300, c = 50, l = 80, fixup = TRUE)
colorGrey    <- hcl(h = 0, c = 0, l = 80, fixup = TRUE)

myPalette <- c("blue" = colorBlue,
                "red" = colorRed,
                "magenta" = colorMagenta,
                "grey" = colorGrey)

# Palette de couleurs
paletteCerema <- fromJSON(file = "palette_cerema.json")