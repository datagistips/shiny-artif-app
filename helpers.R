getStatsFlux <- function(flux, codeInsee) {
  
  myCols <-  names(flux)[grep("^art[0-9]{2}\\S+[0-9]{2}$", names(flux))]
  
  # Filtre par commune
  df <- flux %>% filter(idcom == codeInsee) 
  
  # Colonnes intéressantes
  df <- df[, c("idcom", "idcomtxt", myCols)]
  
  # Long format
  df <- df %>% gather("variable", # key
                      "value",    # value
                      myCols) # variables
  
  # Année et type
  df$year <- gsub("art([0-9]{2})(\\S+)[0-9]{2}", "20\\1", df$variable)
  df$type <- gsub("art([0-9]{2})(\\S+)[0-9]{2}", "\\2", df$variable)
  
  # Renomme les valeurs de type
  df <- df %>% mutate(type = case_when(
    type == "hab" ~ "Habitat",
    type == "act" ~ "Activité",
    type == "mix" ~ "Mixte",
    type == "inc" ~ "Inconnu"))
  
  # Réagence les colonnes
  df <- df[, c("idcom", "idcomtxt", "year", "type", "value")]
  
  return(df)
}

makeStream <- function(flux, codeInsee) {
  
  df <- flux %>% getStatsFlux(codeInsee)
  
  # L'ordre des inverse dans les streamgraphs
  df$type <- factor(df$type, levels = c("Inconnu", "Mixte", "Activité", "Habitat"))
  
  # Plot
  df %>%
    streamgraph("type", "value", "year", sort = FALSE, height = '350px') %>%
    sg_axis_x(1, "Année", "%Y") %>% 
    sg_fill_manual(rev(myPalette))
}

makeTreemap <- function(flux, codeInsee) {
  df <- flux %>% filter(idcom == codeInsee)
  
  df <- df %>% gather("variable",
                      "value",
                      c("arthab0920", "artact0920", "artmix0920", "artinc0920"))
  
  df$variable <- case_when(
    df$variable == "arthab0920" ~ "Habitat",
    df$variable == "artact0920" ~ "Activité",
    df$variable == "artmix0920" ~ "Mixte",
    df$variable == "artinc0920" ~ "NC"
  )
  
  # Par. treemap
  labels = df$variable
  parents = rep("", nrow(df))
  values = df$value
  
  fig <- plot_ly(
    type="treemap",
    labels=labels,
    parents=parents,
    values=values,
    marker=list(colors = myPalette))
  
  fig
}

getCommCenter <- function(comms, coords) {
  pt <- coords %>% st_point %>% st_sfc %>% st_set_crs(4326)
  i <- st_intersects(comms, pt)
  w <- which(sapply(i, function(x) length(x) != 0))
  codeInsee <- comms$INSEE_COM[w]
  return(codeInsee)
}