# =============================================================
# Carte des lieux culturels en France métropolitaine
# -------------------------------------------------------------
# Jardins remarquables, Musées de France et Maisons des Illustres :
# trois labels du ministère de la Culture, issus des données
# ouvertes publiées sur data.gouv.fr.
#
# La base "lieux.rds" a été nettoyée et fusionnée en amont
# (voir le script de nettoyage).
# =============================================================


# 1. Packages ---------------------------------------------------
library(shiny)    # création de l'application
library(bslib)    # mise en page (bandeau + carte)
library(leaflet)  # carte interactive
library(dplyr)    # manipulation des données


# 2. Données ----------------------------------------------------
lieux <- readRDS("lieux.rds")

# Nom complet de chaque catégorie, affiché dans la fiche
libelles <- c(jardin = "Jardin remarquable",
              musee  = "Musée de France",
              maison = "Maison des Illustres")

# Couleur des points sur la carte, une par catégorie
couleurs <- c(jardin = "#2e7d32", musee = "#c62828", maison = "#1565c0")


# 3. Fiche affichée au clic sur un point --------------------------
# Une information par ligne. Si une information est absente,
# la ligne n'est pas affichée.
ligne <- function(label, valeur) {
  ifelse(is.na(valeur) | valeur %in% c("", "Non renseigné"),
         "",
         paste0(label, " : ", valeur, "<br>"))
}

lieux$fiche <- paste0(
  "<b>", lieux$nom, "</b><br>",
  ligne("Type",      libelles[lieux$bien]),
  ligne("Adresse",   lieux$adresse_complete),
  ligne("Téléphone", lieux$telephone),
  ligne("Statut",    lieux$statut),   # jardins uniquement
  ligne("Style",     lieux$style),    # jardins uniquement
  ligne("Site",      ifelse(is.na(lieux$site_web) | lieux$site_web == "Non renseigné",
                            NA,
                            paste0("<a href='", lieux$site_web, "' target='_blank'>",
                                   lieux$site_web, "</a>")))
)


# 4. Interface ------------------------------------------------------
ui <- page_sidebar(
  title = "Lieux culturels en France métropolitaine",
  
  # Bandeau de gauche : filtres et présentation
  sidebar = sidebar(
    checkboxGroupInput("bien", "Type de lieu",
                       choices = c("Jardins remarquables"  = "jardin",
                                   "Musées de France"      = "musee",
                                   "Maisons des Illustres" = "maison"),
                       selected = c("jardin", "musee", "maison")),
    
    radioButtons("statut", "Statut des jardins",
                 choices = c("Tous", "Public", "Privé")),
    
    # Choix d'une région (liste construite à partir des données)
    selectInput("region", "Région",
                choices = c("Toutes les régions", sort(unique(na.omit(lieux$region))))),
    
    # Recherche d'un lieu par son nom
    textInput("recherche", "Rechercher un lieu", placeholder = "exemple: Musee orsay"),
    
    textOutput("compteur"),
    
    hr(),
    p("Données : ministère de la Culture, via data.gouv.fr.",
      "Cliquez sur un point pour afficher ses informations.")
  ),
  
  # Zone principale : la carte
  leafletOutput("carte", height = "100%")
)


# 5. Serveur --------------------------------------------------------
server <- function(input, output, session) {
  
  # Lieux à afficher selon les filtres choisis
  donnees <- reactive({
    d <- filter(lieux, bien %in% input$bien)
    if (input$statut != "Tous") {
      # Seuls les jardins ont un statut : les autres lieux restent affichés
      d <- filter(d, bien != "jardin" | grepl(input$statut, statut))
    }
    if (input$region != "Toutes les régions") {
      d <- filter(d, region == input$region)
    }
    if (nzchar(input$recherche)) {
      # Recherche sans tenir compte des majuscules
      d <- filter(d, grepl(input$recherche, nom, ignore.case = TRUE))
    }
    d
  })
  
  # Fond de carte OpenStreetMap centré sur la France, avec la légende.
  # La légende utilise les mêmes vecteurs que les points
  # (couleurs et libelles) : elle correspond toujours à la carte.
  output$carte <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 2.5, lat = 46.6, zoom = 6) |>
      addLegend("bottomright",
                colors = unname(couleurs),
                labels = unname(libelles),
                title  = "Type de lieu")
  })
  
  # Points : mis à jour à chaque changement de filtre
  observe({
    d <- donnees()
    leafletProxy("carte") |>
      clearMarkers() |>
      addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.8,
                       fillColor = unname(couleurs[d$bien]),
                       popup = ~fiche)
  })
  
  # Nombre de lieux affichés
  output$compteur <- renderText(paste(nrow(donnees()), "lieux affichés"))
}


# 6. Lancement --------------------------------------------------------
shinyApp(ui, server)