# 
# Carte des lieux culturels en France métropolitaine
#
# Jardins remarquables, Musées de France et Maisons des Illustres
# (labels du ministère de la Culture, données ouvertes data.gouv.fr)
#
# Les trois bases ont été nettoyées et fusionnées au préalable
# dans un script de préparation ; l'app lit directement le résultat
# (lieux.rds donc format rds).
# 

library(shiny)
library(bslib)
library(leaflet)
library(dplyr)


# Données -------------------------------------------------------

lieux <- readRDS("lieux.rds")

# Libellés et couleurs par catégorie : définis une seule fois et
# réutilisés pour les points, la légende et les fiches
libelles <- c(jardin = "Jardin remarquable",
              musee  = "Musée de France",
              maison = "Maison des Illustres")

couleurs <- c(jardin = "#2e7d32", musee = "#c62828", maison = "#1565c0")


# Fiches (points)

# Renvoie les infos renseigné dans la base de donnée
# ou rien si l'information n'est pas renseignée
ligne <- function(label, valeur) {
  ifelse(is.na(valeur) | valeur %in% c("", "Non renseigné"),
         "",
         paste0(label, " : ", valeur, "<br>"))
}

lien_site <- ifelse(
  is.na(lieux$site_web) | lieux$site_web == "Non renseigné",
  NA,
  paste0("<a href='", lieux$site_web, "' target='_blank'>", lieux$site_web, "</a>")
)

lieux$fiche <- paste0(
  "<b>", lieux$nom, "</b><br>",
  ligne("Type",      libelles[lieux$bien]),
  ligne("Adresse",   lieux$adresse_complete),
  ligne("Téléphone", lieux$telephone),
  ligne("Statut",    lieux$statut),  # jardins uniquement
  ligne("Style",     lieux$style),   # jardins uniquement
  ligne("Site",      lien_site)
)


# Interface de la page

ui <- page_sidebar( #la  bare de coté
  title = "Lieux culturels en France métropolitaine",
  
  sidebar = sidebar(
    checkboxGroupInput("bien", "Type de lieu",
                       choices = c("Jardins remarquables"  = "jardin",
                                   "Musées de France"      = "musee",
                                   "Maisons des Illustres" = "maison"),
                       selected = names(libelles)),
    
    # le statut public/privé n'existe que dans la base des jardins ...
    radioButtons("statut", "Statut des jardins",
                 choices = c("Tous", "Public", "Privé")),
    
    selectInput("region", "Région",
                choices = c("Toutes les régions", sort(unique(na.omit(lieux$region))))),
    
    textInput("recherche", "Rechercher un lieu", placeholder = "ex. Giverny"),
    
    textOutput("compteur"),
    
    hr(),
    p("Données : ministère de la Culture, via data.gouv.fr.",
      "Cliquez sur un point pour afficher ses informations.")
  ),
  
  leafletOutput("carte", height = "100%")
)


# Serveur de l'app

server <- function(input, output, session) {
  
  # Application successive des filtres
  donnees <- reactive({
    d <- filter(lieux, bien %in% input$bien)
    
    if (input$statut != "Tous") {
      # on ne filtre que les jardins, les autres lieux restent visibles
      d <- filter(d, bien != "jardin" | grepl(input$statut, statut))
    }
    if (input$region != "Toutes les régions") {
      d <- filter(d, region == input$region)
    }
    if (nzchar(input$recherche)) {
      d <- filter(d, grepl(input$recherche, nom, ignore.case = TRUE))
    }
    d
  })
  
  # Fond de carte et légende
  output$carte <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 2.5, lat = 46.6, zoom = 6) |>
      addLegend("bottomright",
                colors = unname(couleurs),
                labels = unname(libelles),
                title  = "Type de lieu")
  })
  
  # Mise à jour des points
  observe({
    d <- donnees()
    leafletProxy("carte") |>
      clearMarkers() |>
      addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude,
                       radius = 5, stroke = FALSE, fillOpacity = 0.8,
                       fillColor = unname(couleurs[d$bien]),
                       popup = ~fiche)
  })
  
  output$compteur <- renderText(paste(nrow(donnees()), "lieux affichés"))
}


shinyApp(ui, server)