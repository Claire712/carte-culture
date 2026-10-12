# =============================================================
# Carte des lieux culturels en France métropolitaine
# Nettoyage et fusion : jardins remarquables, musées de France,
# maisons des Illustres
# =============================================================

# 0. Packages et données ---------------------------------------

library(dplyr)
library(stringr)
library(purrr)
library(readxl)

# Chemins relatifs : ouvrir le projet RStudio (.Rproj) du dossier "culture"
# et placer les fichiers de données dans un sous-dossier "data/"
setwd('/Users/claireiannetta/Desktop/Portfolio/culture')
jardin <- read.csv2("base_des_lieux_labels_jardins_remarquables.csv")
site   <- read.csv2("base_des_lieux_labels_maisons_des_illustres.csv")
musee  <- read_excel("liste-officielle-museesdf-20220127-data.xlsx")


# 1. Nettoyer les données ---------------------------------------

## 1.1. Jardins ------------------------------------------------

### Enlever les régions d'outre-mer et les codes région manquants
jardin <- jardin %>%
  filter(!is.na(code_region), !code_region %in% c(1, 2, 3, 4, 6))

### Garder les bonnes colonnes
jardin_final <- jardin %>%
  select(nom_du_jardin, code_postal, region, departement, adresse_complete,
         commune, latitude, longitude, telephones,
         site_internet_et_autres_liens, types)

### Statut (privé / public) et style(s) du jardin
jardin_final <- jardin_final %>%
  mutate(
    statut = case_when(
      str_detect(types, "Privé") & str_detect(types, "Public") ~ "Privé et public",
      str_detect(types, "Privé")  ~ "Privé",
      str_detect(types, "Public") ~ "Public",
      TRUE ~ "Non renseigné"
    ),
    style = types |>
      str_remove_all('\\[|\\]|"') |>
      str_split(",\\s*") |>
      map_chr(~ {
        x <- str_trim(.x)
        x <- x[!is.na(x) & str_starts(x, "Jardin|Parc")]
        if (length(x) == 0) "Non renseigné" else paste(x, collapse = ", ")
      })
  )

### Téléphone : premier numéro, chiffres et + uniquement
jardin_final <- jardin_final %>%
  mutate(
    telephone = str_extract(telephones, '(?<=")[^"]+(?=")'),
    telephone = str_remove_all(telephone, "[^0-9+]"),
    telephone = if_else(is.na(telephone) | telephone == "", "Non renseigné", telephone)
  )

### Site web : premier lien
jardin_final <- jardin_final %>%
  mutate(
    site_web = str_extract(site_internet_et_autres_liens, "https?://[^\"',;\\]\\s]+"),
    site_web = coalesce(site_web, "Non renseigné")
  )

### Adresse
jardin_final <- jardin_final %>%
  mutate(adresse_complete = if_else(
    is.na(adresse_complete) | str_trim(adresse_complete) == "",
    "Non renseigné", adresse_complete
  ))

### Supprimer les colonnes brutes et ajouter la catégorie
jardin_final <- jardin_final %>%
  select(-telephones, -site_internet_et_autres_liens, -types) %>%
  rename(nom = nom_du_jardin) %>%
  mutate(bien = "jardin")

rm(jardin)


## 1.2. Musées -------------------------------------------------

### Garder et renommer les colonnes
musee_final <- musee %>%
  select(
    nom              = `Nom officiel du musée`,
    code_postal      = `Code Postal`,
    region           = `Région administrative`,
    departement      = `Département`,
    adresse_complete = `Adresse`,
    commune          = `Commune`,
    latitude         = `Latitude`,
    longitude        = `Longitude`,
    telephone        = `Téléphone`,
    site_web         = `URL`
  )

### Enlever l'outre-mer
musee_final <- musee_final %>%
  filter(!str_detect(region, regex("outre-mer", ignore_case = TRUE)) | is.na(region))

### Téléphone : chiffres et + uniquement, premier numéro
musee_final <- musee_final %>%
  mutate(
    telephone = str_remove_all(telephone, "[^0-9+]"),
    telephone = if_else(str_starts(telephone, "\\+"),
                        str_sub(telephone, 1, 12),
                        str_sub(telephone, 1, 10)),
    telephone = if_else(is.na(telephone) | telephone == "", "Non renseigné", telephone)
  )

### Adresse
musee_final <- musee_final %>%
  mutate(adresse_complete = if_else(
    is.na(adresse_complete) | str_trim(adresse_complete) == "",
    "Non renseigné", adresse_complete
  ))

### Site web : premier lien, avec https:// si l'adresse commence par www.
musee_final <- musee_final %>%
  mutate(
    site_web = str_extract(site_web, "https?://[^\"',;\\]\\s]+|www\\.[^\"',;\\]\\s]+"),
    site_web = if_else(str_starts(site_web, "www\\."), paste0("https://", site_web), site_web),
    site_web = coalesce(site_web, "Non renseigné"),
    bien     = "musee"
  )

rm(musee)


## 1.3. Maisons des Illustres -----------------------------------

### Garder et renommer les colonnes
maison_final <- site %>%
  select(
    nom              = nom_mdi,
    code_postal,
    code_region,
    region,
    departement,
    adresse_complete,
    commune,
    latitude,
    longitude,
    telephone        = telephones,
    site_web         = site_internet_et_autres_liens
  )

### Enlever l'outre-mer, l'étranger et les codes région manquants
maison_final <- maison_final %>%
  filter(!is.na(code_region), !code_region %in% c(1, 2, 3, 4, 6))

### Téléphone : premier numéro, chiffres et + uniquement
maison_final <- maison_final %>%
  mutate(
    telephone = str_extract(telephone, '(?<=")[^"]+(?=")'),
    telephone = str_remove_all(telephone, "[^0-9+]"),
    telephone = if_else(is.na(telephone) | telephone == "", "Non renseigné", telephone)
  )

### Site web : premier lien
maison_final <- maison_final %>%
  mutate(
    site_web = str_trim(str_extract(site_web, '(?<=")[^"]+(?=")')),
    site_web = if_else(is.na(site_web) | site_web == "", "Non renseigné", site_web)
  )

### Adresse
maison_final <- maison_final %>%
  mutate(
    adresse_complete = if_else(
      is.na(adresse_complete) | str_trim(adresse_complete) == "",
      "Non renseigné", adresse_complete
    ),
    bien = "maison"
  )

rm(site)


# 2. Fusionner les bases ----------------------------------------

### Harmoniser les types de colonnes
harmoniser <- function(df) {
  df %>%
    mutate(
      across(any_of(c("latitude", "longitude")), ~ as.numeric(as.character(.x))),
      across(any_of(c("code_region", "telephone", "site_web", "bien")), as.character),
      code_postal = str_pad(as.character(code_postal), width = 5, pad = "0")
    )
}

### Concaténer (les colonnes absentes d'une base sont remplies avec NA)
lieux <- bind_rows(
  harmoniser(jardin_final),
  harmoniser(musee_final),
  harmoniser(maison_final)
)

### Retirer les lieux sans coordonnées (impossibles à placer sur la carte)
lieux <- lieux %>%
  filter(!is.na(latitude), !is.na(longitude))

### Vérifications
count(lieux, bien)
glimpse(lieux)

# 3. Sauvegarder la base pour l'application ---------------------
saveRDS(lieux, "lieux.rds")
