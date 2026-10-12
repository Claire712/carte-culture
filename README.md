# Projet de carte culturelle

**[Voir la carte interactive](https://claire712.github.io/carte-culture/)**

## Présentation

Ce projet est une carte interactive présentant des lieux culturels de France métropolitaine :
les **Musées de France**, les **Jardins remarquables** et les **Maisons des Illustres**.

Chaque lieu est représenté par un point dont la couleur dépend de sa catégorie.
Il est possible de filtrer les lieux par région et par statut des jardins (public ou privé),
ainsi que de rechercher un lieu précis par son nom.

En cliquant sur un point, une fiche s'affiche avec les informations relatives au lieu avec
adresse, numéro de téléphone, lien vers le site internet, etc.

## Données

Les données sont issues de plusieurs bases open data du ministère de la Culture, disponibles sur
[data.gouv.fr](https://www.data.gouv.fr).

La base finale a été produite avec le langage R. Le traitement prélable contient une sélection des variables utiles, exclusion des territoires
d'outre-mer, normalisation des numéros de téléphone, des sites web et des adresses, puis fusion
des trois bases.

## Réalisation

L'application a été développée avec **Shiny** et **Leaflet**, puis convertie en site statique
avec **Shinylive** pour être hébergée gratuitement sur GitHub Pages.


## Outils

R · dplyr · stringr · Leaflet Shiny  · Shinylive · GitHub Pages
