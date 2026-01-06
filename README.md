Projet : Analyse et visualisation des détections de feux en Grèce (R)

Description
 - Ce projet réalise une analyse statistique, temporelle et spatiale d'un jeu de données de détections de feux nommé `cleaned.csv`.
 - L'objectif : produire des graphiques (histogrammes, séries temporelles, cartes statiques et interactives) et des analyses exploratoires des relations feu–météo.

Contenu du dépôt
 - `analysis_fire.R` : script R principal qui effectue l'import, le nettoyage, les analyses et génère les sorties dans `outputs/`.
 - `cleaned.csv` : fichier de données d'entrée (géolocalisation, luminosité, FRP, variables météo, dates).
 - `outputs/` : dossier généré contenant images PNG, CSV de résumés et `interactive_map.html` (avec dossier compagnon).

Colonnes attendues (exemples)
 - `latitude`, `longitude`, `acq_date`
 - `brightness`, `frp`, `confidence`, `daynight`, `type`
 - `tavg`, `tmin`, `tmax`, `prcp`, `wspd`

Pré-requis
 - R (version 4.0 ou supérieure recommandée)
 - Connexion Internet pour installer les packages la première fois

Packages R utilisés
 - `tidyverse` (dont `dplyr`, `ggplot2`, `readr`)
 - `lubridate` (gestion des dates)
 - `leaflet` (cartes interactives)
 - `sf` (optionnel pour analyses spatiales avancées)
 - `htmlwidgets` (sauvegarde de la carte interactive)

Installation des packages
Le script `analysis_fire.R` tente d'installer automatiquement les packages manquants. Vous pouvez aussi les installer manuellement dans une session R :

```r
install.packages(c("tidyverse","lubridate","leaflet","sf","htmlwidgets"), repos = "https://cloud.r-project.org", dependencies = TRUE)
```

Exécution
 - Exécution non interactive (PowerShell / CMD) :

```powershell
Rscript analysis_fire.R
```

 - Exécution interactive (R ou RStudio) : ouvrir `analysis_fire.R` et exécuter pas à pas.

Sorties produites (dans `outputs/`)
 - `summary_stats.csv` : statistiques descriptives des variables principales
 - `brightness_hist.png`, `brightness_box.png` : distribution de la luminosité
 - `daily_count.png`, `daily_mean_brightness.png` : graphiques temporels
 - `spatial_brightness.png` : carte statique des points (colorés par brightness)
 - `interactive_map.html` + dossier `interactive_map_files/` : carte interactive (points cliquables)
 - `correlations.csv` : matrice de corrélation simple entre variables sélectionnées

Comment ouvrir la carte interactive
 - Double-cliquer sur `outputs/interactive_map.html` (conserver le dossier `interactive_map_files` à côté de l'HTML).
 - Depuis PowerShell :

```powershell
Start-Process outputs\interactive_map.html
```

 - Si le navigateur bloque les ressources locales, lancer un serveur HTTP local et ouvrir l'URL :

```powershell
# (nécessite Python installé)
python -m http.server 8000
# puis ouvrir http://localhost:8000/outputs/interactive_map.html
```

Points d'interprétation rapide
 - Vérifier la distribution de `brightness` et `frp` pour détecter valeurs extrêmes.
 - Examiner les séries temporelles : pics de détections, saisonnalité.
 - Sur la carte interactive, rechercher des clusters/spots où la densité est plus élevée.
 - Analyser les corrélations entre `brightness` (ou `frp`) et `tmax`, `prcp`, `wspd` pour explorer les relations feu–météo.

Limitations et améliorations possibles
 - Nettoyage avancé : filtrer doublons, sources d'erreurs, seuils de `confidence`.
 - Hotspot detection : utiliser `sf` + méthodes de densité spatiale (kernel density) ou `spatstat`.
 - Rapport R Markdown : produire un document reproductible (`report.Rmd`) et l'exporter en HTML/PDF (nécessite Pandoc pour PDF/self-contained HTML).

Contact / Aide
 - Si vous voulez que j'ajoute un rapport R Markdown, l'analyse mensuelle, ou la détection de hotspots, dites-moi quelle option vous préférez et je l'implémente.

Fichier principal
 - `analysis_fire.R` — voir et exécuter pour reproduire toutes les étapes.

Bonne exploration !
