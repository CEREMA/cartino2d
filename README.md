# Cartino2D

**Cartino2D** – Automatisation des modèles hydrauliques 2D pour la CARTgraphie des INOndations 2D
---
## Version
Première version référencée sur GitHub.

---
## Contenu du dépôt
- **Code source principal** : Script R nommé `C2D_Run.R`.
- **Installation et exemples** : Voir la section [Releases](https://github.com/CEREMA/cartino2d/releases) pour les instructions d’installation et des exemples d’utilisation.

---
## Objet de Cartino2D
**Cartino2D (C2D)**, développé par le [Cerema](https://www.cerema.fr/fr), est un cadre automatisé pour déployer des modèles hydrauliques 2D résolvant les équations de Saint-Venant (via [**Telemac-2D**](https://www.opentelemac.org/)), intégrant pluie, infiltration et débits. 
Il permet de :
- Générer automatiquement des maillages adaptés au relief (structurés ou non).
- Gérer des ouvrages hydrauliques et des sections de contrôle.
- Spatialiser les paramètres et données d’entrée (rugosité, Curve Number, pluie, débit, etc.) à partir de bases géospatiales hétérogènes.
- Supporter des simulations multi-résolution (jusqu’à une définition métrique).
- Sectoriser automatiquement le domaine pour lancer des modèles Telemac-2D en parallèle.

**Applications** : Évaluation ou pré-évaluation des aléas d’inondation, **à l’échelle locale ou nationale**.

---
## Description détaillée

### Contexte
Cartino2D (« CARTographie des INOndations 2D ») est une suite de routines R pour automatiser les calculs avec **Telemac-2D**. Il s’inscrit dans la continuité de [Cartino1D](https://github.com/CEREMA/cartino1D).

**Développé dans le cadre de** :
- Des conventions de R&D pour le [**PAPI des Petits Côtiers Toulonnais**](https://metropoletpm.fr/nos-missions/cadre-de-vie-environnement/prevenir-des-inondations-et-papi-pct)
- Des travaux d'aléas sur les communes de Montpellier et Castelnau-le-Lez pour la [**DDTM34**](https://www.herault.gouv.fr/Actions-de-l-Etat/Environnement-eau-chasse-risques-naturels-et-technologiques/Risques-naturels-et-technologiques/Transmission-des-informations-aux-maires-TIM/Les-Porter-a-connaissance-PAC-de-l-Herault/MONTPELLIER)
- L'[**ANR PICS**](https://anr.fr/Projet-ANR-17-CE03-0011) (ANR-17-CE03-0011) piloté par [**l’Université Gustave Eiffel**](https://www.univ-gustave-eiffel.fr/)
- Des conventions de R&D pour le [**PAPI 3 Vistre**](https://papi3.vistre-vistrenque.fr/synthese-programme) (secteur de Nîmes).
- Des conventions de R&D sur le ruissellement pour la [**Métropole Aix-Marseille-Provence**](https://deliberations.ampmetropole.fr/documents/metropole/deliberations/2023/03/16/ANNEXE/49593_49593_cerema_annexe.pdf).
- L’[**ANR MUFFINS**](https://anr.fr/Projet-ANR-21-CE04-0021) (Projet-ANR-21-CE04-0021) piloté par l'[**INRAE**](https://www.inrae.fr/).
- L’expérimentation de la **cartographie nationale des inondations** (DGPR).
- ...

**Dépôt légal** :
Cartino2D a été déposé à l’[Agence de Protection des Programmes (APP)](https://www.app.asso.fr/) en 2024, avec un renouvellement en [2025](https://secure2.iddn.org/app.server/certificate/?sn=2024210024001&key=2d788c83810665859381fcd0815a6673fa5d88de88a3a0c00cf0fc17909cb3a1&lang=fr).

**Collaborations** :
Ce projet est le fruit d’échanges techniques avec les partenaires cités précédement en particulier:
- l’**Université Gustave Eiffel** et l'**INRAE**
- les collectivités pour leurs analyses critiques de l'application de Cartino2D sur leur territoire. 

---
### Configuration requise
- **Système** : PC Windows (actuellement obligatoire).
- **Ressources** : Capacités CPU importantes pour exploiter pleinement Cartino2D et Telemac-2D.
- **Dépendances** : Librairies R, GRASS, QGIS, GMSH, Python (pputils), etc.

**Prérequis utilisateur** :
Une expérience autonome avec **Telemac-2D** est nécessaire.

---
### Fonctionnalités clés

1. **Noyau numérique et modes de simulation**
   - Intégration de **Telemac-2D** pour résoudre les équations de Saint-Venant en eaux peu profondes (avec inertie), compatible avec pluie spatialisée et infiltration ([lien vers la branche muffins](https://gitlab.pam-retd.fr/otm/telemac-mascaret/-/tree/muffins)).
   - Deux stratégies principales :
     - **C2D-Pluie** : Simulation pluie–ruissellement–inondation sur sous-bassins (production de ruissellement à partir de champs de pluie spatialisés).
     - **C2D-Débits** : Simulation d’inondation dans les plaines alluviales, forcée par des apports de débits (statistiques ou observés).

2. **Génération automatique de maillages**
   - Maillages non structurés (via [**GMSH**](https://gmsh.info/)) adaptés au relief, avec affinement le long des talwegs.
   - Maillages multi-types : grille carrée (deux triangles par carré) pour grands domaines.
   - Gestion topographique des limites (buffers + pente artificielle) pour des conditions aux limites stables.

3. **Ingestion et traitement des données**
   - Prise en charge de la pluie homogène (moyenne sur un bassin) ou spatialisée (radar ANTILOPE, produits statistiques [SHYREG]( https://shyreg.pluie.recover.inrae.fr).
   - Injection de débits ponctuels (base [SHYREG](https://shyreg.recover.inrae.fr/).), mode « permanent » privilégié pour les approches statistiques.

4. **Paramétrisation spatiale automatique**
   - Rugosité hydraulique (Strickler) : assignation via bases d’occupation du sol ([BDTopo](https://geoservices.ign.fr/bdtopo), [OSM](https://www.openstreetmap.org), [RPG](geoservices.ign.fr/rpg), [European Urban Atlas](https://land.copernicus.eu/en/products/urban-atlas), [OCSGE](https://geoservices.ign.fr/ocsge), [CLC](https://land.copernicus.eu/en/products/corine-land-cover), etc.).
   - Paramètres de ruissellement (Curve Number, SCS-CN) : mosaïque multi-sources, rasterisation (1 m par défaut), résampling (5 m, 25 m).

5. **Représentation d’ouvrages hydrauliques**
   - Import de collecteurs/ponceaux (fichier SIG avec attributs : largeur, cotes entrée/sortie).
   - Discrétisation fine pour levées, digues ou barrages fins.
   - Gestion des sous-passages d’infrastructures (liens avec [FILINO](https://github.com/CEREMA/filino)).

6. **Sectorisation automatique du domaine**
   - Simulation multi-résolution préalable (grille 25 m → 5 m) pour détecter les zones inondables.
   - Distinction entre zones « pluie/flash » et zones « débit/rivière » (critères : temps de pic, seuils de débit/hauteur).
   - Nettoyage et fusion des petits bassins orphelins.

7. **Pré-traitements topographiques**
   - Outils **FILINO** pour filtrer/traiter les MNT LiDAR et en particulier [LidarHD IGN](https://geoservices.ign.fr/lidarhd).
   - Plugin QGIS **OHFLASH** pour vérifier les données SIG et identifier les ouvrages/dépressions.

8. **Sorties, analyses et contrôles**
   - Export des variables : hauteur d’eau, vitesse, niveau, nombre de Froude, temps de pic, débits scalaires, etc.
   - Génération de secteurs et transects pour extraire hydrogrammes, hauteurs, côtes, débits.
   - Post-traitement : assemblage des résultats, vidéos temporelles, comparaison avec référentiels (repères de crues, interventions pompiers).

9. **Échelle d’application et performances**
   - Multi-échelle : du national (France, 25 m) au métrique fin (maillage non structuré 3–25 m en zones urbaines).
   - Calcul distribué : traitements parallèles, tests sur HPC (Cerema, [Ifremer](https://www.ifremer.fr/fr), [GENCI](https://www.genci.fr) - AD012A14287, AD012A14287R1 et AD012A14287R2).

10. **Limites et précautions**
    - Hypothèses non modélisées : barrages activement opérés, brèches de digues, submersions marines dynamiques, transport solide/débris, pompages et transvasements...
    - Résolution et vectorisation : remises à l’échelle (25 m) peuvent atténuer les petits aménagements (digues fines, ponts). La vaeur de la résolution est un indice des obejts pouvant être pris en compte dans les simulations hydrauliques.
    - Qualité des bases : précision dépendante des DTM LiDAR et des bases d’ouvrages/usage du sol ; corrections manuelles fréquentes en zones urbaines complexes.

---
## Développement et support
- **Évolution continue** : Le dépôt sera amené à évoluer.
- **Responsabilité** : Les auteurs ne s’engagent pas à prendre en compte les demandes externes et ne sont pas responsables des données produites par les utilisateurs.

---
## Dernière mise à jour
24/11/2025
