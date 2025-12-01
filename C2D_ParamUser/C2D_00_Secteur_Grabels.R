# Cartino2D_Secteurs
# Création du fichier de base de cartino2D pour les secteurs

####################################################################################################################
#### LIEN FICHIERS #################################################################################################
####################################################################################################################
dsnlayer = "C:\\Cartino2D_Github\\France"
Doss_Qml  = "_Cartino2D_Qml_et_autre"
# Nom des zones a modifier
nomlayerC="Cartino2D_Secteur_Grabels.gpkg"

# Referentiel geographique
EPSG=2154

####################################################################################################################
#### PARAMETRES INTEGRES COMME DES CHAMPS DANS LE FICHIER SHAPE ####################################################
#### VOUS POUVEZ ENSUITE MODIFIER LES ATTRIBUTS DANS QGIS######### #################################################
####################################################################################################################
### ATTENTION TOUS LES LIENS VERS DES TABLES FINISSENT EN .shp

Exzeco     = "C:\\Cartino2D_Github\\France\\_Exzeco\\MTPCLL2024_GMSHtSceMaxMedContFrance_ZI_5met1m.gpkg"    # Taille de maille ou nom de la couche Exzeco ou zone inondable/interet associee 
SourceDeb  = ""     # Vide ou Lien vers le Fichier de sources de debit individualise
NOMPOST    = "Grabels"     # Nom du postraitement si on veut fusionner les resultats finaux
MNT        = "C:\\Cartino2D_Github\\France\\_MNT\\MTP_CLL_2024\\TA_MTP_CLL_2024.gpkg" # Lien obligatoire vers la table d'assemblage de MNT
Cont1_Buf  = 150    # Buffer (p.e 150P ou 250Q) sur tout le secteur => frontières de calcul final
# Buffer à ajuster en fonction de la largeur de la rivière et Valable que pour le cas d'injection de débit, sinon = NA
Cont1_Dx   = 25     # Pas de découpage des frontières de calcul
Cont2_Buf  = -100   # Buffer (p.e -100P ou -200Q) negatif pour délimiter la zone à garder et desserer le maillage si maille variable
Ratio_Dx   = 2 / 3  # Le découpage en segment produit parfois des coupures quasi moitie du temps, on va filtrer pour supprimer tous les points trop proches
Exz1_Buf   = 0      # Buffer sur Exzeco ou zone inondable/interet 1er paquet de points si maille variable
Exz1_Dx    = 3      # Découpage d'Exzeco ou zone inondable/interet 1er paquet de points si maille variable
Exz2_Buf   = 20     # Buffer sur Exzeco ou zone inondable/interet 2eme paquet de points si maille variable
Exz2_Dx    = 10     # Découpage d'Exzeco ou zone inondable/interet 2eme paquet de points si maille variable
Exz3_Buf   = 40     # Buffer sur Exzeco ou zone inondable/interet 3eme paquet de points si maille variable
Exz3_Dx    = 20     # Découpage d'Exzeco ou zone inondable/interet 3eme paquet de points si maille variable
Friction   = "C:\\Cartino2D_Github\\France\\_Strickler\\TA_Strickler_MTP20240705.gpkg"   # Frottement: Valeur ou Lien vers la table d'assemblage
CN         = "C:\\Cartino2D_Github\\France\\_CN\\TA_CN_C_MTP20240705.gpkg"   # Curve Number: Valeur ou Lien vers la table d'assemblage
              
CN_HYETO   = 0      # Savoir où on prend en compte le Curve Number, 1 dans le fichier hyéto injecté dans Telemac, 0 dans Telemac directement,
OH         = ""     # Vide ou Lien vers la table des OH
LigContr   = ""     # Vide ou Lien vers la table des lignes de contraintes
SectCont   = "C:\\Cartino2D_Github\\France\\_SectionsControles\\SectControl_MTPCLL2024.gpkg"     # Vide ou Lien vers la table des sections de controles
DecAltiMNT = -25    # Creusement du MNt sur les bords
NINTERPMNT = 1      # Topo récupérées avec le nombre de point le plus proche
COTEAVALM  = 0.5    # Niveau marine ou Cote aval imposé si la frontière a une altitude négative
COTEAVALAJ = 0.1    # Valeur ajoutee a la altitudejout minimale dans le domaine valide
TEMPS_DEBH = 0      # Temps de début  ancienne methode avec shyreg a revoir ulterieurement heure à lire dans les fichiers sortis de RexPluieSHYREG
DURATION   = 14     # Temps de fin    ancienne methode avec shyreg a revoir ulterieurement calcul valeur prise à -1h du pic principal round((3*zozo*1000000)^0.5/(3600*0.5)+0.51)
TIME_STEP  = 0.5      # Pas de temps en seconde
PRINTPERIO = -1     # Export des résultats : 0 = varibles à la fin; -1 = variables max; 5,10,20,60.. pas de temps en minutes d export des variables resultats

# Mots-clés pour lancer les étapes
STEP_PRE  = 0