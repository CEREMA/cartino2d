### pense-bête
# si on utilise deux profils identiques pour deux biefs, attention si on veut reprendre, cération de doublons
# si deux profils se croisent idem, , attention si on veut reprendre, cération de doublons

# intégrer les semi de points

# faire de meilleures lignes de contraintes

cat("\014")
library(sf)
library(stringr)
chem_routine=dirname(rstudioapi::getActiveDocumentContext()$path)
source(file.path(chem_routine,"PreC2D_______Topo1_Manuel_Sections.R"))
source(file.path(chem_routine,"PreC2D_______Topo2_Section_MNT.R"))
source(file.path(chem_routine,"Users","PreC2D_TTMMNTLienOutilsPC.R"))

list_Users=list.files(file.path(chem_routine,"Users"), pattern="PreC2D_TTMMNT_")

nchoixZS = select.list(
  list_Users,
  title = "Choix dela zone des secteurs à traiter",
  multiple = T,
  graphics = T
)
nlal = which(list_Users %in% nchoixZS)

if (length(nlal)>0)
{Secteur=list()}

inc=0
for (izs in nlal)
{
  source(file.path(chem_routine,"Users",list_Users[izs]), encoding="utf-8")
  
  Choix_=cbind("Pied de berges automatiques - non integré       ...:",
               "Tampon auto et MNT autour des semi-points 3D              :",
               "Tampon auto autour des points 3D                   :",
               "Points3D => lignes 3D (par tampon)                 :",
               "Ordonner les lignes 3D (Amont/aval ou contraire)   :",
               "Sens des lignes 3D (Rive aguche/droite ou contraire:",
               "Vérification de la procédure de création du MNT    :",
               "Création du MNT                                    :")
  
  param=cbind(
    "",#basename(nomRives),
    nomSemiPts3D,
    basename(nomPts3D),
    paste(basename(nomPts3D),basename(nomTampon)),
    basename(nomLignes3D),
    basename(nomLignes3D),
    paste(basename(nomLignes3D), basename(nomRives)),
    paste(basename(nomLignes3D), basename(nomRives))
  )
  
  Choix=paste0(Choix_,param)
  preselec=Choix[which(nchar(param)>1)[1]:length(param)]
  
  titre=paste0(list_Users[izs]," :Etape de traitement de données géomètres ou sections manuelles")
  nChoix = select.list(
    Choix,
    title = titre,
    preselect = preselec,
    multiple = T,
    graphics = T
  )
  nFIL = which(Choix %in% nChoix)
  if (length(nFIL)==0){print("VOUSAVEZVOULUQUECAFASSEBADABOOM_CESTGAGNE");BOOM=BOOOM}
  Etap=matrix(0,length(Choix),1)
  Etap[nFIL]=1
  
  crs <- st_crs(EPSG)
  
  if (nchar(nomSemiPts3D)>1){nomQgis=file.path(dirname(nomSemiPts3D),paste0(basename(dirname(nomSemiPts3D)),".qgs"))}
  if (nchar(nomLignes3D)>1){nomQgis=file.path(dirname(nomLignes3D),paste0(basename(dirname(nomLignes3D)),".qgs"))}
  if (nchar(nomPts3D)>1){nomQgis=file.path(dirname(nomPts3D),paste0(basename(dirname(nomPts3D)),".qgs"))}
  unlink(nomQgis)
  copie_Qgis=file.copy(file.path(chem_routine,"ProjetTopoMano_Base.qgs"),nomQgis)
  if (copie_Qgis==F){boom=bug_de_copie_de_qgis}
  # Lire le contenu du fichier texte
  file_content <- readLines(nomQgis, warn = FALSE)
  # Remplacer la chaîne de caractères
  file_content <- str_replace_all(file_content, "nomContourGrossierBiefs", nomBief)
  file_content <- str_replace_all(file_content, "nomPoints3D_de_Base", nomPts3D)
  # file_content <- str_replace_all(file_content, "nomSemiPoints3D_de_Base", nomSemiPts3D)
  file_content <- str_replace_all(file_content, "nomLignes3D_ou_2D_de_Base", nomLignes3D)
  file_content <- str_replace_all(file_content, "nomRives_de_Base", nomRives)
  file_content <- str_replace_all(file_content, "nomTA_Base", nomTA_LidarBase)
  # écraser le fichier original, utilisez le même chemin de fichier
  writeLines(file_content, nomQgis)
  
  # Etape principale
  if (Etap[2]==1){SemiPts3D_miniMNT(nomSemiPts3D,nomBief,NBief,DistBufSemPts3D,AireInterieurMin,EPSG)}
  if (Etap[3]==1){nomTampon=Pts3D_Tampon(nomPts3D,nomBief,NBief,DistBuf,EPSG)}
  if (Etap[4]==1){nomLignes3D=Pts3D_Lignes3D(nomPts3D,nomTampon,nomBief,NBief)}
  if (Etap[5]==1){nomLignes3D=Lignes3D_Ord(nomLignes3D,nomBief,NBief)}
  if (Etap[6]==1){nomLignes3D=Lignes3D_Sens(nomLignes3D,nomBief,NBief)}
  if (Etap[8]==1){EtapMNT=1}else{EtapMNT=0}
  #0 VERIFIER le travail manuel
  #1 le MNT
  if (Etap[7]==1 | Etap[8]==1) 
  {
    #  Dossier de travail ATTENTION AUX / et pas \\
    # Creation du répertoire pour l'export du MNT
    dsnlayerTravail=file.path(dirname(nomLignes3D),"Export")
    if (file.exists(dsnlayerTravail)==F){dir.create(dsnlayerTravail)}
    Sections_Raster(dsnlayerTravail,nomLignes3D,nomRives,nomTA_LidarBase,nomTA_LidarC2D,reso,EPSG,EtapMNT)
  } 
}