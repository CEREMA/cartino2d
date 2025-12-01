# Cartino2D_Secteurs
# Création du fichier de base de cartino2D pour les secteurs
# Choix des différents paramétrages
cat("\014") # Nettoyage de la console
cat("########################################################################################################\n")
cat("######################### C2D A LIRE SVP ###############################################################\n")
cat("---------------- DEMARRAGE -----------------------------------------------------------------------------\n")
cat("Pour lancer l'étape 0 de Cartino2D, vous devez disposer de:\n")
cat("1/ Un fichier vecteur SIG avec des géométries de secteurs de calculs\n")
cat("1.a/ De type bassins versants pour des forçages Pluie\n")
cat("1.b/ De type fonds de vallée pour des forçages Débit\n")
cat("2/ Vous devez aussi renseigner un 1er fichier de paramètres dans le dossier\n")
cat(file.path(chem_routine,"C2D_ParamUser"),"avec un début de nom de fichier commençant par 'C2D_00_Secteur_Xxxx.R'\n")
cat("A vous de changer Xxxx par le nom de votre choix\n")
cat("Les paramètres à intégrer dans ce fichier sont expliqués, merci de prendre exemple sur un fichier existant\n")
cat("Un nouveau fichier SIG d'entrée pour les étapes suivantes de Cartino ou des besoins de PréTraitement sont ainsi créés\n")
cat("OBJECTIF DE CETTE ETAPE 0: créer un fichier SIG avec une table attributaire qui sera la base de pilotage des étapes ultérieures\n")
cat("-------------------------------------------------------------------------\n")
cat("Si aucune boite ne s'ouvre, vous n'avez pas de fichiers",file.path(chem_routine,"C2D_ParamUser","C2D_00_Secteur_Xxxx.R\n"))
cat("######################### Fin C2D A LIRE ###############################################################\n")
listSect=list.files(file.path(chem_routine,"C2D_ParamUser"), pattern="C2D_00_")

nchoixZS = select.list(
  listSect,
  title = "Choix dela zone des secteurs à traiter",
  multiple = T,
  graphics = T
)
nlal = which(listSect %in% nchoixZS)

if (length(nlal)>0)
{Secteur=list()}

inc=0
for (izs in nlal)
{
  inc=inc+1
  if (inc>0){  Sys.sleep(2);cat("Pause de 2 secondes pour avoir un autre nom de fichier...\n") }
  cat("#######################################################################\n")
  cat("Traitement de :",listSect[izs],"\n")
  cat("#######################################################################\n")
  source(file.path(chem_routine,"C2D_ParamUser",listSect[izs]), encoding="utf-8")
  
  # Analyse du pas de temps
  Pas_En_XY=ifelse(is.na(as.numeric(Exzeco))==F,Exzeco,Exz1_Dx)
  RelaXT=Pas_En_XY/TIME_STEP
  if (RelaXT<2 | RelaXT>7)
  {
    ChoixTIME_STEP = cbind(
      paste0("Continuer - Pas_En_XY(",round(Pas_En_XY,1),"m)/TIME_STEP (",round(TIME_STEP,1),"s) => VitesseMax",round(RelaXT,1),"m/s"),
      "Boom")
    titreChoixTIME_STEP="? valeur pas de temps"
    
    nChoixTIME_STEP = select.list(
      ChoixTIME_STEP,
      title = titreChoixTIME_STEP,
      preselect = ChoixTIME_STEP[2],
      multiple = T,
      graphics = T
    )
    
    if (ChoixTIME_STEP[which(ChoixTIME_STEP %in% nChoixTIME_STEP)]==ChoixTIME_STEP[2]){BADABOOM=VOUS_DEVEZ_CHANGER_LA_VALEUR_DU_PAS_DE_TEMPS___Variable_TIME_STEP}
  }
  
  
  
  # Nom export auto avec date et heure
  nom_exp = paste0("Cartino2D_Secteur_",format(Sys.time(),format="%Y%m%d_%H%M%S"),".gpkg")
  
  ####################################################################################################################
  ########### NE PAS TOUCHER #########################################################################################
  ####################################################################################################################
  crs = st_crs(EPSG)
  # Ouverture du contour - nommage des secteurs avec surface et position
  if(file.exists(file.path(dsnlayer,nomlayerC))==F) {cat(file.path(dsnlayer,nomlayerC), " INTROUVABLE\n")}
  
  contours <- st_read(file.path(dsnlayer,nomlayerC))
  contours=contours[which(st_is_empty(contours)==F),]
  
  #st_is_empty(contours)
  cat("#######################################################################\n")
  st_crs(contours) = crs
  Surf_km2 = st_area(contours) / 1000000
  
  centre_coord <- st_geometry(contours) %>% 
    st_centroid() %>% 
    st_coordinates() %>% 
    floor()
  
  contours$NOM = paste0(
    "C",
    formatC(floor(Surf_km2), width = 4, flag = "0"),
    "_",
    formatC(
      round(1000000 * (Surf_km2 - floor(Surf_km2))),
      width = 6,
      flag = "0",
      format = "d"
    ),
    "km_",
    "X",as.character(centre_coord[,1]),
    "Y",as.character(centre_coord[,2])
  )
  
  NOM = data.frame(contours$NOM)
  ID=data.frame(ID=1:dim(NOM)[1])
  COMMENT="Commentaires"
  
  # Formatage des noms ------------------------------------------------
  colnames(NOM) = "NOM"
  TabParam = cbind(
    ID,
    STEP_PRE,
    NOM,
    COMMENT,
    Exzeco,
    SourceDeb,
    NOMPOST,
    MNT,
    Friction,
    CN,
    CN_HYETO,
    OH,
    LigContr,
    SectCont,
    COTEAVALM,
    COTEAVALAJ,
    TEMPS_DEBH,
    DURATION,
    TIME_STEP,
    PRINTPERIO,
    DecAltiMNT,
    NINTERPMNT,
    Cont1_Buf,
    Cont1_Dx,
    Cont2_Buf,
    Ratio_Dx,
    Exz1_Buf,
    Exz1_Dx,
    Exz2_Buf,
    Exz2_Dx,
    Exz3_Buf,
    Exz3_Dx
  )
  
  #   Export             -----------------------------------------------------
  Beau <- cbind.data.frame(TabParam,st_geometry(contours))
  Beau = st_as_sf(Beau)
  st_crs(Beau) <- crs
  
  # print("VOIR FRED PONS - MODIF")
  # print("VOIR FRED PONS - MODIF")
  # print("VOIR FRED PONS - MODIF")
  
  pgb <- txtProgressBar(min = 0, max = nrow(Beau),style=3)

  PlusMoins=c(2,-5)  
  PlusMoins=c(120,-360)
  for (incB in 1:nrow(Beau))
  {
    setTxtProgressBar(pgb, incB)
    forme_Old=st_make_valid(st_geometry(Beau[incB,]))
    forme_tmp=st_make_valid(st_buffer(st_buffer(forme_Old,PlusMoins[1]),PlusMoins[2]))
    forme_New=st_make_valid(st_union(forme_tmp,forme_Old))
    st_geometry(Beau[incB,])=forme_New
  }
  cat("\n")
  
  nomsansrivieres=file.path(dsnlayer,"tmp_sansriviere.gpkg")
  st_write(Beau,nomsansrivieres, delete_layer = T, quiet = T)
  
  nomBeau=file.path(dsnlayer, nom_exp)
  AireMin=200*200
  
  # Enlever les petits trous
  cmd <- paste0(qgis_process, " run native:deleteholes",
                " --INPUT=", shQuote(nomsansrivieres),
                " --MIN_AREA=", AireMin,
                " --OUTPUT=", shQuote(nomBeau))
  print(cmd); system(cmd)
  
  Beau=st_read(nomBeau)
  
  unlink(nomsansrivieres)
  
  # print("FIN VOIR FRED PONS - MODIF")
  # print("FIN VOIR FRED PONS - MODIF")
  # print("FIN VOIR FRED PONS - MODIF")
  # st_write(Beau,dsn = ,layer = nom_exp,delete_layer = T, quiet = T)
  
  file.copy(file.path(chem_routine,"C2D",Doss_Qml,"Cartino2D_Secteurs.qml"),
            file.path(dsnlayer,paste0(substr(nom_exp, 1, rev(gregexpr("\\.", nom_exp)[[1]])[1] - 1),".qml")))
  cat("\n")
  cat("###################################################################################\n")
  cat(nom_exp, "créé avec les paramètres ",listSect[izs],"\n")
  
  if (length(nlal)>0)
  {Secteur[[inc]]=Beau}
}

if (length(nlal)>1)
{
  Secteur_F = do.call(rbind, Secteur)
  nom_exp = paste0("Cartino2D_Secteur_",format(Sys.time(),format="%Y%m%d_%H%M"),"FUS.gpkg")
  st_write(Secteur_F,dsn = file.path(dsnlayer, nom_exp),layer = nom_exp,delete_layer = T, quiet = T)
  file.copy(file.path(chem_routine,"C2D",Doss_Qml,"Cartino2D_Secteurs.qml"),
            file.path(dsnlayer,paste0(substr(nom_exp, 1, rev(gregexpr("\\.", nom_exp)[[1]])[1] - 1),".qml")))
}else{
  nom_exp=nomBeau
}

cat("\n")
cat("\n")
cat("########################################################################################################\n")
cat("######################### C2D A LIRE SVP ###############################################################\n")
cat("Pour continuer les autres étapes après l'étape 0\n") 
cat("1/ Ouvrir un fichier existant type ",file.path(chem_routine,"C2D_ParamUser/C2D_ParamUtilisateurXXXXXXX.R"),"\n")
cat("2/ Recopier dsnlayer = ",dsnlayer,"\n") 
cat("3/ Mettre le nom de fichier dans la variable nomlayerC=",basename(nom_exp),"\n")
cat("4/ Si vous ouvrez ce fichier dans QGIS, il doit avoir un style vous permettant de lancer des actions\n")
cat("4.1/ d'ouverture des tables d'assemblages présentes dans voter table (MNT obligatoire, Strickler, CN, OH, Sections de contrôle, lignes contraintes\n")
cat("4.2/ d'ouverture des répertoires créés ultérieurement\n")
cat("Vous avez peut-être des traitements à faire avec PreC2D comme:\n")
cat("- Créer des tables de CN, Strickler..., vous devrez reprendre la création de vos secteurs pour intégrer ces tables\n")
cat("- Récupérer de la pluie (sans incidence sur cette étape\n")
cat("-...\n")
cat("\n")
cat("Si vos données d'entrées sont prêtes, vous pouvez passer aux étapes suivantes\n")
cat("Après expérience; il est d'usage de lancer les étapes 1/2/3/4/5 en même temps\n")
cat("######################### Fin C2D A LIRE ###############################################################\n")
