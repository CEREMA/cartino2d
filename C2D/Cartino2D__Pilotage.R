##Logique : 1.étapes 1 jusqu'à étape 5 pour tous les secteurs 
############ afin de lancer tous les calculs 
###########2. étapes suivantes pour récupérer et post traiter les résultats 
# require(ssh)

options(error = browser
)

crs = st_crs(EPSG)

debit_sc = list()


inc15 = 0

# Ouverture du contour
contours <- st_read(file.path(dsnlayer,nomlayerC))
st_crs(contours) = crs

# Champ=contours$SourceDeb
# NomChamp="SourceDeb"
fun_check_Champ =
  function(Champ,NomChamp) {
    if(!is.numeric(Champ)){
      Champ=unique(Champ)
      Champ=Champ[which(nchar(Champ)>2)]
      Champ=Champ[which(substr(Champ,2,2)==":")]
      if (length(which(file.exists(Champ)==F))>0)
      {
        print(paste("BUG Champ:",NomChamp, Champ[file.exists(Champ)==F],"INTROUVABLE"))
      }
      if (length(which(tools::file_ext(Champ)!="shp"))>0)
      {
        print(paste("BUG Champ:",NomChamp, Champ[which(tools::file_ext(Champ)!="shp")],"NOM FICHIER INVALIDE"))
      }
    }
  }

fun_check_Champ(contours$Exzeco,"Exzeco")
fun_check_Champ(contours$SourceDeb,"SourceDeb")
fun_check_Champ(contours$MNT,"MNT")
fun_check_Champ(contours$Friction,"Friction")
fun_check_Champ(contours$CN,"CN")
fun_check_Champ(contours$OH,"OH")
fun_check_Champ(contours$LigContr,"LigContr")
fun_check_Champ(contours$SectCont,"SectCont")

# là où on a tous les fichiers de qml, cas mascaret, qgz...
dsnlayer_PreRequis = "\\_Cartino2D_PreRequis"

fichiers_source <- list.files(file.path(chem_routine,"C2D"),pattern="\\.R",full.names = TRUE)
nici=substr(fichiers_source,nchar(fichiers_source)-1,nchar(fichiers_source))==".R"
fichiers_source=fichiers_source[nici]
fichiers_source <- fichiers_source[!grepl('Cartino2D__Pilotage', fichiers_source)]
fichiers_source <- fichiers_source[!grepl('CreationSecteur', fichiers_source)]

# contours=contours[which(contours$STEP_PRE>=-1),]
contours=contours[order(contours$STEP_PRE,contours$NOM),]

# Choix des secteurs à traiter

if (dim(contours)[1]<500)
{
  # par choix en boite de dialogue
  nchoixS = select.list(contours$NOM,preselect = contours$NOM[which(contours$STEP_PRE==0)],
                        title = "Choisir les secteurs à traiter ( boite si moins de 50 pré-choix",multiple = T,graphics = T)
  nlalaS = which(contours$NOM %in% nchoixS)
  if (length(nlalaS)==0){VOUSAVEZPASCHOISI=BADABOOM}
  # On focalise sur le champ ATRAITER==1
  contours=contours[nlalaS,]
  contours$STEP_PRE=0
}

nsecteur = which(contours$STEP_PRE == 0)
contours$BUG <- rep("",length(contours$NOM))

if (ETAPE[9]==1 | ETAPE[10]==1)
{
  contours=contours[which(contours$STEP_PRE==0),]
  contours=contours[order(contours$NOM,decreasing=T),]
  nsecteur = which(contours$STEP_PRE == 0)
}

# print(contours$NOM)
cat("Traitement de: ",contours[nsecteur, ]$NOM,"\n")

cat("Pause: ",tpsPause)
Sys.sleep(tpsPause)

# contours <- contours[nsecteur,]
# nsecteur <- 1:length(contours$NOM)
for (isrc in 1:length(fichiers_source)){
  source(fichiers_source[isrc])
}
if(nb_proc_pre>1){
  cl <- parallel::makeCluster(nb_proc_pre) 
  doParallel::registerDoParallel(cl)
  foreach::foreach(ic = 1:length(nsecteur), 
                   .combine = 'c',
                   .inorder = FALSE,
                   .packages = c("sf","dplyr","utils","sfheaders","tidyverse","readxl")) %dopar%{
                     
                     skip_to_next <- FALSE
                     #############################################################################
                     ################ CONTOUR #####################################################
                     #############################################################################
                     # Récupération du contour i
                     contour = contours[nsecteur[ic], ]
                     if(contour$STEP_PRE!=0) next
                     
                     # Récupération du nom du contour
                     nomcontour = contour$NOM
                     dsnlayerC = file.path(dsnlayer, contour$NOMPOST, nomcontour)
                     
                     # contour$NOM=0 dans pputils, il ne veut qu'une colonne
                     contourPPutils = contour[,1]
                     contourPPutils$NOM = 0
                     SecteurGRASS=paste0(dirname(SecteurGRASS_),"_",format(Sys.time(),format="%Y%m%d_%H%M%S"),'/',contour$NOMPOST,"_",nomcontour,"/",basename(SecteurGRASS_))
                     
                     # ETAPE 1******ETAPE 1******ETAPE 1 ---------------------------------------
                     Etape1_fn_parallel()
                     
                     
                     # ETAPE 2******ETAPE 2******ETAPE 2 ---------------------------------------
                     Etape2_fn_parallel()
                     
                     # ETAPE 3******ETAPE 3******ETAPE 3 ---------------------------------------
                     #############################################################################
                     ################ Conditions limites##########################################
                     #############################################################################
                     Etape3_fn_parallel()
                     # 
                     # # ETAPE 4******ETAPE 4******ETAPE 4 ---------------------------------------  
                     # #############################################################################
                     # ################ Fichier cas et hyeto ##########################################
                     # #############################################################################
                     Etape4_fn_parallel()
                     
                     
                   }
  
  parallel::stopCluster(cl)
  
  
}


# ETAPE 11******ETAPE 11******ETAPE 11 ---------------------------------------    

if (ETAPE[11] == 1)
{
  Avanc = file.path(dsnlayer, "Suivicalcul.txt")
  file.create(Avanc)
  ASC_Final = paste0(extsortieASC[1], ".asc")
  for (isort in 2:length(extsortieASC))
  {
    ASC_Final = paste(ASC_Final, paste0(extsortieASC[isort], ".asc"), sep =
                        ";")
  }
  write(
    paste(
      "CAS",
      "RES",
      paste0(extsortieASC[4], "_Brut.asc"),
      ASC_Final,
      sep = ";"
    ),
    file = Avanc,
    append = T
  )
}


if (exists("SensCalcul")==T)
{
  if ((SensCalcul)==-1){contours= contours[order(contours$NOM,decreasing = TRUE ),]}
  if ((SensCalcul)== 1){contours= contours[order(contours$NOM,decreasing = FALSE),]}
}

for (ic in 1:length(nsecteur)){
  skip_to_next <- FALSE
  #############################################################################
  ################ CONTOUR #####################################################
  #############################################################################
  # Récupération du contour i
  contour = contours[nsecteur[ic], ]
  if(contour$STEP_PRE!=0) next
  
  # Récupération du nom du contour
  nomcontour = contour$NOM
  dsnlayerC = file.path(dsnlayer, contour$NOMPOST, nomcontour)
  
  # contour$NOM=0 dans pputils, il ne veut qu'une colonne
  contourPPutils = contour[,1]
  contourPPutils$NOM = 0
  
  if(nb_proc_pre==1){
    SecteurGRASS=paste0(dirname(SecteurGRASS_),"_",format(Sys.time(),format="%Y%m%d_%H%M%S"),'/',contour$NOMPOST,"_",nomcontour,"/",basename(SecteurGRASS_))
    # ETAPE 1******ETAPE 1******ETAPE 1 ---------------------------------------
    skip_to_next <- Etape1_fn(skip_to_next)
    
    if(skip_to_next){
      contours$BUG[nsecteur[ic]] <- "Problème étape 1"
      contours$STEP_PRE[nsecteur[ic]] <- -2
      next
    }
    # ETAPE 2******ETAPE 2******ETAPE 2 ---------------------------------------
    skip_to_next <-  Etape2_fn()
    if(skip_to_next){
      contours$BUG[nsecteur[ic]] <- "Problème étape 2"
      contours$STEP_PRE[nsecteur[ic]] <- -2
      next
    }
    
    # ETAPE 3******ETAPE 3******ETAPE 3 ---------------------------------------
    #############################################################################
    ################ Conditions limites##########################################
    #############################################################################
    skip_to_next <- Etape3_fn()
    if(skip_to_next){
      contours$BUG[nsecteur[ic]] <- "Problème étape 3"
      contours$STEP_PRE[nsecteur[ic]] <- -2
      next
    }
    # ETAPE 4******ETAPE 4******ETAPE 4 ---------------------------------------  
    #############################################################################
    ################ Fichier cas et hyeto ##########################################
    #############################################################################
    skip_to_next <- Etape4_fn()
    if(skip_to_next){
      contours$BUG[nsecteur[ic]] <- "Problème étape 4"
      contours$STEP_PRE[nsecteur[ic]] <- -2
      next
    }
    
    
  }
  
  
  # ETAPE 5******ETAPE 5******ETAPE 5 ---------------------------------------  
  # tryCatch({      if (ETAPE[5] == 1)
  # {
  #   
  #   name_cas_all <- readLines(file.path(dsnlayerC,"casfiles.txt"))
  #   
  # }}, error = function(e) {skip_to_next <<- TRUE})
  # if(skip_to_next){
  #   contours$BUG[nsecteur[ic]] <- "Problème étape 5"
  #   contours$STEP_PRE[nsecteur[ic]] <- -2
  #   next
  # }
  
  tryCatch({        if(ETPCALCUL[3]==1){
    name_cas_all <- readLines(file.path(dsnlayerC,"casfiles.txt"))
    preparation_cluster()
    launch_cluster_margny()
  }}, error = function(e) {skip_to_next <<- TRUE})
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "Problème préparation/lancement calcul cluster"
    contours$STEP_PRE[nsecteur[ic]] <- -2
    next
  }
  
  tryCatch({        if(ETPCALCUL[6]==1){
    name_cas_all <- readLines(file.path(dsnlayerC,"casfiles.txt"))
    preparation_cluster()
    launch_cluster_ifremer()
  }}, error = function(e) {skip_to_next <<- TRUE})
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "Problème préparation/lancement calcul cluster"
    contours$STEP_PRE[nsecteur[ic]] <- -2
    next
  }
  
  tryCatch({        if(ETPCALCUL[8]==1){
    name_cas_all <- readLines(file.path(dsnlayerC,"casfiles.txt"))
    preparation_cluster()
    launch_cluster_tgcc()
  }}, error = function(e) {skip_to_next <<- TRUE})
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "Problème préparation/lancement calcul cluster"
    contours$STEP_PRE[nsecteur[ic]] <- -2
    next
  }
  
  if(ETPCALCUL[1]==1 | ETPCALCUL[2]==1)
  {
    if (file.exists(file.path(dsnlayerC,"casfiles.txt"))==T)
    {
      name_cas_all <- readLines(file.path(dsnlayerC,"casfiles.txt"))
      
      tryCatch({if (ETPCALCUL[1]==1) {
        
        #############################################################################
        ################ CONTOUR #####################################################
        #############################################################################
        # Récupération du contour i
        
        # Récupération du nom du contour
        nomcontour = contour$NOM
        dsnlayerC = file.path(dsnlayer,contour$NOMPOST, nomcontour)
        cas_name_run <- name_cas_all
        
        batchfilescalar()
        
        T2D_run(paste0(nomcontour, ".bat"), dsnlayerC)
        
      }}, error = function(e) {skip_to_next <<- TRUE})
      if(skip_to_next){
        contours$BUG[nsecteur[ic]] <- "Problème préparation/lancement calcul cluster"
        contours$STEP_PRE[nsecteur[ic]] <- -2
        next
      }
      if(skip_to_next){
        contours$BUG[nsecteur[ic]] <- "Problème lancement calcul local scalaire"
        contours$STEP_PRE[nsecteur[ic]] <- -2
        next
      }
      
      tryCatch({if (ETPCALCUL[2]==1) {
        
        #############################################################################
        ################ CONTOUR #####################################################
        #############################################################################
        # Récupération du contour i
        
        # Récupération du nom du contour
        nomcontour = contour$NOM
        dsnlayerC = file.path(dsnlayer,contour$NOMPOST, nomcontour)
        cas_name_run <- name_cas_all
        
        
        batchfileparallel()
        
        T2D_run( paste0(nomcontour, ".bat"), dsnlayerC)
        
      }}, error = function(e) {skip_to_next <<- TRUE})
      if(skip_to_next){
        contours$BUG[nsecteur[ic]] <- "Problème lancement calcul local parallèle"
        contours$STEP_PRE[nsecteur[ic]] <- -2
        next
      }
      if(nb_proc_post==1){
        # ETAPE 6******ETAPE 6******ETAPE 6 ---------------------------------------  
        skip_to_next <- Etape6_fn()
        if(skip_to_next){
          contours$BUG[nsecteur[ic]] <- "Problème étape 6"
          contours$STEP_PRE[nsecteur[ic]] <- -2
          next
        }
        # ETAPE 7******ETAPE 7******ETAPE 7 ---------------------------------------
        Etape7_fn()
        # ETAPE 8******ETAPE 8******ETAPE 8 ---------------------------------------  
        Etape8_fn()
        # ETAPE 9******ETAPE 9******ETAPE 9 ---------------------------------------  
        Etape9_fn()
        # ETAPE 10******ETAPE 10******ETAPE 10 ---------------------------------------  
        Etape10_fn()
      }
      # ETAPE 11******ETAPE 11******ETAPE 11 ---------------------------------------   
      Etape11_fn()
      # ETAPE 13******ETAPE 13******ETAPE 13 ---------------------------------------   
      Etape13_fn()
      # ETAPE 14******ETAPE 14******ETAPE 14 --------------------------------------- 
      Etape14_fn()
      # ETAPE 15******ETAPE 15******ETAPE 15 --------------------------------------- 
      Etape15_fn()
      
    }
  }
} 

if(ETPCALCUL[4]==1){
  
  fincalcul <- rep(1,length(nsecteur))
  nbfinalcul <- length(contours$STEP_PRE)-length(which(contours$STEP_PRE!=0))
  while (nbfinalcul>0) {
    
    testcalculist <- testfincalcul_margny(nsecteur,fincalcul,nbfinalcul)
    fincalcul <- testcalculist[[1]]
    nbfinalcul <- testcalculist[[2]]
    
    for (ic in 1:length(nsecteur)){
      
      if(fincalcul[ic]==3){
        contours$STEP_PRE[nsecteur[ic]] <- -2
        contours$BUG[nsecteur[ic]] <- "soucis dans le calcul cluster"
      }  
      
      if(fincalcul[ic]==2){  
        
        contour = contours[nsecteur[ic], ]
        if(contour$STEP_PRE!=0) next
        
        
        # Récupération du nom du contour
        nomcontour = contour$NOM
        dsnlayerC = file.path(dsnlayer,contour$NOMPOST, nomcontour)
        
        # contour$NOM=0 dans pputils, il ne veut qu'une colonne
        contourPPutils = contour[, 1]
        contourPPutils$NOM = 0
        
        name_cas_all <- readLines(file.path(dsnlayerC,"casfiles.txt"))
        
        # merge_cluster()
        # 
        # checkmerge <- " int 1"
        # 
        # checkmerge <- testfinmerge(checkmerge)
        download_results_margny()
        # if(ETPCALCUL[5]==1){
        # 
        # }
        
        if(nb_proc_post==1){
          # ETAPE 6******ETAPE 6******ETAPE 6 ---------------------------------------  
          Etape6_fn()
          # ETAPE 7******ETAPE 7******ETAPE 7 ---------------------------------------
          Etape7_fn()
          # ETAPE 8******ETAPE 8******ETAPE 8 ---------------------------------------  
          Etape8_fn()
          # ETAPE 9******ETAPE 9******ETAPE 9 ---------------------------------------  
          Etape9_fn()
          # ETAPE 10******ETAPE 10******ETAPE 10 ---------------------------------------  
          Etape10_fn()
        }
        # ETAPE 11******ETAPE 11******ETAPE 11 ---------------------------------------   
        Etape11_fn()
        # ETAPE 13******ETAPE 13******ETAPE 13 ---------------------------------------   
        Etape13_fn()
        # ETAPE 14******ETAPE 14******ETAPE 14 --------------------------------------- 
        Etape14_fn()
        # ETAPE 15******ETAPE 15******ETAPE 15 --------------------------------------- 
        Etape15_fn()
        contours$STEP_PRE[nsecteur[ic]] <- 1
      }
      
    }
  }
  
  
  
}   
#-----------------------------------------
###########si on choisit une étape de post traitement
if(ETPCALCUL[4]!=1 & ETPCALCUL[5]==1){
  #### Erreur Nabil à checker
  for (ic in 1:length(nsecteur)) {
    
    download_results()
    
  }
}

if(ETPCALCUL[6]==1 & ETPCALCUL[7]==1){
  
  
  fincalcul <- rep(1,length(nsecteur))
  nbfinalcul <- length(contours$STEP_PRE)-length(which(contours$STEP_PRE!=0))
  while (nbfinalcul>0) {
    
    testcalculist <- testfincalcul_ifremer(nsecteur,fincalcul,nbfinalcul)
    fincalcul <- testcalculist[[1]]
    nbfinalcul <- testcalculist[[2]]
    
    for (ic in 1:length(nsecteur)){
      
      if(fincalcul[ic]==3){
        contours$STEP_PRE[nsecteur[ic]] <- -2
        contours$BUG[nsecteur[ic]] <- "soucis dans le calcul cluster"
      }  
      
      if(fincalcul[ic]==2){  
        
        contour = contours[nsecteur[ic], ]
        if(contour$STEP_PRE!=0) next
        
        
        # Récupération du nom du contour
        nomcontour = contour$NOM
        dsnlayerC = file.path(dsnlayer,contour$NOMPOST, nomcontour)
        
        # contour$NOM=0 dans pputils, il ne veut qu'une colonne
        contourPPutils = contour[, 1]
        contourPPutils$NOM = 0
        
        name_cas_all <- readLines(file.path(dsnlayerC,"casfiles.txt"))
        
        
        
        download_results_ifremer()
        
        
        if(nb_proc_post==1){
          # ETAPE 6******ETAPE 6******ETAPE 6 ---------------------------------------  
          Etape6_fn()
          # ETAPE 7******ETAPE 7******ETAPE 7 ---------------------------------------
          Etape7_fn()
          # ETAPE 8******ETAPE 8******ETAPE 8 ---------------------------------------  
          Etape8_fn()
          # ETAPE 9******ETAPE 9******ETAPE 9 ---------------------------------------  
          Etape9_fn()
          # ETAPE 10******ETAPE 10******ETAPE 10 ---------------------------------------  
          Etape10_fn()
        }
        # ETAPE 11******ETAPE 11******ETAPE 11 ---------------------------------------   
        Etape11_fn()
        # ETAPE 13******ETAPE 13******ETAPE 13 ---------------------------------------   
        Etape13_fn()
        # ETAPE 14******ETAPE 14******ETAPE 14 --------------------------------------- 
        Etape14_fn()
        # ETAPE 15******ETAPE 15******ETAPE 15 --------------------------------------- 
        Etape15_fn()
        contours$STEP_PRE[nsecteur[ic]] <- 1
      }
      
    }
  }
  
  
  
}

if(ETPCALCUL[8]==1 & ETPCALCUL[9]==1){
  
  
  fincalcul <- rep(1,length(nsecteur))
  nbfinalcul <- length(contours$STEP_PRE)-length(which(contours$STEP_PRE!=0))
  while (nbfinalcul>0) {
    
    testcalculist <- testfincalcul_tgcc(nsecteur,fincalcul,nbfinalcul)
    fincalcul <- testcalculist[[1]]
    nbfinalcul <- testcalculist[[2]]
    
    for (ic in 1:length(nsecteur)){
      
      if(fincalcul[ic]==3){
        contours$STEP_PRE[nsecteur[ic]] <- -2
        contours$BUG[nsecteur[ic]] <- "soucis dans le calcul cluster"
      }  
      
      if(fincalcul[ic]==2){  
        
        contour = contours[nsecteur[ic], ]
        if(contour$STEP_PRE!=0) next
        
        
        # Récupération du nom du contour
        nomcontour = contour$NOM
        dsnlayerC = file.path(dsnlayer,contour$NOMPOST, nomcontour)
        
        # contour$NOM=0 dans pputils, il ne veut qu'une colonne
        contourPPutils = contour[, 1]
        contourPPutils$NOM = 0
        
        name_cas_all <- readLines(file.path(dsnlayerC,"casfiles.txt"))
        
        
        
        download_results_tgcc()
        
        
        if(nb_proc_post==1){
          # ETAPE 6******ETAPE 6******ETAPE 6 ---------------------------------------  
          Etape6_fn()
          # ETAPE 7******ETAPE 7******ETAPE 7 ---------------------------------------
          Etape7_fn()
          # ETAPE 8******ETAPE 8******ETAPE 8 ---------------------------------------  
          Etape8_fn()
          # ETAPE 9******ETAPE 9******ETAPE 9 ---------------------------------------  
          Etape9_fn()
          # ETAPE 10******ETAPE 10******ETAPE 10 ---------------------------------------  
          Etape10_fn()
        }
        # ETAPE 11******ETAPE 11******ETAPE 11 ---------------------------------------   
        Etape11_fn()
        # ETAPE 13******ETAPE 13******ETAPE 13 ---------------------------------------   
        Etape13_fn()
        # ETAPE 14******ETAPE 14******ETAPE 14 --------------------------------------- 
        Etape14_fn()
        # ETAPE 15******ETAPE 15******ETAPE 15 --------------------------------------- 
        Etape15_fn()
        contours$STEP_PRE[nsecteur[ic]] <- 1
      }
      
    }
  }
  
  
  
}

if(ETPCALCUL[6]!=1 & ETPCALCUL[7]==1){
  fincalcul <- rep(1,length(nsecteur))
  nbfinalcul <- length(contours$STEP_PRE)-length(which(contours$STEP_PRE!=0))
  while (nbfinalcul>0) {
    
    testcalculist <- testfincalcul_ifremer(nsecteur,fincalcul,nbfinalcul)
    fincalcul <- testcalculist[[1]]
    # if (fincalcul!=-99) # Condition s'il n'y a pas eu de calcul
    # {
    nbfinalcul <- testcalculist[[2]]
    
    for (ic in 1:length(nsecteur)){
      
      if(fincalcul[ic]==3){
        contours$STEP_PRE[nsecteur[ic]] <- -2
        contours$BUG[nsecteur[ic]] <- "soucis dans le calcul cluster"
      }  
      
      if(fincalcul[ic]==2){  
        
        contour = contours[nsecteur[ic], ]
        if(contour$STEP_PRE!=0) next
        
        
        # Récupération du nom du contour
        nomcontour = contour$NOM
        dsnlayerC = file.path(dsnlayer,contour$NOMPOST, nomcontour)
        
        # contour$NOM=0 dans pputils, il ne veut qu'une colonne
        contourPPutils = contour[, 1]
        contourPPutils$NOM = 0
        
        name_cas_all <- readLines(file.path(dsnlayerC,"casfiles.txt"))
        
        
        
        download_results_ifremer()
        
        contours$STEP_PRE[nsecteur[ic]] <- 1
        
        # }
      }
    }
  }
}

if(ETPCALCUL[8]!=1 & ETPCALCUL[9]==1){
  fincalcul <- rep(1,length(nsecteur))
  nbfinalcul <- length(contours$STEP_PRE)-length(which(contours$STEP_PRE!=0))
  while (nbfinalcul>0) {
    
    testcalculist <- testfincalcul_tgcc(nsecteur,fincalcul,nbfinalcul)
    fincalcul <- testcalculist[[1]]
    # if (fincalcul!=-99) # Condition s'il n'y a pas eu de calcul
    # {
    nbfinalcul <- testcalculist[[2]]
    
    for (ic in 1:length(nsecteur)){
      
      if(fincalcul[ic]==3){
        contours$STEP_PRE[nsecteur[ic]] <- -2
        contours$BUG[nsecteur[ic]] <- "soucis dans le calcul cluster"
      }  
      
      if(fincalcul[ic]==2){  
        
        contour = contours[nsecteur[ic], ]
        if(contour$STEP_PRE!=0) next
        
        
        # Récupération du nom du contour
        nomcontour = contour$NOM
        dsnlayerC = file.path(dsnlayer,contour$NOMPOST, nomcontour)
        
        # contour$NOM=0 dans pputils, il ne veut qu'une colonne
        contourPPutils = contour[, 1]
        contourPPutils$NOM = 0
        
        name_cas_all <- readLines(file.path(dsnlayerC,"casfiles.txt"))
        
        
        
        download_results_tgcc()
        
        contours$STEP_PRE[nsecteur[ic]] <- 1
        
        # }
      }
    }
  }
}


if(length(which(ETPCALCUL==1))==0){
  if(nb_proc_post==1){
    
    
    for (ic in 1:length(nsecteur)){
      skip_to_next <- FALSE
      #############################################################################
      ################ CONTOUR #####################################################
      #############################################################################
      # Récupération du contour i
      contour = contours[nsecteur[ic], ]
      if(contour$STEP_PRE!=0) next
      
      # Récupération du nom du contour
      nomcontour = contour$NOM
      dsnlayerC = file.path(dsnlayer,contour$NOMPOST, nomcontour)
      
      # contour$NOM=0 dans pputils, il ne veut qu'une colonne
      contourPPutils = contour[,1]
      contourPPutils$NOM = 0
      # ETAPE 6******ETAPE 6******ETAPE 6 --------------------------------------- 
      skip_to_next <-  Etape6_fn()
      if(skip_to_next){
        contours$BUG[nsecteur[ic]] <- "Problème étape 6"
        contours$STEP_PRE[nsecteur[ic]] <- -2
        next
      }
      # ETAPE 7******ETAPE 7******ETAPE 7 ---------------------------------------
      skip_to_next <-  Etape7_fn()
      if(skip_to_next){
        contours$BUG[nsecteur[ic]] <- "Problème étape 7"
        contours$STEP_PRE[nsecteur[ic]] <- -2
        next
      }
      # ETAPE 8******ETAPE 8******ETAPE 8 ---------------------------------------  
      Etape8_fn()
      # ETAPE 9******ETAPE 9******ETAPE 9 ---------------------------------------  
      Etape9_fn()
      # ETAPE 10******ETAPE 10******ETAPE 10 ---------------------------------------  
      Etape10_fn()
      # ETAPE 11******ETAPE 11******ETAPE 11 ---------------------------------------   
      Etape11_fn()
      # ETAPE 13******ETAPE 13******ETAPE 13 ---------------------------------------   
      Etape13_fn()
      # ETAPE 14******ETAPE 14******ETAPE 14 --------------------------------------- 
      Etape14_fn()
      # ETAPE 15******ETAPE 15******ETAPE 15 --------------------------------------- 
      Etape15_fn()
      
    }
  }else{
    cl <- parallel::makeCluster(nb_proc_post) 
    doParallel::registerDoParallel(cl)
    foreach::foreach(ic = 1:length(nsecteur), 
                     .combine = 'c',
                     .multicombine=TRUE,
                     .inorder = FALSE,
                     .packages = c("sf","dplyr","utils","sfheaders","tidyverse","ggplot2","readxl","plotly")) %dopar%{
                       skip_to_next <- FALSE
                       #############################################################################
                       ################ CONTOUR #####################################################
                       #############################################################################
                       # Récupération du contour i
                       contour = contours[nsecteur[ic], ]
                       # if(contour$STEP_PRE!=0) next
                       
                       # Récupération du nom du contour
                       nomcontour = contour$NOM
                       dsnlayerC = file.path(dsnlayer,contour$NOMPOST, nomcontour)
                       
                       # contour$NOM=0 dans pputils, il ne veut qu'une colonne
                       contourPPutils = contour[,1]
                       contourPPutils$NOM = 0
                       SecteurGRASS=paste0(dirname(SecteurGRASS_),"_",format(Sys.time(),format="%Y%m%d_%H%M%S"),'/',contour$NOMPOST,"_",nomcontour,"/",basename(SecteurGRASS_))
                       
                       # ETAPE 6******ETAPE 6******ETAPE 6 --------------------------------------- 
                       Etape6_fn_parallel()
                       # ETAPE 7******ETAPE 7******ETAPE 7 ---------------------------------------
                       Etape7_fn_parallel()
                       
                       # ETAPE 9******ETAPE 9******ETAPE 9 ---------------------------------------  
                       Etape9_fn_parallel()
                       # ETAPE 10******ETAPE 10******ETAPE 10 ---------------------------------------  
                       Etape10_fn_parallel()
                       unlink(dirname(SecteurGRASS),recursive=TRUE)
                       
                       
                     }
    
    parallel::stopCluster(cl)
    
    cl <- parallel::makeCluster(nb_proc_post) 
    doParallel::registerDoParallel(cl)
    debit_sc <- foreach::foreach(icp = 1:length(nsecteur), 
                                 .combine = 'list',
                                 .multicombine = TRUE,
                                 .inorder = FALSE,
                                 .packages = c("sf","dplyr","utils","sfheaders","tidyverse","ggplot2","readxl")) %dopar%{
                                   # ETAPE 8******ETAPE 8******ETAPE 8 --------------------------------------- 
                                   skip_to_next <- FALSE
                                   #############################################################################
                                   ################ CONTOUR #####################################################
                                   #############################################################################
                                   # Récupération du contour i
                                   contour = contours[nsecteur[icp], ]
                                   # if(contour$STEP_PRE!=0) next
                                   
                                   # Récupération du nom du contour
                                   nomcontour = contour$NOM
                                   dsnlayerC = file.path(dsnlayer,contour$NOMPOST, nomcontour)
                                   
                                   # contour$NOM=0 dans pputils, il n
                                   debits_df=Etape8_fn_parallel()
                                   return(debits_df)
                                   
                                 }
    
    parallel::stopCluster(cl)
    
    
    
  }
}
#-----------------------------------------------------

# ETAPE 7******ETAPE 7******ETAPE 7 --------------------------------------- 
if (ETAPE[7] == 1)
{
  # recup de la longeur
  contours[, 'DUREE2'] = 0.01 * floor(100 * (55 / 60 + (contours$Longueur /
                                                          (3600 * 0.75))))
  print(cbind(contours$NOM, contours$DURATION, contours$DUREE))
  contours[, 'DUREE3'] = min(cbind(contours$DUREE2, contours$DURATION))
  st_write(
    contours,
    dsn = file.path(dsnlayer, paste0(nomlayerC, "_5.shp")),
    layer = paste0(nomlayerC, "_"),
    delete_layer = T,
    quiet = T
  )
}


# ETAPE 8******ETAPE 8******ETAPE 8 --------------------------------------- 
# if(nb_proc_post==1){


if (ETAPE[8] == 1)
{
  TabDebit = do.call(rbind, debit_sc)
  contours[nsecteur, colnames(debit_sc)] = do.call(rbind, debit_sc)
  BeauDebit = merge(contours[nsecteur, ], TabDebit)
  file.remove(file.path(dsnlayer, paste0(nomlayerC, "_DEBIT_SCENARIO.shp")))
  st_write(
    BeauDebit,
    dsn = file.path(dsnlayer, paste0(nomlayerC, "_DEBIT_SCENARIO.shp")),
    layer = paste0(nomlayerC, "_"),
    delete_layer = T,
    quiet = T
  )
}
# }

# ETAPE 12******ETAPE 12******ETAPE 12 ---------------------------------------
if (ETAPE[12] == 1){
  # SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
  # contours <- st_read(file.path(dsnlayer,nomlayerC))
  # st_crs(contours) = crs
  
  # là où on a tous les fichiers de qml, cas mascaret, qgz...
  dsnlayer_PreRequis = "\\_Cartino2D_PreRequis"
  
  nsecteur = which(contours$STEP_PRE == 0)
  # print(contours$NOM)
  cat("Traitement de: ",contours[nsecteur, ]$NOM,"\n")
  # contours <- contours[nsecteur,]
  nsecteur <- 1:length(contours$NOM)
  Etape12_FusionASC(dsnlayer,contour,contours,SecteurGRASS_,BatGRASS,EPSG,OSGeo4W_path,NProcGrass,chem_routine,Doss_Qml)
}

if (ETAPE[1]==1)
{
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- ETAPE 1 permet de: \n")
  cat("1/ Faire des tampons positifs et négatifs pour définir les zones de calcul et la zone valide de chaque secteur\n")
  cat("2/ Modifier le MNT soit en abaissant sur tous les côtés (Pluie) ou en gérant la frontière aval (Débit)\n")
  cat("3/ Préparer les données optionnelles de Strickler, CN, d'ouvrages, de sections de controles...\n")
  cat("\n")
  cat("Les fichiers d'entrée pour l'étape suivante sont ainsi créés\n")
  cat("Vous pouvez vérifier vos résultats en allant dans les dossiers associés au champ NOMPOST de vos secteurs\n")
  cat("Par exemple, le dernier dossier est:",file.path(dsnlayer,contour$NOMPOST,contour$NOM),"\n")
  cat("Un projet QGIS",file.path(dsnlayer,contour$NOMPOST,contour$NOM,"Cartino2D_ProjetQgis.qgz")," est disponible\n")
  cat("Merci de zoomer sur une couche disponible, l'ouverture sur la localisation de votre secteur n'est pas gerée\n")
  cat("Merci de ne pas fermer les couches indisponibles dans le projet Qgis, certaines seront créées ultérieurement\n")
  cat("ATTENTION à fermer le projet QGIS avant les étapes suivantes\n")
  cat("######################### Fin C2D A LIRE ###############################################################\n")
}
if (ETAPE[2]==1)
{
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- ETAPE 2 permet de réaliser le maillage: \n")
  cat("1/ Réglé\n")
  cat("2/ Destructuré\n")
  cat("le fichier SELAFIN de Telemac est ainsi créé\n")
  cat("\n")
  cat("Les fichiers d'entrée pour l'étape suivante sont ainsi créés\n")
  cat("Vous pouvez vérifier vos résultats en allant dans les dossiers associés au champ NOMPOST de vos secteurs\n")
  cat("Par exemple, le dernier dossier est:",file.path(dsnlayer,contour$NOMPOST,contour$NOM),"\n")
  cat("Un projet QGIS",file.path(dsnlayer,contour$NOMPOST,contour$NOM),"/Cartino2D_ProjetQgis.qgz est disponible\n")
  cat("Le fichier SELAFIN ",file.path(dsnlayer,contour$NOMPOST,contour$NOM,"Cartino2D.slf"),"\n")
  cat("ne peut pas être ouvert directement dans QGIS, vous devez l'ouvrir manuellement\n")
  cat("ATTENTION à fermer le projet QGIS avant les étapes suivantes\n")
  cat("######################### Fin C2D A LIRE ###############################################################\n")
}
if (ETAPE[3]==1)
{
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- ETAPE 3 permet de traiter les conditions limites: \n")
  cat("ATTENTION, toujours vérifier ces données de frontières en cas de bug de Telemac étape suivante!\n")
  cat("\n")
  cat("Les fichiers d'entrée pour l'étape suivante sont ainsi créés\n")
  cat("Vous pouvez vérifier vos résultats en allant dans les dossiers associés au champ NOMPOST de vos secteurs\n")
  cat("Par exemple, le dernier dossier est:",file.path(dsnlayer,contour$NOMPOST,contour$NOM),"\n")
  cat("Un projet QGIS",file.path(dsnlayer,contour$NOMPOST,contour$NOM),"/Cartino2D_ProjetQgis.qgz est disponible\n")
  cat("ATTENTION à fermer le projet QGIS avant les étapes suivantes\n")
  cat("######################### Fin C2D A LIRE ###############################################################\n")
}
if (ETAPE[4]==1)
{
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- ETAPE 4 permet de créer les fichiers cas de Telemac: \n")
  cat ("Vous avez généré plusieurs fichiers cas en fonction du nombre de scénarios de pluie ou de débit prédéfinis\n")
  cat ("Les altitudes initiales, des conditions limites, des pas de temps, nombre d'itérations, variables d'export sont générées.\n")
  cat("Les données optionnelles d'ouvrages, de sections de controles, d'obturation des ouvrages sont ainsi créés\n")
  cat("Quelques images jpg sot produites pour voir la(les) pluie(s) en entrée de vos cas\n")
  cat("\n")
  cat("Par exemple, le dernier dossier est:",file.path(dsnlayer,contour$NOMPOST,contour$NOM),"\n")
  cat("Les fichiers d'entrée pour l'étape suivante de lancement de Telemac sont ainsi créés\n")
  T2D_Cas=list.files(file.path(dsnlayer,contour$NOMPOST,contour$NOM),".cas$")
  T2D_Cas=T2D_Cas[-which(T2D_Cas=="Cartino2D.cas")]
  print(as.data.frame(T2D_Cas))
  cat("\n")
  cat("Si vous n'avez aucun fichier, vous n'avez pas de fichier d'entrée de pluie ou de débit\n")
  cat("Pour la pluie\n")
  cat("1/ Voir dans le dossier ",dsnPluie,"\n")
  cat("2/ Lancer PreC2D sur les pluies SHYREG (INRAE) ou des pluies historiques\n")
  cat("Pour les débits, plus complexe...\n")
  cat("######################### Fin C2D A LIRE ###############################################################\n")
}
if (ETAPE[5]==1)
{
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- ETAPE 5 permet de lancer les cas de Telemac: \n")
  cat("Par exemple, dans le dossier ",dsnlayerC,"\n")
  cat("Lorsque les calculs sont finis sur votre PC/station ou après chargement des données d'un centre de calcul\n")
  cat("Vous avez généré un(des) fichiers .res de type SELAFIN que vous pouvez ouvrir dans QGIS\n")
  T2D_Res=list.files(file.path(dsnlayer,contour$NOMPOST,contour$NOM),".res$")
  print(as.data.frame(T2D_Res))
  cat("\n")
  cat("Vous avez généré un(des) fichiers .sortie pour suivre le log. Ce fichier peut être utile pour analyser les instabilités de Telemac\n")
  T2D_Sortie=list.files(file.path(dsnlayer,contour$NOMPOST,contour$NOM),".sortie$")
  print(as.data.frame(T2D_Sortie))
  cat("\n")
  cat("Vous avez aussi generé des fichiers de sortie texte dans le cas de données optionnelles d'ouvrages, de sections de controles\n")
  cat("\n")
  cat("Par exemple, le dernier dossier est:",file.path(dsnlayer,contour$NOMPOST,contour$NOM),"\n")
  cat("Maintenant il va falloir exploiter ces résultats\n")
  cat("Après expérience; il est d'usage de lancer les étapes 6/7/8 et même 9 en même temps\n")
  cat("######################### Fin C2D A LIRE ###############################################################\n")
}
if (ETAPE[6]==1)
{
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- ETAPE 6 permet de faire un bilan de volume et débit: \n")
  cat("sous forme d'image Fichier jpg\n")
  cat("Fichier gpkg de débit max aux ouvrages\n")
  cat("\n")
  cat("Nabil, à changer après tes reprises de post\n")
  cat("\n")
  cat("Par exemple, le dernier dossier est:",file.path(dsnlayer,contour$NOMPOST,contour$NOM),"\n")
  cat("######################### Fin C2D A LIRE ###############################################################\n")
}
if (ETAPE[8]==1)
{
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- ETAPE 6 (ancien 8) permet de faire un bilan de volume et débit et sections de controle: \n")
  cat("Fichiers html jpg\n")
  cat("Fichier gpkg de débit max aux ouvrages\n")
  cat("\n")
  cat("Nabil, à changer après tes reprises de post\n")
  cat("\n")
  cat("Par exemple, le dernier dossier est:",file.path(dsnlayer,contour$NOMPOST,contour$NOM),"\n")
  cat("######################### Fin C2D A LIRE ###############################################################\n")
}
if (ETAPE[9]==1)
{
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- ETAPE 7 (ancien 9) Conversion du fichier Res en Raster: \n")
  cat("Les fichiers res sont convertis en raster sur l'ensemble de la zone de calcul\n")
  cat("Attendre l'étape suivante pour analyser les résultats, aucun qml disponible\n")
  cat("Sauf si vous avez des grosses instabilités, on peut mieux comprendre ce qui se passe aux frontières\n")
  cat("Les extensions sont en .ASC alors que ce sont des TIF pour faciliter le bascule entre le POST QGIS auto (privilegié) et le POST PPUTILS en réserve\n")
  cat("\n")
  cat("Par exemple, le dernier dossier est:",file.path(dsnlayer,contour$NOMPOST,contour$NOM,"POST"),"\n")
  cat("Les fichier de débits linéiques scalaires sont les suivants:\n")
  T2D_HSF=list.files(file.path(dsnlayer,contour$NOMPOST,contour$NOM,"POST"),"HSF_m2_s_Brut.asc")
  print(as.data.frame(T2D_HSF))
  cat("######################### Fin C2D A LIRE ###############################################################\n")
}
if (ETAPE[10]==1)
{
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- ETAPE 8  (ancien 10) Raster conservé uniquement dans l'emprise VALIDE: \n")
  cat("Les fichiers RASTER sont découpés sur la zone de validité\n")
  cat("Vous pouvez analyser vos résultats format GPKG en SIG secteur par secter si vous le voulez\n")
  cat("Vous pouvez aussi attendre l'étape 12 si vous avez de très nombreux secteurs\n")
  cat("Nom code HWM_m    => High Water Mark soit Cote d'Eau Max\n")
  cat("Nom code HV_m_s   => High Velocity => Vitesse Max\n")
  cat("Nom code HSF_m2_s => Débit scalaire Max\n")
  cat("Nom code Bottom   => Bottom => Topographie\n")
  cat("Nom code HWT_s    => High Water Time => Temps du pic de cote d'eau\n")
  cat("Nom code HWH_m    => High Water Heigth => Hauteur d'eau maximum\n")
  cat("D'autres codes peuvent apparaitre en fonction de choix du paramètre PRINTPERIO\n")
  cat("\n")
  cat("Par exemple, le dernier dossier est:",file.path(dsnlayer,contour$NOMPOST,contour$NOM,"POST"),"\n")
  cat("Les fichier de Hauteur d'eau sont les suivants:\n")
  T2D_HWH=list.files(file.path(dsnlayer,contour$NOMPOST,contour$NOM,"POST"),"HWH_m.gpkg$")
  print(as.data.frame(T2D_HWH))
  cat("######################### Fin C2D A LIRE ###############################################################\n")
}
if (ETAPE[12]==1)
{
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- ETAPE 9 (ancien 12) Fusion des raster de plusieurs sceteurs sur un même scénario de forçage: \n")
  cat("Les fichiers RASTER de chaque secteur et d'un même scénario sont fusionnés en récupérant le max\n")
  cat("Vous devez analyser vos résultats format GPKG en SIG dans le répertoire _FUSION\n")
  cat("\n")
  cat("Par exemple, le dernier travail se trouve dans le dossier:",file.path(dsnlayer,contour$NOMPOST,"_FUSION"),"\n")
  cat("Les fichier de Hauteur d'eau sont les suivants:\n")
  T2D_HWH=list.files(file.path(dsnlayer,contour$NOMPOST,"_FUSION"),"HWH_m.gpkg$")
  print(as.data.frame(T2D_HWH))
  cat("######################### Fin C2D A LIRE ###############################################################\n")
}
