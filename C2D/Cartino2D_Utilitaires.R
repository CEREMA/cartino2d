#################################################################################################
################   Buf_et_Coup
#################################################################################################
#-----------------------------------------------------------------------
#----------------DataObs_choix----------------------------
#-----------------------------------------------------------------------
# Gestion des différents formats de pluie

DataObs_choix=function()
{
  library(readxl)
  chem_routine=R.home(component = "Cerema")
  paste0(chem_routine,"/PreC2D")
  Link_Data = "Data_obs.xlsx"
  Link_DataXLS = read_excel(file.path(chem_routine,"PreC2D",Link_Data))
  Link_DataXLS=Link_DataXLS[order(Link_DataXLS$Evenements),]
  nchoix = select.list(Link_DataXLS$Evenements,title = "Choix", multiple = T, graphics = T)
  Search = which(Link_DataXLS$Evenements %in% nchoix)
  ListeObs = as.data.frame(Link_DataXLS[Search,])
  # iShyreg=which(Link_DataXLS$TypPluie=="Stat" & Link_DataXLS$EPSG==ListeCas$EPSG)
  # Link_DataXLS$Dossier_PluieRaster[iShyreg]
  # ListeCas$Dossier_SHYREG=Link_DataXLS$Dossier_PluieRaster[iShyreg]
  return(ListeObs)
}
# Tampon et coupure commune à tous les contours Exzeco
Buf_et_Coup = function(dsnlayerC,
                       nomsortie,
                       nom_STEP2_Zone,
                       Zone,
                       ContourMax,
                       Exz_Buf,
                       Exz_Dx,
                       Ratio_Dx,
                       crs)
{
  ###########################################################################
  # Exzeco buffer exzeco pour nouvelle 2eme contrainte point
  
  Exz <- Zone %>%
    st_buffer(Exz_Buf) %>%
    st_cast("MULTILINESTRING",
            do_split = TRUE) %>%
    st_cast("LINESTRING",
            do_split = TRUE) %>%
    st_intersection(ContourMax) %>%
    st_simplify(preserveTopology = FALSE,
                dTolerance = 0.33 * Exz_Dx) %>%
    st_segmentize(Exz_Dx, crs = crs)
  
  st_crs(Exz) = crs
  
  ###########################################################################
  print("Possible plantage export st_write.... - mettre en commentaires si c'est le cas")
  # st_write(Exz,dsn=file.path(dsnlayerC,paste0(nomsortie,".shp")),layer=nomsortie, delete_layer=T, quiet=T)
  Exz = st_collection_extract(Exz, "LINESTRING")
  st_write(
    Exz,
    dsn = file.path(dsnlayerC, paste0(nomsortie, ".shp")),
    layer = nomsortie,
    delete_layer = T,
    quiet = T
  )
  file.copy(file.path(dsnlayer, paste0(nom_STEP2_Zone, ".qml")),
            file.path(dsnlayerC, paste0(nomsortie, ".qml")),
            copy.date = TRUE)
  
  # FONCTIONNettoyage des points trop proches
  Temp = st_sf(col = 0, st_cast(st_sfc(st_cast(
    st_sfc(Exz), "MULTIPOINT"
  )), "POINT"))
  
  Pts = st_coordinates(Temp)
  Proches = sapply(1:dim(Pts)[1],
                   function(x) {
                     ifelse((min((Pts[-x, 1] - Pts[x, 1]) ^ 2 + (Pts[-x, 2] - Pts[x, 2]) ^ 2) < (Ratio_Dx * Exz_Dx) ^ 2), 1, 0)
                   })
  
  Exz_Pts = Pts[which(Proches == 0), ]
  print(dim(Exz_Pts))
  
  return(list(Exz_Pts = Exz_Pts, Exz = Exz))
}

#################################################################################################
################   Gestion_Sections_Controle
#################################################################################################

# Gestion des sections de controle
Gestion_Sections_Controle = function(dsnlayer, dsnlayerC, contour, ExportTopo)
{
  nom_SectCont = contour$SectCont
  if (file.exists(nom_SectCont) == TRUE)
  {
    # Lecture du réseau
    SectControl = st_read(nom_SectCont)
    
    # On récupère ce qui intersecte
    # nbRC=st_intersects(SectControl,contour)
    contour_2 <- sfheaders::sf_remove_holes(contour)
    nbRC = st_within(SectControl, contour_2)
    
    n_int = which(sapply(nbRC, length) > 0)
    
    #
    if (length(n_int > 0) > 0)
    {
      print(paste0(nomcontour, " ---- Récupération des sections de controle"))
      SectControl = SectControl[n_int, ]
      st_write(
        SectControl,
        dsn = file.path(dsnlayerC, "SectControl_Select.shp"),
        delete_layer = T,
        quiet = T
      )
      
      if (ExportTopo == 1)
      {
        for (is in 1:dim(SectControl)[1])
        {
          nom_contour = paste0("Sect", is)
          nom_contour_gpkg=file.path(dsnlayerC,paste0(nom_contour,".gpkg"))
          ######################################################################
          #GRAVe valeur autour de la section, à paramétrer
          Chasse = 5
          st_write(st_buffer(SectControl[is, ], Chasse),nom_contour_gpkg,delete_layer=TRUE,quiet=T)
          
          # importation vecteur dans grass grand contour
          cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nom_contour_gpkg," output=",nom_contour)
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          # browser()
          unlink(nom_contour_gpkg)
          
          cmd=paste0("r.mask -r")
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          cmd=paste0("r.mask --quiet --overwrite vector=",nom_contour)
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          
          nomoutput = file.path(dsnlayerC, paste0(SectControl[is, ]$ID, "topo.xyz"))
          cmd=paste0("r.out.xyz --quiet --overwrite input=","MNTFinal"," output=",nomoutput," separator=space")
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          cmd=paste0("r.mask -r")
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          xyz = read.table(nomoutput)
          ligne = st_coordinates(SectControl[is, ])
          browser()
          UV = ligne[2, ] - ligne[1, ]
          UV = UV / sum(UV ^ 2) ^ 0.5
          browser()
          nom = SectControl[is, ]$ID
          nom2 = strsplit(nom, "_")
          lg = nchar(nom2[[1]][length(nom2[[1]])])
          nom3 = substr(nom, 1, nchar(nom) - (lg + 1))
          Cote = as.numeric(nom2[[1]][length(nom2[[1]])])
          
          xyz$S = (xyz[, 1] - ligne[1, 1]) * UV[1] + (xyz[, 2] - ligne[1, 2]) *
            UV[2]
          xyz$AmAv = (xyz[, 1] - ligne[1, 1]) * UV[2] - (xyz[, 2] - ligne[1, 2]) *
            UV[1]
          
          nomoutput = file.path(dsnlayerC, paste0(SectControl[is, ]$ID, "topoSC.xyz"))
          write.table(xyz, nomoutput, row.names = F)
          
          nbcote = which(xyz$AmAv >= 0)
          
          # Enregistrement de l'image
          jpeg(
            filename = paste0(dsnlayerC, "\\", nom, "_topo.jpg"),
            width = 31.5,
            height = 44.55,
            units = "cm",
            quality = 75,
            res = 200
          )
          
          
          plot(xyz[nbcote, 4], xyz[nbcote, 3], main = nom)
          points(xyz[-nbcote, 4], xyz[-nbcote, 3], pch = 2)
          
          dz = 0.5
          points(
            c(min(xyz[, 4]), max(xyz[, 4])),
            c(Cote, Cote),
            "l",
            col = "blue",
            lwd = 3,
            lty = 1
          )
          points(
            c(min(xyz[, 4]), max(xyz[, 4])),
            c(Cote - dz, Cote - dz),
            "l",
            col = "blue",
            lwd = 2,
            lty = 3
          )
          points(
            c(min(xyz[, 4]), max(xyz[, 4])),
            c(Cote + dz, Cote + dz),
            "l",
            col = "blue",
            lwd = 2,
            lty = 3
          )
          dev.off()
        }
      }
    }
  }
}

#################################################################################################
################   Ligne limitée  à 70 caractères
#################################################################################################

LigneBonneLongueur = function(Lignes, liglong, Texte)
{
  afaire = 1
  inc = length(Lignes) + 1
  
  while (afaire == 1)
  {
    # print(Lignes)
    # print(Texte)
    if (is.na(Texte) == F)
    {
      if (nchar(Texte) > liglong)
      {
        indvirg = gregexpr(";", Texte)[[1]]
        if (max(indvirg) < liglong)
        {
          Lignes[inc] = substr(Texte, 1, max(indvirg))
          Texte = substr(Texte, max(indvirg) + 1, nchar(Texte))
        } else{
          Lignes[inc] = substr(Texte, 1, indvirg[which((indvirg < liglong) == F)[1] -
                                                   1])
          Texte = substr(Texte, indvirg[which((indvirg < liglong) == F)[1] -
                                          1] + 1, nchar(Texte))
        }
        inc = inc + 1
        
      }
    }
    
    print(Lignes)
    print(Texte)
    
    if (is.na(Texte) == F)
    {
      if (nchar(Texte) <= liglong)
      {
        Lignes[inc] = Texte
        inc = inc + 1
        afaire = 0
      }
    } else
    {
      afaire = 0
      # Lignes[inc]=""
    }
  }
  return(Lignes)
}

#################################################################################################
################   Enleve les trous d'un vecteur
#################################################################################################

EnleveTrou = function(vecteur)
{
  # # Zones d'intérêts
  # dsnlayer="D:\\Tempete_Alex_2020\\Moi\\BaseEmprise"
  # nomlayer=  "EAIP_RTM_ExzItalie_Telemac1"
  #
  # #Zones d'intéret
  # vecteur=st_read(dsnlayer,
  #                 layer = nomlayer)
  vecteur = st_cast(vecteur, "POLYGON")
  crs = st_crs(EPSG)
  Res = list()
  for (i in 1:dim(vecteur)[1])
  {
    Zone_ok = vecteur[i, ]
    Zone_ok = st_cast(Zone_ok[st_area(Zone_ok) == max(st_area(Zone_ok)), ], "POLYGON")
    nb1 = which(st_coordinates(Zone_ok)[, 3] == 1 &
                  st_coordinates(Zone_ok)[, 4] == 1)
    toto = list()
    toto[[1]] = st_coordinates(Zone_ok)[nb1, 1:2]
    Res[[i]] = st_sf(data.frame(Aire = "0"),
                     "geometry" = st_sfc(st_polygon(toto, dim = "XY")),
                     crs = crs)
  }
  final = do.call(rbind, Res)
  final[, "Aire"] = round(st_area(final) / 10)
  final = st_buffer(final, 0)
  # st_write(final ,file.path(dsnlayer,"final.shp"), delete_layer=T, quiet=T)
  return(final)
}

#################################################################################################
################   enleve les nouveaux points trop proche des anciens
#################################################################################################

GardeNvxPointsLoinPointsInitiaux = function(NewPoints, IniPoint, Ratio, Dist)
{
  for (incIni in (1:dim(IniPoint)[1]))
  {
    if (length(NewPoints) > 1)
    {
      asupp = which((NewPoints[, 1] - IniPoint[incIni, 1]) ^ 2 + (NewPoints[, 2] -
                                                                    IniPoint[incIni, 2]) ^ 2 < (Ratio * Dist) ^ 2)
      if (length(asupp) > 0)
      {
        NewPoints = NewPoints[-asupp, ]
      }
    }
    # cat(dim(NewPoints)," ",length(NewPoints),"\n")
    if (length(NewPoints) == 2)
    {
      NewPoints = NA
    }
  }
  return(NewPoints)
}


####### fichier prérequis copie ###################
# Copie du dossier base dans dossier export
# copie du cas 
# copie des bons fichiers fortran
# si pluie spatialisée raindef=4

copie_prerequis = function()
{
  if (file.exists(dsnlayerC)==F){dir.create(dsnlayerC, recursive = T)}
  qmlcasqgz = list.files(file.path(chem_routine,"C2D",Doss_Base), recursive = F)
  nici=grep(qmlcasqgz,pattern="USER_FORTRA")
  if (length(nici)>0){qmlcasqgz=qmlcasqgz[-nici]}
  file.copy(file.path(chem_routine,"C2D",Doss_Base, qmlcasqgz),
            file.path(dsnlayerC, qmlcasqgz))
  
  for (USER_FORTRAN in c("USER_FORTRAN_PH","USER_FORTRAN_PS"))
  {
    if (file.exists(file.path(dsnlayerC,USER_FORTRAN))==F){dir.create(file.path(dsnlayerC,USER_FORTRAN))}
    
    qmlcasqgz = list.files(file.path(chem_routine,"C2D",Doss_Base, USER_FORTRAN))
    
    if(is.na(contour$OH)){
      qmlcasqgz <- qmlcasqgz[-which(qmlcasqgz=="buse.f" | qmlcasqgz=="debsce.f" )]
      
    }
    file.copy(
      file.path(chem_routine,"C2D",Doss_Base, USER_FORTRAN, qmlcasqgz),
      file.path(dsnlayerC, USER_FORTRAN, qmlcasqgz)
      ,overwrite = TRUE)
  }
}



########################plot section de controle 

#
plot_sc_obs_oh <- function()
{
  
  SC_list <- list.files(dsnlayerC,pattern = "SC.txt")
  ################################graphiques html des sections de contrôle###############################
  if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp")) & !is_empty(SC_list))
  {
    if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
      
      sc_sf <- st_read(file.path(dsnlayerC,"SectControl_Select.shp"))
      if("Q" %in% names(sc_sf)){
        sc_sf <- sc_sf[,-which(names(sc_sf)=="Q")]
        st_write(sc_sf,file.path(dsnlayerC,"SectControl_Select.shp"),delete_dsn = TRUE)
        sc_sf <- st_read(file.path(dsnlayerC,"SectControl_Select.shp"))
        
      }
      
      cl_names <- c("nom","numsec","Time","Q","Zmoy","Zmax","Zmin","Hmax","Htot")
      # ListeObs <- DataObs_choix()
      funx <- function(x){
        as.numeric(x)
      }
      
      
    }
    ############## resultat nabil
    
    dsnscobs <- file.path(dsnlayer,"_SectionsControles",contour$NOMPOST)
    for (i_filesc in 1:length(SC_list)) {
      
      
      # name_case <- paste0("brute_",strsplit(SC_list[i_filesc],"SC")[[1]][1])
      name_case_split <- strsplit(paste0("brute_",SC_list[i_filesc]),"_")[[1]]
      name_case <- do.call(paste, c(as.list(name_case_split[1:6]), sep = "_"))
      chem_pluie <- file.path(dsnPluie,contour$NOMPOST)
      file_brute <- list.files(chem_pluie,pattern = name_case,full.names = T)
      file_brute <- file_brute[1]
      
      if(!is.na(file_brute)){
        tab_brute <- read.table(file_brute,header = TRUE)
        
      }
      
      
      if(grepl("Evt", SC_list[i_filesc], fixed = TRUE)){
        
        name_split <- strsplit(SC_list[i_filesc],"_")[[1]]
        
        DUREE=(as.numeric(substr(name_split[5],1,2))+as.numeric(substr(name_split[5],4,5))/60)*3600
        
        datedebut <- gsub('[^0-9]','',name_split[4])
        datedebut <- strptime(x = datedebut,format = "%Y%m%d%H%M" )
        
        
        tab_sc_path <- file.path(dsnlayerC,SC_list[i_filesc])
        tab_sc <- read.csv(file = tab_sc_path,
                           header =FALSE,sep = "",skip = 2,col.names = cl_names 
        )
        # GRAVE NABIL
        # debut_time <- which(tab_sc$Time=="60.00" | tab_sc$Time==60)
        pasexport=60
        pasexportstr=sprintf("%.2f", pasexport)
        debut_time <- which(tab_sc$Time==pasexportstr | tab_sc$Time==pasexport)
        if(min(debut_time)>1){
          tab_sc <- tab_sc[-(1:(debut_time-1)),]
        }
        
        
        tab_sc[,2:length(colnames(tab_sc))] <- data.frame(lapply(tab_sc,funx))[,2:length(colnames(tab_sc))]
        tab_sc$DATE <- datedebut+as.numeric(tab_sc$Time)
        # tab_sc <- inner_join(tab_sc,sc_sf,by = c("nom"="ID"))
        
        
      }else{
        
        name_split <- strsplit(SC_list[i_filesc],"_")[[1]]
        
        
        DUREE=(as.numeric(substr(name_split[7],1,2))+as.numeric(substr(name_split[8],1,2))/60)*3600
        
        tab_sc_path <- file.path(dsnlayerC,SC_list[i_filesc])
        tab_sc <- read.csv(file = tab_sc_path,
                           header =FALSE,sep = "",skip = 2,col.names = cl_names 
        )
        # GRAVE NABIL
        # debut_time <- which(tab_sc$Time=="60.00" | tab_sc$Time==60)
        pasexport=60
        pasexportstr=sprintf("%.2f", pasexport)
        debut_time <- which(tab_sc$Time==pasexportstr | tab_sc$Time==pasexport)
        if(min(debut_time)>1){
          tab_sc <- tab_sc[-(1:(debut_time-1)),]
        }
        
        tab_sc[,2:length(colnames(tab_sc))] <- data.frame(lapply(tab_sc,funx))[,2:length(colnames(tab_sc))]
        
        
        
      }
      
      
      
      tabmaxq <- tab_sc %>% 
        group_by(nom) %>% 
        filter(abs(Q) == max(abs(min(Q)),abs(max(Q)))) %>% 
        filter(Time == min(Time))
      tabmaxq$nom <- as.character(tabmaxq$nom)
      sc_sf$ID <- as.character(sc_sf$ID)
      if("Q" %in% names(sc_sf)){
        sc_sf <- sc_sf[,-which(names(sc_sf)=="Q")]
      }
      new_sc_sf <- left_join(x =sc_sf,y = tabmaxq,by=c("ID"="nom") )
      new_sc_sf$Q <- abs(new_sc_sf$Q )
      if(grepl("Evt", SC_list[i_filesc], fixed = TRUE)){
        new_sc_sf$DATE <- as.character(new_sc_sf$DATE)
      }
      
      new_sc_sf$NomSecteur=basename(dsnlayerC)
      new_sc_sf$H_Obs=paste0(tools::file_path_sans_ext(SC_list[i_filesc]) ,"_PN_PB_H_Obs.html")
      new_sc_sf$Q_Obs=paste0(tools::file_path_sans_ext(SC_list[i_filesc]) ,"_PN_PB_Q_Obs.html")
      new_sc_sf$Z_Obs=paste0(tools::file_path_sans_ext(SC_list[i_filesc]) ,"_PN_PB_Z_Obs.html")
      if("fid" %in% names(new_sc_sf)){
        new_sc_sf <- new_sc_sf[,-which(names(new_sc_sf)=="fid")]
      }
      st_write(new_sc_sf,
               dsn = file.path(dsnlayerC,paste0(tools::file_path_sans_ext(SC_list[i_filesc]) ,"_Qmax.gpkg")),driver = "GPKG",
               delete_dsn = TRUE,delete_layer = TRUE)
      
      file.copy(file.path(chem_routine,"C2D",Doss_Qml,"SC_Qmax.qml"),
                file.path(dsnlayerC,paste0(tools::file_path_sans_ext(SC_list[i_filesc]) ,"_Qmax.qml")))
    }
  }
  # browser()
  ############################ hyetogrammes et cumuls de pluie##############################
  Listsortie=list.files(file.path(dsnlayerC),pattern="s.sortie")
  
  FrEng=rbind(cbind("Intensité Pluie (mm/h)","Rainfall intensity (mm/h)"),
              cbind("Début Evènement ","Start of the event "),
              cbind("Cumul Pluie (mm)","Rainfall accumulation"),
              cbind("Pluie brute","Total rainfall"),
              cbind("Pluie nette","Efficient rainfall"),
              cbind("",""))
  iFRENG=1
  
  print(Listsortie)
  if (length(grep(Listsortie,pattern = '.txt'))>0){Listsortie=Listsortie[-grep(Listsortie,pattern = '.txt')]}
  print(Listsortie)
  
  
  if (length(Listsortie)>0)
  { 
    ContourAire=st_read(file.path(dsnlayerC,"Step_Cartino2d_1_Contour.shp"))
    Aire=st_area(ContourAire)
    units(Aire)=NULL
    # Fonction pour extraire la racine commune 
    
    
    if(length(SC_list)>0){
      extraire_racine <- function(fichier_txt, fichiers_sortie) {
        racine <- sub("\\SC.txt$", "", fichier_txt)
        correspondances <- grep(racine, fichiers_sortie, value = TRUE)
        return(correspondances)
      }
      # Appliquer la fonction à chaque fichier txt
      resultat <- lapply(SC_list, extraire_racine, Listsortie)
      
      # Nommer les éléments de la liste pour une meilleure lisibilité
      names(resultat) <- SC_list
    }else{
      
      resultat=Listsortie
    }
    
    
    
    
    # Pour chaque fichier txt, si plusieurs fichiers de sortie correspondent, ne garder que le plus récent
    resultat_final <- lapply(resultat, function(x) {
      if (length(x) > 1) {
        dates <- sub(".*\\.cas_(\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s)\\.sortie", "\\1", x)
        x[which.max(as.POSIXct(dates, format = "%Y-%m-%d-%Hh%Mmin%Ss"))]
      } else {
        x
      }
    })
    Listsortie <- resultat_final
    
    
    
    for (ils in 1:length(Listsortie)){
      nomsortie=Listsortie[ils]
      print(nomsortie)
      
      substr(nomsortie,1,nchar(nomsortie)-30)
      
      
      if (file.exists(file.path(dsnlayerC,substr(nomsortie,1,nchar(nomsortie)-30)))==T)
      {
        Lignes=readLines(con=file.path(dsnlayerC,substr(nomsortie,1,nchar(nomsortie)-30)))
        
        RecupTps=rbind("TIME STEP ")
        indic=regexpr(RecupTps[1,1],as.character(Lignes))
        Past_Ts=as.numeric(substr(Lignes[which(indic>-1)],38,nchar(Lignes[which(indic>-1)])))
        
        Lignes=readLines(con=file.path(dsnlayerC,nomsortie))
        MotsCles=rbind(cbind("ITERATION ",12,22),
                       cbind("RUNOFF_SCS_CN : ACCUMULATED RAINFALL",44,61),
                       cbind("VOLUME IN THE DOMAIN",28,44),
                       cbind("ADDITIONAL VOLUME DUE TO SOURCE TERMS",44,61),
                       cbind("RELATIVE ERROR IN VOLUME AT T",58,75))
        
        
        
        
        for (ic in 1:dim(MotsCles)[1]){
          # print(MotsCles[ic,1])
          indic=regexpr(MotsCles[ic,1],as.character(Lignes))
          correspond=which(indic>-1)
          # Ajout car j ai ecrit dans buse.f plusieurs fois culvert...
          correspond=correspond[which(is.na(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3])))==F)]
          
          if (ic==1)
          {
            correspond <- correspond[-1]
            niter=length(correspond)
            tab=matrix(0,niter,dim(MotsCles))
            referenceT=correspond
            tab[,1]=matrix(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3])))
          }else{
            if (length(correspond)>0)
            {
              if(length(correspond)==length(referenceT)){
                tab[,ic]=matrix(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3]))) 
                
              }else{
                bonindice=sapply(1:length(correspond), function(x) {which(referenceT>=correspond[x])[1]})
                if(!is_empty(which(is.na(bonindice))) ){
                  correspond=correspond[-which(is.na(bonindice))]
                  bonindice=bonindice[-which(is.na(bonindice))]
                  
                }
                tab[na.omit(bonindice),ic]=matrix(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3]))) 
              }
            }
          }
          
        }
        pasexport=(tab[2,1]-tab[1,1])*Past_Ts
        names(pasexport)=NULL
        minute=tab[1,1]/60
        tab[,1]=tab[,1]/minute
        colnames(tab)=MotsCles[,1]
        write.table(tab,
                    file=file.path(dsnlayerC,paste0(substr(nomsortie,1,nchar(nomsortie)-7),"_sortie.txt")),
                    sep=";")
        
        
        if (gregexpr("Evt",nomsortie)[[1]][1]>0){
          
          
          name_split <- strsplit(nomsortie[[1]],"_")[[1]]
          
          
          date_deb <- gsub('[^0-9]','',name_split[4])
          date_deb <- strptime(x = date_deb,format = "%Y%m%d%H%M" )
          
          
          TextAbsc=paste0(FrEng[2,iFRENG],date_deb," TU")
        }else{
          date_deb <- "190001010000"
          date_deb <- strptime(x = date_deb,format = "%Y%m%d%H%M" )
        }
        
        #Calcul pluie brute nette volume brut net
        #Pluie Brute
        XPluieBrut=c(0,tab[,1],(max(tab[,1])+pasexport))+date_deb
        PluieBrut=0*tab[,2] 
        PluieBrut[1]=tab[1,2]
        PluieBrut[-1]=tab[-1,2]-tab[-dim(tab)[1],2]
        PluieBrut=c(0,1000*PluieBrut*3600/(pasexport),0)
        
        # Pluie Nette
        PluieNette=c(0,1000*tab[,4]/Aire*3600/Past_Ts,0)
        
        #Volume nette
        Vnet=1000*sapply(1:dim(tab)[1], function(x) {sum(tab[1:x,4]/Aire*pasexport/Past_Ts)})
        Vnet=c(NA,Vnet,NA)
        Vbrut = c(NA,tab[,2]*1000,NA)
        
        
        maxpn <- round(max(Vnet,na.rm = TRUE))
        maxpb <- round(max(Vbrut,na.rm = TRUE))
        
        
        # browser()
        nomhyeto_cumul=file.path(dsnlayerC,paste0(substr(nomsortie,1,nchar(nomsortie)-34),"hyeto_cumul.csv"))
        format(XPluieBrut,format="%Y/%m/%d %H:%M:%S")
        tabexp_=cbind( format(XPluieBrut,format="%Y/%m/%d %H:%M:%S"),PluieBrut,PluieNette,Vbrut,Vnet)
        colnames=cbind("XPluieBrut","PluieBrut","PluieNette","Vbrut","Vnet")
        write.csv(tabexp_,file=nomhyeto_cumul)
        # if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp")))
        
        
        ############## resultat nabil
        if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
          sc_sf <- st_read(file.path(dsnlayerC,"SectControl_Select.shp"))
          
          SC_list <- list.files(dsnlayerC,pattern = "SC.txt")
          
          cl_names <- c("nom","numsec","Time","Q","Zmoy","Zmax","Zmin","Hmax","Htot")
          
          funx <- function(x){
            as.numeric(x)
          }
        }
        
        i_filesc=ils
        
        
        
        file.path(dsnPluie,contour$NOMPOST)
        
        pattern_pluie <- strsplit(SC_list[i_filesc],split = "SC")[[1]][1]
        # list.files(file.path(dsnPluie,contour$NOMPOST),pattern = "f")
        if(grepl("Evt", SC_list[i_filesc], fixed = TRUE)){
          
          name_case_split <- strsplit(paste0("brute_",SC_list[i_filesc]),"_")[[1]]
          name_case <- do.call(paste, c(as.list(name_case_split[1:6]), sep = "_"))
          chem_pluie <- file.path(dsnPluie,contour$NOMPOST)
          file_brute <- list.files(chem_pluie,pattern = name_case,full.names = T)
          file_brute <- file_brute[1]
          if(!is.na(file_brute)){
            tab_brute <- read.table(file_brute,header = TRUE)
            cumul_brute <- tab_brute/15
            cumul_brute_ <- cumul_brute
            for (ibrut in 2:length(cumul_brute$Time)) {
              if(ibrut==2){
                cumul_brute$Val[ibrut]=cumul_brute_$Val[ibrut]+cumul_brute_$Val[ibrut-1]
              }else{
                cumul_brute$Val[ibrut]=cumul_brute_$Val[ibrut]+sum(cumul_brute_$Val[1:(ibrut-1)])
              }
              
              
              
            }
          }
          
          name_split <- strsplit(SC_list[i_filesc],"_")[[1]]
          name_case_split_ <- strsplit(SC_list[i_filesc],"_")[[1]]
          name_case_ <- do.call(paste, c(as.list(name_case_split_[1:6]), sep = "_"))
          splt_h <- strsplit(name_split[5],"h")[[1]]
          DUREE=as.numeric(splt_h[1])+as.numeric(substr(splt_h[2],1,2))/60
          # DUREE=(as.numeric(substr(name_split[5],1,2))+as.numeric(substr(name_split[5],4,5))/60)*3600
          
          datedebut <- gsub('[^0-9]','',name_split[4])
          datedebut <- strptime(x = datedebut,format = "%Y%m%d%H%M" )
          
          
          tab_sc_path <- file.path(dsnlayerC,SC_list[i_filesc])
          tab_sc <- read.csv(file = tab_sc_path,
                             header =FALSE,sep = "",skip = 2,col.names = cl_names 
          )
          # GRAVE NABIL
          # debut_time <- which(tab_sc$Time=="60.00" | tab_sc$Time==60)
          # chaine_formatee <- sprintf("%.2f", pasexport)
          pasexportstr=sprintf("%.2f", pasexport)
          debut_time <- which(tab_sc$Time==pasexportstr | tab_sc$Time==pasexport)
          if(min(debut_time)>1){
            tab_sc <- tab_sc[-(1:(debut_time-1)),]
          }
          
          ######################################  
          ###############################################################################
          #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
          tabexp_=cbind( format(XPluieBrut,format="%Y/%m/%d %H:%M:%S"),PluieBrut,PluieNette,Vbrut,Vnet)
          tab_sc[,2:length(colnames(tab_sc))] <- data.frame(lapply(tab_sc,funx))[,2:length(colnames(tab_sc))]
          tab_sc$DATE <- datedebut+as.numeric(tab_sc$Time)
          tab_sc$nom <- as.character(tab_sc$nom)
          sc_sf$ID <- as.character(sc_sf$ID)
          tab_sc <- inner_join(tab_sc,sc_sf,by = c("nom"="ID"))
          sc_names <- unique(tab_sc$NOM)
          sc_list_obs <- list.files(path = dsnscobs,full.names = T)
          sc_list_match <- unlist(lapply(sc_names, function(x) grep(x, sc_list_obs, value = TRUE)))
          if(length(sc_list_match)>0){
            sc_list_match_Q <- unlist(lapply("_Q.csv", function(x) grep(x, sc_list_match, value = TRUE)))
            sc_list_match_Z <- unlist(lapply("_Z.csv", function(x) grep(x, sc_list_match, value = TRUE)))
            sc_list_match_H <- unlist(lapply("_H.csv", function(x) grep(x, sc_list_match, value = TRUE)))
            
            myfiles = lapply(sc_list_match_Q, read.csv)
            myfilesZ = lapply(sc_list_match_Z, read.csv)
            myfilesH = lapply(sc_list_match_H, read.csv)
          }
          
          
          tabexp_df <- as.data.frame(tabexp_)
          colnames(tabexp_df) <- colnames
          
          tab_pb <- data.frame(Time = as.POSIXct(tabexp_df$XPluieBrut),
                               Val = abs(round(as.numeric(tabexp_df$PluieBrut),1)),
                               Nom = "Pluie brute")
          tab_pb <- tab_pb[-c(1,length(tab_pb$Time)),]
          tab_pn <- data.frame(Time = as.POSIXct(tabexp_df$XPluieBrut),
                               Val = abs(round(as.numeric(tabexp_df$PluieNette),1)),
                               Nom = "Pluie nette")
          tab_pn <- tab_pn[-c(1,2),]
          tab_vb <- data.frame(Time=as.POSIXct(tabexp_df$XPluieBrut) ,
                               Val=round(as.numeric(tabexp_df$Vbrut,1)),
                               Nom="Cumul brut")
          
          if(!is.na(file_brute)){
            tab_vb <- na.omit(tab_vb)
            tab_vb$Val <-  cumul_brute$Val[1:length(tab_vb$Val)]
          }
          tab_vn <- data.frame(Time=as.POSIXct(tabexp_df$XPluieBrut),
                               Val=round(as.numeric(tabexp_df$Vnet,1)) ,
                               Nom="Cumul net")
          
          tab_all_cumul <- rbind.data.frame(na.omit(tab_vb),na.omit(tab_vn))
          
          
          
          
          pt_pluie <- as.numeric(gsub("\\D", "", name_split[10]))
          if(!is.na(file_brute)){
            # tab_all_cumul$Val <-  cumul_brute$Val
            tab_pb$Val <- tab_brute$Val[1:length(tab_pb$Val)]*3600/((pasexport)*pt_pluie)
          }
          
          tab_pn$Time <- tab_pb$Time
          tab_pluieall <- rbind.data.frame(tab_pb,tab_pn)
          lim_inf <- max(min(as.POSIXct(tab_sc$DATE),na.rm = T),min(as.POSIXct(tab_pluieall$Time),na.rm = T))
          lim_sup <- min(max(as.POSIXct(tab_sc$DATE),na.rm = T),max(as.POSIXct(tab_pluieall$Time),na.rm = T))
          if(length(sc_list_match)>0){
            mydf <- do.call(rbind.data.frame,myfiles )
            ajouter_heure_si_manquante <- function(date_str) {
              if (grepl(" ", date_str)) { # Vérifie si un espace est présent (indiquant potentiellement une heure)
                return(date_str) # L'heure est probablement déjà là
              } else {
                return(paste0(date_str, " 00:00:00")) # Ajoute l'heure de minuit
              }
            }
            # dates_texte_modifiees <- unname(sapply(mydf$Date, ajouter_heure_si_manquante))
            mydf$Date <- as.POSIXct(unname(sapply(mydf$Date, ajouter_heure_si_manquante)),tz="UTC",format="%Y-%m-%d %H:%M:%S")
            # name_split
            # mydf$Date <- as.POSIXct(mydf$Date,tz="UTC")
            mydf_select <- mydf %>% 
              filter(Date >= as.POSIXct(lim_inf) & Date < as.POSIXct(lim_sup) )
            tab_sc_csel <- data.frame(Date=tab_sc$DATE,
                                      Q=tab_sc$Q,
                                      NOM=tab_sc$NOM)
            if(nrow(mydf_select)!=0){
              names(mydf_select)=c("Date","Q","NOM")
            }
            
            tab_sc_all <- rbind.data.frame(tab_sc_csel,mydf_select)
            tab_sc_all$Date <- as.POSIXct(tab_sc_all$Date)
            tab_sc_all$Q <- abs(tab_sc_all$Q)
            
            
            mydfZ <- do.call(rbind.data.frame,myfilesZ )
            ajouter_heure_si_manquante <- function(date_str) {
              if (grepl(" ", date_str)) { # Vérifie si un espace est présent (indiquant potentiellement une heure)
                return(date_str) # L'heure est probablement déjà là
              } else {
                return(paste0(date_str, " 00:00:00")) # Ajoute l'heure de minuit
              }
            }
            # dates_texte_modifiees <- unname(sapply(mydf$Date, ajouter_heure_si_manquante))
            if(!is_empty(mydfZ)){
              mydfZ$Date <- as.POSIXct(unname(sapply(mydfZ$Date, ajouter_heure_si_manquante)),tz="UTC",format="%Y-%m-%d %H:%M:%S")
              
            }else{
              mydfZ$Date <- as.POSIXct(mydfZ$Date,tz="UTC")
            }
            # name_split
            # mydfZ$Date <- as.POSIXct(mydfZ$Date,tz="UTC")
            mydfZ_select <- mydfZ %>% 
              filter(Date >= as.POSIXct(lim_inf) & Date < as.POSIXct(lim_sup) )
            
            ajouter_heure_si_manquante <- function(date_str) {
              if (grepl(" ", date_str)) { # Vérifie si un espace est présent (indiquant potentiellement une heure)
                return(date_str) # L'heure est probablement déjà là
              } else {
                return(paste0(date_str, " 00:00:00")) # Ajoute l'heure de minuit
              }
            }
            # dates_texte_modifiees <- unname(sapply(mydf$Date, ajouter_heure_si_manquante))
            mydf$Date <- as.POSIXct(unname(sapply(mydf$Date, ajouter_heure_si_manquante)),tz="UTC",format="%Y-%m-%d %H:%M:%S")
            name_split
            
            mydfH <- do.call(rbind.data.frame,myfilesH )
            mydfH$Date <- as.POSIXct(unname(sapply(mydfH$Date, ajouter_heure_si_manquante)),tz="UTC",format="%Y-%m-%d %H:%M:%S")
            
            # mydfH$Date <- as.POSIXct(mydfH$Date,tz="UTC")
            mydfH_select <- mydfH %>% 
              filter(Date >= as.POSIXct(lim_inf) & Date < as.POSIXct(lim_sup) )
            tab_sc_cselZ <- data.frame(Date=tab_sc$DATE,
                                       Z=tab_sc$Zmoy,
                                       NOM=tab_sc$NOM)
            tab_sc_cselZ$Z[which(tab_sc_cselZ$Z==0)] <- NA
            if(nrow(mydfZ_select)!=0){
              names(mydfZ_select)=c("Date","Z","NOM")
            }
            tab_sc_allZ <- rbind.data.frame(tab_sc_cselZ,mydfZ_select)
            tab_sc_allZ$Date <- as.POSIXct(tab_sc_allZ$Date)
            
            
            tab_sc_cselH <- data.frame(Date=tab_sc$DATE,
                                       H=tab_sc$Hmax,
                                       NOM=tab_sc$NOM)
            # tab_sc_cselH$H[which(tab_sc_cselH$H==0)] <- NA
            if(nrow(mydfH_select)!=0){
              names(mydfH_select)=c("Date","H","NOM")
            }
            tab_sc_allH <- rbind.data.frame(tab_sc_cselH,mydfH_select)
            tab_sc_allH$Date <- as.POSIXct(tab_sc_allH$Date)
            
            
          }else{
            tab_sc_all <- tab_sc
            tab_sc_all$Date <- tab_sc_all$DATE
            tab_sc_all$Q <- abs(tab_sc_all$Q)
            tab_sc_allZ <- tab_sc
            tab_sc_allZ$Date <- tab_sc_allZ$DATE
            tab_sc_allZ$Z <- tab_sc$Zmoy
            tab_sc_allZ$Z[which(tab_sc_allZ$Z==0)] <- NA
            tab_sc_allH <- tab_sc
            tab_sc_allH$Date <- tab_sc_allH$DATE
            tab_sc_allH$H <- tab_sc$Hmax
          }
          g1 <- ggplot(tab_pluieall, aes(Time , Val,fill = Nom)) +
            geom_col(position = 'identity',width = 10) +
            scale_fill_manual(values=c( 
              "#33FFFF", 
              "#9933FF")) +
            theme_bw() +
            ylab("Intentisté [mm/h]") +
            scale_x_datetime(#date_breaks = "day",
              limits = c(lim_inf, 
                         lim_sup),
              expand = c(0,0))+
            theme(axis.title.x    = element_blank(),
                  axis.text.x     = element_blank(),
                  axis.ticks.x    = element_blank(),
                  axis.title.y=element_text(vjust = 2),
                  legend.title = element_blank(),
                  legend.key.size = unit(0.4,"cm") ,
                  legend.text = element_text(size = 10),
                  legend.position = "right")
          
          g3 <- ggplot(tab_all_cumul, aes(Time  ,Val)) +
            geom_line(aes(color = Nom),size=1.1) +
            scale_color_manual(values=c( 
              "#33FFFF", 
              "#9933FF")) +
            theme_bw() +
            ylab("Cumul [mm]") +
            scale_x_datetime(#date_breaks = "day",
              limits = c(lim_inf, 
                         lim_sup),
              expand = c(0,0))+
            theme(axis.title.x    = element_blank(),
                  axis.text.x     = element_blank(),
                  axis.ticks.x    = element_blank(),
                  axis.title.y=element_text(vjust = 2),
                  legend.title = element_blank(),
                  legend.key.size = unit(0.4,"cm") ,
                  legend.text = element_text(size = 10),
                  legend.position = "right")
          # tab_sc$Q <- abs(tab_sc$Q)
          tab_sc_all$TYPE <- "solid"
          tab_sc_all$TYPE[grepl("_obs", tab_sc_all$NOM)]="dashed"
          df_sans_na <- tab_sc_all %>%
            dplyr::distinct(NOM,.keep_all = TRUE) 
          
          
          df_sans_na <- df_sans_na[order(df_sans_na$NOM), ]
          g2 <- ggplot(data =tab_sc_all,aes(x = Date, y=Q))+
            geom_line(aes(color = NOM),size=0.7,linetype = tab_sc_all$TYPE) +
            scale_linetype_manual(values=df_sans_na$TYPE,guide = "none")+
            ylab("Débit [m3/s]") +
            xlab("Date (heure météo)")+
            scale_x_datetime(#date_breaks = "day",
              limits = c(lim_inf, 
                         lim_sup),
              expand = c(0,0))+
            theme_bw() +
            theme(                    
              legend.title = element_blank(),
              legend.key.size = unit(0.4,"cm") ,
              legend.text = element_text(size = 10),
              legend.position = "right")
          
          tab_sc_allZ$TYPE <- "solid"
          tab_sc_allZ$TYPE[grepl("_obs", tab_sc_allZ$NOM)]="dashed"
          
          df_sans_naz <- tab_sc_allZ %>%
            dplyr::distinct(NOM,.keep_all = TRUE) 
          
          
          df_sans_naz <- df_sans_naz[order(df_sans_naz$NOM), ]
          
          
          g4 <- ggplot(data =tab_sc_allZ,aes(x = Date, y=Z,linetype = NOM))+
            geom_line(aes(color = NOM),size=0.7) +
            scale_linetype_manual(values=df_sans_naz$TYPE,guide = "none")+
            # scale_color_manual(sample(colors(), 38))+
            ylab("Côte [m]") +
            xlab("Date (heure météo)")+
            scale_x_datetime(#date_breaks = "day",
              limits = c(lim_inf, 
                         lim_sup),
              expand = c(0,0))+
            theme_bw() +
            theme(                    
              legend.title = element_blank(),
              legend.key.size = unit(0.4,"cm") ,
              legend.text = element_text(size = 10),
              legend.position = "right")
          
          tab_sc_allH$TYPE <- "solid"
          tab_sc_allH$TYPE[grepl("_obs", tab_sc_allH$NOM)]="longdash"
          
          df_sans_nah <- tab_sc_all %>%
            dplyr::distinct(NOM,.keep_all = TRUE) 
          
          
          df_sans_nah <- df_sans_nah[order(df_sans_nah$NOM), ]
          g5 <- ggplot(data =tab_sc_allH,aes(x = Date, y=H))+
            geom_line(aes(color = NOM),size=0.7,linetype = tab_sc_allH$TYPE) +
            scale_linetype_manual(values=df_sans_nah$TYPE,guide = "none")+
            ylab("Hauteur [m]") +
            xlab("Date (heure météo)")+
            scale_x_datetime(#date_breaks = "day",
              limits = c(lim_inf, 
                         lim_sup),
              expand = c(0,0))+
            theme_bw() +
            theme(                    
              legend.title = element_blank(),
              legend.key.size = unit(0.4,"cm") ,
              legend.text = element_text(size = 10),
              legend.position = "right")
          
          
          fig1 <- plotly::ggplotly(g1,dynamicTicks = T) 
          fig2 <- plotly::ggplotly(g2,dynamicTicks = T)
          fig3 <- plotly::ggplotly(g3,dynamicTicks = T)
          fig4 <- plotly::ggplotly(g4,dynamicTicks = T)
          fig5 <- plotly::ggplotly(g5,dynamicTicks = T)
          fig <- plotly::subplot(fig3,fig1, fig2, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
            plotly::layout(title = list(text = paste0("Hyéto + Hydrogramme ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                           plot_bgcolor='#e5ecf6',
                           xaxis = list(
                             zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff'       #☻ type = 'date'
                             # tickformat = "%d %B (%a)<br>%Y"
                           ),
                           yaxis = list(
                             zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff'))
          
          figz <- plotly::subplot(fig3,fig1, fig4, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
            plotly::layout(title = list(text = paste0("Hyéto + Côtes ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                           plot_bgcolor='#e5ecf6',
                           xaxis = list(
                             zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff'       #☻ type = 'date'
                             # tickformat = "%d %B (%a)<br>%Y"
                           ),
                           yaxis = list(
                             zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff'))
          
          
          figh <- plotly::subplot(fig3,fig1, fig5, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
            plotly::layout(title = list(text = paste0("Hyéto + Hauteur ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                           plot_bgcolor='#e5ecf6',
                           xaxis = list(
                             zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff'       #☻ type = 'date'
                             # tickformat = "%d %B (%a)<br>%Y"
                           ),
                           yaxis = list(
                             zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff'))
          
          
          # 
          # nomsortie=Listsortie[ils]
          # print(nomsortie)
          # 
          # substr(nomsortie,1,nchar(nomsortie)-30)
          
          Lignes=readLines(con=file.path(dsnlayerC,substr(nomsortie,1,nchar(nomsortie)-30)))
          
          RecupTps=rbind("TIME STEP ")
          indic=regexpr(RecupTps[1,1],as.character(Lignes))
          Past_Ts=as.numeric(substr(Lignes[which(indic>-1)],38,nchar(Lignes[which(indic>-1)])))
          # browser()
          Lignes=readLines(con=file.path(dsnlayerC,nomsortie))
          # Mots-clés généraux
          MotsCles <- rbind(
            cbind("RELATIVE ERROR IN VOLUME AT T", 37, 54),
            cbind("RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61),
            cbind("VOLUME IN THE DOMAIN", 28, 44),
            cbind("ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61),
            cbind("RELATIVE ERROR IN VOLUME AT T", 58, 75),
            cbind(paste0("FLUX BOUNDARY    ", 1:9), 25, 42),
            cbind(paste0("FLUX BOUNDARY    ", 10:30), 25, 42),
            cbind(paste0("CULVERT            ", 1:9, "  DIS"),37,62),
            cbind(paste0("CULVERT           ", 10:99, "  DIS"),37,62)
          )
          
          
          
          PosFB=grep(MotsCles,pattern="FLUX BOUNDARY")
          PosCu=grep(MotsCles,pattern="CULVERT")
          PosCuL=PosCu[length(PosCu)/2+(1:(length(PosCu)/2))]
          PosCu=PosCu[1:(length(PosCu)/2)]
          
          # MotsCles=rbind(cbind("RELATIVE ERROR IN VOLUME AT T",37,54),
          #                cbind("RUNOFF_SCS_CN : ACCUMULATED RAINFALL",44,61),
          #                cbind("VOLUME IN THE DOMAIN",28,44),
          #                cbind("FLUX BOUNDARY    1",25,42),
          #                cbind("RELATIVE ERROR IN VOLUME AT T",58,75))
          #
          # FLUX BOUNDARY    1:    -0.000000     M3/S  ( >0 : ENTERING  <0 : EXITING )
          # FLUX BOUNDARY    2:
          
          for (ic in 1:dim(MotsCles)[1]){
            # print(MotsCles[ic,1])
            indic=regexpr(MotsCles[ic,1],as.character(Lignes))
            correspond=which(indic>-1)
            # Ajout car j ai ecrit dans buse.f plusieurs fois culvert...
            correspond=correspond[which(is.na(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3])))==F)]
            
            if (ic==1)
            {
              niter=length(correspond)
              tab=matrix(0,niter,dim(MotsCles))
              referenceT=correspond
              tab[,1]=matrix(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3])))
            }else{
              if (length(correspond)>0)
              {
                
                # IL Y A UN BUG ((niter-length(correspond)+1):niter)
                # (niter-length(correspond)+1)
                # bonindice=((niter-length(correspond)+1):niter)
                
                bonindice=sapply(1:length(correspond), function(x) {which(referenceT>=correspond[x])[1]})
                # tab[do.call(rbind,bonindice),ic]=matrix(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3])))
                tab[bonindice,ic]=matrix(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3])))   
              }
            }
            
          }
          
          colnames(tab)=MotsCles[,1]
          # ... (previous code remains the same until the plotting section) ...
          flux_cols_to_keep <- sapply(grep("FLUX BOUNDARY", colnames(tab), value = TRUE), function(col) {
            any(tab[, col] != 0, na.rm = TRUE)
          })
          
          # Identify Culvert columns (DIS) with at least one value > 0
          culvert_cols_to_keep <- sapply(grep("CULVERT", colnames(tab), value = TRUE), function(col) {
            any(tab[, col] != 0, na.rm = TRUE)
          })
          
          tab <- tab[, c(TRUE, rep(TRUE, 4), flux_cols_to_keep, culvert_cols_to_keep)]  # Keep first 5 cols + selected
          
          write.table(tab,
                      file=file.path(dsnlayerC,paste0(substr(nomsortie,1,nchar(nomsortie)-7),"_sortie.txt")),
                      sep=";")
          
          
          
          # Suppression du volume initial (confition limite)
          if (length(tab)>2){ # Corrected condition: Check for more than one row
            tab[, 3] = tab[, 3] - tab[1, 3]  # Subtract initial volume
            TimeStep = tab[1, 1]  # Get the time step
            CumulDebit = matrix(0, nrow(tab), 10) #nrow instead of dim
            # ...
            tab[, 4] = tab[, 4] / Past_Ts  # Calculate volume per time step
            CumulAddVol = sapply(1:nrow(tab), function(x) { TimeStep * sum(tab[1:x, 4]) }) #nrow
            
            # --- Data Preparation for ggplot2 ---
            
            # Convert 'tab' to a data frame (if it isn't already)
            tab_df <- as.data.frame(tab)
            tab_df$Time <- tab_df[, 1]  # Add a 'Time' column for x-axis
            nsortie=length(Listsortie)
            # 1. Volume Plot Data
            volume_data <- data.frame(
              Time = tab_df$Time,
              Injecte = CumulAddVol,
              Domaine = tab_df[, 3],
              `Domaine + sorties` = tab_df[, 3] + rowSums(CumulDebit[, 1:nsortie, drop = FALSE]),
              check.names = FALSE  # Prevent column name modification
            )
            
            # Add cumulative discharge columns, handling potential NAs, and filtering!
            for (isor in 1:nsortie) {
              col_name <- paste0("Sortie ", isor)
              volume_data[[col_name]] <- CumulDebit[, isor]
            }
            
            volume_long <- pivot_longer(volume_data,
                                        cols = -Time,
                                        names_to = "Variable",
                                        values_to = "Volume")
            
            # 2. Rainfall Plot Data (Already in a good format)
            rainfall_data <- data.frame(
              Time = tab_df$Time,
              Rainfall = tab_df[, 2]
            )
            
            # 3. Net Volume Plot Data (Already in a good format)
            netvolume_data <- data.frame(
              Time = tab_df$Time,
              NetVolume = tab_df[, 4]
            )
            MotsCles2=colnames(tab_df)
            PosFB2=grep(MotsCles2,pattern="FLUX BOUNDARY")
            PosCu2=grep(MotsCles2,pattern="CULVERT")
            # 4. Discharge Plot Data
            discharge_data <- tab_df[, c("Time", MotsCles2[PosFB2])]
            discharge_data <- discharge_data[, colSums(is.na(discharge_data)) < nrow(discharge_data)] # Remove all-NA columns
            # Filter discharge data to keep only columns with at least one non-zero, non-NA value
            # discharge_data <- discharge_data[, c(TRUE, sapply(discharge_data[, -1, drop = FALSE], function(x) any(!is.na(x) & x != 0)))]
            
            discharge_long <- pivot_longer(discharge_data,
                                           cols = -Time,
                                           names_to = "Boundary",
                                           values_to = "Discharge")
            
            
            # 5. Culvert Discharge Plot Data
            if(!is_empty(PosCu2)){
              culvert_data <- tab_df[, c("Time", MotsCles2[PosCu2])]
              culvert_data <- culvert_data[, colSums(is.na(culvert_data)) < nrow(culvert_data)]  # Remove columns that are all NA
              # Filter culvert data to keep only columns with at least one non-zero value
              culvert_data <- culvert_data[, c(TRUE, sapply(culvert_data[, -1, drop = FALSE], function(x) any(!is.na(x) & x != 0)))]
              
              culvert_long <- pivot_longer(culvert_data,
                                           cols = -Time,
                                           names_to = "Culvert",
                                           values_to = "Discharge")
            }
            
            
            
            
            # --- ggplot2 Plotting ---
            
            # Create the plots (rest of plotting code remains the same)
            p_volume <- ggplot(volume_long, aes(x = Time, y = Volume, color = Variable)) +
              geom_line() +
              labs(title = nomsortie,
                   x = "Temps Telemac (s)",
                   y = "Volume (m3)",
                   color = "Variable") +
              theme_bw()
            
            p_rainfall <- ggplot(rainfall_data, aes(x = Time, y = Rainfall)) +
              geom_line() +
              labs(title = "Pluie Injectee",
                   x = "Temps Telemac (s)",
                   y = "Cumul (m)") +
              theme_bw()
            
            p_netvolume <- ggplot(netvolume_data, aes(x = Time, y = NetVolume)) +
              geom_line() +
              labs(title = "Volume net",
                   x = "Temps Telemac (s)",
                   y = "Apport a chaque pas de calcul (m3)") +
              theme_bw()
            
            
            debit_sortie <- data.frame(Time=discharge_long$Time+datedebut,Q=discharge_long$Discharge,Type=discharge_long$Boundary)
            
            p_discharge <- ggplot(debit_sortie, aes(x = Time, y = -Q, color = Type)) +
              geom_line() +
              labs(title = "Debit Sorties",
                   x = "Date ",
                   y = "Debit (m3/s)",
                   color = "Boundary") +
              theme_bw() +
              ylim(0, NA)  # Important: Set y-axis to start at 0
            
            p_discharge <- ggplot(data =debit_sortie,aes(x = Time, y=abs(Q)))+
              geom_line(aes(color = Type),size=0.7) +
              # scale_linetype_manual(values=df_sans_na$TYPE,guide = "none")+
              ylab("Débit [m3/s]") +
              xlab("Date (heure météo)")+
              scale_x_datetime(#date_breaks = "day",
                limits = c(lim_inf, 
                           lim_sup),
                expand = c(0,0))+
              theme_bw() +
              theme(                    
                legend.title = element_blank(),
                legend.key.size = unit(0.4,"cm") ,
                legend.text = element_text(size = 10),
                legend.position = "right")
            
            if(!is_empty(PosCu2) ){
              debit_buses <- data.frame(Time=culvert_long$Time+datedebut,Q=culvert_long$Discharge,Type=culvert_long$Culvert)
              p_culvert <- ggplot(debit_buses, aes(x = Time, y = Q, color = Type)) +
                geom_line() +
                labs(title = "Debit Ouvrages",
                     x = "Date",
                     y = "Debit (m3/s)",
                     color = "Culvert") +
                theme_bw() +
                ylim(0, max(culvert_long$Discharge, na.rm = TRUE)) # Set y limits based on the data
              
            }
            # Combine and save
            # Convert ggplot2 plots to plotly interactive plots
            gp_volume <- ggplotly(p_volume, tooltip = c("x", "y", "color"),dynamicTicks = TRUE)
            gp_rainfall <- ggplotly(p_rainfall, tooltip = c("x", "y"),dynamicTicks = TRUE)
            gp_netvolume <- ggplotly(p_netvolume, tooltip = c("x", "y"),dynamicTicks = TRUE)
            gp_discharge <- ggplotly(p_discharge, tooltip = c("x", "y", "color"),dynamicTicks = TRUE)
            if(!is_empty(PosCu2) ){
              gp_culvert <- ggplotly(p_culvert, tooltip = c("x", "y", "color"),dynamicTicks = TRUE)
            }
            
            # Combine plots using subplot (from plotly)
            # combined_plotly <- subplot(gp_volume, gp_rainfall, gp_netvolume, gp_discharge, gp_culvert,
            #                            nrows = 5, # Arrange plots vertically
            #                            titleX = TRUE, titleY = TRUE, # Show axis titles
            #                            margin = 0.05, # Adjust margins as needed
            #                            shareX = TRUE)  # Share the x-axis (very important for time series)
            # 
            # 
            # # Save as a single HTML file
            # htmlwidgets::saveWidget(combined_plotly,
            #                         file = file.path(dsnlayerC, paste0(substr(nomsortie, 1, nchar(nomsortie) - 7), ".html")),
            #                         selfcontained = TRUE) # Embed all dependencies
            # 
            
            figsortie <- plotly::subplot(fig3,fig1, gp_discharge, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
              plotly::layout(title = list(text = paste0("Hyéto + Q sortie ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                             plot_bgcolor='#e5ecf6',
                             xaxis = list(
                               zerolinecolor = '#ffff',
                               zerolinewidth = 2,
                               gridcolor = 'ffff'       #☻ type = 'date'
                               # tickformat = "%d %B (%a)<br>%Y"
                             ),
                             yaxis = list(
                               zerolinecolor = '#ffff',
                               zerolinewidth = 2,
                               gridcolor = 'ffff'))
            figsortieV <- plotly::subplot(fig1,gp_volume, gp_discharge, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
              plotly::layout(title = list(text = paste0("Hyéto + Vol et Q sortie  ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                             plot_bgcolor='#e5ecf6',
                             xaxis = list(
                               zerolinecolor = '#ffff',
                               zerolinewidth = 2,
                               gridcolor = 'ffff'       #☻ type = 'date'
                               # tickformat = "%d %B (%a)<br>%Y"
                             ),
                             yaxis = list(
                               zerolinecolor = '#ffff',
                               zerolinewidth = 2,
                               gridcolor = 'ffff'))
            
            if(!is_empty(PosCu2) ){
              figculvert <- plotly::subplot(fig3,fig1, gp_culvert, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
                plotly::layout(title = list(text = paste0("Hyéto + Q OH ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                               plot_bgcolor='#e5ecf6',
                               xaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'       #☻ type = 'date'
                                 # tickformat = "%d %B (%a)<br>%Y"
                               ),
                               yaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'))
            }
          }
          if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
            sc_sf$ID <- as.character(sc_sf$ID)
            tab_sc <- inner_join(tab_sc,sc_sf,by = c("nom"="ID"))
            filepath_plotly_all <- file.path(dsnlayerC,paste0(gsub("\\..*", "", SC_list[i_filesc]),"_PN_PB_Q_Obs.html"))
            if(!file.exists(filepath_plotly_all)){
              htmlwidgets::saveWidget(plotly::partial_bundle(fig),
                                      file = filepath_plotly_all,
                                      libdir = "lib")
              
              filepath_plotly_allz <- file.path(dsnlayerC,paste0(gsub("\\..*", "", SC_list[i_filesc]),"_PN_PB_Z_Obs.html"))
              
              
              htmlwidgets::saveWidget(plotly::partial_bundle(figz),
                                      file = filepath_plotly_allz,
                                      libdir = "lib")
              
              filepath_plotly_allH <- file.path(dsnlayerC,paste0(gsub("\\..*", "", SC_list[i_filesc]),"_PN_PB_H_Obs.html"))
              
              htmlwidgets::saveWidget(plotly::partial_bundle(figh),
                                      file = filepath_plotly_allH,
                                      libdir = "lib")
            }
          }
          split_listsortie <- strsplit(Listsortie[[i_filesc]],"_")[[1]]
          namesortie <- paste(split_listsortie[1:7],collapse = "_")
          filepath_plotly_allqs <- file.path(dsnlayerC,paste0(gsub("\\..*", "", namesortie),"_PN_PB_QS_Obs.html"))
          if(!file.exists(filepath_plotly_allqs)){
            htmlwidgets::saveWidget(plotly::partial_bundle(figsortie),
                                    file = filepath_plotly_allqs,
                                    libdir = "lib")
            filepath_plotly_allvqs <- file.path(dsnlayerC,paste0(gsub("\\..*", "", namesortie),"_PN_PB_VQS_Obs.html"))
            # filepath_plotly_allvqs <- file.path(dsnlayerC,paste0(gsub("\\..*", "", Listsortie[[i_filesc]],"_PN_PB_VQS_Obs.html")))
            htmlwidgets::saveWidget(plotly::partial_bundle(figsortieV),
                                    file = filepath_plotly_allvqs,
                                    libdir = "lib")
            
            
            if(!is_empty(PosCu2) ){
              filepath_plotly_allOH <- file.path(dsnlayerC,paste0(gsub("\\..*", "", namesortie),"_PN_PB_QOH_Obs.html"))
              
              htmlwidgets::saveWidget(plotly::partial_bundle(figculvert),
                                      file = filepath_plotly_allOH,
                                      libdir = "lib")
            }
          }
          if(dir.exists(file.path(dsnlayerC,"lib"))){
            
            unlink(file.path(dsnlayerC,"lib"),recursive = TRUE)
          }
          if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
            tabmaxq <- tab_sc %>% 
              group_by(nom) %>% 
              filter(abs(Q) == max(abs(min(Q)),abs(max(Q)))) %>% 
              filter(Time == min(Time))
            tabmaxq$nom <- as.character(tabmaxq$nom)
            sc_sf$ID <- as.character(sc_sf$ID)
            if("Q" %in% names(sc_sf)){
              sc_sf <- sc_sf[,-which(names(sc_sf)=="Q")]
            }
            new_sc_sf <- left_join(x =sc_sf,y = tabmaxq,by=c("ID"="nom") )
            new_sc_sf$Q <- abs(new_sc_sf$Q )
            if(grepl("Evt", SC_list[i_filesc], fixed = TRUE)){
              new_sc_sf$DATE <- as.character(new_sc_sf$DATE)
            }
          }
          
          
          
        }else{
          
          name_case_split <- strsplit(paste0("brute_",SC_list[i_filesc]),"_")[[1]]
          name_case <- do.call(paste, c(as.list(name_case_split[1:6]), sep = "_"))
          chem_pluie <- file.path(dsnPluie,contour$NOMPOST)
          file_brute <- list.files(chem_pluie,pattern = name_case,full.names = T)
          file_brute <- file_brute[1]
          
          # if(is_empty(file_brute)){
          #   file_brute <- list.files(chem_pluie,pattern = name_case_split[5],full.names = T)
          # }
          if(!is.na(file_brute)){
            tab_brute <- read.table(file_brute,header = TRUE)
            cumul_brute <- tab_brute/5
            cumul_brute_ <- cumul_brute
            for (ibrut in 2:length(cumul_brute$Time)) {
              if(ibrut==2){
                cumul_brute$Val[ibrut]=cumul_brute_$Val[ibrut]+cumul_brute_$Val[ibrut-1]
              }else{
                cumul_brute$Val[ibrut]=cumul_brute_$Val[ibrut]+sum(cumul_brute_$Val[1:(ibrut-1)])
              }
              
              
              
            }
          }
          name_split3 <- strsplit(Listsortie[[i_filesc]],"_")
          name_split <- strsplit(SC_list[i_filesc],"_")[[1]]
          name_case_split_ <- strsplit(SC_list[i_filesc],"_")[[1]]
          name_case_ <- do.call(paste, c(as.list(name_case_split_[1:6]), sep = "_"))
          
          DUREE= (as.numeric(gsub("[^0-9.]", "", name_split3[[1]][7])) + as.numeric(gsub("[^0-9.]", "", name_split3[[1]][8]))/60)*3600
          # DUREE=(as.numeric(substr(name_split[5],1,2))+as.numeric(substr(name_split[5],4,5))/60)*3600
          
          datedebut <- "190001010000"
          datedebut <- strptime(x = datedebut,format = "%Y%m%d%H%M" )
          
          if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
            tab_sc_path <- file.path(dsnlayerC,SC_list[i_filesc])
            tab_sc <- read.csv(file = tab_sc_path,
                               header =FALSE,sep = "",skip = 2,col.names = cl_names 
            )
            # GRAVE NABIL
            # debut_time <- which(tab_sc$Time=="60.00" | tab_sc$Time==60)
            pasexportstr=sprintf("%.2f", pasexport)
            debut_time <- which(tab_sc$Time==pasexportstr | tab_sc$Time==pasexport)
            
            if(min(debut_time)>1){
              tab_sc <- tab_sc[-(1:(debut_time-1)),]
            }
          }
          ##################################################################################################  
          ###############################################################################
          #{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
          tabexp_=cbind( format(XPluieBrut,format="%Y/%m/%d %H:%M:%S"),PluieBrut,PluieNette,Vbrut,Vnet)
          
          if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
            tab_sc[,2:length(colnames(tab_sc))] <- data.frame(lapply(tab_sc,funx))[,2:length(colnames(tab_sc))]
            tab_sc$DATE <- datedebut+as.numeric(tab_sc$Time)
            tab_sc$nom <- as.character(tab_sc$nom)
            sc_sf$ID <- as.character(sc_sf$ID)
            tab_sc <- inner_join(tab_sc,sc_sf,by = c("nom"="ID"))
            sc_names <- unique(tab_sc$NOM)
            sc_list_obs <- list.files(path = dsnscobs,full.names = T)
            sc_list_match <- unlist(lapply(sc_names, function(x) grep(x, sc_list_obs, value = TRUE)))
            if(length(sc_list_match)>0){
              sc_list_match_Q <- unlist(lapply("_Q.csv", function(x) grep(x, sc_list_match, value = TRUE)))
              sc_list_match_Z <- unlist(lapply("_Z.csv", function(x) grep(x, sc_list_match, value = TRUE)))
              sc_list_match_H <- unlist(lapply("_H.csv", function(x) grep(x, sc_list_match, value = TRUE)))
              
              myfiles = lapply(sc_list_match_Q, read.csv)
              myfilesZ = lapply(sc_list_match_Z, read.csv)
              myfilesH = lapply(sc_list_match_H, read.csv)
            }
          }
          
          tabexp_df <- as.data.frame(tabexp_)
          colnames(tabexp_df) <- colnames
          
          tab_pb <- data.frame(Time = as.POSIXct(tabexp_df$XPluieBrut),
                               Val = abs(round(as.numeric(tabexp_df$PluieBrut),1)),
                               Nom = "Pluie brute")
          tab_pb <- tab_pb[-c(1,length(tab_pb$Time)),]
          tab_pn <- data.frame(Time = as.POSIXct(tabexp_df$XPluieBrut),
                               Val = abs(round(as.numeric(tabexp_df$PluieNette),1)),
                               Nom = "Pluie nette")
          tab_pn <- tab_pn[-c(1,2),]
          tab_vb <- data.frame(Time=as.POSIXct(tabexp_df$XPluieBrut) ,
                               Val=round(as.numeric(tabexp_df$Vbrut,1)),
                               Nom="Cumul brut")
          if(!is.na(file_brute)){
            tab_vb <- na.omit(tab_vb)
            tab_vb$Val <-  cumul_brute$Val
          }
          tab_vn <- data.frame(Time=as.POSIXct(tabexp_df$XPluieBrut),
                               Val=round(as.numeric(tabexp_df$Vnet,1)) ,
                               Nom="Cumul net")
          
          tab_all_cumul <- rbind.data.frame(na.omit(tab_vb),na.omit(tab_vn))
          
          
          
          
          pt_pluie <- as.numeric(gsub("\\D", "", name_split[10]))
          if(!is.na(file_brute)){
            # tab_all_cumul$Val <-  cumul_brute$Val
            tab_pb$Val <- tab_brute$Val*3600/((pasexport)*pt_pluie)
          }
          
          tab_pn$Time <- tab_pb$Time
          tab_pluieall <- rbind.data.frame(tab_pb,tab_pn)
          if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
            lim_inf <- max(min(as.POSIXct(tab_sc$DATE),na.rm = T),min(as.POSIXct(tab_pluieall$Time),na.rm = T))
            lim_sup <- max(max(as.POSIXct(tab_sc$DATE),na.rm = T),max(as.POSIXct(tab_pluieall$Time),na.rm = T))
          }else{
            lim_inf <- min(as.POSIXct(tab_pluieall$Time),na.rm = T)
            lim_sup <- max(as.POSIXct(tab_pluieall$Time),na.rm = T)
          }
          
          if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
            if(length(sc_list_match)>0){
              mydf <- do.call(rbind.data.frame,myfiles )
              ajouter_heure_si_manquante <- function(date_str) {
                if (grepl(" ", date_str)) { # Vérifie si un espace est présent (indiquant potentiellement une heure)
                  return(date_str) # L'heure est probablement déjà là
                } else {
                  return(paste0(date_str, " 00:00:00")) # Ajoute l'heure de minuit
                }
              }
              # dates_texte_modifiees <- unname(sapply(mydf$Date, ajouter_heure_si_manquante))
              mydf$Date <- as.POSIXct(unname(sapply(mydf$Date, ajouter_heure_si_manquante)),tz="UTC",format="%Y-%m-%d %H:%M:%S")
              name_split
              # mydf_select <- mydf[grepl(paste0("_",name_split[4],"_"),mydf$nom) ,]
              mydf_select <- mydf %>%
                filter(Date >= as.POSIXct(lim_inf) & Date < as.POSIXct(lim_sup) )
              
              # if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
              tab_sc_csel <- data.frame(Date=tab_sc$DATE,
                                        Q=tab_sc$Q,
                                        NOM=tab_sc$NOM)
              tab_sc_all <- rbind.data.frame(tab_sc_csel,mydf_select)
              tab_sc_all$Date <- as.POSIXct(tab_sc_all$Date)
              tab_sc_all$Q <- abs(tab_sc_all$Q)
              
              
              mydfZ <- do.call(rbind.data.frame,myfilesZ )
              mydfZ$Date <- as.POSIXct(mydfZ$Date,tz="UTC")
              mydfZ_select <- mydfZ %>% 
                filter(Date >= as.POSIXct(lim_inf) & Date < as.POSIXct(lim_sup) )
              
              mydfH <- do.call(rbind.data.frame,myfilesH )
              mydfH$Date <- as.POSIXct(unname(sapply(mydfH$Date, ajouter_heure_si_manquante)),tz="UTC",format="%Y-%m-%d %H:%M:%S")
              mydfH_select <- mydfH %>% 
                filter(Date >= as.POSIXct(lim_inf) & Date < as.POSIXct(lim_sup) )
              tab_sc_cselZ <- data.frame(Date=tab_sc$DATE,
                                         Z=tab_sc$Zmoy,
                                         NOM=tab_sc$NOM)
              tab_sc_cselZ$Z[which(tab_sc_cselZ$Z==0)] <- NA
              tab_sc_allZ <- rbind.data.frame(tab_sc_cselZ,mydfZ_select)
              tab_sc_allZ$Date <- as.POSIXct(tab_sc_allZ$Date)
              
              
              tab_sc_cselH <- data.frame(Date=tab_sc$DATE,
                                         H=tab_sc$Hmax,
                                         NOM=tab_sc$NOM)
              # tab_sc_cselH$H[which(tab_sc_cselH$H==0)] <- NA
              tab_sc_allH <- rbind.data.frame(tab_sc_cselH,mydfH_select)
              tab_sc_allH$Date <- as.POSIXct(tab_sc_allH$Date)
              
              
            }else{
              if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
                tab_sc_all <- tab_sc
                tab_sc_all$Date <- tab_sc_all$DATE
                tab_sc_all$Q <- abs(tab_sc_all$Q)
                tab_sc_allZ <- tab_sc
                tab_sc_allZ$Date <- tab_sc_allZ$DATE
                tab_sc_allZ$Z <- tab_sc$Zmoy
                tab_sc_allZ$Z[which(tab_sc_allZ$Z==0)] <- NA
                tab_sc_allH <- tab_sc
                tab_sc_allH$Date <- tab_sc_allH$DATE
                tab_sc_allH$H <- tab_sc$Hmax
              }
            }
          }
          g1 <- ggplot(tab_pluieall, aes(Time , Val,fill = Nom)) +
            geom_col(position = 'identity',width = 10) +
            scale_fill_manual(values=c( 
              "#33FFFF", 
              "#9933FF")) +
            theme_bw() +
            ylab("Intentisté [mm/h]") +
            scale_x_datetime(#date_breaks = "day",
              limits = c(lim_inf, 
                         lim_sup),
              expand = c(0,0))+
            theme(axis.title.x    = element_blank(),
                  axis.text.x     = element_blank(),
                  axis.ticks.x    = element_blank(),
                  axis.title.y=element_text(vjust = 2),
                  legend.title = element_blank(),
                  legend.key.size = unit(0.4,"cm") ,
                  legend.text = element_text(size = 10),
                  legend.position = "right")
          
          g3 <- ggplot(tab_all_cumul, aes(Time  ,Val)) +
            geom_line(aes(color = Nom),size=1.1) +
            scale_color_manual(values=c( 
              "#33FFFF", 
              "#9933FF")) +
            theme_bw() +
            ylab("Cumul [mm]") +
            scale_x_datetime(#date_breaks = "day",
              limits = c(lim_inf, 
                         lim_sup),
              expand = c(0,0))+
            theme(axis.title.x    = element_blank(),
                  axis.text.x     = element_blank(),
                  axis.ticks.x    = element_blank(),
                  axis.title.y=element_text(vjust = 2),
                  legend.title = element_blank(),
                  legend.key.size = unit(0.4,"cm") ,
                  legend.text = element_text(size = 10),
                  legend.position = "right")
          # tab_sc$Q <- abs(tab_sc$Q)
          if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
            tab_sc_all$TYPE <- "solid"
            tab_sc_all$TYPE[grepl("_obs", tab_sc_all$NOM)]="dashed"
            df_sans_na <- tab_sc_all %>%
              dplyr::distinct(NOM,.keep_all = TRUE) 
            
            
            df_sans_na <- df_sans_na[order(df_sans_na$NOM), ]
          }
          
          fig1 <- plotly::ggplotly(g1,dynamicTicks = T) 
          
          fig3 <- plotly::ggplotly(g3,dynamicTicks = T)
          if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
            filepath_plotly_all <- file.path(dsnlayerC,paste0(gsub("\\..*", "", SC_list[i_filesc]),"_PN_PB_Q_Obs.html"))
            if(!file.exists(filepath_plotly_all)){
              g2 <- ggplot(data =tab_sc_all,aes(x = Date, y=round(Q,digits=1)))+
                geom_line(aes(color = NOM),size=0.7,linetype = tab_sc_all$TYPE) +
                scale_linetype_manual(values=df_sans_na$TYPE,guide = "none")+
                ylab("Débit [m3/s]") +
                xlab("Date (heure météo)")+
                scale_x_datetime(#date_breaks = "day",
                  limits = c(lim_inf, 
                             lim_sup),
                  expand = c(0,0))+
                theme_bw() +
                theme(                    
                  legend.title = element_blank(),
                  legend.key.size = unit(0.4,"cm") ,
                  legend.text = element_text(size = 10),
                  legend.position = "right")
              
              tab_sc_allZ$TYPE <- "solid"
              tab_sc_allZ$TYPE[grepl("_obs", tab_sc_allZ$NOM)]="dashed"
              
              df_sans_naz <- tab_sc_allZ %>%
                dplyr::distinct(NOM,.keep_all = TRUE) 
              
              
              df_sans_naz <- df_sans_naz[order(df_sans_naz$NOM), ]
              
              
              g4 <- ggplot(data =tab_sc_allZ,aes(x = Date, y=Z,linetype = NOM))+
                geom_line(aes(color = NOM),size=0.7) +
                scale_linetype_manual(values=df_sans_naz$TYPE,guide = "none")+
                # scale_color_manual(sample(colors(), 38))+
                ylab("Côte [m]") +
                xlab("Date (heure météo)")+
                scale_x_datetime(#date_breaks = "day",
                  limits = c(lim_inf, 
                             lim_sup),
                  expand = c(0,0))+
                theme_bw() +
                theme(                    
                  legend.title = element_blank(),
                  legend.key.size = unit(0.4,"cm") ,
                  legend.text = element_text(size = 10),
                  legend.position = "right")
              
              tab_sc_allH$TYPE <- "solid"
              tab_sc_allH$TYPE[grepl("_obs", tab_sc_allH$NOM)]="longdash"
              
              df_sans_nah <- tab_sc_all %>%
                dplyr::distinct(NOM,.keep_all = TRUE) 
              
              
              df_sans_nah <- df_sans_nah[order(df_sans_nah$NOM), ]
              g5 <- ggplot(data =tab_sc_allH,aes(x = Date, y=H))+
                geom_line(aes(color = NOM),size=0.7,linetype = tab_sc_allH$TYPE) +
                scale_linetype_manual(values=df_sans_nah$TYPE,guide = "none")+
                ylab("Hauteur [m]") +
                xlab("Date (heure météo)")+
                scale_x_datetime(#date_breaks = "day",
                  limits = c(lim_inf, 
                             lim_sup),
                  expand = c(0,0))+
                theme_bw() +
                theme(                    
                  legend.title = element_blank(),
                  legend.key.size = unit(0.4,"cm") ,
                  legend.text = element_text(size = 10),
                  legend.position = "right")  
              
              fig2 <- plotly::ggplotly(g2,dynamicTicks = T)
              fig4 <- plotly::ggplotly(g4,dynamicTicks = T)
              fig5 <- plotly::ggplotly(g5,dynamicTicks = T)
              fig <- plotly::subplot(fig3,fig1, fig2, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
                plotly::layout(title = list(text = paste0("Hyéto + Hydrogramme ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                               plot_bgcolor='#e5ecf6',
                               xaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'       #☻ type = 'date'
                                 # tickformat = "%d %B (%a)<br>%Y"
                               ),
                               yaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'))
              
              figz <- plotly::subplot(fig3,fig1, fig4, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
                plotly::layout(title = list(text = paste0("Hyéto + Côtes ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                               plot_bgcolor='#e5ecf6',
                               xaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'       #☻ type = 'date'
                                 # tickformat = "%d %B (%a)<br>%Y"
                               ),
                               yaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'))
              
              
              figh <- plotly::subplot(fig3,fig1, fig5, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
                plotly::layout(title = list(text = paste0("Hyéto + Hauteur ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                               plot_bgcolor='#e5ecf6',
                               xaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'       #☻ type = 'date'
                                 # tickformat = "%d %B (%a)<br>%Y"
                               ),
                               yaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'))
            }
            
          }
          
          
          split_listsortie <- strsplit(Listsortie[[i_filesc]],"_")[[1]]
          namesortie <- paste(split_listsortie[1:7],collapse = "_")
          filepath_plotly_allvqs <- file.path(dsnlayerC,paste0(gsub("\\..*", "", namesortie),"_PN_PB_VQS_Obs.html"))
          if(!file.exists(filepath_plotly_allvqs) ){
            Lignes=readLines(con=file.path(dsnlayerC,substr(nomsortie,1,nchar(nomsortie)-30)))
            
            RecupTps=rbind("TIME STEP ")
            indic=regexpr(RecupTps[1,1],as.character(Lignes))
            Past_Ts=as.numeric(substr(Lignes[which(indic>-1)],38,nchar(Lignes[which(indic>-1)])))
            # browser()
            Lignes=readLines(con=file.path(dsnlayerC,nomsortie))
            # Mots-clés généraux
            MotsCles <- rbind(
              cbind("RELATIVE ERROR IN VOLUME AT T", 37, 54),
              cbind("RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61),
              cbind("VOLUME IN THE DOMAIN", 28, 44),
              cbind("ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61),
              cbind("RELATIVE ERROR IN VOLUME AT T", 58, 75),
              cbind(paste0("FLUX BOUNDARY    ", 1:9), 25, 42),
              cbind(paste0("FLUX BOUNDARY   ", 10:80), 25, 42),
              cbind(paste0("CULVERT            ", 1:9, "  DIS"),37,62),
              cbind(paste0("CULVERT           ", 10:99, "  DIS"),37,62)
            )
            
            
            
            PosFB=grep(MotsCles,pattern="FLUX BOUNDARY")
            PosCu=grep(MotsCles,pattern="CULVERT")
            PosCuL=PosCu[length(PosCu)/2+(1:(length(PosCu)/2))]
            PosCu=PosCu[1:(length(PosCu)/2)]
            
            # MotsCles=rbind(cbind("RELATIVE ERROR IN VOLUME AT T",37,54),
            #                cbind("RUNOFF_SCS_CN : ACCUMULATED RAINFALL",44,61),
            #                cbind("VOLUME IN THE DOMAIN",28,44),
            #                cbind("FLUX BOUNDARY    1",25,42),
            #                cbind("RELATIVE ERROR IN VOLUME AT T",58,75))
            #
            # FLUX BOUNDARY    1:    -0.000000     M3/S  ( >0 : ENTERING  <0 : EXITING )
            # FLUX BOUNDARY    2:
            
            for (ic in 1:dim(MotsCles)[1]){
              # print(MotsCles[ic,1])
              indic=regexpr(MotsCles[ic,1],as.character(Lignes))
              correspond=which(indic>-1)
              # Ajout car j ai ecrit dans buse.f plusieurs fois culvert...
              correspond=correspond[which(is.na(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3])))==F)]
              
              if (ic==1)
              {
                niter=length(correspond)
                tab=matrix(0,niter,dim(MotsCles))
                referenceT=correspond
                tab[,1]=matrix(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3])))
              }else{
                if (length(correspond)>0)
                {
                  
                  # IL Y A UN BUG ((niter-length(correspond)+1):niter)
                  # (niter-length(correspond)+1)
                  # bonindice=((niter-length(correspond)+1):niter)
                  
                  bonindice=sapply(1:length(correspond), function(x) {which(referenceT>=correspond[x])[1]})
                  # tab[do.call(rbind,bonindice),ic]=matrix(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3])))
                  tab[bonindice,ic]=matrix(as.numeric(substr(Lignes[correspond],MotsCles[ic,2],MotsCles[ic,3])))
                }
              }
              
            }
            
            # ... (votre code initial pour Lignes, RecupTps, Past_Ts, MotsCles, PosFB, PosCu etc. reste le même) ...
            
            # Assurez-vous que les colonnes 2 et 3 de MotsCles sont numériques
            # Ceci est important car cbind peut les avoir converties en chaînes.
            # MotsClesNum <- MotsCles
            # MotsClesNum[,2] <- as.integer(as.numeric(MotsClesNum[,2]))
            # MotsClesNum[,3] <- as.integer(as.numeric(MotsClesNum[,3]))
            # 
            # # Traitement du premier mot-clé (initialisation)
            # ic <- 1
            # keyword_info <- MotsClesNum[ic, ]
            # indic <- regexpr(keyword_info[1], Lignes, fixed = TRUE) # fixed=TRUE si pas de regex
            # correspond <- which(indic > -1)
            # 
            # # Filtrage pour les valeurs numériques valides
            # valeurs_potentielles_str <- substr(Lignes[correspond], keyword_info[2], keyword_info[3])
            # valides_num <- !is.na(suppressWarnings(as.numeric(valeurs_potentielles_str))) # suppressWarnings pour éviter les messages de coercion
            # correspond <- correspond[valides_num]
            # valeurs_numeriques <- as.numeric(valeurs_potentielles_str[valides_num])
            # 
            # if (length(correspond) == 0) {
            #   stop(paste("Le premier mot-clé ('", keyword_info[1], "') n'a pas été trouvé ou n'a pas de valeurs numériques associées. Impossible d'initialiser la table.", sep=""))
            # }
            # 
            # niter <- length(correspond)
            # # Initialiser avec NA pour mieux voir les manquants
            # tab <- matrix(NA_real_, nrow = niter, ncol = dim(MotsClesNum)[1])
            # referenceT <- correspond # Ce sont les numéros de ligne du "TIME STEP" ou équivalent
            # tab[, ic] <- valeurs_numeriques
            # 
            # # Boucle pour les mots-clés suivants
            # if (dim(MotsClesNum)[1] > 1) {
            #   for (ic in 2:dim(MotsClesNum)[1]) {
            #     keyword_info <- MotsClesNum[ic, ]
            #     # print(keyword_info[1]) # Décommenter pour le débogage
            # 
            #     indic <- regexpr(keyword_info[1], Lignes, fixed = TRUE) # fixed=TRUE si pas de regex
            #     correspond_ic <- which(indic > -1)
            # 
            #     if (length(correspond_ic) > 0) {
            #       # Filtrage pour les valeurs numériques valides
            #       valeurs_potentielles_str_ic <- substr(Lignes[correspond_ic], keyword_info[2], keyword_info[3])
            #       valides_num_ic <- !is.na(suppressWarnings(as.numeric(valeurs_potentielles_str_ic)))
            # 
            #       correspond_ic_filtre <- correspond_ic[valides_num_ic]
            #       valeurs_numeriques_ic <- as.numeric(valeurs_potentielles_str_ic[valides_num_ic])
            # 
            #       if (length(correspond_ic_filtre) > 0) {
            #         # OPTIMISATION MAJEURE ICI
            #         # 'findInterval' trouve l'indice dans 'referenceT' pour chaque 'correspond_ic_filtre'.
            #         # Cela signifie à quel "pas de temps" (ligne de 'tab') chaque valeur appartient.
            #         bonindice <- findInterval(correspond_ic_filtre, referenceT)
            # 
            #         # S'assurer que les indices sont valides (findInterval peut retourner 0)
            #         indices_valides_pour_tab <- bonindice > 0 & bonindice <= niter
            # 
            #         if(any(indices_valides_pour_tab)) {
            #           # Assigner les valeurs aux bonnes lignes de la table 'tab'
            #           # Si plusieurs correspond_ic_filtre tombent dans le même intervalle de referenceT,
            #           # findInterval donnera le même indice pour eux. L'assignation vectorielle
            #           # écrasera les valeurs précédentes pour cet indice, ce qui semble être
            #           # le comportement implicite de votre code original avec sapply si plusieurs
            #           # 'correspond' pointaient vers le même 'bonindice'.
            #           tab[bonindice[indices_valides_pour_tab], ic] <- valeurs_numeriques_ic[indices_valides_pour_tab]
            #         }
            #       }
            #     }
            #   }
            # }
            
            # Maintenant 'tab' devrait contenir vos données extraites.
            # Past_Ts peut être ajouté à 'tab' si nécessaire, par exemple comme première colonne.
            # Si Past_Ts a la même longueur que niter:
            # tab_final <- cbind(Past_Ts, tab)
            # (Attention à la correspondance des lignes/pas de temps)
            # --- Étape 1: Définition améliorée de MotsCles ---
            
            
            
            
            
            colnames(tab)=MotsCles[,1]
            # ... (previous code remains the same until the plotting section) ...
            flux_cols_to_keep <- sapply(grep("FLUX BOUNDARY", colnames(tab), value = TRUE), function(col) {
              any(tab[, col] != 0, na.rm = TRUE)
            })
            
            # Identify Culvert columns (DIS) with at least one value > 0
            culvert_cols_to_keep <- sapply(grep("CULVERT", colnames(tab), value = TRUE), function(col) {
              any(tab[, col] != 0, na.rm = TRUE)
            })
            
            tab <- tab[, c(TRUE, rep(TRUE, 4), flux_cols_to_keep, culvert_cols_to_keep)]  # Keep first 5 cols + selected
            
            write.table(tab,
                        file=file.path(dsnlayerC,paste0(substr(nomsortie,1,nchar(nomsortie)-7),"_sortie.txt")),
                        sep=";")
            
            
            MotsCless=colnames(tab)
            PosFB2=grep(MotsCless,pattern="FLUX BOUNDARY")
            # Suppression du volume initial (confition limite)
            if (length(tab)>2){ # Corrected condition: Check for more than one row
              tab[, 3] = tab[, 3] - tab[1, 3]  # Subtract initial volume
              TimeStep = tab[1, 1]  # Get the time step
              CumulDebit = matrix(0, nrow(tab), length(PosFB2)) #nrow instead of dim
              nsortie=0
              
              
              
              for (ip in PosFB2){ #6:15)
                # print(dim(sapply(1:dim(tab)[1], function(x) {-TimeStep*sum(tab[1:x,ip])})))
                CumulDebit[,ip-5]=sapply(1:dim(tab)[1], 
                                         function(x) {-TimeStep*sum(tab[1:x,ip])})
                # browser()
                tab[is.na(tab[,ip])==T,ip]=0
                if (max(abs(tab[,ip]))>0)
                {
                  nsortie=nsortie+1
                }
              }
              # ...
              tab[, 4] = tab[, 4] / Past_Ts  # Calculate volume per time step
              CumulAddVol = sapply(1:nrow(tab), function(x) { TimeStep * sum(tab[1:x, 4]) }) #nrow
              
              # --- Data Preparation for ggplot2 ---
              
              # Convert 'tab' to a data frame (if it isn't already)
              tab_df <- as.data.frame(tab)
              tab_df$Time <- tab_df[, 1]  # Add a 'Time' column for x-axis
              nsortie2=length(PosFB2)
              # 1. Volume Plot Data
              volume_data <- data.frame(
                Time = tab_df$Time,
                Injecte = CumulAddVol,
                Domaine = tab_df[, 3],
                `Domaine + sorties` = tab_df[, 3] + rowSums(CumulDebit[, 1:nsortie2, drop = FALSE]),
                check.names = FALSE  # Prevent column name modification
              )
              
              # Add cumulative discharge columns, handling potential NAs, and filtering!
              for (isor in 1:nsortie2) {
                col_name <- paste0("Sortie ", isor)
                volume_data[[col_name]] <- CumulDebit[, isor]
              }
              
              volume_long <- pivot_longer(volume_data,
                                          cols = -Time,
                                          names_to = "Variable",
                                          values_to = "Volume")
              
              # 2. Rainfall Plot Data (Already in a good format)
              rainfall_data <- data.frame(
                Time = tab_df$Time,
                Rainfall = tab_df[, 2]
              )
              
              # 3. Net Volume Plot Data (Already in a good format)
              netvolume_data <- data.frame(
                Time = tab_df$Time,
                NetVolume = tab_df[, 4]
              )
              MotsCles2=colnames(tab_df)
              PosFB2=grep(MotsCles2,pattern="FLUX BOUNDARY")
              PosCu2=grep(MotsCles2,pattern="CULVERT")
              # 4. Discharge Plot Data
              discharge_data <- tab_df[, c("Time", MotsCles2[PosFB2])]
              discharge_data <- discharge_data[, colSums(is.na(discharge_data)) < nrow(discharge_data)] # Remove all-NA columns
              # Filter discharge data to keep only columns with at least one non-zero, non-NA value
              # discharge_data <- discharge_data[, c(TRUE, sapply(discharge_data[, -1, drop = FALSE], function(x) any(!is.na(x) & x != 0)))]
              
              discharge_long <- pivot_longer(discharge_data,
                                             cols = -Time,
                                             names_to = "Boundary",
                                             values_to = "Discharge")
              
              
              # 5. Culvert Discharge Plot Data
              if(!is_empty(PosCu2) ){
                culvert_data <- tab_df[, c("Time", MotsCles2[PosCu2])]
                culvert_data <- culvert_data[, colSums(is.na(culvert_data)) < nrow(culvert_data)]  # Remove columns that are all NA
                # Filter culvert data to keep only columns with at least one non-zero value
                culvert_data <- culvert_data[, c(TRUE, sapply(culvert_data[, -1, drop = FALSE], function(x) any(!is.na(x) & x != 0)))]
                
                culvert_long <- pivot_longer(culvert_data,
                                             cols = -Time,
                                             names_to = "Culvert",
                                             values_to = "Discharge")
              }
              
              # Récupérer les niveaux uniques de la variable 'Variable'
              niveaux_variable <- unique(volume_long$Variable)
              nombre_niveaux <- length(niveaux_variable)
              # Définir les couleurs pour les 3 premiers éléments (adaptez ces couleurs à vos besoins)
              couleurs_specifiques <- c("green","blue" , "red") # Exemple de couleurs spécifiques
              volume_long$Time=volume_long$Time+datedebut
              # Vérifier s'il y a au moins 3 niveaux pour attribuer les couleurs spécifiques
              if (nombre_niveaux >= 3) {
                # Assigner les couleurs spécifiques aux 3 premiers niveaux
                palette_couleurs <- setNames(couleurs_specifiques, niveaux_variable[1:3])
                
                # S'il y a plus de 3 niveaux, générer des couleurs aléatoires pour les niveaux restants
                if (nombre_niveaux > 3) {
                  niveaux_restants <- niveaux_variable[4:nombre_niveaux]
                  # Générer des couleurs aléatoires. 'hue_pal()' crée une palette de couleurs basée sur la teinte.
                  # Assurez-vous que le nombre de couleurs générées correspond au nombre de niveaux restants.
                  couleurs_aleatoires <- scales::hue_pal()(length(niveaux_restants))
                  names(couleurs_aleatoires) <- niveaux_restants
                  # Combiner les couleurs spécifiques et aléatoires
                  palette_couleurs <- c(palette_couleurs, couleurs_aleatoires)
                }
              } else {
                # Si moins de 3 niveaux, générer des couleurs aléatoires pour tous les niveaux
                palette_couleurs <- scales::hue_pal()(nombre_niveaux)
                names(palette_couleurs) <- niveaux_variable
              }
              types_lignes_personnalises <- c("solid", "solid", "12",rep("dotdash",nsortie2) )
              tailles_lignes_personnalisees <- c(0.5, 0.5, 1.2, rep(0.5,nsortie2)) # Exemple : tailles en mm
              p_volume <- ggplot(volume_long, aes(x = Time, y = Volume, color = Variable, linetype = Variable, size = Variable)) +
                geom_line() +
                labs(title = nomsortie,
                     x = "Temps Telemac (s)",
                     y = "Volume (m3)",
                     color = "Variable",
                     linetype = "Variable",
                     size = "Variable") +
                theme_bw() +
                scale_linetype_manual(values = types_lignes_personnalises) + # Définissez les types de lignes
                scale_size_manual(values = tailles_lignes_personnalisees) + # Définissez les tailles de lignes
                scale_color_manual(values = palette_couleurs) # <<< Ajoutez cette ligne pour définir les couleurs
              
              # p_volume <- ggplot(volume_long, aes(x = Time, y = Volume, color = Variable, linetype = Variable, size = Variable)) + # <<< Ajoutez size = Variable ici
              #   geom_line() +
              #   labs(title = nomsortie,
              #        x = "Temps Telemac (s)",
              #        y = "Volume (m3)",
              #        color = "Variable",
              #        linetype = "Variable",
              #        size = "Variable") + # <<< Optionnel: personnalisez le titre de la légende size
              #   theme_bw() +
              #   scale_linetype_manual(values = types_lignes_personnalises) + # <<< Définissez les types de lignes
              #   scale_size_manual(values = tailles_lignes_personnalisees) # <<< Ajoutez cette ligne pour définir les tailles de li
              # 
              # # p_volume <- ggplot(volume_long, aes(x = Time, y = Volume, color = Variable, linetype = Variable)) + # <<< Ajoutez linetype = Variable ici
              # #   geom_line() +
              #   labs(title = nomsortie,
              #        x = "Temps Telemac (s)",
              #        y = "Volume (m3)",
              #        color = "Variable",
              #        linetype = "Variable") + # <<< Optionnel: personnalisez le titre de la légende linetype
              #   theme_bw() +
              #   scale_linetype_manual(values = types_lignes_personnalises) 
              # # --- ggplot2 Plotting ---
              # 
              # # Create the plots (rest of plotting code remains the same)
              # volume_long$Time=volume_long$Time+datedebut
              # p_volume <- ggplot(volume_long, aes(x = Time, y = Volume, color = Variable)) +
              #   geom_line() +
              #   labs(title = nomsortie,
              #        x = "Temps Telemac (s)",
              #        y = "Volume (m3)",
              #        color = "Variable") +
              #   theme_bw()
              
              p_rainfall <- ggplot(rainfall_data, aes(x = Time, y = Rainfall)) +
                geom_line() +
                labs(title = "Pluie Injectee",
                     x = "Temps Telemac (s)",
                     y = "Cumul (m)") +
                theme_bw()
              
              p_netvolume <- ggplot(netvolume_data, aes(x = Time, y = NetVolume)) +
                geom_line() +
                labs(title = "Volume net",
                     x = "Temps Telemac (s)",
                     y = "Apport a chaque pas de calcul (m3)") +
                theme_bw()
              
              
              debit_sortie <- data.frame(Time=discharge_long$Time+datedebut,Q=discharge_long$Discharge,Type=discharge_long$Boundary)
              
              p_discharge <- ggplot(debit_sortie, aes(x = Time, y = -Q, color = Type)) +
                geom_line() +
                labs(title = "Debit Sorties",
                     x = "Date ",
                     y = "Debit (m3/s)",
                     color = "Boundary") +
                theme_bw() +
                ylim(min(-debit_sortie$Q), NA)  # Important: Set y-axis to start at 0
              
              
              if(!is_empty(PosCu2) ){
                debit_buses <- data.frame(Time=culvert_long$Time+datedebut,Q=culvert_long$Discharge,Type=culvert_long$Culvert)
                p_culvert <- ggplot(debit_buses, aes(x = Time, y = Q, color = Type)) +
                  geom_line() +
                  labs(title = "Debit Ouvrages",
                       x = "Date",
                       y = "Debit (m3/s)",
                       color = "Culvert") +
                  theme_bw() +
                  ylim(0, max(culvert_long$Discharge, na.rm = TRUE)) # Set y limits based on the data
                
              }
              # Combine and save
              # Convert ggplot2 plots to plotly interactive plots
              gp_volume <- ggplotly(p_volume, tooltip = c("x", "y", "color"),dynamicTicks = T)
              gp_rainfall <- ggplotly(p_rainfall, tooltip = c("x", "y"),dynamicTicks = T)
              gp_netvolume <- ggplotly(p_netvolume, tooltip = c("x", "y"),dynamicTicks = T)
              gp_discharge <- ggplotly(p_discharge, tooltip = c("x", "y", "color"),dynamicTicks = T)
              if(!is_empty(PosCu2) ){
                gp_culvert <- ggplotly(p_culvert, tooltip = c("x", "y", "color"),dynamicTicks = T)
              }
              
              # Combine plots using subplot (from plotly)
              # combined_plotly <- subplot(gp_volume, gp_rainfall, gp_netvolume, gp_discharge, gp_culvert,
              #                            nrows = 5, # Arrange plots vertically
              #                            titleX = TRUE, titleY = TRUE, # Show axis titles
              #                            margin = 0.05, # Adjust margins as needed
              #                            shareX = TRUE)  # Share the x-axis (very important for time series)
              # 
              # 
              # # Save as a single HTML file
              # htmlwidgets::saveWidget(combined_plotly,
              #                         file = file.path(dsnlayerC, paste0(substr(nomsortie, 1, nchar(nomsortie) - 7), ".html")),
              #                         selfcontained = TRUE) # Embed all dependencies
              # 
              
              figsortie <- plotly::subplot(fig3,fig1, gp_discharge, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
                plotly::layout(title = list(text = paste0("Hyéto + Q sortie ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                               plot_bgcolor='#e5ecf6',
                               xaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'       #☻ type = 'date'
                                 # tickformat = "%d %B (%a)<br>%Y"
                               ),
                               yaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'))
              
              figsortieV <- plotly::subplot(fig1,gp_volume, gp_discharge, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
                plotly::layout(title = list(text = paste0("Hyéto + Vol et Q sortie  ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                               plot_bgcolor='#e5ecf6',
                               xaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'       #☻ type = 'date'
                                 # tickformat = "%d %B (%a)<br>%Y"
                               ),
                               yaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'))
              
              if(!is_empty(PosCu2) ){
                figculvert <- plotly::subplot(fig3,fig1, gp_culvert, nrows = 3,shareX=TRUE,heights = c(0.5/4,1/4,2.5/4), titleY = TRUE, titleX = TRUE) %>%
                  plotly::layout(title = list(text = paste0("Hyéto + Q OH ",name_case_," (Pluie sur tout le bassin)")),legend=list(title=list(text='')),
                                 plot_bgcolor='#e5ecf6',
                                 xaxis = list(
                                   zerolinecolor = '#ffff',
                                   zerolinewidth = 2,
                                   gridcolor = 'ffff'       #☻ type = 'date'
                                   # tickformat = "%d %B (%a)<br>%Y"
                                 ),
                                 yaxis = list(
                                   zerolinecolor = '#ffff',
                                   zerolinewidth = 2,
                                   gridcolor = 'ffff'))
                
              }
            }
            
            
          }
          ##############################################################OH Sortie###################       
          if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
            sc_sf$ID <- as.character(sc_sf$ID)
            tab_sc <- inner_join(tab_sc,sc_sf,by = c("nom"="ID"))
            filepath_plotly_all <- file.path(dsnlayerC,paste0(gsub("\\..*", "", SC_list[i_filesc]),"_PN_PB_Q_Obs.html"))
            if(!file.exists(filepath_plotly_all)){
              htmlwidgets::saveWidget(plotly::partial_bundle(fig),
                                      file = filepath_plotly_all,
                                      libdir = "lib")
              
              filepath_plotly_allz <- file.path(dsnlayerC,paste0(gsub("\\..*", "", SC_list[i_filesc]),"_PN_PB_Z_Obs.html"))
              
              
              htmlwidgets::saveWidget(plotly::partial_bundle(figz),
                                      file = filepath_plotly_allz,
                                      libdir = "lib")
              
              filepath_plotly_allH <- file.path(dsnlayerC,paste0(gsub("\\..*", "", SC_list[i_filesc]),"_PN_PB_H_Obs.html"))
              
              htmlwidgets::saveWidget(plotly::partial_bundle(figh),
                                      file = filepath_plotly_allH,
                                      libdir = "lib")
            }
          }
          split_listsortie <- strsplit(Listsortie[[i_filesc]],"_")[[1]]
          namesortie <- paste(split_listsortie[1:7],collapse = "_")
          filepath_plotly_allqs <- file.path(dsnlayerC,paste0(gsub("\\..*", "", namesortie),"_PN_PB_QS_Obs.html"))
          if(!file.exists(filepath_plotly_allqs)){
            htmlwidgets::saveWidget(plotly::partial_bundle(figsortie),
                                    file = filepath_plotly_allqs,
                                    libdir = "lib")
            filepath_plotly_allvqs <- file.path(dsnlayerC,paste0(gsub("\\..*", "", namesortie),"_PN_PB_VQS_Obs.html"))
            # filepath_plotly_allvqs <- file.path(dsnlayerC,paste0(gsub("\\..*", "", Listsortie[[i_filesc]],"_PN_PB_VQS_Obs.html")))
            htmlwidgets::saveWidget(plotly::partial_bundle(figsortieV),
                                    file = filepath_plotly_allvqs,
                                    libdir = "lib")
            
            
            if(!is_empty(PosCu2) ){
              filepath_plotly_allOH <- file.path(dsnlayerC,paste0(gsub("\\..*", "", namesortie),"_PN_PB_QOH_Obs.html"))
              
              htmlwidgets::saveWidget(plotly::partial_bundle(figculvert),
                                      file = filepath_plotly_allOH,
                                      libdir = "lib")
            }
          }
          if(dir.exists(file.path(dsnlayerC,"lib"))){
            
            unlink(file.path(dsnlayerC,"lib"),recursive = TRUE)
          }
          if(file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))){
            tabmaxq <- tab_sc %>% 
              group_by(nom) %>% 
              filter(abs(Q) == max(abs(min(Q)),abs(max(Q)))) %>% 
              filter(Time == min(Time))
            tabmaxq$nom <- as.character(tabmaxq$nom)
            sc_sf$ID <- as.character(sc_sf$ID)
            if("Q" %in% names(sc_sf)){
              sc_sf <- sc_sf[,-which(names(sc_sf)=="Q")]
            }
            new_sc_sf <- left_join(x =sc_sf,y = tabmaxq,by=c("ID"="nom") )
            new_sc_sf$Q <- abs(new_sc_sf$Q )
            if(grepl("Evt", SC_list[i_filesc], fixed = TRUE)){
              new_sc_sf$DATE <- as.character(new_sc_sf$DATE)
            }
          }
          
          
        }
        
      }
    }
    
  }
}
#######################plot section de controle 
Export_GRD <- function(NomGRD,dimensionPtsElem,PTS,ELEM)
{
  # zzNomGRD=file.create(NomGRD)
  cat("Ecriture parfois longue du fichier ",NomGRD)
  # writeLines(Entete, file = zzNomGRD)
  # writeLines("ADCIRC", NomGRD)
  # writeLines(as.character(dimensionPtsElem), NomGRD, append=T)
  writeLines(c("ADCIRC", dimensionPtsElem), NomGRD)
  write.table(PTS,file =NomGRD,col.names = FALSE,row.names = FALSE,sep = " ",append = TRUE )
  write.table(ELEM,file =NomGRD,col.names = FALSE,row.names = FALSE,sep = " ",append = TRUE )
  cat(" - Terminé \n")
}