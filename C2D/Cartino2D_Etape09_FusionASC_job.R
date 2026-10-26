Cartino2D_Etape12_FusionASC_job=function(dsnlayer,contour,contours,SecteurGRASS_,BatGRASS,EPSG,nsecteur,OSGeo4W_path,NProcGrass,chem_routine,Doss_Qml,Scena,isc,nompost,ipost,dsnlayerRes)
{
  ########################################################################
  ############# Exportation des OH et SC #################################
  ########################################################################
  extension=cbind("SC_Qmax.gpkg","OH.gpkg")
  
  CPattern=strsplit(Scena[isc]," ")[[1]]
  for (iext in extension)
  {
    
    listeSCOH=list.files(file.path(dsnlayer,contour$NOMPOST),pattern=iext,recursive = T,full.names = T)
    
    CPattern=strsplit(Scena[isc]," ")[[1]]
    for (ip in 1:length(CPattern))
    {
      CPat=CPattern[ip]
      ici=grep(listeSCOH,pattern=CPat)
      if (length(ici)>0){listeSCOH=listeSCOH[ici]}
      
      commun=intersect(contours$NOM,basename(dirname(listeSCOH)))
      listeSCOH=listeSCOH[which(basename(dirname(listeSCOH)) %in% commun)]
    }
    
    if (length(listeSCOH)>0)
    {
      if (iext=="SC_Qmax.gpkg")
      {
        listeGPKG=lapply(listeSCOH, function(x) {cbind(st_read(x)[,cbind("ID","NOM","DATE","Q","Zmoy","Hmax","NomSecteur")],SECTEUR=basename(x))})
      }else{
        # listeGPKG=lapply(listeSCOH, function(x) {cbind(st_read(x),SECTEUR=basename(x))})
        listeGPKG=lapply(listeSCOH, function(x) {cbind(st_read(x)[,cbind("id","NOM","LRG","HAUT1","HAUT2","Z1","Z2","FRIC","CIRC","LENGTH","MaxQ")],SECTEUR=basename(x))})
      }    
      
      SCOH_Fus=do.call(rbind, listeGPKG)
      # print(SCOH_Fus$SECTEUR)
      st_write(SCOH_Fus,file.path(dsnlayerRes,paste0(nompost[ipost],"_",gsub(" ","_",Scena[isc]),"_",iext)),delete_layer = T, quiet = T)
      
      if (iext=="SC_Qmax.gpkg")
      {
      file.copy(file.path(chem_routine,"C2D",Doss_Qml,"SC_Qmax.qml"),
                file.path(dsnlayerRes,paste0(nompost[ipost],"_",gsub(" ","_",Scena[isc]),"_",substr(iext,1,nchar(iext)-5),".qml")))
      }else{
        file.copy(file.path(chem_routine,"C2D",Doss_Qml,"MaxQ_OuvHydrau_Select.qml"),
                  file.path(dsnlayerRes,paste0(nompost[ipost],"_",gsub(" ","_",Scena[isc]),"_",substr(iext,1,nchar(iext)-5),".qml")))
      }
    }
  }
  
  ########################################################################
  ############# Exportation des Rasters ##################################
  ########################################################################
  faitRast=1
  if (faitRast==1)
  {
    extsortieASC = cbind("Bottom_m", "HWM_m", "HWT_s", "HV_m_s", "HSF_m2_s", "HWH_m")
    extsortieASC = cbind("HWH_m", "HSF_m2_s", "HV_m_s","Bottom_m", "HWM_m", "HWT_s")
    # extsortieASC = cbind("HSF_m2_s")
    for (iparam in 1:length(extsortieASC))
    {  
      print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      nomexport=paste0(nompost[ipost],"t",gsub(" ","_",Scena[isc]),extsortieASC[iparam])
      nomsortie=paste0(dsnlayerRes,"\\",nomexport,".gpkg")
      
      print(paste0("Scenario - Parametre ",nomexport))
      
      # Si le raster de fusion existe déjà, on passe au suivant pour gagner du temps
      if (file.exists(nomsortie)) {
        print(paste0("   --> Deja fusionne, on passe au suivant : ", nomsortie))
        next
      }
      
      fichiers_a_fusionner = c()
      
      for (ic in 1:length(nsecteur))
      {
        #############################################################################
        ################ CONTOUR #####################################################
        #############################################################################
        # Recuperation du contour i
        contour=contours[nsecteur[ic],]
        
        # Recuperation du nom du contour
        nomcontour=contour$NOM
        dsnlayerC = file.path(dsnlayer, contour$NOMPOST, nomcontour)
        
        CPattern=strsplit(Scena[isc]," ")[[1]]
        ListeASC=list.files(file.path(dsnlayerC,"POST"),pattern=CPattern[1])
        if (length(CPattern)>1)
        {
          for (ip in 2:length(CPattern))
          {
            CPat=CPattern[ip]
            ici=grep(ListeASC,pattern=CPat)
            if (length(ici)>0){ListeASC=ListeASC[ici]}
          }
        }
        
        finchar=paste0(extsortieASC[iparam],".gpkg")
        
        indic=regexpr(finchar,substr(ListeASC,(nchar(ListeASC)-nchar(finchar)),nchar(ListeASC)))
        if (max(indic)>-1)
        {
          nbasc=which(indic>1)
          for (isasc in 1:length(nbasc))
          {
            fichier = file.path(dsnlayerC,"POST",ListeASC[nbasc[isasc]])
            print(fichier)
            fichiers_a_fusionner = c(fichiers_a_fusionner, fichier)
          }
        }
      }
      
      if (length(fichiers_a_fusionner) > 0)
      {  
        nomsortie=paste0(dsnlayerRes,"\\",nomexport,".gpkg")
        if (file.exists(nomsortie)) { unlink(nomsortie, force=TRUE) }
        
        if (length(fichiers_a_fusionner) > 1) {
            library(terra)
            rasts <- lapply(fichiers_a_fusionner, rast)
            
            # Alignement propre sur la grille du premier raster (evite l'erreur resolution mismatch)
            r_master <- rasts[[1]]
            rasts <- lapply(rasts, function(r) {
                # Si la grille est decalee meme d'un micron a cause de GRASS
                if (!all(res(r) == res(r_master)) || !all(origin(r) == origin(r_master))) {
                    # On recalcule l'emprise exacte pour coller parfaitement a r_master
                    e_align <- align(ext(r), r_master)
                    r_cible <- rast(e_align, res=res(r_master), crs=crs(r_master))
                    # On resample proprement (Nearest Neighbor pour ne pas modifier les valeurs)
                    return(resample(r, r_cible, method="near"))
                }
                return(r)
            })
            
            src <- sprc(rasts)
            
            # Utilisation d'un tryCatch pour intercepter l'erreur "[mosaic] there are no cell values"
            # qui survient si tous les rasters sont completement vides (ex: aucun alea pour ce scenario)
            tryCatch({
                mosaic(src, fun="max", filename=nomsortie, overwrite=TRUE, filetype="GPKG")
            }, error = function(e) {
                print(paste("   --> Info : Fusion impossible (rasters probablement vides) :", e$message))
                print("   --> Copie du premier raster comme resultat de fusion par defaut.")
                file.copy(fichiers_a_fusionner[1], nomsortie, overwrite=TRUE)
            })
        } else {
            file.copy(fichiers_a_fusionner[1], nomsortie, overwrite=TRUE)
        }
        
        nomexpqml=paste0(dsnlayerRes,"\\",nomexport,".qml")
        if (file.exists(nomexpqml)==TRUE) {file.remove(nomexpqml)}
        file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(extsortieASC[iparam],".qml")),
                  nomexpqml,overwrite = T)
        
        setwd(dsnlayerRes)
        cmd = paste0(OSGeo4W_path," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r NEAREST ",shQuote(nomsortie)," 2 4 8 16 32 64 128 256")
        print(cmd);toto=system(cmd);print(toto)
      }
    }
  }
}
