#################################################################################################
################   Etape10_PostTelemacGrass
#################################################################################################

Etape10_PostTelemacGrass=function()
{
  dsnlayerC_ <- file.path(dsnlayerC,"Post")
  
  listeRES=list.files(dsnlayerC,pattern=".res$")
  toremove <- (grepl(pattern = "WD",x =listeRES) & grepl(pattern = ".res",x =listeRES)) | (grepl(pattern = "SFR",x =listeRES) & grepl(pattern = ".res",x =listeRES))
  listeRES <- listeRES[!toremove]
  if (length(listeRES)>0)
  {
    for (ires in 1:length(listeRES))
    {
      ncharsupp=1
      nomres=substr(listeRES[ires],ncharsupp,(nchar(listeRES[ires])-4))
      listeRaster=list.files(dsnlayerC_,pattern=nomres)
      ici=which(substr(listeRaster,nchar(listeRaster)-8,nchar(listeRaster))=="_Brut.asc")
      if (length(ici)>0)
      { 
        #Creation d'un monde GRASS
        SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"_",basename(dsnlayerC),"_",ic,"/",basename(SecteurGRASS_))
        unlink(dirname(SecteurGRASS),recursive=TRUE)
        toto=system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
        if (toto!=0){cat("Vous avez un problème dans ",BatGRASS,"\n");BUGDEGRASS=BOOM}
        system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
        
        listeRaster=listeRaster[ici]
        extsortieASC=substr(listeRaster,nchar(nomres)+2,nchar(listeRaster)-9)
        if (file.exists(paste0(dsnlayerC,"\\",nomres,"_",extsortieASC[length(extsortieASC)],".gpkg"))==FALSE)
        { 
          cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC,paste0(nom_STEP2_Zone_Valid_Calcul,".shp"))," output=",nom_STEP2_Zone_Valid_Calcul)
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          
          for (iparam in 1:length(extsortieASC))
          {
            nomsortieASC=paste0(dsnlayerC_,"\\",nomres,"_",extsortieASC[iparam],"_Brut.asc")
            nomsortieASCok=paste0(dsnlayerC_,"\\",nomres,"_",extsortieASC[iparam],".gpkg")
            
            if(!(is.na(contour$SourceDeb) | nchar(contour$SourceDeb)==0))
            {
              contour_valide <- st_read(file.path(dsnlayerC,paste0(nom_STEP2_Zone_Valid_Calcul,".shp")))
              
              contour_valide <- st_buffer(contour_valide,-(as.numeric(contour$Exzeco)/4))
              
            }
            
            ##############Creation d'un fichier VRT###############
            print(paste0(nomcontour," ---- Travail du lidar dans Grass"))
            # Importation raster dans Grass
            nomASCg=paste0(nomres,"_",extsortieASC[iparam])
            cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomsortieASC," output=",nomASCg)
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            
            # dcélaration de la réguion sur le raster
            cmd=paste0("g.region --overwrite raster=",nomASCg)
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            
            # Suppression des 0
            if (iparam>1)
            {
              cmd=paste0("r.null map=",nomASCg," setnull=0-0.005")
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            }
            
            # masque sur le grand contour
            cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            
            #zoom sur le MNT
            cmd=paste0("g.region --overwrite zoom=","MASK")
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            
            # Arrondi à 2 chiffres significatifs
            MNTFinalRond="MNTFinalRond"
            cmd=paste0("r.mapcalc --quiet --overwrite ",
                       MNTFinalRond,"=0.01*round(100*",nomASCg,")")
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            
            # Export du fichier
            cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",MNTFinalRond," output=",nomsortieASCok," type=Float32 format=GPKG nodata=-9999")
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            # browser()
            
            cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortieASCok," 2 4 8 16 32 64 128 256")
            print(cmd);system(cmd)
            
            if (file.exists(nomsortieASCok)==T){file.remove(nomsortieASC)}
          }
          
          # calcul de la cote maximale si pas présente (max de pluie à la fin par exemple
          if (length(which(extsortieASC=="HWH_m"))==0)
          {
            nTopo=which(extsortieASC=="Bottom_m")
            nCote=which(extsortieASC=="HWM_m")
            if (length(nTopo)>0 & length(nCote)>0)
            {
              nomexp=paste0(nomres,"_","HWH_m")
              cmd=paste0("r.mapcalc --quiet --overwrite ",nomexp,"=",nomres,"_",extsortieASC[nCote],"-",nomres,"_",extsortieASC[nTopo])
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              nomsortieASCok=paste0(dsnlayerC_,"\\",nomexp,".gpkg")
              
              cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomexp," output=",nomsortieASCok," type=Float32 format=GPKG nodata=-9999")
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortieASCok," 2 4 8 16 32 64 128 256")
              print(cmd);system(cmd)
            }
          }
          if (length(which(extsortieASC=="HWM_m"))==0)
          {
            print("Export HWM, on ne pourrait garder que là où HWH et HSF >0")
            nTopo=which(extsortieASC=="Bottom_m")
            nHeau=which(extsortieASC=="HWH_m")
            nHSF=which(extsortieASC=="HSF_m2_s")
            if (length(nTopo)>0 & length(nHeau)>0)# & length(nCote)>0)
            {
              cmd=paste0("r.mask -r")
              # cmd=paste0("r.mask --quiet --overwrite raster=",nomres,"_",extsortieASC[nHSF])
              # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              nomexp=paste0(nomres,"_","HWM_m")
              cmd=paste0("r.series --overwrite input=",nomres,"_",extsortieASC[nTopo],",",nomres,"_",extsortieASC[nHeau]," output=",nomexp," method=sum"," nprocs=",NProcGrass)
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              nomsortieASCok=paste0(dsnlayerC_,"\\",nomres,"_","HWM_m",".gpkg")
              
              cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomexp," output=",nomsortieASCok," type=Float32 format=GPKG nodata=-9999")
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortieASCok," 2 4 8 16 32 64 128 256")
              print(cmd);system(cmd)
            }
          }
        }else{
          print(paste0('Déjà présent: ',paste0(dsnlayerC,"\\",nomres,"_",extsortieASC[length(extsortieASC)],".gpkg")))
        }
        
        listeRasterGpkg=list.files(dsnlayerC_,pattern=nomres)
        iciGpkg=which(substr(listeRasterGpkg,nchar(listeRasterGpkg)-4,nchar(listeRasterGpkg))==".gpkg")
        if (length(iciGpkg)>0)
        {
          listeRasterGpkg=listeRasterGpkg[iciGpkg]
          extsortieGPKG=substr(listeRasterGpkg,nchar(nomres)+2,nchar(listeRasterGpkg)-5)
          nomcopieqml=paste0(dsnlayerC_,"\\",nomres,"_",extsortieGPKG,".qml")
          file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(extsortieGPKG,".qml")),
                    nomcopieqml,overwrite = T)
        }
        
        
        unlink(dirname(SecteurGRASS),recursive=TRUE)
      }
    }
  }
}

Etape10_fn <- function(){
  tryCatch({          if (ETAPE[10] == 1)
  {
    
    Etape10_PostTelemacGrass()
    
  }}, error = function(e) {skip_to_next <<- TRUE})
  
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "problème étape 10"
    contours$STEP_PRE[nsecteur[ic]] <- -2
  }
}


Etape10_fn_parallel <- function(){
  if (ETAPE[10] == 1)
  {
    
    Etape10_PostTelemacGrass()
    
  }
}