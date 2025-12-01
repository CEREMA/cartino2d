#################################################################################################
################   Etape9_PostTelemacPPutils
#################################################################################################

Etape9_PostTelemacPPutils=function()
{
  if (exists("choixPPutilsQgis")==F)
  {
    # choixPPutilsQgis= 1 # pputils
    choixPPutilsQgis= 2 # qgis
  }
  
  listeRES=list.files(dsnlayerC,pattern=".res$")
  
  toremove <- (grepl(pattern = "WD",x =listeRES) & grepl(pattern = ".res",x =listeRES)) | (grepl(pattern = "SFR",x =listeRES) & grepl(pattern = ".res",x =listeRES))
  listeRES <- listeRES[!toremove]
  if (length(listeRES)>0)
  {
    dsnlayerC_ <- file.path(dsnlayerC,"Post")
    dir.create(dsnlayerC_)
    for (ires in 1:length(listeRES))
    {
      # Mettre une conditions pour ne pas refaire si cela a déjà été fait
      # si brut ou final existe...
      ncharsupp=1
      nomres=substr(listeRES[ires],ncharsupp,(nchar(listeRES[ires])-4))
      print(nomres)
      
      checkspatial <- grepl( "spatial",nomres, fixed = TRUE)
      checkshys <- grepl( "shyreg_spat",nomres, fixed = TRUE)
      checkshPB <- grepl( "shyreg_spPB",nomres, fixed = TRUE)
      checkshPN <- grepl( "shyreg_spPN",nomres, fixed = TRUE)
      
      if(checkspatial)
      {
        nomcas_ <- substr(nomres,9,nchar(nomres)) 
        filecas <- file.path(dsnlayerC,paste0(nomcas_,"_s.cas"))
      }else if (checkshys){
        nomcas_ <- substr(nomres,13,nchar(nomres)) 
        filecas <- file.path(dsnlayerC,paste0(nomcas_,"_SHYS.cas"))
      }else if (checkshPB){
        nomcas_ <- substr(nomres,13,nchar(nomres)) 
        filecas <- file.path(dsnlayerC,paste0(nomcas_,"_spPB.cas"))
      }else if (checkshPN){
        nomcas_ <- substr(nomres,13,nchar(nomres)) 
        filecas <- file.path(dsnlayerC,paste0(nomcas_,"_spPN.cas"))
      }else{
        nomcas_ <- substr(nomres,7,nchar(nomres))
        filecas <- file.path(dsnlayerC,paste0(nomcas_,".cas"))
      }
      
      if (file.exists(filecas))
      {
        Lignes=readLines(con=filecas)
        
        RecupTps=rbind("DURATION   ",
                       "GRAPHIC PRINTOUT PERIOD",
                       "TIME STEP ")
        indic1=regexpr(RecupTps[1,1],as.character(Lignes))
        
        Durt_s=as.numeric(substr(Lignes[which(indic1>-1)],38,nchar(Lignes[which(indic1>-1)])))
        indic2=regexpr(RecupTps[2,1],as.character(Lignes))
        
        Past_s=as.numeric(substr(Lignes[which(indic2>-1)],34,nchar(Lignes[which(indic2>-1)])))
        
        indic3=regexpr(RecupTps[3,1],as.character(Lignes))
        Past_Ts=as.numeric(substr(Lignes[which(indic3>-1)],38,nchar(Lignes[which(indic3>-1)])))
        TpsSortie=Durt_s/(Past_s*Past_Ts)
        
        if (is.na(as.numeric(contour$Exzeco))==T)
        {
          cellsize=1
        }else{
          cellsize=as.numeric(contour$Exzeco)/2
          if (dim(Rel_Param_ext)[1]==1 & Rel_Param_ext[1]=="HWT25m_s"){cellsize=contour$Exzeco}
        }
        
        ##### PPUTILS
        if (choixPPutilsQgis==1)
        {
          # Pour voir les variables mais inutile
          cmd=paste0(Bug_python," ",file.path(chemin_pputils,"probe_fp.py")," -i ", paste0(dsnlayerC,"\\",listeRES[ires]))
          wd=getwd()
          setwd(dsnlayerC)
          print(cmd);system(cmd)
          setwd(wd)
          
          Ligneprobe=readLines(con=file.path(dsnlayerC,"Infos_PROBE_Putils.txt"))
          nbreparam=as.numeric(str_split(Ligneprobe[1],":")[[1]][2])
          NomParam=Ligneprobe[2:(nbreparam+1)]

          for (iparam in 1:nbreparam)
          {
            extsortie=Rel_Param_ext[which(Rel_Param_ext[,2]==NomParam[iparam]),1] #2 pour pputils
            nomsortieASC=paste0(dsnlayerC_,"\\",nomres,"_",extsortie,"_Brut.asc")
            
            # modif en gpkg
            nomsortieASCok=paste0(dsnlayerC_,"\\",nomres,"_",extsortie,".gpkg")
            if (file.exists(nomsortieASC)==FALSE)
            {       
              if (file.exists(nomsortieASCok)==FALSE)
              {
                
                print(paste("Résolution Export:",cellsize))
                if(TpsSortie>floor(TpsSortie)){
                  TpsSortie_ <- as.integer(floor(TpsSortie+1))
                }else{
                  TpsSortie_ <- as.integer(TpsSortie)
                }
                
                cmd=paste0(Bug_python," sel2asc_cerema.py -i ", paste0(dsnlayerC,"\\",listeRES[ires]),
                           " -v ",iparam-1," -t ",TpsSortie_," -s ",cellsize," -o ",
                           nomsortieASC)
                print(cmd);system(cmd)
                
                #######################################################################
                #######################################################################
                
              }else{
                print(paste0('Déjà présent: ',nomsortieASCok))
              }
            }else{
              print(paste0('Déjà présent: ',nomsortieASC))
            }
          }
        }
        
        #######################################################################
        #######################################################################
        # TELEMAC ET QGIS
        if (choixPPutilsQgis==2)
        {        
          nomajeter=paste0(nomres,"ajeter.txt")
          nomajeter=paste0("ajeter.txt")
          # Récupération par Telemac
          zzRV=paste0(file.path(dsnlayerC,"RecupVariable.bat"))
          # write(paste0("CALL ",C:\\TELEMAC\\V8P4,"\\configs\\pysource.bat"               , file = zzRV)
          write(paste0("CALL ",telemac_folder,"\\configs\\pysource.bat")      , file = zzRV)
          write(""                                                            , file = zzRV, append=T)
          write("SET T2DEXEC=%HOMETEL%\\scripts\\python3\\run_telfile.py"     , file = zzRV, append=T)
          write("SET PATHEXEC=%~dp0"                                          , file = zzRV, append=T)
          write(""                                                            , file = zzRV, append=T)
          write("cd %PATHEXEC%"                                               , file = zzRV, append=T)
          write(""                                                            , file = zzRV, append=T)
          write(paste0("python %T2DEXEC% scan ",listeRES[ires], " > ",nomajeter), file = zzRV, append=T)
          write("cmd /k "                                                     , file = zzRV, append=T)
          setwd(dsnlayerC)
          toto=system(zzRV)
          
          LignezzRV=readLines(con=file.path(dsnlayerC,nomajeter))
          nici=which(regexpr(pattern=" - Number of records:",as.character(LignezzRV))>0)
          nici=which(regexpr(pattern="  - Number of variables:",as.character(LignezzRV))>0)
          nbreparam=as.numeric(str_split(LignezzRV[nici],":")[[1]][2])
          NomParam=substr(LignezzRV[(nici+1):(nici+nbreparam)],13,28)
          nici=which(regexpr(pattern="- X range ",as.character(LignezzRV))>0)
          xminxmax=substr(LignezzRV[nici],16,nchar(LignezzRV[nici])-1)
          xmin=as.numeric(str_split(xminxmax,",")[[1]][1])
          xmax=as.numeric(str_split(xminxmax,",")[[1]][2])
          nici=which(regexpr(pattern="- Y range ",as.character(LignezzRV))>0)
          yminymax=substr(LignezzRV[nici],16,nchar(LignezzRV[nici])-1)
          ymin=as.numeric(str_split(yminymax,",")[[1]][1])
          ymax=as.numeric(str_split(yminymax,",")[[1]][2])
          
          nici=which(regexpr(pattern="- Date: ",as.character(LignezzRV))>0)
          DateBase=substr(LignezzRV[nici],11,30)
          nici=which(regexpr(pattern="  - Time range: ",as.character(LignezzRV))>0)
          Tempsfinal=as.numeric(str_split(substr(LignezzRV[nici],19,nchar(LignezzRV[nici])-1),",")[[1]][2])
          
          # Convertir la date de base en objet POSIXct
          DateBase_POSIXct <- as.POSIXct(DateBase, format = "%Y-%m-%d %H:%M:%S")
          # Ajouter la durée en secondes à la date de base
          DateFinal <- DateBase_POSIXct + Tempsfinal
          # Formater la date finale au format souhaité
          DateFinal_formatted <- format(DateFinal, format = "%Y-%m-%dT%H:%M:%SZ")
          
          # for (iparam in 1:nbreparam){codeparam=ifelse(iparam==1,0,paste0(codeparam,",",iparam-1))}
          
          xmin_=xmin-cellsize/2
          xmax_=xmax+cellsize/2
          ymin_=ymin-cellsize/2
          ymax_=ymax+cellsize/2
          
          decaligroup=0
          for (iparam in 1:nbreparam)
          {
            extsortie=Rel_Param_ext[which(Rel_Param_ext[,3]==NomParam[iparam]),1] #2 pour pputils
            if (NomParam[iparam]=="VELOCITY U      "){decaligroup=-1}
            
            if (length(extsortie)>0)
            {
              nomsortieASCok=paste0(dsnlayerC_,"\\",nomres,"_",extsortie,".gpkg")
              print(nomsortieASCok)
              
              if (file.exists(nomsortieASCok)==FALSE)
              {  
                nomsortieBrutTIF=paste0(dsnlayerC_,"\\",nomres,"_",extsortie,"_BrutTIF.tif")
                cmd <- paste0(qgis_process, " run native:meshrasterize",
                              " --INPUT=",shQuote(listeRES[ires]),
                              " --EXTENT=",xmin,",",xmax,",",ymin,",",ymax,
                              " --DATASET_GROUPS=",iparam-1+decaligroup,
                              " --DATASET_TIME=",shQuote(DateFinal_formatted),
                              " --EPSG=",EPSG,
                              " --PIXEL_SIZE=",cellsize,
                              " --OUTPUT=",nomsortieBrutTIF)#,
                # " > NUL 2>&1")
                print(cmd);toto=system(cmd,intern = TRUE)
                
                print(toto)
                
                indic=regexpr("ERROR: Status 7: Requested index: -1 is out of scope for dataset groups",toto)
                correspond=which(indic>-1)
                if (length(correspond)>1)
                {
                  cat("Il y a des problèmes de cohérences de dates entre telemac et qgis\n")
                  cat("Mettre la variable choixPPutilsQgis= 1 # pputils dans votre fichier paramutiisateur")
                  browser()
                }
                
                
                nomsortieBrutTIF_decalXY=paste0(dsnlayerC_,"\\",nomres,"_",extsortie,"_BrutTIF_decalXY.tif")
                
                cmd <- paste0(qgis_process, " run gdal:warpreproject",
                              " --distance_units=meters --area_units=m2",
                              " --ellipsoid=EPSG:7019",
                              " --INPUT=",shQuote(nomsortieBrutTIF),
                              # " --TARGET_EXTENT_CRS='EPSG:2154'",
                              " --TARGET_EXTENT=",xmin_,",",xmax_,",",ymin_,",",ymax_,
                              " --MULTITHREADING=false",
                              " --EXTRA=",
                              " --OUTPUT=",shQuote(nomsortieBrutTIF_decalXY))
                print(cmd);toto=system(cmd)
                
                unlink(file.path(dirname(nomsortieBrutTIF),list.files(dirname(nomsortieBrutTIF),pattern=basename(nomsortieBrutTIF))))
                
                nomsortieASC=paste0(dsnlayerC_,"\\",nomres,"_",extsortie,"_Brut.asc")
                
                file.rename(nomsortieBrutTIF_decalXY,nomsortieASC)
              }
              else{
                print(paste0('Déjà présent: ',nomsortieASCok))
              }
            }
          }
        }
      }
    }
  }
}


Etape9_fn <- function(){
  tryCatch({          if (ETAPE[9] == 1)
  {
    ##### interpolation des résultats avec Pputils
    setwd(chemin_pputils)
    
    Etape9_PostTelemacPPutils()
  }}, error = function(e) {skip_to_next <<- TRUE})
  
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "problème étape 9"
    contours$STEP_PRE[nsecteur[ic]] <- -2
  }
}

Etape9_fn_parallel <- function(){
  if (ETAPE[9] == 1)
  {
    ##### interpolation des résultats avec Pputils
    setwd(chemin_pputils)
    
    Etape9_PostTelemacPPutils()
  }
}

