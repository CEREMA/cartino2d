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
        listeGPKG=lapply(listeSCOH, function(x) {cbind(st_read(x)[,cbind("ID","NOM","numsec","Time","Q","Zmoy","Zmax","Zmin","Hmax","Htot","NomSecteur","H_Obs","Q_Obs","Z_Obs")],SECTEUR=basename(x))})
      }else{
        # listeGPKG=lapply(listeSCOH, function(x) {cbind(st_read(x),SECTEUR=basename(x))})
        listeGPKG=lapply(listeSCOH, function(x) {cbind(st_read(x)[,cbind("ID","MATERIAUX","LRG","HAUT1","HAUT2","Z1","Z2","FRIC","CIRC","TYPE_OUVRA","SECT_HYDRO","LENGTH","MaxQ")],SECTEUR=basename(x))})
      }    

      SCOH_Fus=do.call(rbind, listeGPKG)
      # print(SCOH_Fus$SECTEUR)
      st_write(SCOH_Fus,file.path(dsnlayerRes,paste0(nompost[ipost],"_",gsub(" ","_",Scena[isc]),"_",iext)),delete_layer = T, quiet = T)
      file.copy(file.path(chem_routine,"C2D",Doss_Qml,"SC_Qmax.qml"),
                file.path(dsnlayerRes,paste0(nompost[ipost],"_",gsub(" ","_",Scena[isc]),"_",substr(iext,1,nchar(iext)-5),".qml")))
    }
  }
  
  ########################################################################
  ############# Exportation des Rasters ##################################
  ########################################################################
  faitRast=1
  if (faitRast==1)
  {
    #Creation d'un monde GRASS
    SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"_",isc,"/",basename(SecteurGRASS_))
    unlink(dirname(SecteurGRASS),recursive=TRUE)
    toto=system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
    if (toto!=0){cat("Vous avez un problème dans ",BatGRASS,"\n");BUGDEGRASS=BOOM}
    system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
    
    extsortieASC = cbind("Bottom_m", "HWM_m", "HWT_s", "HV_m_s", "HSF_m2_s", "HWH_m")
    extsortieASC = cbind("HWH_m", "HSF_m2_s", "HV_m_s","Bottom_m", "HWM_m", "HWT_s")
    # extsortieASC = cbind("HSF_m2_s")
    for (iparam in 1:length(extsortieASC))
    {  
      print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      nomexport=paste0(nompost[ipost],"t",gsub(" ","_",Scena[isc]),extsortieASC[iparam])
      print(paste0("Scenario - Paramètre ",nomexport))
      listegrass=""
      inc=1
      for (ic in 1:length(nsecteur))
      {
        #############################################################################
        ################ CONTOUR #####################################################
        #############################################################################
        # Récupération du contour i
        contour=contours[nsecteur[ic],]
        
        # Récupération du nom du contour
        nomcontour=contour$NOM
        dsnlayerC = file.path(dsnlayer, contour$NOMPOST, nomcontour)
        
        # hyeto_files <- grep(pattern = "(?=.*Sce)(?=.*cas$)|(?=.*Evt$)(?=.*cas$)", 
        #                     x = list.files(path = dir_from,full.names = TRUE), value = TRUE, perl = TRUE)
        
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
            print(ListeASC[nbasc[isasc]])
            # Importation raster dans Grass
            nomoutput=paste0("R",inc)
            cmd=paste0("r.in.gdal -o --quiet --overwrite input=",file.path(dsnlayerC,"POST",ListeASC[nbasc[isasc]])," output=",nomoutput)
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            listegrass=paste(listegrass,nomoutput,",",sep="")
            inc=inc+1
          }
        }
      }
      if (inc>1)
      {  #---- Reglage de la region
        listegrass=substr(listegrass,1,nchar(listegrass)-1)
        cmd=paste0("g.region --overwrite -a raster=",listegrass)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        #---- Calcul de la statistique
        cmd=paste0("r.series --quiet --overwrite input=",listegrass," output=",nomexport," method=maximum"," nprocs=",NProcGrass)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        #---- Exportation du résultat
        nomsortie=paste0(dsnlayerRes,"\\",nomexport,".gpkg")
        cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomexport," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        
        nomexpqml=paste0(dsnlayerRes,"\\",nomexport,".qml")
        if (file.exists(nomexpqml)==TRUE) {file.remove(nomexpqml)}
        file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(extsortieASC[iparam],".qml")),
                  nomexpqml,overwrite = T)
        
        setwd(dsnlayerRes)
        cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
        print(cmd);toto=system(cmd);print(toto)
      }
    }
    unlink(dirname(SecteurGRASS),recursive=TRUE)
  }
}