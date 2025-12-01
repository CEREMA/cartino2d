StricklerCN=function(Zones,iZone,Departement)
{
  #---- BBox pour n'importer que ce qui est dedans dans R
  Zone=Zones[iZone,]
  bbox=st_bbox(Zone)
  
  bbox_grass <- paste0(bbox$xmin, ',', bbox$ymin, ',', bbox$xmax, ',', bbox$ymax)
  bbox_wkt <- paste0("POLYGON((",bbox$xmin, " ",bbox$ymin, ",",bbox$xmax, " ",bbox$ymin, ",",bbox$xmax, " ",bbox$ymax, ",",bbox$xmin, " ",bbox$ymax, ",",bbox$xmin, " ",bbox$ymin, "))")
  
  xabs=c(bbox$xmin,bbox$xmin,bbox$xmax,bbox$xmax,bbox$xmin)
  yabs=c(bbox$ymax,bbox$ymin,bbox$ymin,bbox$ymax,bbox$ymax)
  tour=list()
  tour[[1]]=matrix(c(xabs,yabs),5,2)
  
  tour=st_sf(data.frame(ZONE=Zone$ZONE,
                        "geometry" =st_sfc(st_polygon(tour,dim="XY"))),
             crs=EPSG)
  
  # opt_BDtopoOCSGE=1
  # 
  # if (opt_BDtopoOCSGE!=0)
  # {
  nb=st_intersects(Departement,Zone)
  n_int = which(sapply(nb, length)>0)
  Dpt=Departement[n_int,]
  
  listeDptBDTopo_tmp=list()
  listeDptOCS_GE_tmp=list()
  iDpt_OCS=1
  for (iDpt in 1:dim(Dpt)[1])
  {
    # ouverture de la BDTopo
    listeDptBDTopo_tmp_=list.files(dsnDptBDTopo,pattern=paste0("_D",ifelse(nchar(Dpt$INSEE_DEP[iDpt])==2,paste0("0",Dpt$INSEE_DEP[iDpt]),Dpt$INSEE_DEP[1]),"-"),recursive=T)
    listeDptBDTopo_tmp_=file.path(dsnDptBDTopo,listeDptBDTopo_tmp_[grep(listeDptBDTopo_tmp_,pattern=".gpkg")])
    if (length(listeDptBDTopo_tmp_)==0)
    {
      cat("BDTopo non présente ",Dpt$INSEE_DEP[iDpt]," Merci de la télécharger\n")
      # Badaboom=boom
    }
    listeDptBDTopo_tmp[[iDpt]]=listeDptBDTopo_tmp_
    
    listeDptOCS_GE_tmp_=list.files(dsnDptOCS_GE,pattern="OCCUPATION_SOL.shp",recursive=T)
    indOCS_GE=grep(listeDptOCS_GE_tmp_,pattern=paste0("_D",ifelse(nchar(Dpt$INSEE_DEP[iDpt])==2,paste0("0",Dpt$INSEE_DEP[iDpt]),Dpt$INSEE_DEP[1]),"_"))
    if( length(indOCS_GE)>0)
    {
      listeDptOCS_GE_tmp_=file.path(dsnDptOCS_GE,listeDptOCS_GE_tmp_[indOCS_GE])
      listeDptOCS_GE_tmp[[iDpt_OCS]]=listeDptOCS_GE_tmp_
      iDpt_OCS=iDpt_OCS+1
    }else{
      cat("-------------------------------------------------------------------\n")
      cat("-------------------------------------------------------------------\n")
      cat("-------------------------------------------------------------------\n")
      cat("OCS_GE non présente ",Dpt$INSEE_DEP[iDpt]," Merci de la télécharger\n")
      cat("dans le répertoire",dsnDptOCS_GE,"---------------------------\n")
      cat("-------------------------------------------------------------------\n")
      cat("-----------------------Pause de 0.5 secondes------------------------\n")
      Sys.sleep(0.5)
    }
  }
  # }
  
  # Boucle sur Strickler ou CN
  for (iparam in 1:length(nom_Param))
  {
    
    nom_Param_=nom_Param[iparam]
    if (nchar(dsnlayerStriCN)==0 | dsnlayerStriCNMano==1)
    {
      sous_doss=ifelse(substr(nom_Param_,1,2)=="CN","_CN",paste0("_",nom_Param_))
      
      dsnlayerStriCN_=file.path(dsnlayer,sous_doss,SousDossUser,nom_Param_)
    }else{
      dsnlayerStriCN_=dsnlayerStriCN
    }
    
    AFAIRE=0
    for (iResoStrCN in ResoStrCN)
    {
      tReso=formatC(iResoStrCN, width = 3, flag = "0")
      NomGPKG=file.path(dsnlayerStriCN_,
                        paste0("_Reso",tReso),
                        paste0(nom_Param_,"_",Zone$ZONE,"_Res",tReso,"m.gpkg"))
      if (file.exists(NomGPKG)==F)
      {AFAIRE=1}else{cat(NomGPKG," déjà présent\n")}
    }

    if (AFAIRE==1)
    {
      # browser()
      if (dir.exists(dsnlayerStriCN_)==F){dir.create(dsnlayerStriCN_,recursive = T)}
      
      #### ICI
      nomZone_tmp=file.path(dsnlayerStriCN_,paste0("Contour",Zone$ZONE,".gpkg"))
      if (file.exists(dirname(nomZone_tmp))==F){dir.create(dirname(nomZone_tmp))}
      st_write(tour,nomZone_tmp, delete_layer=T, quiet=T)
      
      # faisle=0
      # if (faisle==1)
      # {
      browser()
      if(exists("listeRast")==T){rm(listeRast)}
      
      
      Ord_=ifelse(nom_Param_=="Strickler","Ord_Str","Ord_CN")
      data_ <- arrange(data,Ord_ )
      data_=data[which(is.na(data[,Ord_])==F),]
      
      Param=data_[,nom_Param_]
      colnames(Param)="Param"
      data_$Param=Param
      
      #Creation d'un monde GRASS
      SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"_",iZone,"/",basename(SecteurGRASS_))
      unlink(dirname(SecteurGRASS),recursive=TRUE)
      system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
      system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
      
      # if (opt_BDtopoOCSGE!=0)
      # {
      listeDptBDTopo=do.call(rbind,listeDptBDTopo_tmp)
      listeDptOCS_GE=do.call(rbind,listeDptOCS_GE_tmp)
      # }
      
      nomZoneG="Zone"
      cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomZone_tmp," output=",nomZoneG," min_area=0.000000001")
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      cmd=paste0("g.region -a --quiet --overwrite vector=",nomZoneG," res=",ResoStrCN[1])
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      inc=1
      # Boucle sur les lignes du tableur excel pour écrire le Strickler et le CN
      # 1:dim(data_)[1]
      for (i in 1:dim(data_)[1])
        # test sur troncon_de_route, troncon_de_voie_ferree, peuplement de conifere (OCS_GE)
      {
        print(data_[i,])
        # Récupération des données du tableur excel
        chemin = as.character(data_$Chemin[i])
        fichier = as.character(data_$Fichier[i])
        coucheGPKG = as.character(data_$CoucheGPKG[i])
        coucheIntersection = as.character(data_$coucheIntersection[i])
        champ = as.character(data_$Champ[i])
        valChamp = as.character(data_$ValChampRecherche[i])
        nom = as.character(data_$NomCerema[i])
        type_geom = as.character(data_$Type_geometrie[i])
        Param = as.numeric(data_$Param$Param[i])
        Buffer_Champ = as.character(data_$Buffer_Champ[i])
        Superf=as.character(data_$Superficie[i])
        if (is.na(type_geom)) next
        nomfichier=file.path(chemin,fichier)
        if (chemin=="BDTopo"){nomfichier=listeDptBDTopo}
        if (chemin=="OCS_GE"){nomfichier=listeDptOCS_GE}
        if (chemin=="FichiersFonciers"){nomfichier=paste0("ff2023_dep.d",Dpt$INSEE_DEP[iDpt],"_fftp_2023_pnb10_parcelle")}
        
        for (ifich in nomfichier)
        {
          Nada=0
          NomG=paste0("R",formatC(inc, width = 3, flag = "0"))
          is.na(as.numeric(Buffer_Champ))
          # Cas de lignes avec un tampon provenant d'une largeur dans une colonne de la table attributaire
          
          champ = strsplit(x = champ, split = ",")[[1]]
          valChamp = strsplit(x = valChamp, split = ',')[[1]]
          
          if (type_geom=="Ligne") 
          {
            NomG_buf=paste0(NomG,"_buf")
            
            if (length(champ)>1){
              
              requete = paste0('SELECT * FROM ', shQuote(coucheGPKG), ' WHERE ', champ[1], ' ', valChamp[1])
              
              for (i in 2:length(champ)){
                
                requete = paste0(requete, ' AND ', champ[i], ' ', valChamp[i])
                
              }
              
              LigneTr = st_read(ifich,layer=coucheGPKG, wkt_filter = bbox_wkt, query = requete)
              
            } else {
              
              LigneTr=st_read(ifich,layer=coucheGPKG, wkt_filter = bbox_wkt, 
                              query = paste0('SELECT * FROM ', shQuote(coucheGPKG), ' WHERE ', champ, ' ', valChamp))
            }
            
            if (is.na(coucheIntersection)==F){
              
              Intersection = st_read(ifich, layer = coucheIntersection, wkt_filter = bbox_wkt)
              nbIntersect = st_intersects(LigneTr, Intersection)
              indIntersect = which(sapply(nbIntersect, length) > 0)
              LigneTr = LigneTr[indIntersect,]
              
            }
            
            # Gestion des tampons pour des polylignes avec un champ de largeur (route de la BDTopo)
            if (is.na(as.numeric(Buffer_Champ))==T)
            {
              
              SeuilLargVoie=2
              Val_tmp=LigneTr[,Buffer_Champ]
              st_geometry(Val_tmp)=NULL
              LigneTr$tmp=as.numeric(as.matrix(Val_tmp/2))
              LigneTrBuf=st_union(st_buffer(LigneTr,dist=LigneTr$tmp))
              
            }else{
              # Gestion des tampons pour des polylignes avec la valeur fourni dans le fichier Excel
              print(unique(LigneTr[,champ[1]]))
              if (length(LigneTr[,champ[1]])>0)
              {
                LigneTrBuf=st_union(st_buffer(LigneTr,dist=as.numeric(Buffer_Champ)))
              }else{Nada=1}
              
            }
            cat("Nada ",Nada)
            if (Nada==0)
            {
              # Export du fichier tampon avant import dans Grass
              nom_tmp=file.path(dsnlayerStriCN_,paste0("Contour",Zone$ZONE,"tmp.gpkg"))
              st_write(LigneTrBuf,nom_tmp, delete_dsn=T,delete_layer=T, quiet=T)
              
              cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nom_tmp," output=",NomG," min_area=0.000000001")
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            }
          }
          
          # Gestion des polygones
          if (type_geom=="Polygone")
          {
            cmd=paste0("v.in.ogr -o -r")
            # Il n'aime pas les bigs de gémoétroes de la BDTopo GPKG mais il faut garder ceux de l'OCS...
            if (substr(ifich,nchar(ifich)-4,nchar(ifich))==".gpkg")
              # {cmd=paste0(cmd,"")}
            {cmd=paste0(cmd," -c")}
            cmd=paste0(cmd," --quiet --overwrite input=",ifich)
            
            if  (is.na(coucheGPKG)==F)
            {
              cmd=paste0(cmd," layer=",coucheGPKG)
            }
            
            if (is.na(champ[1])==F)
            {
              
              if (length(champ)>1){
                
                requete = paste0(champ[1], " ", valChamp[1])
                
                for (i in 2:length(champ)){
                  
                  requete = paste0(requete, ' AND ', champ[i], ' ', valChamp[i])
                  
                }
                
                cmd=paste0(cmd," where=",shQuote(requete))
                
              } else {
                
                cmd=paste0(cmd," where=",shQuote(paste0(champ, " ", valChamp)))
                
              }
              
            }
            cmd=paste0(cmd," output=",NomG," min_area=0.000000001")
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            
            if(is.na(coucheIntersection)==F){
              
              cmd=paste0("v.in.ogr -o -r")
              if (substr(ifich,nchar(ifich)-4,nchar(ifich))==".gpkg")
              {cmd=paste0(cmd," -c")}
              cmd=paste0(cmd," --quiet --overwrite input=",ifich, " layer=", coucheIntersection)
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              outputIntersect = paste0(NomG, "Intersect")
              cmd = paste0("v.select --overwrite ainput=", NomG, " binput=", coucheIntersection, " operator=intersects output=", outputIntersect)
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              cmd = paste0("g.remove -f type=vector name=", NomG)
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              cmd = paste0("g.rename vector=", paste0(outputIntersect, ",", NomG))
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            }
          }
          
          if (type_geom=="PostGre")
          {
            cmd=paste0("db.login -p --overwrite driver=pg",
                       " database=",host_db,
                       " user=",user_db,
                       " password=",password_db,
                       " host=",host_db,
                       " port=",port_db)
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            
            # cmd=paste0("v.in.ogr -o -r input=PG:dbname=",sig user=svgc password=svgc host=172.23.210.16 port=5432 layer=ff2022_dep.d34_fftp_2022_pnb10_parcelle output=d34
            cmd=paste0("v.in.ogr -o -r --overwrite input=",
                       shQuote(paste0("PG:dbname=",dbname_db,
                                      " user=",user_db,
                                      " password=",password_db,
                                      " host=",host_db,
                                      " port=",port_db)),
                       " layer=",nomfichier,
                       " output=",NomG,
                       " where=",shQuote(paste0("ST_Area(geompar) ",Superf," AND nlocal > 0")))
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          }
          
          # S'il y a un travail raster à faire
          if (Nada==0)
          {
            
            cmd=paste0("v.to.rast --quiet --overwrite input=",NomG," output=",NomG)
            if (is.na(Param)==F)
            {
              # Lecture de la valeur du Param dans le fichier Excel
              cmd=paste0(cmd," use=val value=",Param)
            }else{
              # Lecture de la valeur du Param directement dans le fichier SIG
              cmd=paste0(cmd," use=attr attribute_column=",valChamp)
            }
            
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            
            inc=inc+1
          }
          
        }
      }
      
      cmd=paste0("g.list type=raster pattern=R* separator=comma")
      print(cmd);rlist=system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd), intern = TRUE)
      print(rlist)
      listeRast=rlist[length(rlist)]
      print(listeRast)
      
      
      # On colle les raster ensemble
      cmd=paste0("r.patch --quiet --overwrite input=",listeRast ," output=",nom_Param_)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      NomUnivar=file.path(dsnlayerStriCN_,paste0("Contour",Zone$ZONE,"univar.txt"))
      cmd=paste0("r.univar --quiet --overwrite map=",nom_Param_," output=",NomUnivar)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      nullcells=as.numeric(scan(file=NomUnivar,NomUnivar,sep=":",skip=1,nlines=1,dec="."))[2]
      print(nullcells)
      
      if (nullcells > 0){
        
        if(nom_Param_ == "Strickler"){
          ValeurParDef = 15
        } else {
          ValeurParDef = 74
        }
        
        write(paste0("Il y a ", nullcells, " cellules vides.\nLes cellules vides ont été remplacées par la valeur ", ValeurParDef, "."),file.path(dsnlayerStriCN_,paste0("NullCells",Zone$ZONE,".txt")))
        
        cmd=paste0("r.null map=",nom_Param_," null=",ValeurParDef)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      }
      
      if (substr(nom_Param_,1,2)=="CN")
      {
        # Création de la couche S=25400/CN-254
        nomS="S_SCS"
        exp=paste0(nomS,"=25400/",nom_Param_,"-254")
        cmd=paste0("r.mapcalc --overwrite ",exp)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      }
      
      for (iResoStrCN in ResoStrCN)
      {
        tReso=formatC(iResoStrCN, width = 3, flag = "0")
        NomGPKG=file.path(dsnlayerStriCN_,
                          paste0("_Reso",tReso),
                          paste0(nom_Param_,"_",Zone$ZONE,"_Res",tReso,"m.gpkg"))
        
        if(dir.exists(dirname(NomGPKG))==F){dir.create(dirname(NomGPKG),recursive = T)}
        
        if (iResoStrCN!=ResoStrCN[1])
        {
          # Limitation de la région de travail et gestion de la résolution
          cmd=paste0("g.region --quiet --overwrite res=",as.character(iResoStrCN))
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          # Modification de la résolution
          nom_a_resamp=ifelse(substr(nom_Param_,1,2)=="CN",nomS,nom_Param_)
          
          nom_tmp=paste0("P",Zone$ZONE,tReso)
          cmd=paste0("r.resamp.stats --quiet --overwrite input=",nom_a_resamp," output=",nom_tmp)
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          if (substr(nom_Param_,1,2)=="CN")
          {
            # Création de la couche CN_SCS=25400/(CN+254)
            nomCNtmp=paste0("CN_SCS",tReso)
            exp=paste0(nomCNtmp,"=25400/(",nom_tmp,"+254)")
            cmd=paste0("r.mapcalc --overwrite ",exp)
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            nom_tmp=nomCNtmp
          }
          
        }else{
          nom_tmp=nom_Param_
        }
        cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nom_tmp," output=",NomGPKG," type=Float32 format=GPKG nodata=-9999")
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",NomGPKG," 2 4 8 16 32 64 128 256")
        print(cmd);system(cmd)
      }
      
      unlink(nomZone_tmp)
      unlink(NomUnivar)
      unlink(dirname(SecteurGRASS),recursive=TRUE)
      unlink(nom_tmp)
      
      # }
    }else{
      cat(NomGPKG," déjà présent\n")
    }
  }
}