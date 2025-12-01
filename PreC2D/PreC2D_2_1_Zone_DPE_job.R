PreC2D_1_1_Zone_DPE_job=function(SecteurGRASS_,iraci,raci,Extens,dsnDPE,resolution)
{
  
  SecteurGRASS=SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"_",iraci,"/",basename(SecteurGRASS_))
  unlink(dirname(SecteurGRASS),recursive=TRUE)
  system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
  system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
  
  
  
  # Ouverture de HWH, HSF et HWT
  for (i in 1:3)
  {
    # Import du raster
    # if (file.exists(file.path(dsnDPE,paste0(raci,Extens[1,i],Extens[2,i],".gpkg")))==F)
    # {cat(file.path(dsnDPE,paste0(raci,Extens[1,i],Extens[2,i],".gpkg"))," n'existe pas","\n");boom=modifdsnDPEetraci}
    if (file.exists(file.path(dsnDPE,paste0(raci,Extens[1,i],Extens[2,i],".gpkg")))==T)
    {
      cmd=paste0("r.in.gdal -o --quiet --overwrite input=",file.path(dsnDPE,paste0(raci,Extens[1,i],Extens[2,i],".gpkg"))," output=",Extens[1,i])
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    }
  }
  
  # Limitation de la région de travail
  cmd=paste0("g.region --quiet --overwrite raster=",Extens[1,1],",",Extens[1,2])
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  masque="masque"
  # importation vecteur contour
  cmd=paste0("v.in.ogr -o -r --quiet --overwrite input=",nommasque," output=",masque)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # masque sur le grand contour
  cmd=paste0("r.mask --quiet --overwrite vector=",masque)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Limitation de la région de travail
  cmd=paste0("g.region --quiet --overwrite zoom=","MASK")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #-------------------- ZI
  # Bug, ne garde qui si l'un et l'autre ont des valeurs, bug sur nodata
  # nomInond="ZI"
  # exp=paste0(nomInond," =if(",Extens[1,1],">",Extens[3,1]," || ",
  #            Extens[1,2],">",Extens[3,2],",1,null())")
  # cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #-------------------- ZI
  nomInond1="ZI1"
  exp=paste0(nomInond1," =if(",Extens[1,1],">",Extens[3,1],",1,null())")
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomInond2="ZI2"
  exp=paste0(nomInond2," =if(",Extens[1,2],">",Extens[3,2],",1,null())")
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomInond="ZI"
  cmd=paste0("r.series --overwrite input=",nomInond1,",",nomInond2," output=",nomInond," method=maximum")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Emprise pour Carte Socle
  BufPlus=1*resolution
  BufMoins=1*resolution
  
  BufPlusMoins(nomInond,seuilSup0,BufPlus,BufMoins,"-s",dsnDPE,-99,1,SecteurGRASS)
  nomvectsortie=file.path(dsnDPE,paste0(raci,"_",nomInond,".gpkg"))



  source(file.path(chem_routine,"PreC2D","PreC2D_6_2_Decrenelage_job.R"))
  AireMin_Inter=630
  AireMin_Exter=25*25*2.01
  Decrenelage(nomvectsortie,AireMin_Inter,AireMin_Exter,EPSG,resolution)
  
  if (file.exists(file.path(dsnDPE,paste0(raci,Extens[1,3],Extens[2,3],".gpkg")))==T)
  {
    
    #--------------------- ZI Flash
    nomInond_Flash="ZI_Flash"
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomInond_Flash," =if(",nomInond,"==1,if(",Extens[1,3],"<",Extens[3,3],",2,null()))")))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    BufPlus=1*resolution
    BufMoins=1*resolution
    BufPlusMoins(nomInond_Flash,5.1*resolution^2,BufPlus,BufMoins,"",dsnDPE,-99,1,SecteurGRASS)

    #--------------------- ZI Crue
    nomInond_Crue0="ZI_Crue"
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomInond_Crue0," =if(",nomInond,"==1,if(",Extens[1,3],">",as.numeric(Extens[3,3])-60*DeltaPicH_PluieDebit,",1,null()))")))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    BufPlus=2*resolution
    BufMoins=2*resolution
    BufPlusMoins(nomInond_Crue0,seuilSup0,BufPlus,BufMoins,"-s",dsnDPE,-99,1,SecteurGRASS)
    
    ZoneCrue <- st_read(file.path(dsnDPE,paste0(raci,"_",nomInond_Crue0,".gpkg")))
    if (nrow(ZoneCrue)>0)
    {
      st_crs(ZoneCrue)=EPSG
      ZoneCrue$Identifiant=FILINO_NomMasque(ZoneCrue)
      ZoneCrue=ZoneCrue[order(ZoneCrue$Identifiant,decreasing = TRUE),]
      
      st_write(ZoneCrue,file.path(dsnDPE,paste0(raci,"_SecteurCoursEau_Debit.gpkg")), delete_layer=T, quiet=T)
    }
    # # Travail sur les sauts de temps
    # Voisinage=3
    # DelatTps=5*60 # en secondes
    # # Masque sur la zone inondée nomInond
    # cmd=paste0("r.mask -i --quiet --overwrite raster=",nomInond)
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # 
    # nomtmp_BufT1="BufT1"
    # cmd=paste0("r.resample --quiet --overwrite input=","MASK"," output=",nomtmp_BufT1)
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # 
    # cmd=paste0("r.mask -r")
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # 
    # BufT=2*resolution
    # nomtmp_BufT2="BufT2"
    # cmd=paste0("r.buffer --quiet --overwrite input=",nomtmp_BufT1," output=",nomtmp_BufT2," distance=",BufT)
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # 
    # cmd=paste0("r.mask -i --quiet --overwrite raster=",nomtmp_BufT2)
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # 
    # nomsortieGPKG=file.path(dsnDPE,paste0(raci,"_Tps.gpkg"))
    # cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",Extens[1,3]," output=",nomsortieGPKG," type=Float32 format=GPKG nodata=-9999")
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # # Min sur un voisinage du temps circulaire
    # minTps="MinTemps"
    # cmd=paste0("r.neighbors --quiet --overwrite -c input=",Extens[1,3]," output=",minTps," size=",Voisinage," method=minimum")
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # # max sur un voisinage du temps circulaire
    # maxTps="MaxTemps"
    # cmd=paste0("r.neighbors --quiet --overwrite -c input=",Extens[1,3]," output=",maxTps," size=",Voisinage," method=maximum")
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # # Différence Min Max de temps
    # DiffTps="DiffTemps"
    # exp=paste0(DiffTps," =if(",maxTps,">",minTps,"+",DelatTps,",1,null())")
    # cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # nomsortieGPKG=file.path(dsnDPE,paste0(raci,"_DiffTps.gpkg"))
    # cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",DiffTps," output=",nomsortieGPKG," type=Float32 format=GPKG nodata=-9999")
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # Suppression du masque (s'il existe)
    cmd=paste0("r.mask -r")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    
    # #--------------------- Merge ZICrue ZIFlash
    # nomInond_FC="ZI"
    # cmd=paste0(qgis_process, " run native:mergevectorlayers")
    # cmd=paste0(cmd," --LAYERS=",shQuote(file.path(dsnDPE,paste0(raci,"_",nomInond_Flash,".gpkg"))))
    # cmd=paste0(cmd," --LAYERS=",shQuote(file.path(dsnDPE,paste0(raci,"_",nomInond_Crue0,".gpkg"))))
    # # cmd=paste0(cmd,paste0(" --CRS=QgsCoordinateReferenceSystem('EPSG:",EPSG,"') "))
    # cmd=paste0(cmd," --OUTPUT=",shQuote(shQuote(file.path(dsnDPE,paste0(raci,"_",nomInond_FC,".gpkg")))))
    # print(cmd);system(cmd)
    # 
    # qgis_process run native:mergevectorlayers --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 
    # --LAYERS='C:/Cartino2D/France/MTPCLL2024/C166_662686km_X769396Y6281411/Post/hyeto_C166_662686km_X769396Y6281411_SceMaxMedContFrance_00h_00m_14h_00m__ZI_Crue.gpkg|layername=FinBricolage'
    # --LAYERS='C:/Cartino2D/France/MTPCLL2024/C166_662686km_X769396Y6281411/Post/hyeto_C166_662686km_X769396Y6281411_SceMaxMedContFrance_00h_00m_14h_00m__ZI_Flash.gpkg|layername=FinBricolage'
    # --OUTPUT=TEMPORARY_OUTPUT
    
    
    #----------------------- Endorésime
    #### Calcul du temps de fin
    NomUnivar=file.path(dsnDPE,paste0(Extens[1,3],"_runivar.txt"))
    cmd=paste0("r.univar --quiet --overwrite map=",Extens[1,3]," output=",NomUnivar)
    system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    TempsFin=as.numeric(scan(file=NomUnivar,NomUnivar,sep=":",skip=7,nlines=1,dec=".")[2])
    unlink(NomUnivar)
    
    nomInond_Endo="Endo"
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomInond_Endo," =if(",Extens[1,2],">",Extens[3,2],",if(",Extens[1,3],">=",TempsFin,",1,null()),null())")))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    BufPlus=3*resolution
    BufMoins=2*resolution
    BufPlusMoins(nomInond_Endo,0,BufPlus,BufMoins,"",dsnDPE,-99,1,SecteurGRASS)
    
    Endo=st_read(file.path(dsnDPE,paste0(raci,"_",nomInond_Endo,".gpkg")))
    if (nrow(Endo)>0)
    {
      Endo=st_buffer(Endo,0)
      Endo$Aire=round(st_area(Endo))
      Endo$Verif=0
      nomendomanuel=file.path(dsnDPE,paste0(raci,"_",nomInond_Endo,"_Manuel.gpkg"))
      st_write(Endo,nomendomanuel, delete_layer=T, quiet=T)
    }else{
      nomendomanuel="pas endoreismes"
    }
  }
  unlink(dirname(SecteurGRASS),recursive=TRUE)
  
  
  
}