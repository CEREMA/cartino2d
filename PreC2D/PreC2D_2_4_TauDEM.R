if (exists("PreC2D_03_TauDEM")==F)
{
  chem_routine=R.home(component = "cerema")
  Numero_de_calcul=1
  options(error=browser)
  ## VARIABLES A MODIFIER --------------------------------------------------------
  chemin="D:\\Taudem_R"
  
  ## NE PAS MODIFIER ------------------------------------------------------------
  dir  = "01-DATA/01-IN"
  dir_tau_riv = "01-DATA/02-OUT"
  
  OSGeo4W_path="C://QGIS//OSGeo4W.bat"
  
  manu=0
  if (manu==1)
  {
    riv_name=cbind( "Lac","D13EnVau","D13Luynes","D13MarsStBarn","D30StAnast","D74Filliere")
    riv_name=cbind("MAMP_Huv")
    format_MNT = cbind(".tif",".tif",".tif",".tif",".tif",".tif",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt",".vrt")
    # format_MNT = cbind(".asc")
    # format_MNT=".gpkg"       
  }else{
    listechoixvrt=list.files(dir,pattern=".vrt$")
    listechoixtif=list.files(dir,pattern=".tif$")
    listechoixasc=list.files(dir,pattern=".asc")
    listechoixasc=listechoixasc[-(grep(".asc.",listechoixasc))]
    
    listechoix=as.list(rbind(data.frame(liste=listechoixtif),data.frame(liste=listechoixvrt),data.frame(liste=listechoixasc)))$liste
    
    # listechoix=list.files(dir,pattern=".vrt")
    listechoix=select.list(
      listechoix,
      title = "Choix",
      multiple = T,
      graphics = T
    )
    if (length(listechoix)==0) {BADABOOM=VOUSDEVEZCHOISIR}
    riv_name=t(as.data.frame(substr(listechoix,1,nchar(listechoix)-4)))
    format_MNT=t(as.data.frame(substr(listechoix,nchar(listechoix)-3,nchar(listechoix))))
  }
  
  
  
  # Coupure à diverses tailles pour réflechir aux parents
  
  src_thres = rbind(c(10000000,5000000, 2000000,1000000,500000), c(1,1,1,1,1))
  src_thres = rbind(c(10000000,5000000, 2000000,1000000,500000), c(0.5,0.5,0.5,0.5,0.5))
  # src_thres = rbind(c(10000000,5000000, 2000000,1000000,500000), c(5,5,5,5,5))
  # src_thres = rbind(c(10000000,5000000, 2000000,1000000,500000), c(25,25,25,25,25))
  # src_thres = rbind(c(1000000,500000), c(0.5,0.5))
  # Premiere parenthese : les valeurs de seuil a definir en m2 du + gros au + petit
  # Seconde parenthese : les resolutions MNT 
  
  nproc = 5
  
  DINF = FALSE
  # Permet de choisir entre la methode D8 et la methode D-Infinie
  # DINF = TRUE : choix de la methode D-Infinie
  # DINF = FALSE : choix de la methode D8
  
  # Seuil_Surface = 1000 #m2
  seuil1 = 1000
  # Seuil pour ne garder que les bv pas trop petits!
  
  ## Packages -------------------------------------------------------------------
  library(raster)
  # library(rgdal)
  # library(rgeos)
  library(mapview)
  library(sp)
  library(sf)
  library(units)
  # library(RSAGA)
  
  # library(stars)
  # fun_gpkg=function(nomtif){
  #   # INUTILE + lourd...
  #   # raci=file.path(chemin,substr(nomtif,1,nchar(nomtif)-4))
  #   # cmd = paste0(shQuote(paste0(dirname(OSGeo4W_path),"\\bin\\gdal_translate"))," -of GPKG ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-co  APPEND_SUBDATASET=YES ", "-co TILE_FORMAT=PNG_JPEG ",raci,".tif ",raci,".gpkg")
  #   # system(cmd)
  #   # cmd=paste0(shQuote(paste0(dirname(OSGeo4W_path),"\\bin\\gdaladdo"))," --config OGR_SQLITE_SYNCHRONOUS OFF -r AVERAGE ",shQuote(paste0(raci,".gpkg")),"  2 4 8 16 32 64 128 256")
  #   # system(cmd)
  # }
  
  nb_riv=dim(riv_name)[2]
  dirProjQgz=dir
}

# Boucle sur les zone
setwd(dir=chemin)
for (ir in 1:nb_riv)
{
  dir_tau_riv2=file.path(dir_tau_riv,riv_name[ir])
  dir.create(dir_tau_riv2)
  ## PIT---------------------------------------------------------------------
  # Suppression des cuvettes
  demfile = file.path(dir, paste0(riv_name[ir], format_MNT[ir]))
  
  file.copy(demfile,file.path(dir_tau_riv2,"MNT.vrt"),overwrite = T)
  
  file.copy(file.path(dirProjQgz, "ProjetType.qgz"),
            file.path(dir_tau_riv2,paste0(riv_name[ir],".qgz")))
  felfile = file.path(dir_tau_riv2, paste0(riv_name[ir], "_fel.tif"))
  cmd_str = paste("mpiexec -n", nproc, "PitRemove", "-z", demfile, "-fel", felfile)
  
  system(cmd_str)
  # fun_gpkg(felfile)
  
  depfile=file.path(dir_tau_riv2, "dep.tif")
  
  # cmd =  paste(shQuote(OSGeo4W_path), "gdal_calc","--calc",'"A-B"', "--format", "GTiff", "--type", "Float32", "-A", felfile, "--A_band", 1, "-B",
  #              demfile, "--B_band", 1, "--co", "COMPRESS=DEFLATE", "--co", "PREDICTOR=2", "--co" ,"ZLEVEL=9", "--co","BIGTIFF=YES","--outfile", depfile)
  # system(cmd) 
  
  # Assigne une projection
  
  cmd <- paste0(qgis_process, " run gdal:assignprojection",
                " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                " --INPUT=",shQuote(felfile),
                " --CRS=EPSG:",EPSG)
  print(cmd); system(cmd)

  # Fait la différence
  cmd <- paste0(qgis_process, " run gdal:rastercalculator",
                " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                " --INPUT_A=", shQuote(felfile),
                " --BAND_A=1",
                " --INPUT_B=", shQuote(demfile),
                " --BAND_B=1",
                " --FORMULA=A-B --EXTENT_OPT=0 --RTYPE=5 --OPTIONS= --EXTRA=",
                " --OUTPUT=", shQuote(depfile))
  print(cmd); system(cmd)
  
  # fun_gpkg(depfile)
  
  if (src_thres[1,1]!=-9999)
  {
    ## D8 ---------------------------------------------------------------------
    
    felfile = file.path(dir_tau_riv2, paste0(riv_name[ir], "_fel.tif"))
    pfile   = file.path(dir_tau_riv2, paste0(riv_name[ir], "_p.tif"))
    sd8file = file.path(dir_tau_riv2, paste0(riv_name[ir], "_sd8.tif"))
    cmd_str =
      paste("mpiexec -n", nproc, "D8Flowdir",
            "-fel", felfile, "-p", pfile, "-sd8", sd8file)
    
    system(cmd_str)
    # fun_gpkg(sd8file)
    
    
    ## AREAD8 -----------------------------------------------------------------
    ## Accumulation
    
    pfile   = file.path(dir_tau_riv2, paste0(riv_name[ir], "_p.tif"))
    ad8file = file.path(dir_tau_riv2, paste0(riv_name[ir], "_ad8.tif"))
    cmd_str =
      paste("mpiexec -n", nproc, "AreaD8",
            "-p", pfile, "-ad8", ad8file, "-nc")
    system(cmd_str)
    # fun_gpkg(ad8file)
    
    if (DINF==T)
    {    
      # ## DINF ------------------------------------------------------------------------
      angfile = file.path(dir_tau_riv2,  paste0(riv_name[ir], "_ang_Dinf.tif"))
      slpfileDinf = file.path(dir_tau_riv2,  paste0(riv_name[ir], "_slp_Dinf.tif"))
      cmd_str = 
        paste("mpiexec -n", nproc, "DinfFlowDir", 
              "-fel", felfile, "-ang", angfile, "-slp", slpfileDinf)
      system(cmd_str)
      
      # ## AREADINF --------------------------------------------------------------------
      scafileDinf = file.path(dir_tau_riv2, paste0("adinf.tif"))
      cmd_str = 
        paste("mpiexec -n", nproc, "AreaDinf", 
              "-ang", angfile, "-sca", scafileDinf, "-nc")
      system(cmd_str)
    }
    
    ## BOUCLE POUR PLUSIEURS SEUILS ----------------------------------------------- 
    for (i in 1:dim(src_thres)[2])
    {
      ## THRESHOLD --------------------------------------------------------------                                            
      # Seuil                                                                                                                   
      
      thres_value = as.character(formatC(src_thres[1,i]/src_thres[2,i]^2,digits=0,format="d"))
      # MAROC thres_valueD= as.character(formatC(src_thres[1,i]/1000000                 ,digits=0,format="d"))
      thres_valueD= as.character(formatC(src_thres[1,i]                 ,digits=0,format="d"))
      dir.create(file.path(dir_tau_riv2, thres_valueD))                                               
      
      ssafile = file.path(dir_tau_riv2, paste0(riv_name[ir], "_ad8.tif"))  
      
      srcfile = file.path(dir_tau_riv2, thres_valueD, paste0(riv_name[ir], "_src.tif"))
      cmd_str = 
        paste("mpiexec -n", nproc, "Threshold", 
              "-ssa", ssafile, "-src", srcfile, "-thresh", thres_value)                                            
      system(cmd_str)                             
      
      
      ## STREAMNET --------------------------------------------------------------
      # Creation d'un shapefile contenant le reseau hydrographique
      srcfile    = file.path(dir_tau_riv2, thres_valueD, paste0(riv_name[ir], "_src.tif"))
      ordfile    = file.path(dir_tau_riv2, thres_valueD, "ord_tau.tif")
      treefile   = file.path(dir_tau_riv2, thres_valueD, "tree_tau.dat")
      coordfile  = file.path(dir_tau_riv2, thres_valueD, "coord_tau.dat")
      netfile    = file.path(dir_tau_riv2, thres_valueD, "streamnet.shp")
      wfile      = file.path(dir_tau_riv2, thres_valueD, "watershed.tif")
      
      cmd_str = 
        paste("mpiexec -n", nproc, "StreamNet", 
              "-fel", felfile, "-p", pfile, "-ad8", ad8file, "-src", srcfile,
              "-ord", ordfile, "-tree", treefile, "-coord", coordfile,
              "-net", netfile, "-w", wfile)
      system(cmd_str)
      
      ## BOUCLE DU BUG PYTHON
      ## SUPPRIME L'ETAPE AVEC GDAL POLYGONIZE ET DU COUP L'ECRITURE DU SHAPEFILE CONTENANT LES BV
      
      ## Raster to Polygn / BV -------------------------------------------------
      nombv = file.path(getwd(), dir_tau_riv2, thres_valueD, "ws_tau.shp") 
      
      cmd = paste(shQuote(OSGeo4W_path), "gdal_polygonize",                            #nom de la couche      nom du champ
                  wfile,nombv,"-b", "1", "-f", shQuote("ESRI Shapefile"))
      system(cmd)
      
      ## Union / BV -------------------------------------------------------------
      
      bv = st_read(nombv)
      if (dim(bv)[1]>0)
      {
        if (i!=dim(src_thres)[2])
        {
          # Tous les cas
          bvd =bv
        }else{
          # récupération de tous les bords  
          #on fait un raster de 0 s'il y a le mnt
          SecteurGRASS_="C:/GRASSDATA/Taudem/Temp"
          SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
          
          unlink(dirname(SecteurGRASS),recursive=TRUE)
          system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
          system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
          
          MNTpourContourtif=file.path(getwd(),demfile)
          nomMNT_T="MNTpourContourtif"
          cmd=paste0("r.in.gdal -o --quiet --overwrite input=",MNTpourContourtif," output=",nomMNT_T)
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          #           Contourtif=file.path(getwd(),dir_tau_riv2, "contour.tif")
          # cmd =  paste(shQuote(OSGeo4W_path), "gdal_calc","--calc",'"A*0"', "--format", "GTiff", "--type", "Float32", "-A", file.path(getwd(),demfile), "--A_band", 1, "--co", "COMPRESS=DEFLATE", "--co", "PREDICTOR=2", "--co" ,"ZLEVEL=9", "--co","BIGTIFF=YES","--outfile", Contourtif)
          # system(cmd) 
          
          cmd=paste0("g.region -a --quiet --overwrite raster=",nomMNT_T)
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          nomContour="Contour"
          cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomContour," =if(",nomMNT_T," >-99,1,null())")))
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          cmd=paste0("r.to.vect --quiet --overwrite input=",nomContour," output=",nomContour," type=area")
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          nomContourGPKG1=file.path(getwd(),dir_tau_riv2, "contour1.gpkg")
          cmd=paste0("v.out.ogr --quiet --overwrite input=",nomContour," output=",nomContourGPKG1)
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          unlink(dirname(SecteurGRASS),recursive=TRUE)
          
          # nomContour=file.path(dir_tau_riv2, "contour.shp")
          # cmd = paste(shQuote(OSGeo4W_path), "gdal_polygonize",                            #nom de la couche      nom du champ
          #             Contourtif,nomContour,"-b", "1", "-f", shQuote("ESRI Shapefile"))
          # system(cmd)
          
          nomContourGPKG2=file.path(getwd(),dir_tau_riv2, "contour2.gpkg")
          # Enlever les petits trous
          cmd <- paste0(qgis_process, " run native:deleteholes",
                        " --INPUT=", shQuote(nomContourGPKG1),
                        " --MIN_AREA=", 2000000,
                        " --OUTPUT=", shQuote(nomContourGPKG2))
          print(cmd); system(cmd)
          
          contour = st_read(nomContourGPKG2)
          st_crs(contour)=st_crs(bv)
          # garde les bords non couverts
          bords=st_cast(st_cast(st_difference(st_buffer(st_union(contour),-1.5*ResoMNTpourTaudem),st_union(bv)),"MULTIPOLYGON"),"POLYGON")
          # BUGAMODIFIER=voirnabil
          # bords2=st_cast(bords[4,],"POLYGON")
          st_write(bords, file.path(dir_tau_riv2, thres_valueD), "Bords",
                   driver="ESRI Shapefile", delete_layer=T)
          bords=st_read(file.path(dir_tau_riv2, thres_valueD))
          # fiusoionner la derniere couche et les bords
          colnames(bords)[1]="DN"
          bvd =rbind(bv,bords)
          st_crs(bvd)=st_crs(bv)
        }
        
        bvd = cbind(bvd, "BV_ID"= 0)
        bvd$BV_ID = (1:dim(bvd)[1])
        bvd = bvd[,-1]
        
        ## Calcul de l'aire du BV en m2 puis en km2
        bvd$AREA_m2 = st_area(bvd)
        
        units(seuil1)<- "m^2"
        units(bvd$AREA_m2) <- "m^2"
        bvd=bvd[which(bvd$AREA_m2>seuil1),]
        
        bvd$AREA_km2 = round(bvd$AREA_m2/1000000,3)
        units(bvd$AREA_km2) <- "km^2"
        
        st_write(bvd, file.path(dir_tau_riv2, thres_valueD), "watershed",
                 driver="ESRI Shapefile", delete_layer=T)
        #GRAVE couper les bassins les plus petits avec le réseau
      }
      ## Suppression des fichiers temporaires ----------------------------------
      file.remove(file.path(getwd(), dir_tau_riv2, thres_valueD, "ws_tau.shp"))
      file.remove(file.path(getwd(), dir_tau_riv2, thres_valueD, "ws_tau.dbf"))
      file.remove(file.path(getwd(), dir_tau_riv2, thres_valueD, "ws_tau.prj"))
      file.remove(file.path(getwd(), dir_tau_riv2, thres_valueD, "ws_tau.shx"))
      
    }
    file.rename(ad8file,file.path(dir_tau_riv2, paste0("ad8.tif"))) 
    
  }
}
repPetBv=file.path(dir_tau_riv2, thres_valueD)
# print("Si ca a bien marché pour CARTINO2D,")
# print("cela peut être intéressant de séparer les bassin en rive droite et gauche du réseau calculé")
# print("Qgis")
# print("Boite à outil")
# print("SAGA")
# print("Vector polygon tools")
# print("Polygon-line intersection")
# print("A vous de choisir quels Bv vous coupez avec quel réseau, sinon ce serait fait dans la routine!")

cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("###################################################################################\n")
cat("######################### C2D A LIRE SVP ###########################################\n")
cat("------------------------- Etape PreC2D 2_4-------------------------------------------\n")
cat("Un dossier pour le calcul de bassin versant a été créé:\n")
cat("Un projet Qgis spécifique uniquement à cette partie est disponible dans ce repertoire.\n")
cat(getwd(),"\n")
cat(file.path(dir_tau_riv2,paste0(riv_name[ir],".qgz")),"\n")
cat("Ce projet est totalement inclus dans le répertoire général de l'étape PreC2D_2.\n")
cat("Vous pouvez vérifier les calculs\n")
cat("######################### Fin C2D A LIRE ###########################################\n")

setwd(chem_routine)