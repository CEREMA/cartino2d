# Nabil, si j'ai bien compris, tout est dans une autre fonction, pq?

#################################################################################################
################   Etape5_Telemac
#################################################################################################

#######################################################################
###################### Préparation Cluster ############################
#######################################################################

######################## Détection des fichiers .res ###########################
detect_res_files <- function(dir_from){
  hyeto_files <- grep(pattern = "(?=.*Sce)(?=.*cas$)|(?=.*Hist$)(?=.*cas$)|(?=.*Evt$)(?=.*cas$)", 
                      x = list.files(path = dir_from,full.names = TRUE), value = TRUE, perl = TRUE)
  spatial_files <- grep(pattern = "(?=.*Evts)(?=.*cas$)", 
                        x = list.files(path = dir_from,full.names = TRUE), value = TRUE, perl = TRUE)
  shys_files <- grep(pattern = ".*SHYS.cas$|.*spPN.cas$|.*spPB.cas$", 
                     x = list.files(path = dir_from,full.names = TRUE), value = TRUE, perl = TRUE)

  cas_files <- c(hyeto_files,shys_files, spatial_files)
  
  res_files=c()
  if (length(hyeto_files!=0)){
    res_hyeto_files <- paste0("hyeto_",tools::file_path_sans_ext(basename(hyeto_files)),".res")
    res_files=append(res_files, res_hyeto_files)
  }
  if (length(spatial_files!=0)){
    res_spatial_files <- paste0("spatial_",substr(tools::file_path_sans_ext(basename(spatial_files)),1,nchar(tools::file_path_sans_ext(basename(spatial_files))) - 2),".res")
    res_files=append(res_files, res_spatial_files)
  }
  if (length(shys_files!=0)){
    rac=basename(shys_files)
    rac=substr(rac,nchar(rac)-7,nchar(rac)-4)
    # OLD res_shys_files <- paste0("shyreg_spat_",substr(tools::file_path_sans_ext(basename(shys_files)),1,nchar(tools::file_path_sans_ext(basename(shys_files))) - 5),".res")    
    res_shys_files <- paste0("shyreg_",rac,"_",substr(tools::file_path_sans_ext(basename(shys_files)),1,nchar(tools::file_path_sans_ext(basename(shys_files))) - 5),".res")
    res_files=append(res_files, res_shys_files)
  }
  # res_files <- c(res_hyeto_files,res_shys_files, res_spatial_files)
  res_files <- file.path(dsnlayerC,res_files)
  res_cas_files <- list(res_files, cas_files)
  
  return(res_cas_files)
}
######################## Ecriture du fichier launch ############################
ecriture_fichier_sh <- function(casfiles, dto_, dirs){
  
  fun_doss <- function(x){
    dto_[x] <- file.path(dirs,paste0(nomcontour,"_",as.character(x),".sh"))
  }
  dto_ <- sapply(X =1:length(casfiles),FUN = fun_doss )
  file.create(dto_)
  
  dto <- lapply(X = 1:length(dto_) , FUN = function(X){file(dto_[X],"wb")})
  
  nbnodes = as.numeric(scan(
    file.path(dsnlayerC, paste0(nom_maillage,"_MNT.grd")),
    skip = 1,
    nlines = 1
  ))[2]
  
  # tests FP MAMP
  # DecoupCoeurs=50000#25000
  NCoeurs=64#48
  nbcores <- floor(nbnodes/DecoupCoeurs)
  ncnode_tgcc <- max(1,floor(nbcores/NCoeurs))
  nbcores <- NCoeurs*ncnode_tgcc
  nbcores=512
  ncnode_tgcc=4
  cat("FP ",nbcores,"\n")
  
  fun_write1 <- function(X){
    # write("     ",file=dto[[X]], append=F)
    
    write("#!/bin/bash",file=dto[[X]], append=F)
    write(paste0("#MSUB -q rome"),file=dto[[X]], append=T)
    # write(paste0("#MSUB -Q long"),file=dto[[X]], append=T)
    write(paste0("#MSUB -T " ,as.character(walltime_tgcc*3600)),file=dto[[X]], append=T)
    write(paste0("#MSUB -n ",as.character(nbcores)),file=dto[[X]], append=T)
    write(paste0("#MSUB -N ",ncnode_tgcc),file=dto[[X]], append=T)
    write(paste0("#MSUB -A ",project_tgcc),file=dto[[X]], append=T)
    write('export ROOT="/ccc/work/cont003/gen14287/gen14287/telemac-mascaret-muffins"',file=dto[[X]], append=T)
    write("source ${ROOT}/configs/tgcc-gnu-ompi.sh",file=dto[[X]], append=T)
    write(paste0("cd /ccc/scratch/cont003/gen14287/",dir_cluster_tgcc,'/',contour$NOMPOST,'/',nomcontour),file=dto[[X]], append=T)
    
    # cat(tryo,file=dto[[X]], append=TRUE, sep = "");
    
    write("\n",file=dto[[X]],append = TRUE)
    
  }
  lapply(X = 1:length(dto_) , FUN = fun_write1)
  
  fun_write2 <- function(X){
    # write(paste0("cd ", contour$NOM),append = TRUE,file = dto[[X]])
    # Modif suite mail TGCC
    write(paste0("telemac2d.py ",casfiles[X] ," -s --ncsize=",as.character(nbcores)),
          # write(paste0("ccc_mprun telemac2d.py ",casfiles[X] ," -s --ncsize=",as.character(nbcores)),
          file = dto[[X]],append = TRUE)
    # write(paste0("cat ",casfiles[x],".out"),
    #       file = dto[[X]],append = TRUE)
    # write("cd ..     ",file=dto[[X]], append=T)
    write("     ",file=dto[[X]], append=T)
    # write(paste0("cat ",casfiles[x],".out"),file=dto[[X]], append=T)
    write("     ",file=dto[[X]], append=T)
  }
  lapply(X = 1:length(dto_) , FUN = fun_write2)
  dtoto <- list(dto, dto_)
  
  return(dtoto)
}

ecriture_fichier_slurm <- function(casfiles, dto_, dirs){
  
  fun_doss <- function(x){
    dto_[x] <- file.path(dirs,paste0(nomcontour,"_",as.character(x),".slurm"))
  }
  dto_ <- sapply(X =1:length(casfiles),FUN = fun_doss )
  file.create(dto_)
  
  dto <- lapply(X = 1:length(dto_) , FUN = function(X){file(dto_[X],"wb")})
  
  nbnodes = as.numeric(scan(
    file.path(dsnlayerC, paste0(nom_maillage,"_MNT.grd")),
    skip = 1,
    nlines = 1
  ))[2]
  
  
  fun_write1 <- function(X){
    # write("     ",file=dto[[X]], append=F)
    
    write("#!/bin/bash",file=dto[[X]], append=F)
    write(paste0("#SBATCH -N 1"),file=dto[[X]], append=T)
    write(paste0("#SBATCH -n 48"),file=dto[[X]], append=T)
    write(paste0("#SBATCH -p prod"),file=dto[[X]], append=T)
    write('export ROOT="/home/dmed/telemac"',file=dto[[X]], append=T)
    write("source ${ROOT}/configs/margny.ompi.sh",file=dto[[X]], append=T)
    write(paste0("cd /home/",dir_cluster_margny,'/',contour$NOMPOST,'/',nomcontour),file=dto[[X]], append=T)
    
    # cat(tryo,file=dto[[X]], append=TRUE, sep = "");
    
    write("\n",file=dto[[X]],append = TRUE)
    
  }
  lapply(X = 1:length(dto_) , FUN = fun_write1)
  
  fun_write2 <- function(X){
    # write(paste0("cd ", contour$NOM),append = TRUE,file = dto[[X]])
    # Modif suite mail TGCC
    write(paste0("telemac2d.py ",casfiles[X] ," -s --ncsize=48"),
          # write(paste0("ccc_mprun telemac2d.py ",casfiles[X] ," -s --ncsize=",as.character(nbcores)),
          file = dto[[X]],append = TRUE)
    # write(paste0("cat ",casfiles[x],".out"),
    #       file = dto[[X]],append = TRUE)
    # write("cd ..     ",file=dto[[X]], append=T)
    write("     ",file=dto[[X]], append=T)
    # write(paste0("cat ",casfiles[x],".out"),file=dto[[X]], append=T)
    write("     ",file=dto[[X]], append=T)
  }
  lapply(X = 1:length(dto_) , FUN = fun_write2)
  dtoto <- list(dto, dto_)
  
  return(dtoto)
}
ecriture_fichier_pbs <- function(casfiles, dto_, dirs){
  
  fun_doss <- function(x){
    dto_[x] <- file.path(dirs,paste0(nomcontour,"_",as.character(x),".pbs"))
  }
  dto_ <- sapply(X =1:length(casfiles),FUN = fun_doss )
  file.create(dto_)
  dto <- lapply(X = 1:length(dto_) , FUN = function(X){file(dto_[X],"wb")})
  
  tryo <- rbind('export HOSTSLIST=`cat $PBS_NODEFILE | uniq | paste -d "','\\','n" - -`')
  fun_write1 <- function(X){
    write("#!/bin/bash",file=dto[[X]], append=F)
    write(paste0("#PBS -q mpi_",as.character(q_ifremer)),file=dto[[X]], append=T)
    write(paste0("#PBS -l walltime=",walltime_ifremer),file=dto[[X]], append=T)
    write(paste0("#PBS -l mem=",as.character(60*q_ifremer),"gb"),file=dto[[X]], append=T)
    write(paste0("#PBS -l ncpus=",as.character(28*q_ifremer)),file=dto[[X]], append=T)
    write('export ROOT="/appli/telemac-mascaret/v8p2r1"',file=dto[[X]], append=T)
    write(". ${ROOT}/configs/datarmor-intel-impi-mkl.sh",file=dto[[X]], append=T)
    write("cd $PBS_O_WORKDIR",file=dto[[X]], append=T)
    write(paste0("cd ${SCRATCH}/eftp/",contour$NOMPOST,"/",nomcontour),file=dto[[X]], append=T)
    
    cat(tryo,file=dto[[X]], append=TRUE, sep = "");
    
    write("\n",file=dto[[X]],append = TRUE)
    write('echo $HOSTSLIST',file=dto[[X]], append=T)
    
  }
  
  lapply(X = 1:length(dto_) , FUN = fun_write1) 
  fun_write2 <- function(X){
    write(paste0("telemac2d.py ",casfiles[X] ," -s --ncsize=",as.character(28*q_ifremer), " --nctile=28 >& ",
                 casfiles[X],".out ","\n", "cat ",casfiles[X],".out"),
          file = dto[[X]],append = TRUE)
    write("     ",file=dto[[X]], append=T)
    write("     ",file=dto[[X]], append=T)
  }
  lapply(X = 1:length(dto_) , FUN = fun_write2)
  dtoto <- list(dto, dto_)
  
  return(dtoto)
}
ecriture_fichier_launch <- function(casfiles, dto_, dirs){
  
  fun_doss <- function(x){
    # dto_[x] <- file.path(dirs,paste0("launchtelemac_",as.character(ic),"_",as.character(x)))
    dto_[x] <- file.path(dirs,paste0(nomcontour,"_launch","_",as.character(x)))
  }
  dto_ <- sapply(X =1:length(casfiles),FUN = fun_doss )
  file.create(dto_)
  dto <- lapply(X = 1:length(dto_) , FUN = function(X){file(dto_[X],"wb")})
  
  fun_write1 <- function(X){
    write("     ",file=dto[[X]], append=F)
    write("!/bin/bash",file=dto[[X]], append=F)
    if(nchar(dir_cluster)>0){
      write("     ",file=dto[[X]], append=T)
      write(paste0("cd ",dir_cluster ),file=dto[[X]], append=T)
      write("     ",file=dto[[X]], append=T)
    }
  }
  lapply(X = 1:length(dto_) , FUN = fun_write1) 
  fun_write2 <- function(X){
    write(paste0("cd ", contour$NOM),append = TRUE,file = dto[[X]])
    write(paste0("runcode.py telemac2d ",casfiles[X] ," -s --ncsize=48 --nctile=48 --ncnode=1"),file = dto[[X]],append = TRUE)
    write("cd ..     ",file=dto[[X]], append=T)
    write("     ",file=dto[[X]], append=T)
  }
  lapply(X = 1:length(dto_) , FUN = fun_write2) 
  dtom_ <- rep("",length(casfiles))
  ## créer le fichier txt d'execution
  fun_doss <- function(x){
    dtom_[x] <- file.path(dirs,paste0(nomcontour,"_merge","_",as.character(x)))
  }
  dtom_ <- sapply(X =1:length(casfiles),FUN = fun_doss )
  file.create(dtom_)
  dtom <- lapply(X = 1:length(dtom_) , FUN = function(X){file(dtom_[X],"wb")})
  
  fun_write1 <- function(X){
    write("     ",file=dtom[[X]], append=F)
    write("!/bin/bash",file=dtom[[X]], append=F)
    if(nchar(dir_cluster)>0){
      write("     ",file=dtom[[X]], append=T)
      write(paste0("cd ",dir_cluster ),file=dtom[[X]], append=T)
      write("     ",file=dtom[[X]], append=T)
    }
  }
  lapply(X = 1:length(dtom_) , FUN = fun_write1)
  fun_write2 <- function(X){
    write(paste0("cd ", contour$NOM),append = TRUE,file = dtom[[X]])
    write(paste0("tmp=$(ls -td ",casfiles[X] ,"*/ " ,"| head -n1)"),append = TRUE,file = dtom[[X]])
    write(paste0("runcode.py telemac2d ",casfiles[X] ," --ncsize=48 --nctile=48 --ncnode=1 --merge -w $tmp"),file = dtom[[X]],append = TRUE)
    write("cd ..     ",file=dtom[[X]], append=T)
    write("     ",file=dtom[[X]], append=T)
  }
  lapply(X = 1:length(dtom_) , FUN = fun_write2)
  
  dtoto_tom <- list(dto, dto_, dtom)
  
  return(dtoto_tom)
}
########################## Préparation du cluster ##############################
preparation_cluster <- function(){
  
  ## créer un dossier pour le cluster dans dsnclayer
  if(dir.exists(file.path(dsnlayer,contour$NOMPOST,"cluster_copy",contour$NOMPOST))){unlink(file.path(dsnlayer,contour$NOMPOST,"cluster_copy"),recursive = TRUE)}
  dir.create(file.path(dsnlayer,contour$NOMPOST,"cluster_copy"))
  dir.create(file.path(dsnlayer,contour$NOMPOST,"cluster_copy",contour$NOMPOST))
  dir_copy <- file.path(dsnlayer,contour$NOMPOST,"cluster_copy",contour$NOMPOST)
  dir_copy_ <- file.path(dsnlayer,contour$NOMPOST,"cluster_copy")
  dirs <- file.path(dir_copy_,contour$NOMPOST,contour$NOM)
  if(dir.exists(dirs)){unlink(dirs,recursive = TRUE)}
  sapply(X =1:length(dirs),FUN = function(x){dir.create(dirs[x])} )
  
  ## créer le fichier txt d'execution
  print(contour$NOM)
  dir_copy_case <- paste0(dir_copy,'\\',contour$NOM)
  dir_from <- file.path(dsnlayerC)
  
  res_cas_files <- detect_res_files(dir_from)
  cas_files <- res_cas_files[[2]]
  dto_ <- rep("",length(cas_files))
  res_files <- res_cas_files[[1]]
  res_exists_ <- file.exists(res_files)
  res_files_=res_files[res_exists_]
  indices_a_supprimer <- c()
  
  # On extrait les noms de fichiers du deuxième vecteur une seule fois avant la boucle.
  noms_fichiers_vec2 <- basename(cas_files)
  
  # On parcourt chaque fichier existant du premier vecteur.
  for (chemin1 in res_files_) {
    
    # On extrait le nom du fichier courant du premier vecteur.
    nom_fichier1 <- basename(chemin1)
    
    # `adist` calcule la distance de Levenshtein entre le nom du fichier de vec1
    # et chaque nom de fichier de vec2 pour une comparaison ciblée.
    distances <- adist(nom_fichier1, noms_fichiers_vec2)
    
    # On trouve la distance minimale.
    min_distance <- min(distances)
    
    # `which()` nous donne TOUS les indices qui correspondent à la distance minimale.
    indices_plus_similaires <- which(distances == min_distance)
    
    # On ajoute ces indices à notre liste d'indices à supprimer.
    indices_a_supprimer <- c(indices_a_supprimer, indices_plus_similaires)
  }
  if(!is.null(indices_a_supprimer)){
    
    cas_files <- cas_files[-indices_a_supprimer]
    casfiles <- basename(cas_files)
    
    
  }else{
    casfiles <- basename(cas_files)
  }

  ##############################################################################
  ##################### Ecriture du fichier launch (pbs, sh) ###################
  ##############################################################################
  
  if(ETPCALCUL[6]==1){
    dtoto <- ecriture_fichier_pbs(casfiles, dto_, dirs)
    dto <- dtoto[[1]]
    dto_ <- dtoto[[2]]
  }else if (ETPCALCUL[8]==1){
    dtoto <- ecriture_fichier_sh(casfiles, dto_, dirs)
    dto <- dtoto[[1]]
    dto_ <- dtoto[[2]]
  }else if (ETPCALCUL[3]==1){
    dtoto<- ecriture_fichier_slurm(casfiles, dto_, dirs)
    dto <- dtoto[[1]]
    dto_ <- dtoto[[2]]
  }
  
  ##############################################################################
  
  file.copy(from = cas_files,to = dir_copy_case,overwrite = TRUE,recursive = TRUE )
  cli_files <- list.files(dir_from,pattern = "\\Cartino2D.cli$",full.names = TRUE)
  file.copy(from = cli_files,to = dir_copy_case,overwrite = TRUE,recursive = TRUE )
  slf_files <- list.files(dir_from,pattern = "\\Cartino2D.slf$",full.names = TRUE)
  file.copy(from = slf_files,to = dir_copy_case,overwrite = TRUE,recursive = TRUE)

  # Modif FP multidébit
  listeliq=list.files(dir_from,pattern=".liq",full.names = TRUE)
  if (length(listeliq)>0){
    file.copy(from = listeliq,to = dir_copy_case,overwrite = TRUE,recursive = TRUE)
  }
  
  if(file.exists(file.path(dir_from,"OuvHydrau_Select.shp"))){
    source_files <- list.files(dir_from,pattern = ".txt",full.names = TRUE)
    fichiers_trouves <- grep("OuvHydrau_Select", source_files, value = TRUE)
    file.copy(from = fichiers_trouves,to = dir_copy_case,overwrite = TRUE,recursive = TRUE)
  }
  if(file.exists(file.path(dir_from,"SectControl_Select.txt"))){
    source_files <- list.files(dir_from,pattern = "SectControl_Select.txt",full.names = TRUE)
    file.copy(from = source_files,to = dir_copy_case,overwrite = TRUE,recursive = TRUE)
    if(ETPCALCUL[8]==1 | ETPCALCUL[3]==1){
      source_files <- list.files(dir_from,pattern = "SC.txt",full.names = TRUE)
      file.copy(from = source_files,to = dir_copy_case,overwrite = TRUE,recursive = TRUE)
    }
  }
  
  pluie_files <- paste0(tools::file_path_sans_ext(res_files[!res_exists_]),".txt")
  file.copy(from = pluie_files, to = dir_copy_case, overwrite = TRUE, recursive = TRUE)
  
  dir.create(file.path(dir_copy_case,"USER_FORTRAN_PH"))
  file.copy(file.path(dir_from,"USER_FORTRAN_PH"),to =dir_copy_case,recursive = TRUE, overwrite = TRUE  )
  dir.create(file.path(dir_copy_case,"USER_FORTRAN_PS"))
  file.copy(file.path(dir_from,"USER_FORTRAN_PS"),to =dir_copy_case,recursive = TRUE, overwrite = TRUE  )
  
  if(ETPCALCUL[6]==1){
    file.copy(file.path(chem_routine,"C2D",Doss_Base,"V8P2","nomvar_telemac2d.f"),to =file.path(dir_copy_case,"USER_FORTRAN_PS"), overwrite = TRUE)
    file.copy(file.path(chem_routine,"C2D",Doss_Base,"V8P2","nomvar_telemac2d.f"),to =file.path(dir_copy_case,"USER_FORTRAN_PH"), overwrite = TRUE)
  }
  if(ETPCALCUL[8]==1){
    file.copy(file.path(chem_routine,"C2D",Doss_Base,"TGCC","nomvar_telemac2d.f"),to =file.path(dir_copy_case,"USER_FORTRAN_PS"), overwrite = TRUE)
    file.copy(file.path(chem_routine,"C2D",Doss_Base,"TGCC","nomvar_telemac2d.f"),to =file.path(dir_copy_case,"USER_FORTRAN_PH"), overwrite = TRUE)
  }
  if(ETPCALCUL[3]==1){
    file.copy(file.path(chem_routine,"C2D",Doss_Base,"USER_FORTRAN_PS","nomvar_telemac2d.f"),to =file.path(dir_copy_case,"USER_FORTRAN_PS"), overwrite = TRUE)
    file.copy(file.path(chem_routine,"C2D",Doss_Base,"USER_FORTRAN_PH","nomvar_telemac2d.f"),to =file.path(dir_copy_case,"USER_FORTRAN_PH"), overwrite = TRUE)
  }
  
  ## aller dans le dossier cluster, 

    lapply(X=1:length(dto_), FUN = function(X){close(dto[[X]])}) 

  
}

#######################################################################
################### Centre de calcul DTecRem ##########################
#######################################################################

################### copie des fichiers sur cluster et lancement du calcul 
launch_cluster_margny <- function(){
  
  dir_from <- file.path(dsnlayerC)
  
  res_cas_files <- detect_res_files(dir_from)
  res_files <- res_cas_files[[1]]
  res_exists_ <- file.exists(res_files)
  cas_files <- res_cas_files[[2]]
  cas_files <- cas_files[!res_exists_]
  casfiles <- basename(cas_files)
  
  if (length(casfiles)>0)
  {
    
    ## zipper et envoyer sur le ftp en extranet
    session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
    ssh::ssh_exec_wait(session, command = c(paste0("cd /home/",dir_cluster_margny),paste0("mkdir ",contour$NOMPOST)))
    dir_copy <- paste0(dsnlayer,"\\",contour$NOMPOST,"\\cluster_copy\\",contour$NOMPOST)
    # dir_copy_ <- paste0(dsnlayer,"\\",contour$NOMPOST,"\\cluster_copy\\",contour$NOMPOST)
    tozip <- contour$NOM
    zipname <- paste0(contour$NOM,".zip")
    zipfullname <- paste0(dir_copy,"\\",zipname)
    setwd(dir_copy)
    if(file.exists(zipname)){file.remove(zipname)}
    zip(zipfile = zipname, files = tozip)
    setwd(dsnlayer)
    session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
    dir_cluster2 <- file.path(paste0("cd /home/",dir_cluster_margny,'/',contour$NOMPOST,'/',contour$NOM))
    ssh::ssh_exec_wait(session, command = c(paste0("cd /home/",dir_cluster_margny,'/',contour$NOMPOST)
                                            ,paste0("rm ", zipname)))
    session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
    ssh::ssh_exec_wait(session, command = c(paste0("cd /home/",dir_cluster_margny,'/',contour$NOMPOST)
                                            ,paste0("rm -r ", contour$NOM)))
    # ftp_txt_file <- file.path(dsnlayerC,"ftpupload.txt")
    zipnamecluster <- paste0("/home/",dir_cluster_margny,'/',contour$NOMPOST,'/',nomcontour,".zip")
    

    session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
    scp_upload(session, zipfullname,to = paste0("/home/",dir_cluster_margny,'/',contour$NOMPOST))
  
    session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
    out <- ssh_exec_wait(session, command = paste0('qstat -u ',dir_cluster_margny),std_out = stdout(),std_err = stderr())
    out <- ssh_exec_wait(session,'sacct --format=JobID,JobName,NodeList,State,Start,End -S $(date -d "yesterday" +%Y-%m-%d)',std_out = stdout(),std_err = stderr())
    
    # out <- ssh_exec_wait(session, command = 'ls -l')
    dir_cluster2 <- file.path(paste0("/home/",dir_cluster_margny,'/',contour$NOMPOST),contour$NOM)
    session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
    ssh::ssh_exec_wait(session, command = c(paste0("cd /home/",dir_cluster_margny,'/',contour$NOMPOST)
                                            ,paste0("unzip ", zipname)))
    # ssh::ssh_exec_wait(session, command = c("cd /home3/scratch/nhocini/eftp","ls"))
    # ssh::scp_upload(session, file_path,to = dir_cluster)
    
    
    for (i in 1:length(casfiles)) {
      session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
      out <- ssh_exec_wait(session, command = c(paste0('cd ',dir_cluster2),paste0('chmod u+x ',nomcontour,"_",as.character(i),".slurm")))
    }
    # if(!exists('list_jobs')){
    #   list_jobs <<- vector("list", length(nsecteur))
    # }
    jobids <- c() 
    for (i in 1:length(casfiles)) {
      session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
      jobid <- capture.output(str(ssh_exec_wait(session, 
                                                command = c(paste0("cd ",dir_cluster2)
                                                            ,paste0('sbatch ' ,'./',nomcontour,
                                                                    "_",as.character(i),'.slurm')))))[1]  
      
      jobids[i] <- as.character(as.numeric(gsub("\\D", "", jobid)))
      
    }
    jobs_txt_file <- file.path(dsnlayerC,paste0(nomcontour,"_margy_jobs.txt"))
    if(file.exists(jobs_txt_file)){
      file.remove(jobs_txt_file)
      
    }
    if(file.exists(jobs_txt_file)){
      write(jobids,file=jobs_txt_file, append=T)
      
    }else{
      write.table(jobids,
                  file = file.path(dsnlayerC,paste0(nomcontour,"_margy_jobs.txt")),
                  row.names = FALSE,col.names = FALSE)
    }
  }
  
  # list_jobs[[ic]] <- jobids
  
  # which(sapply(list_jobs,Negate(is.null)))
}


checksortie_margny <- function(sct){
  # browser()
  # Nabil à vérifier
  contour_ <- contours[nsecteur[sct],]
  if (file.exists(file.path(dsnlayer,contour_$NOMPOST,contour_$NOM,paste0(contour_$NOM,"_margy_jobs.txt")))==F)
  {
    cat(contour_$NOM,"- Pas de job lancé sur margny")
    checkvector <- c(-1,-1)
  }else{
    print(contour_$NOM)
    
    session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
    dir_to_down <- file.path(dsnlayer,contour_$NOMPOST,contour_$NOM)
    dir_from_down <- file.path(paste0("/home/",dir_cluster_margny),contour_$NOMPOST,contour_$NOM)
    
    res_cas_files <- detect_res_files(dir_to_down)
    cas_files <- res_cas_files[[2]]
    verif_res <- res_cas_files[[1]]
    verif_res_ <- file.exists(verif_res)
    cas_files <- basename(cas_files[!verif_res_])
    
    checkstrings <- c()
    checkjobs <- c()
    for (i in 1:(length(cas_files))) {
      
      session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
      sortie_check <- capture.output(str( ssh_exec_wait(
        session,
        command = c(paste0("cd ", file.path(dir_from_down)),
                    paste0("find . -type f -name ",r"("*.sortie")"," -a -name ",shQuote(paste0("*",cas_files[i],"*"),type = "sh")," | tail -1")
        )
      )
      )
      )[1]
      session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
      checkstring <- suppressWarnings(capture.output(str(ssh_exec_wait(
        session,
        command = c(paste0("cd ", file.path(dir_from_down)),
                    paste0("grep -q ",shQuote("CORRECT END OF RUN",type = "sh")," ",shQuote(paste0("",substring(sortie_check,3),""),type = "sh"))
        )
      )
      )
      )[1])
      
      if (file.exists(file.path(dsnlayer,contour_$NOMPOST,contour_$NOM,paste0(contour_$NOM,"_margy_jobs.txt")))==F){next}
      
      jobid <-read.table(file.path(dsnlayer,contour_$NOMPOST,contour_$NOM,paste0(contour_$NOM,"_margy_jobs.txt")))[i,1]
      # jobid <- list_jobs[[sct]][i]
      session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
      checkjob_ <- suppressWarnings(capture.output(str(ssh_exec_wait(
        session,
        command = paste0('sacct -j ',jobid)
        
      )
      )
      )[3])
      
      checkjob1 <- grepl('COMPLETED',checkjob_)
      checkjob2 <- grepl('RUNNING',checkjob_)
      checkjob3 <- grepl('PENDING',checkjob_)
      if(isTRUE(checkjob1) | isTRUE(checkjob2) | isTRUE(checkjob3)){
        checkjob <- " int 0"
      }else{
        
        checkjob <- " int 1"
      }
      checkjobs <- cbind(checkjobs,checkjob)
      checkstrings <- cbind(checkstrings,checkstring)
      
      
    }
    calculspastermines <- length(which(checkstrings!=" int 0"))
    cat(contour_$NOM,paste0(calculspastermines," Calculs ne sont pas terminés, ou se sont arrétés, ou avaient déjà des résultats (fichier res)\n"))
    # if(length(which((checkjobs)==" int 0"))==length(cas_files)){
    if(any((checkjobs)==" int 0")){
      checkjob_ <- " int 0"
    }else{
      checkjob_ <- " int 1"
    }
    # if(length(which((checkstrings)==" int 0"))==length(cas_files)){
    if(any((checkstrings)==" int 0")){
      checkstring_ <- " int 0"
    }else{
      checkstring_ <- " int 1"
    }
    checkvector <- c(checkjob_,checkstring_)
    return(checkvector)
  }
}

testfincalcul_margny <- function(nsecteur,fincalcul,nbfinalcul){
  for (sct in 1:length(nsecteur)) {
    if(contours$STEP_PRE[nsecteur[sct]]!=0){next}    
    
    checkvector <- checksortie_margny(sct)  
    checkjob <- checkvector[1]
    checkstring <- checkvector[2]    
    
    
    
    if(checkstring==" int 0"){
      fincalcul[sct] <- fincalcul[sct]+1
      nbfinalcul <- nbfinalcul-1
      cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
      break()
    }
    
    if(checkjob!=" int 0"){
      cat("\n ******************************************************* \n
                        Le calcul tourne encore \n 
            ******************************************************* \n"
      )
      cat("\n ******************************************************* \n
            30 secondes avant la prochaine vérification \n
             ******************************************************* \n")
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
    }
    if(checkjob==" int 0" & checkstring==" int 0"){
      
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
      
      checkvector <- checksortie_margny(sct)  
      checkjob <- checkvector[1]
      checkstring <- checkvector[2]
      if(checkstring==" int 0"){
        fincalcul[sct] <- fincalcul[sct]+1
        nbfinalcul <- nbfinalcul-1
        cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
      }else{        
        
        fincalcul[sct] <- fincalcul[sct]+2
        nbfinalcul <- nbfinalcul-1
        
        cat("\n ******************************************************************************* \n
                  Le calcul s'est arrêté, vérifiez les fichiers d'entrée et/ou le pas de temps \n 
              \n ******************************************************************************** \n")
      }
    }
    if(checkjob==" int 0" & checkstring==" int 0"){
      
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
      
      checkvector <- checksortie_margny(sct)  
      checkjob <- checkvector[1]
      checkstring <- checkvector[2]
      if(checkstring==" int 0"){
        fincalcul[sct] <- fincalcul[sct]+1
        nbfinalcul <- nbfinalcul-1
        cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
      }else{        
        
        fincalcul[sct] <- fincalcul[sct]+2
        nbfinalcul <- nbfinalcul-1
        
        cat("\n ******************************************************************************* \n
                  Le calcul s'est arrêté, vérifiez les fichiers d'entrée et/ou le pas de temps \n 
              \n ******************************************************************************** \n")
      }
    }
  } 
  testcalcullist <- list(fincalcul,nbfinalcul)
  return(testcalcullist)
  
}

download_results_margny <- function(){
  # require(ssh)
  session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
  print(contour$NOM)
  dir_to_down <- file.path(dsnlayer,contour$NOMPOST,contour$NOM)
  dir_from_down <- file.path(paste0("/home/",dir_cluster_margny,"/",contour$NOMPOST),contour$NOM)
  
  res_cas_files <- detect_res_files(dir_to_down)
  cas_files <- res_cas_files[[2]]
  res_files <- res_cas_files[[1]]
  verif_res <- file.exists(res_files)
  res_files <- basename(res_files[!verif_res])
  cas_files <- basename(cas_files[!verif_res])
  
  # ajout fp 20240726
  if (length(cas_files)>0)
  {
    sortie_files <- tools::file_path_sans_ext(cas_files)
    for (ids in 1:length(cas_files)) {
      resf <- sortie_files[ids]
      
      if(substr(resf,(nchar(resf)-1),nchar(resf))=="_s"){
        sortie_files[ids] <- substr(resf,1,(nchar(resf)-2))
      } else if (substr(resf,(nchar(resf)-4),nchar(resf))=="_SHYS") {
        sortie_files[ids] <- substr(resf,1,(nchar(resf)-5))
      }else{
        sortie_files[ids] <- substr(resf,1,(nchar(resf)-7))
      }
      
      session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
      res_telemac <- capture.output(str(ssh_exec_wait(
        session,
        command = c(paste0("cd ", dir_from_down),
                    paste0(" find . -type f -name ",r"('*.res')"," -a -name ",shQuote(paste0("*",res_files[ids],"*"),type = "sh")," | head -n1")
                    
        )
      )
      )
      )[1]
      
      session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
      foldersortie <- capture.output(str( ssh_exec_wait(
        session,
        command = c(paste0("cd ", dir_from_down),
                    paste0("tmp=$(ls -td ",cas_files[ids] ,"*/ " ,"| head -n1)"), 
                    "echo $tmp")
      )
      )
      )[1]
      
      session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
      sortie_telemac <- capture.output(str( ssh_exec_wait(
        session,
        command = c(paste0("cd ", file.path(dir_from_down,foldersortie)),
                    paste0("find . -type f -name ",r"("*.sortie")"," -a -name ",shQuote(paste0("*",cas_files[ids],"*"),type = "sh")," | tail -1")
                    
        )
      )
      )
      )[1]
      session <- ssh::ssh_connect(host = hoteDTecREM,clef_margny)
      sc_telemac <- capture.output(str(ssh_exec_wait(
        session,
        command = c(paste0("cd ", dir_from_down),
                    paste0(" find . -type f -name ",r"('*SC.txt')"," -a -name ",shQuote(paste0("*",sortie_files[ids],"*"),type = "sh")," | head -n1")
                    # paste0(" find . -type f -name ",r"('*SC.txt')"," -a -name ",shQuote(paste0("*",substr(sortie_files[ids],1,nchar(sortie_files[ids])-5),"*"),type = "sh")," | head -n1")
        )
      )
      )
      )[1]
      # browser()
      res_file <- file.path(dir_from_down,substring(res_telemac,3))
      cmd = paste0(pscp_path," -unsafe -scp -i ",clef_margny_ppk,
                   " ",hoteDTecREM,":",res_file," ",dir_to_down)
      system(cmd)
      # browser()
      sc_file <- file.path(dir_from_down,substring(sc_telemac,3))
      cmd = paste0(pscp_path," -unsafe -scp -i ",clef_margny_ppk,
                   " ",hoteDTecREM,":",sc_file," ",dir_to_down)
      system(cmd)
      sortie_file <- file.path(dir_from_down,foldersortie,substring(sortie_telemac,3))
      cmd = paste0(pscp_path," -unsafe -scp -i ",clef_margny_ppk,
                   " ",hoteDTecREM,":",sortie_file," ",dir_to_down)
      system(cmd)
    }
    
  }else{
    cat("Tout est déjà chargé\n")
  }
}
################### assemblage des résultats sur le cluster 
merge_cluster <- function(){
  session <- ssh::ssh_connect(host = hoteDTecREM,passwd = MdPDTecREM)
  dir_cluster2 <- file.path(dir_cluster,contour$NOM)
  casfiles <- list.files(dsnlayerC,pattern = "\\.cas$")
  casfiles <- casfiles[which(casfiles!="Cartino2D.cas")]
  for (i in 1:length(casfiles)) {
    out <- ssh_exec_wait(session, command = c(paste0('cd ',dir_cluster2),paste0('chmod u+x ',nomcontour,"_merge","_",as.character(i))))
  }
  for (i in 1:length(casfiles)) {
    try(out <- ssh_exec_wait(session, command = c(paste0('nohup ','/home-ac/pons/',dir_cluster2,'/',nomcontour,"_merge","_",as.character(i)))))
    # try(out <- ssh_exec_wait(session, command = c(paste0('qsub -q medq',' /home-ac/pons/',dir_cluster2,'/mergetelemac_',as.character(ic),"_",as.character(i)))))  
  }
  
  ## ne pas supprimer cette ligne, elle permet de lancer le merge en parallèle au lieu de lancer sur master 
  # try(out <- ssh_exec_wait(session, command = c(paste0('qsub -q medq',' /home-ac/pons/',dir_cluster2,'/mergetelemac_',as.character(ic),"_",as.character(i)))))  }
  verif_queue <- capture.output(str(ssh_exec_wait(session, command = 'qstat -q medq',std_out = stdout(),std_err = stderr())))
  runqueue <- regmatches(verif_queue[6], gregexpr("[[:digit:]]+", verif_queue[6]))
  runqueue_num <- as.numeric(unlist(runqueue))
  user_run <- capture.output(str(ssh_exec_wait(session, command = 'qstat -u pons',std_out = stdout(),std_err = stderr())))
  verif_queue <- capture.output(str(ssh_exec_wait(session, command = 'qstat -q medq',std_out = stdout(),std_err = stderr())))
  runqueue <- regmatches(verif_queue[6], gregexpr("[[:digit:]]+", verif_queue[6]))
  runqueue_num <- as.numeric(unlist(runqueue))
  s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
  Sys.sleep(30 - s[3]) 
  print(verif_queue[4:6])
}
################### Téléchargement des résultats du cluster 
download_results <- function(){
  # require(ssh)
  session <- ssh::ssh_connect(host = hoteDTecREM,passwd = MdPDTecREM)
  print(contour$NOM)
  dir_to_down <- file.path(dsnlayer,contour$NOMPOST,contour$NOM)
  dir_from_down <- file.path(dir_cluster,contour$NOM)
  cas_files <- list.files(dir_to_down,pattern = "\\.cas$",full.names = FALSE)
  # cas_files <- cas_files[1:(length(cas_files)-1)]
  casfiles <- casfiles[which(casfiles!="Cartino2D.cas")]
  sortie_files <- tools::file_path_sans_ext(cas_files)
  for (ids in 1:length(cas_files)) {
    resf <- sortie_files[ids]
    if(substr(resf,(nchar(resf)-1),nchar(resf))=="_s"){
      sortie_files[ids] <- substr(resf,1,(nchar(resf)-2))
    } else if (substr(resf,(nchar(resf)-4),nchar(resf))=="_SHYS") {
      sortie_files[ids] <- substr(resf,1,(nchar(resf)-5))
    }else{
      sortie_files[ids] <- substr(resf,1,(nchar(resf)-7))
    }
    res_telemac <- capture.output(str(ssh_exec_wait(
      session,
      command = c(paste0("cd ", dir_from_down),
                  paste0(" find . -type f -name ",r"('*.res')"," -a -name ",shQuote(paste0("*",sortie_files[ids],".res","*"),type = "sh")," | head -n1")
                  
      )
    )
    )
    )[1]
    
    
    foldersortie <- capture.output(str( ssh_exec_wait(
      session,
      command = c(paste0("cd ", dir_from_down),
                  paste0("tmp=$(ls -td ",cas_files[ids] ,"*/ " ,"| head -n1)"), 
                  "echo $tmp")
    )
    )
    )[1]
    
    
    sortie_telemac <- capture.output(str( ssh_exec_wait(
      session,
      command = c(paste0("cd ", file.path(dir_from_down,foldersortie)),
                  paste0("find . -type f -name ",r"("*.sortie")"," -a -name ",shQuote(paste0("*",cas_files[ids],"*"),type = "sh")," | tail -1")
                  
      )
    )
    )
    )[1]
    
    sc_telemac <- capture.output(str(ssh_exec_wait(
      session,
      command = c(paste0("cd ", dir_from_down),
                  paste0(" find . -type f -name ",r"('*SC.txt')"," -a -name ",shQuote(paste0("*",basename(sortie_files[ids]),"*"),type = "sh")," | head -n1")
                  
      )
    )
    )
    )[1]
    res_file <- file.path(dir_from_down,substring(res_telemac,3))
    cmd = paste0(pscp_path," -unsafe -scp -pw ",MdPDTecREM,
                 " ",hoteDTecREM,":",res_file," ",dir_to_down)
    system(cmd)
    sc_file <- file.path(dir_from_down,substring(sc_telemac,3))
    cmd = paste0(pscp_path," -unsafe -scp -pw ",MdPDTecREM,
                 " ",hoteDTecREM,":",sc_file," ",dir_to_down)
    system(cmd)
    sortie_file <- file.path(dir_from_down,foldersortie,substring(sortie_telemac,3))
    cmd = paste0(pscp_path," -unsafe -scp -pw ",MdPDTecREM,
                 " ",hoteDTecREM,":",sortie_file," ",dir_to_down)
    system(cmd)
  }
}
################### Vérification du fichier sortie  
checksortie <- function(sct){
  session <- ssh::ssh_connect(host = hoteDTecREM,passwd = MdPDTecREM)
  contour_ <- contours[nsecteur[sct],]
  print(contour_$NOM)
  dir_to_down <- file.path(dsnlayer,contour_$NOMPOST,contour_$NOM)
  dir_from_down <- file.path(dir_cluster,contour_$NOM)
  cas_files <- list.files(dir_to_down,pattern = "\\.cas$",full.names = FALSE)
  # cas_files <- cas_files[1:(length(cas_files)-1)]
  casfiles <- casfiles[which(casfiles!="Cartino2D.cas")]
  checkstrings <- c()
  checkjobs <- c()
  for (i in 1:(length(cas_files))) {
    foldersortie <- capture.output(str( ssh_exec_wait(
      session,
      command = c(paste0("cd ", dir_from_down),
                  paste0("tmp=$(ls -td ",cas_files[i] ,"*/ " ,"| head -n1)"), 
                  "echo $tmp")
    )
    )
    )[1]
    sortie_check <- capture.output(str( ssh_exec_wait(
      session,
      command = c(paste0("cd ", file.path(dir_from_down,foldersortie)),
                  paste0("find . -type f -name ",r"("*.sortie")"," -a -name ",shQuote(paste0("*",cas_files[i],"*"),type = "sh")," | tail -1")
      )
    )
    )
    )[1]
    checkstring <- suppressWarnings(capture.output(str(ssh_exec_wait(
      session,
      command = c(paste0("cd ", file.path(dir_from_down,foldersortie)),
                  paste0("grep -q ", r"(CORRECT END OF RUN )",shQuote(paste0("",substring(sortie_check,3),""),type = "sh"))
      )
    )
    )
    )[1])
    checkjob <- capture.output(str(ssh_exec_wait(
      session,
      command = c(paste0("cd ", file.path(dir_from_down,foldersortie)),
                  paste0("grep -q ", r"(master0 )",shQuote(paste0("",substring(sortie_check,3),""),type = "sh"))
      )
    )
    )
    )[1]
    
    checkjobs <- cbind(checkjobs,checkjob)
    checkstrings <- cbind(checkstrings,checkstring)
  }
  if(length(which((checkjobs)==" int 0"))==length(cas_files)){
    checkjob_ <- " int 1"
  }else{
    checkjob_ <- " int 0"
  }
  if(length(which((checkstrings)==" int 0"))==length(cas_files)){
    checkstring_ <- " int 0"
  }else{
    checkstring_ <- " int 1"
  }
  checkvector <- c(checkjob_,checkstring_)
  return(checkvector)
}
################### Vérification du merge
checkfinmerge <- function(secteur){
  session <- ssh::ssh_connect(host = hoteDTecREM,passwd = MdPDTecREM)
  print(contour$NOM)
  dir_to_down <- file.path(dsnlayer,contour$NOMPOST,contour$NOM)
  dir_from_down <- file.path(dir_cluster,contour$NOM)
  cas_files <- list.files(dir_to_down,pattern = "\\.cas$",full.names = FALSE)
  # cas_files <- cas_files[1:(length(cas_files)-1)]
  casfiles <- casfiles[which(casfiles!="Cartino2D.cas")]
  sortie_files <- tools::file_path_sans_ext(cas_files)
  checkstrings <- c()
  for (i in 1:(length(cas_files))) {
    checkstring <- capture.output(str( ssh_exec_wait(
      session,
      command = c(paste0("cd ", file.path(dir_from_down)),
                  paste0("find . -type f -name ",r"("*.res")"," -a -name ",shQuote(paste0("*",sortie_files[i],"*"),type = "sh")," | tail -1")
      )
    )
    )
    )[2]
    checkstrings <- cbind(checkstrings,checkstring)
  }
  
  if(length(which((checkstrings)==" int 0"))==length(cas_files)){
    checkstring_ <- " int 0"
  }else{
    checkstring_ <- " int 1"
  }
  checkvector <- c(checkstring_)
  return(checkvector)
}


Etape5_ScriptTelemac=function()
{
  # ScriptFredElma=cbind("TelemacFred.txt","TelemacElMa.txt","TelemacFred_ParalPC.txt","TelemacFred_ParalREM.txt")
  # # paste0("TelemacFred_ParalREM",format(Sys.time(), "%Y%m%d%H%M"),".txt")
  # for (ifem in 1:length(ScriptFredElma))
  # {
  #   ##################################################################
  #   # Fusion des lancement de commande
  #   TelFred=file.path(dsnlayer,paste0(substr(ScriptFredElma[ifem],1,nchar(ScriptFredElma[ifem])-4),format(Sys.time(), "%Y%m%d%H%M"),".txt"))
  #   file.create(TelFred)
  #   
  #   write("     ",file=TelFred, append=F)
  #   write("!/bin/bash",file=TelFred, append=F)
  #   
  #   # Compilation de toutes les lignes de lancement Telemac Secteur par secteur
  #   for (ic in 1:length(nsecteur))
  #   {
  #     # Récupération du contour i
  #     contour=contours[nsecteur[ic],]
  #     # print(contour$NOM)
  #     dsnlayerC=paste0(dsnlayer,'\\',contour$NOM)
  #     # file.exists
  #     if (file.exists(file.path(dsnlayerC,ScriptFredElma[ifem])))
  #     {LignesF=readLines(con=file.path(dsnlayerC,ScriptFredElma[ifem]))
  #     # print(LignesF)
  #     write(LignesF,file=TelFred, append=T)
  #     write("cd ..     ",file=TelFred, append=T)
  #     write("     ",file=TelFred, append=T)
  #     }
  #   }
  print("Etape 5")
  
  # }
  
}

################ tests fin calcul 
testfincalcul <- function(nsecteur,fincalcul,nbfinalcul){
  for (sct in 1:length(nsecteur)) {
    if(contours$STEP_PRE[nsecteur[sct]]!=0){next}    
    
    checkvector <- checksortie(sct)  
    checkjob <- checkvector[1]
    checkstring <- checkvector[2]    
    
    
    
    if(checkstring==" int 0"){
      fincalcul[sct] <- fincalcul[sct]+1
      nbfinalcul <- nbfinalcul-1
      cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
    }
    
    if(checkjob==" int 0"){
      cat("\n ******************************************************* \n
                        Le calcul tourne encore \n 
            ******************************************************* \n"
      )
      cat("\n ******************************************************* \n
            30 secondes avant la prochaine vérification \n
             ******************************************************* \n")
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
    }
    if(checkjob!=" int 1" & checkstring!=" int 0"){
      
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
      
      checkvector <- checksortie(sct)  
      checkjob <- checkvector[1]
      checkstring <- checkvector[2]
      if(checkstring==" int 0"){
        fincalcul[sct] <- fincalcul[sct]+1
        nbfinalcul <- nbfinalcul-1
        cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
      }else{        
        
        fincalcul[sct] <- fincalcul[sct]+2
        nbfinalcul <- nbfinalcul-1
        
        cat("\n ******************************************************************************* \n
                  Le calcul s'est arrêté, vérifiez les fichiers d'entrée et/ou le pas de temps \n 
              \n ******************************************************************************** \n")
      }
    }
  } 
  testcalcullist <- list(fincalcul,nbfinalcul)
  return(testcalcullist)
  
}

################ test fin merge
testfinmerge <- function(checkfinmerge){
  while (checkmerge == " int 1") {
    
    
    checkmerge <- checkfinmerge(contour)  
    
    
    
    if(checkmerge==" int 0"){
      
      cat("\n ******************************************************* \n
                      Le merge  s'est terminé sans soucis \n 
              ******************************************************* \n")
    }
    
    if(checkmerge==" int 1"){
      cat("\n ******************************************************* \n
                        Le merge tourne encore \n 
            ******************************************************* \n"
      )
      cat("\n ******************************************************* \n
            30 secondes avant la prochaine vérification \n
             ******************************************************* \n")
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(50 - s[3])
    }
    if(checkmerge !=" int 0"){
      
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(60 - s[3])
      
      checkmerge <- checkfinmerge(contour)  
      if(checkmerge==" int 0"){
        
        cat("\n ******************************************************* \n
                      Le merge s'est terminé sans soucis \n 
              ******************************************************* \n")
      }else{        
        
        
        cat("\n ******************************************************************************* \n
                  Le merge s'est arrêté, vérifiez les fichiers d'entrée \n 
              \n ******************************************************************************** \n")}
      
      
      
      
    }
    
  }
  
  return(checkmerge) 
}


#######################################################################
################### Centre de calcul IFREMER ##########################
#######################################################################
################### copie des fichiers sur cluster et lancement du calcul 
launch_cluster_ifremer <- function(){
  
  res_cas_files <- detect_res_files(dsnlayerC)
  casfiles <- basename(res_cas_files[[2]])
  res_files <- res_cas_files[[1]]
  res_exists_ <- file.exists(res_files)
  casfiles <- casfiles[!res_exists_]
  
  ## zipper et envoyer sur le ftp en extranet 
  dir_copy <- paste0(dsnlayer,"\\",contour$NOMPOST,"\\cluster_copy\\",contour$NOMPOST)
  tozip <- contour$NOM
  zipname <- paste0(contour$NOM,".zip")
  zipfullname <- paste0(dir_copy,"\\",zipname)
  setwd(dir_copy)
  if(file.exists(zipname)){file.remove(zipname)}
  zip(zipfile = zipname, files = tozip)
  
  setwd(dsnlayer)
  session <- ssh::ssh_connect(host = hoteIfremer,passwd = MdPIfremer)
  ssh::ssh_exec_wait(session, command = c(paste0("cd /home3/scratch/",dir_cluster_ifremer,"/eftp")
                                          ,paste0("mkdir ",contour$NOMPOST)))
  
  dir_cluster2 <- file.path(paste0("/home3/scratch/",dir_cluster_ifremer,"/eftp/",contour$NOMPOST),contour$NOM)
  ssh::ssh_exec_wait(session, command = c(paste0("cd /home3/scratch/",dir_cluster_ifremer,"/eftp/",contour$NOMPOST)
                                          ,paste0("rm ", zipname)))
  ssh::ssh_exec_wait(session, command = c(paste0("cd /home3/scratch/",dir_cluster_ifremer,"/eftp/",contour$NOMPOST)
                                          ,paste0("rm -r ", contour$NOM)))
  # ftp_txt_file <- file.path(dsnlayerC,"ftpupload.txt")
  zipnamecluster <- paste0("/scratch/",contour$NOMPOST,"/",nomcontour,".zip")
  
  cmd <- paste0("curl -# -T ",zipfullname,' -u ',hoteextrifremer,":",Mdpextrifremer," ftp://eftp.ifremer.fr",zipnamecluster)
  print(paste0("Téléversement de : ",zipname));system(cmd)
  # curl -T zipfullname -u ftp://ftp.example.com/dir/path/
  
  
  # 
  # ftpUpload(file.path(dir_copy,zipname),
  #           paste0("ftp://",hoteextrifremer,":",Mdpextrifremer,
  #                  "@eftp.ifremer.fr/scratch/",nomcontour,".zip")
  # )
  
  
  # ssh_key_info()
  out <- ssh_exec_wait(session, command = paste0('qstat -u ',dir_cluster_ifremer),std_out = stdout(),std_err = stderr())
  # out <- ssh_exec_wait(session, command = 'ls -l')
  # dir_cluster2 <- file.path(paste0("/home3/scratch/",dir_cluster_ifremer,"/eftp"),contour$NOM)
  ssh::ssh_exec_wait(session, command = c(paste0("cd /home3/scratch/",dir_cluster_ifremer,"/eftp/",contour$NOMPOST)
                                          ,paste0("unzip ", zipname)))
  # ssh::ssh_exec_wait(session, command = c("cd /home3/scratch/nhocini/eftp","ls"))
  # ssh::scp_upload(session, file_path,to = dir_cluster)
  for (i in 1:length(casfiles)) {
    out <- ssh_exec_wait(session, command = c(paste0('cd ',dir_cluster2),paste0('chmod u+x ',nomcontour,"_",as.character(i),".pbs")))
  }
  # if(!exists('list_jobs')){
  #   list_jobs <<- vector("list", length(nsecteur))
  # }
  jobids <- c() 
  for (i in 1:length(casfiles)) {
    
    jobid <- capture.output(str(ssh_exec_wait(session, 
                                              command = c(paste0('qsub -N ',nomcontour,"_",as.character(i),
                                                                 " ",dir_cluster2,'/',nomcontour,
                                                                 "_",as.character(i),'.pbs')))))[1]  
    
    jobids[i] <- jobid
    
  }
  jobs_txt_file <- file.path(dsnlayerC,paste0(nomcontour,"jobs.txt"))
  if(file.exists(jobs_txt_file)){
    write(jobids,file=jobs_txt_file, append=T)
    
  }else{
    write.table(jobids,
                file = file.path(dsnlayerC,paste0(nomcontour,"jobs.txt")),
                row.names = FALSE,col.names = FALSE)
  }
  
  
  # list_jobs[[ic]] <- jobids
  
  # which(sapply(list_jobs,Negate(is.null)))
}
################### Vérification du fichier sortie  
checksortie_ifremer <- function(sct){
  # browser()
  # Nabil à vérifier
  contour_ <- contours[nsecteur[sct],]
  if (file.exists(file.path(dsnlayer,contour_$NOMPOST,contour_$NOM,paste0(contour_$NOM,"jobs.txt")))==F)
  {
    cat(contour_$NOM,"- Pas de job lancé sur IFREMER")
    checkvector <- c(-1,-1)
  }else{
    print(contour_$NOM)
    session <- ssh::ssh_connect(host = hoteIfremer,passwd = MdPIfremer)
    dir_to_down <- file.path(dsnlayer,contour_$NOMPOST,contour_$NOM)
    dir_from_down <- file.path(paste0("/home3/scratch/",dir_cluster_ifremer,"/eftp"),contour_$NOMPOST,contour_$NOM)
    
    res_cas_files <- detect_res_files(dir_to_down)
    cas_files <- res_cas_files[[2]]
    verif_res <- res_cas_files[[1]]
    verif_res_ <- file.exists(verif_res)
    cas_files <- basename(cas_files[!verif_res_])
    
    checkstrings <- c()
    checkjobs <- c()
    
    for (i in 1:(length(cas_files))) {
      
      sortie_check <- capture.output(str( ssh_exec_wait(
        session,
        command = c(paste0("cd ", file.path(dir_from_down)),
                    paste0("find . -type f -name ",r"("*.out*")"," -a -name ",shQuote(paste0("*",cas_files[i],"*"),type = "sh")," | tail -1")
        )
      )
      )
      )[1]
      checkstring <- suppressWarnings(capture.output(str(ssh_exec_wait(
        session,
        command = c(paste0("cd ", file.path(dir_from_down)),
                    paste0("grep -q ",shQuote("My work is done",type = "sh")," ",shQuote(paste0("",substring(sortie_check,3),""),type = "sh"))
        )
      )
      )
      )[1])
      
      if (file.exists(file.path(dsnlayer,contour_$NOMPOST,contour_$NOM,paste0(contour_$NOM,"jobs.txt")))==F){next}
      
      jobid <-read.table(file.path(dsnlayer,contour_$NOMPOST,contour_$NOM,paste0(contour_$NOM,"jobs.txt")))[i,1]
      # jobid <- list_jobs[[sct]][i]
      
      checkjob <- suppressWarnings(capture.output(str(ssh_exec_wait(
        session,
        command = paste0('qstat ',jobid)
        
      )
      )
      )[1])
      
      
      if(checkjob!=" int 35"){
        checkjob <- " int 1"
      }else{
        
        checkjob <- " int 0"
      }
      checkjobs <- cbind(checkjobs,checkjob)
      checkstrings <- cbind(checkstrings,checkstring)
    }
    if(length(which((checkjobs)==" int 0"))==length(cas_files)){
      checkjob_ <- " int 0"
    }else{
      checkjob_ <- " int 1"
    }
    if(length(which((checkstrings)==" int 0"))==length(cas_files)){
      checkstring_ <- " int 0"
    }else{
      checkstring_ <- " int 1"
    }
    checkvector <- c(checkjob_,checkstring_)
    return(checkvector)
  }
}

testfincalcul_ifremer <- function(nsecteur,fincalcul,nbfinalcul){
  for (sct in 1:length(nsecteur)) {
    if(contours$STEP_PRE[nsecteur[sct]]!=0){next}    
    
    checkvector <- checksortie_ifremer(sct)  
    checkjob <- checkvector[1]
    checkstring <- checkvector[2]
    
    if(checkstring==" int 0"){
      fincalcul[sct] <- fincalcul[sct]+1
      nbfinalcul <- nbfinalcul-1
      cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
    }
    
    # A voir avec Nabil concernant l'ajout de checkstring!=" int 0"
    if(checkjob!=" int 0"){
      cat("\n ******************************************************* \n
                        Le calcul tourne encore \n 
            ******************************************************* \n"
      )
      cat("\n ******************************************************* \n
            30 secondes avant la prochaine vérification \n
             ******************************************************* \n")
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
    }
    
    if(checkjob!=" int 0" & checkstring!=" int 0"){
      
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
      
      checkvector <- checksortie_ifremer(sct)  
      checkjob <- checkvector[1]
      checkstring <- checkvector[2]
      if(checkstring==" int 0"){
        fincalcul[sct] <- fincalcul[sct]+1
        nbfinalcul <- nbfinalcul-1
        cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
      }else{        
        
        fincalcul[sct] <- fincalcul[sct]+2
        nbfinalcul <- nbfinalcul-1
        
        cat("\n ******************************************************************************* \n
                  Le calcul s'est arrêté, vérifiez les fichiers d'entrée et/ou le pas de temps \n 
              \n ******************************************************************************** \n")
      }
    }
    if(checkjob==" int 0" & checkstring!=" int 0"){
      
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
      
      checkvector <- checksortie_ifremer(sct)  
      checkjob <- checkvector[1]
      checkstring <- checkvector[2]
      if(checkstring==" int 0"){
        fincalcul[sct] <- fincalcul[sct]+1
        nbfinalcul <- nbfinalcul-1
        cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
      }else{        
        
        fincalcul[sct] <- fincalcul[sct]+2
        nbfinalcul <- nbfinalcul-1
        
        cat("\n ******************************************************************************* \n
                  Le calcul s'est arrêté, vérifiez les fichiers d'entrée et/ou le pas de temps \n 
              \n ******************************************************************************** \n")
      }
    }
  } 
  testcalcullist <- list(fincalcul,nbfinalcul)
  return(testcalcullist)
  
}

download_results_ifremer <- function(){
  # browser()
  # require(ssh)
  session <- ssh::ssh_connect(host = hoteIfremer,passwd = MdPIfremer)
  print(contour$NOM)
  dir_to_down <- paste0(dsnlayer,'\\',contour$NOMPOST,'\\',contour$NOM)
  dir_from_down <- file.path(paste0("/home3/scratch/",dir_cluster_ifremer,"/eftp"),contour$NOMPOST,contour$NOM)
  res_cas_files <- detect_res_files(dir_to_down)
  cas_files <- basename(res_cas_files[[2]])
  res_files <- res_cas_files[[1]]
  
  # ajout fp 20240726
  if (length(cas_files)>0)
  {
    sortie_files <- tools::file_path_sans_ext(cas_files)
    for (ids in 1:length(cas_files)) {
      
      resf <- sortie_files[ids]
      
      if(substr(resf,(nchar(resf)-1),nchar(resf))=="_s"){
        sortie_files[ids] <- substr(resf,1,(nchar(resf)-2))
      } else if (substr(resf,(nchar(resf)-4),nchar(resf))=="_SHYS") {
        sortie_files[ids] <- substr(resf,1,(nchar(resf)-5))
      }
      
      res_telemac_local <- res_files[ids]
      
      if(file.exists(res_telemac_local)){
        print(paste0(res_telemac_local, " existe déjà"))
      }else{
        res_telemac <- capture.output(str(ssh_exec_wait(
          session,
          command = c(paste0("cd ", dir_from_down),
                      paste0(" find . -type f -name ",r"('*.res')"," -a -name ",shQuote(paste0("*",basename(res_telemac_local),"*"),type = "sh")," | head -n1")
                      
          )
        )
        )
        )[1]
        
        
        
        
        sortie_telemac <- capture.output(str( ssh_exec_wait(
          session,
          command = c(paste0("cd ", file.path(dir_from_down)),
                      paste0("find . -type f -name ",r"("*.sortie")"," -a -name ",shQuote(paste0("*",cas_files[ids],"*"),type = "sh")," | tail -1")
                      
          )
        )
        )
        )[1]
        
        sc_telemac <- capture.output(str(ssh_exec_wait(
          session,
          command = c(paste0("cd ", dir_from_down),
                      paste0(" find . -type f -name ",r"('*SC.txt')"," -a -name ",shQuote(paste0("*",sortie_files[ids],"*"),type = "sh")," | head -n1")
                      
          )
        )
        )
        )[1]
        
        # res_file <- file.path(dir_from_down,substring(res_telemac,3))
        # try(scp_download(session = session,files = res_file,to = dir_to_down), silent = TRUE)
        # sc_file <- file.path(dir_from_down,substring(sc_telemac,3))
        # try(scp_download(session = session,files = sc_file,to = dir_to_down), silent = TRUE)
        # sortie_file <- file.path(dir_from_down,substring(sortie_telemac,3))
        # try(scp_download(session = session,files = sortie_file,dir_to_down,verbose = FALSE), silent = TRUE)
        # 
        # 
        # ftpdown_txt_file <- file.path(dsnlayerC,"ftpdownload.txt")
        
        res_telemac_ftp_path <- paste0("/scratch/",contour$NOMPOST,"/",contour$NOM,"/",substring(res_telemac,3))
        sortie_telemac_ftp_path <- paste0("/scratch/",contour$NOMPOST,"/",contour$NOM,"/",substring(sortie_telemac,3))
        sc_telemac_ftp_path <- paste0("/scratch/",contour$NOMPOST,"/",contour$NOM,"/",substring(sc_telemac,3))
        # id_txt_file <- file(ftpdown_txt_file)
        # write('open "eftp.ifremer.fr"',file=ftpdown_txt_file, append=F)
        # write(paste0("user ",shQuote(hoteextrifremer)," ",shQuote(Mdpextrifremer)),file=ftpdown_txt_file, append=T)
        # write("binary",file=ftpdown_txt_file, append=T)
        # write(paste0("lcd ",dir_to_down ),file=ftpdown_txt_file, append=T)
        # write(paste0("get ",res_telemac_ftp_path ),file=ftpdown_txt_file, append=T)
        # write(paste0("get ",sortie_telemac_ftp_path ),file=ftpdown_txt_file, append=T)
        # if(sc_telemac!=" int 0"){
        #   write(paste0("get ",sc_telemac_ftp_path ),file=ftpdown_txt_file, append=T)
        # }
        # write("disconnect",file=ftpdown_txt_file, append=T)
        # write("quit",file=ftpdown_txt_file, append=T)
        # close(id_txt_file)
        # cmd <- paste0("ftp -n -s:",ftpdown_txt_file)
        # print(cmd);system(cmd)
        if(sc_telemac!=" int 0"){
          cmd <- paste0("curl -# -u ",hoteextrifremer,":",Mdpextrifremer," ftp://eftp.ifremer.fr",sc_telemac_ftp_path," --output ",dir_to_down,"\\",substring(sc_telemac,3))
          print(paste0("Téléchargement de : ",substring(sc_telemac,3)));system(cmd)
          
          
          
          
        }
        cmd <- paste0("curl -# -u ",hoteextrifremer,":",Mdpextrifremer," ftp://eftp.ifremer.fr",sortie_telemac_ftp_path," --output ",dir_to_down,"\\",substring(sortie_telemac,3))
        print(paste0("Téléchargement de : ",substring(sortie_telemac,3)));system(cmd)
        cmd <- paste0("curl -# -u ",hoteextrifremer,":",Mdpextrifremer," ftp://eftp.ifremer.fr",res_telemac_ftp_path," --output ",dir_to_down,"\\",substring(res_telemac,3))
        print(paste0("Téléchargement de : ",substring(res_telemac,3)));system(cmd)
        # curl -u nh32b12:etanche-morcelleraient-interposes ftp://eftp.ifremer.fr/scratch/C07_750832km_X652985Y1796740/C07_750832km_X652985Y1796740_SceMaxMedContFrance_00h_00m_14h_00m.cas_2023-11-27-17h46min09s.sortie --output C:\Cartino2D\France\FIONA3_5m\C07_750832km_X652985Y1796740\C07_750832km_X652985Y1796740_SceMaxMedContFrance_00h_00m_14h_00m.cas_2023-11-27-17h46min09s.sortie
        
        #   if(sc_telemac!=" int 0"){
        #     cmd <- paste0("curl -# -u ",hoteextrifremer,":",Mdpextrifremer," ftp://eftp.ifremer.fr",sc_telemac_ftp_path," --output ",dir_to_down,"\\",substring(sc_telemac,3))
        #     print(paste0("Téléchargement de : ",substring(sc_telemac,3)));system(cmd)
        #   
        #   
        #   
        #   
        # }
      }
    }
    
  }else{
    cat("Tout est déjà chargé\n")
  }
}


#######################################################################
################### Centre de calcul GENCI ############################
#######################################################################

launch_cluster_tgcc <- function(){
  
  dir_from <- file.path(dsnlayerC)
  
  res_cas_files <- detect_res_files(dir_from)
  res_files <- res_cas_files[[1]]
  res_exists_ <- file.exists(res_files)
  cas_files <- res_cas_files[[2]]
  cas_files <- cas_files[!res_exists_]
  casfiles <- basename(cas_files)
  
  if (length(casfiles)>0)
  {
    
    ## zipper et envoyer sur le ftp en extranet
    session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
    ssh::ssh_exec_wait(session, command = c(paste0("cd /ccc/scratch/cont003/gen14287/",dir_cluster_tgcc),paste0("mkdir ",contour$NOMPOST)))
    dir_copy <- paste0(dsnlayer,"\\",contour$NOMPOST,"\\cluster_copy\\",contour$NOMPOST)
    # dir_copy_ <- paste0(dsnlayer,"\\",contour$NOMPOST,"\\cluster_copy\\",contour$NOMPOST)
    tozip <- contour$NOM
    zipname <- paste0(contour$NOM,".zip")
    zipfullname <- paste0(dir_copy,"\\",zipname)
    setwd(dir_copy)
    if(file.exists(zipname)){file.remove(zipname)}
    zip(zipfile = zipname, files = tozip)
    
    setwd(dsnlayer)
    session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
    dir_cluster2 <- file.path(paste0("cd /ccc/scratch/cont003/gen14287/",dir_cluster_tgcc,'/',contour$NOMPOST,'/',contour$NOM))
    ssh::ssh_exec_wait(session, command = c(paste0("cd /ccc/scratch/cont003/gen14287/",dir_cluster_tgcc,'/',contour$NOMPOST)
                                            ,paste0("rm ", zipname)))
    session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
    ssh::ssh_exec_wait(session, command = c(paste0("cd /ccc/scratch/cont003/gen14287/",dir_cluster_tgcc,'/',contour$NOMPOST)
                                            ,paste0("rm -r ", contour$NOM)))
    # ftp_txt_file <- file.path(dsnlayerC,"ftpupload.txt")
    zipnamecluster <- paste0("/ccc/scratch/cont003/gen14287/",dir_cluster_tgcc,'/',contour$NOMPOST,'/',nomcontour,".zip")
    
    # id_txt_file <- file(ftp_txt_file)
    # write('open "eftp.ifremer.fr"',file=ftp_txt_file, append=F)
    # write(paste0("user ",shQuote(hoteextrifremer)," ",shQuote(Mdpextrifremer)),file=ftp_txt_file, append=T)
    # write("binary",file=ftp_txt_file, append=T)
    # write(paste0("put ",zipfullname," ", zipnamecluster ),file=ftp_txt_file, append=T)
    # write("disconnect",file=ftp_txt_file, append=T)
    # write("quit",file=ftp_txt_file, append=T)
    # close(id_txt_file)
    # cmd <- paste0("ftp -n -s:",ftp_txt_file)
    # print(cmd);system(cmd)
    session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
    scp_upload(session, zipfullname,to = paste0("/ccc/scratch/cont003/gen14287/",dir_cluster_tgcc,'/',contour$NOMPOST))
    # cmd <- paste0("curl -# -T ",zipfullname,' -u ',hoteextrifremer,":",Mdpextrifremer," ftp://eftp.ifremer.fr",zipnamecluster)
    # print(paste0("Téléversement de : ",zipname));system(cmd)
    # curl -T zipfullname -u ftp://ftp.example.com/dir/path/
    
    
    # 
    # ftpUpload(file.path(dir_copy,zipname),
    #           paste0("ftp://",hoteextrifremer,":",Mdpextrifremer,
    #                  "@eftp.ifremer.fr/scratch/",nomcontour,".zip")
    # )
    
    
    # ssh_key_info()
    session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
    out <- ssh_exec_wait(session, command = paste0('ccc_mstat -u ',dir_cluster_tgcc),std_out = stdout(),std_err = stderr())
    # out <- ssh_exec_wait(session, command = 'ls -l')
    dir_cluster2 <- file.path(paste0("/ccc/scratch/cont003/gen14287/",dir_cluster_tgcc,'/',contour$NOMPOST),contour$NOM)
    session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
    ssh::ssh_exec_wait(session, command = c(paste0("cd /ccc/scratch/cont003/gen14287/",dir_cluster_tgcc,'/',contour$NOMPOST)
                                            ,paste0("unzip ", zipname)))
    # ssh::ssh_exec_wait(session, command = c("cd /home3/scratch/nhocini/eftp","ls"))
    # ssh::scp_upload(session, file_path,to = dir_cluster)
    
    
    for (i in 1:length(casfiles)) {
      session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
      out <- ssh_exec_wait(session, command = c(paste0('cd ',dir_cluster2),paste0('chmod u+x ',nomcontour,"_",as.character(i),".sh")))
    }
    # if(!exists('list_jobs')){
    #   list_jobs <<- vector("list", length(nsecteur))
    # }
    jobids <- c() 
    for (i in 1:length(casfiles)) {
      session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
      jobid <- capture.output(str(ssh_exec_wait(session, 
                                                command = c(paste0("cd ",dir_cluster2)
                                                            ,paste0('ccc_msub ' ,'./',nomcontour,
                                                                    "_",as.character(i),'.sh')))))[1]  
      
      jobids[i] <- as.character(as.numeric(gsub("\\D", "", jobid)))
      
    }
    jobs_txt_file <- file.path(dsnlayerC,paste0(nomcontour,"_TGCC_jobs.txt"))
    if(file.exists(jobs_txt_file)){
      file.remove(jobs_txt_file)
      
    }
    if(file.exists(jobs_txt_file)){
      write(jobids,file=jobs_txt_file, append=T)
      
    }else{
      write.table(jobids,
                  file = file.path(dsnlayerC,paste0(nomcontour,"_TGCC_jobs.txt")),
                  row.names = FALSE,col.names = FALSE)
    }
  }
  
  # list_jobs[[ic]] <- jobids
  
  # which(sapply(list_jobs,Negate(is.null)))
}
################### Vérification du fichier sortie  
checksortie_tgcc <- function(sct){
  # browser()
  # Nabil à vérifier
  contour_ <- contours[nsecteur[sct],]
  if (file.exists(file.path(dsnlayer,contour_$NOMPOST,contour_$NOM,paste0(contour_$NOM,"_TGCC_jobs.txt")))==F)
  {
    cat(contour_$NOM,"- Pas de job lancé sur TGCC")
    checkvector <- c(-1,-1)
  }else{
    print(contour_$NOM)
    
    session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
    dir_to_down <- file.path(dsnlayer,contour_$NOMPOST,contour_$NOM)
    dir_from_down <- file.path(paste0("/ccc/scratch/cont003/gen14287/",dir_cluster_tgcc),contour_$NOMPOST,contour_$NOM)
    
    res_cas_files <- detect_res_files(dir_to_down)
    cas_files <- res_cas_files[[2]]
    verif_res <- res_cas_files[[1]]
    verif_res_ <- file.exists(verif_res)
    cas_files <- basename(cas_files[!verif_res_])
    
    checkstrings <- c()
    checkjobs <- c()
    for (i in 1:(length(cas_files))) {
      
      session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
      sortie_check <- capture.output(str( ssh_exec_wait(
        session,
        command = c(paste0("cd ", file.path(dir_from_down)),
                    paste0("find . -type f -name ",r"("*.sortie")"," -a -name ",shQuote(paste0("*",cas_files[i],"*"),type = "sh")," | tail -1")
        )
      )
      )
      )[1]
      session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
      checkstring <- suppressWarnings(capture.output(str(ssh_exec_wait(
        session,
        command = c(paste0("cd ", file.path(dir_from_down)),
                    paste0("grep -q ",shQuote("CORRECT END OF RUN",type = "sh")," ",shQuote(paste0("",substring(sortie_check,3),""),type = "sh"))
        )
      )
      )
      )[1])
      
      if (file.exists(file.path(dsnlayer,contour_$NOMPOST,contour_$NOM,paste0(contour_$NOM,"_TGCC_jobs.txt")))==F){next}
      
      jobid <-read.table(file.path(dsnlayer,contour_$NOMPOST,contour_$NOM,paste0(contour_$NOM,"_TGCC_jobs.txt")))[i,1]
      # jobid <- list_jobs[[sct]][i]
      session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
      checkjob_ <- suppressWarnings(capture.output(str(ssh_exec_wait(
        session,
        command = paste0('ccc_mstat -H ',jobid)
        
      )
      )
      )[3])
      
      checkjob1 <- grepl('COMPLETED',checkjob_)
      checkjob2 <- grepl('RUNNING',checkjob_)
      checkjob3 <- grepl('PENDING',checkjob_)
      if(isTRUE(checkjob1) | isTRUE(checkjob2) | isTRUE(checkjob3)){
        checkjob <- " int 0"
      }else{
        
        checkjob <- " int 1"
      }
      checkjobs <- cbind(checkjobs,checkjob)
      checkstrings <- cbind(checkstrings,checkstring)
      
      
    }
    calculspastermines <- length(which(checkstrings!=" int 0"))
    cat(contour_$NOM,paste0(calculspastermines," Calculs ne sont pas terminés, ou se sont arrétés, ou avaient déjà des résultats (fichier res)\n"))
    # if(length(which((checkjobs)==" int 0"))==length(cas_files)){
    if(any((checkjobs)==" int 0")){
      checkjob_ <- " int 0"
    }else{
      checkjob_ <- " int 1"
    }
    # if(length(which((checkstrings)==" int 0"))==length(cas_files)){
    if(any((checkstrings)==" int 0")){
      checkstring_ <- " int 0"
    }else{
      checkstring_ <- " int 1"
    }
    checkvector <- c(checkjob_,checkstring_)
    return(checkvector)
  }
}

testfincalcul_tgcc <- function(nsecteur,fincalcul,nbfinalcul){
  for (sct in 1:length(nsecteur)) {
    if(contours$STEP_PRE[nsecteur[sct]]!=0){next}    
    
    checkvector <- checksortie_tgcc(sct)  
    checkjob <- checkvector[1]
    checkstring <- checkvector[2]    
    
    
    
    if(checkstring==" int 0"){
      fincalcul[sct] <- fincalcul[sct]+1
      nbfinalcul <- nbfinalcul-1
      cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
      break()
    }
    
    if(checkjob!=" int 0"){
      cat("\n ******************************************************* \n
                        Le calcul tourne encore \n 
            ******************************************************* \n"
      )
      cat("\n ******************************************************* \n
            30 secondes avant la prochaine vérification \n
             ******************************************************* \n")
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
    }
    if(checkjob==" int 0" & checkstring==" int 0"){
      
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
      
      checkvector <- checksortie_tgcc(sct)  
      checkjob <- checkvector[1]
      checkstring <- checkvector[2]
      if(checkstring==" int 0"){
        fincalcul[sct] <- fincalcul[sct]+1
        nbfinalcul <- nbfinalcul-1
        cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
      }else{        
        
        fincalcul[sct] <- fincalcul[sct]+2
        nbfinalcul <- nbfinalcul-1
        
        cat("\n ******************************************************************************* \n
                  Le calcul s'est arrêté, vérifiez les fichiers d'entrée et/ou le pas de temps \n 
              \n ******************************************************************************** \n")
      }
    }
    if(checkjob==" int 0" & checkstring==" int 0"){
      
      # s = system.time(Sys.sleep(runif(1, min = 20, max = 21)))
      # Sys.sleep(23 - s[3])
      
      checkvector <- checksortie_tgcc(sct)  
      checkjob <- checkvector[1]
      checkstring <- checkvector[2]
      if(checkstring==" int 0"){
        fincalcul[sct] <- fincalcul[sct]+1
        nbfinalcul <- nbfinalcul-1
        cat("\n ******************************************************* \n
                      Le calcul s'est terminé sans soucis \n 
              ******************************************************* \n")
      }else{        
        
        fincalcul[sct] <- fincalcul[sct]+2
        nbfinalcul <- nbfinalcul-1
        
        cat("\n ******************************************************************************* \n
                  Le calcul s'est arrêté, vérifiez les fichiers d'entrée et/ou le pas de temps \n 
              \n ******************************************************************************** \n")
      }
    }
  } 
  testcalcullist <- list(fincalcul,nbfinalcul)
  return(testcalcullist)
  
}

download_results_tgcc <- function(){
  # require(ssh)
  session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
  print(contour$NOM)
  dir_to_down <- file.path(dsnlayer,contour$NOMPOST,contour$NOM)
  dir_from_down <- file.path(paste0("/ccc/scratch/cont003/gen14287/",dir_cluster_tgcc,"/",contour$NOMPOST),contour$NOM)
  
  res_cas_files <- detect_res_files(dir_to_down)
  cas_files <- res_cas_files[[2]]
  res_files <- res_cas_files[[1]]
  verif_res <- file.exists(res_files)
  res_files <- basename(res_files[!verif_res])
  cas_files <- basename(cas_files[!verif_res])
  
  # ajout fp 20240726
  if (length(cas_files)>0)
  {
    sortie_files <- tools::file_path_sans_ext(cas_files)
    for (ids in 1:length(cas_files)) {
      resf <- sortie_files[ids]
      
      if(substr(resf,(nchar(resf)-1),nchar(resf))=="_s"){
        sortie_files[ids] <- substr(resf,1,(nchar(resf)-2))
      } else if (substr(resf,(nchar(resf)-4),nchar(resf))=="_SHYS") {
        sortie_files[ids] <- substr(resf,1,(nchar(resf)-5))
      }else{
        sortie_files[ids] <- substr(resf,1,(nchar(resf)-7))
      }
      
      session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
      res_telemac <- capture.output(str(ssh_exec_wait(
        session,
        command = c(paste0("cd ", dir_from_down),
                    paste0(" find . -type f -name ",r"('*.res')"," -a -name ",shQuote(paste0("*",res_files[ids],"*"),type = "sh")," | head -n1")
                    
        )
      )
      )
      )[1]
      
      session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
      foldersortie <- capture.output(str( ssh_exec_wait(
        session,
        command = c(paste0("cd ", dir_from_down),
                    paste0("tmp=$(ls -td ",cas_files[ids] ,"*/ " ,"| head -n1)"), 
                    "echo $tmp")
      )
      )
      )[1]
      
      session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
      sortie_telemac <- capture.output(str( ssh_exec_wait(
        session,
        command = c(paste0("cd ", file.path(dir_from_down,foldersortie)),
                    paste0("find . -type f -name ",r"("*.sortie")"," -a -name ",shQuote(paste0("*",cas_files[ids],"*"),type = "sh")," | tail -1")
                    
        )
      )
      )
      )[1]
      session <- ssh::ssh_connect(host = hotetgcc,passwd = MdPtgcc)
      sc_telemac <- capture.output(str(ssh_exec_wait(
        session,
        command = c(paste0("cd ", dir_from_down),
                    paste0(" find . -type f -name ",r"('*SC.txt')"," -a -name ",shQuote(paste0("*",sortie_files[ids],"*"),type = "sh")," | head -n1")
                    
        )
      )
      )
      )[1]
      res_file <- file.path(dir_from_down,substring(res_telemac,3))
      cmd = paste0(pscp_path," -unsafe -scp -pw ",MdPtgcc,
                   " ",hotetgcc,":",res_file," ",dir_to_down)
      system(cmd)
      sc_file <- file.path(dir_from_down,substring(sc_telemac,3))
      cmd = paste0(pscp_path," -unsafe -scp -pw ",MdPtgcc,
                   " ",hotetgcc,":",sc_file," ",dir_to_down)
      system(cmd)
      sortie_file <- file.path(dir_from_down,foldersortie,substring(sortie_telemac,3))
      cmd = paste0(pscp_path," -unsafe -scp -pw ",MdPtgcc,
                   " ",hotetgcc,":",sortie_file," ",dir_to_down)
      system(cmd)
    }
    
  }else{
    cat("Tout est déjà chargé\n")
  }
}


#############################Exécution de telemac 2D en séquentiel ou en parallèle############################################################
#############################valable sur machines personnelles windows########################################################################

batchfilescalar <- function()
{
  
  templatepath <- file.path(chem_routine,"C2D\\_Cartino2D_PreRequis\\lancer_calcul_v8p4_auto.bat")
  batfile <- file.path(dsnlayerC,paste0(nomcontour,".bat"))
  file.copy(from = templatepath,to = batfile,overwrite = TRUE)
  
  x = readLines(batfile)
  
  
  tu1 = gsub("telemacpath",as.character(gsub(r"(\\)",r"(\\\\\\)",telemac_folder)),x[1])
  # tu2 = gsub("cfgname",as.character(cfgscalar),x[3])
  # tu3 = gsub("telemac2d.py_path",as.character(gsub(r"(\\)",r"(\\\\\\)",telemac2d)),x[5])
  # tu4 = gsub("projectpath",as.character(gsub(r"(\\)",r"(\\\\\\)",dsnlayerC)),x[6])
  
  
  x[1]=tu1
  # x[3]=tu2
  # x[5]=tu3
  # x[6]=tu4
  
  
  writeLines(x,batfile,sep = "\n")
  
  cas_files <- list.files(dsnlayerC,pattern = "\\.cas$")
  cas_files <- cas_files[which(cas_files!="Cartino2D.cas")]
  # for (icas in 1:(length(cas_files)-1)) {
  for (icas in 1:(length(cas_files))) {
    write(paste0("python %T2DEXEC% -s ", cas_files[icas]),file = batfile,append = TRUE)
  }
  
  
}

batchfileparallel <- function()
{
  templatepath <- file.path(chem_routine,"C2D\\_Cartino2D_PreRequis\\lancer_calcul_v8p4_auto.bat")
  batfile <- file.path(dsnlayerC,paste0(nomcontour,".bat"))
  file.copy(from = templatepath,to = batfile,overwrite = TRUE)
  
  x = readLines(batfile)
  
  tu1 = gsub("telemacpath",as.character(gsub(r"(\\)",r"(\\\\\\)",telemac_folder)),x[1])
  # tu2 = gsub("cfgname",as.character(cfgparallel),x[3])
  # tu3 = gsub("telemac2d.py_path",as.character(gsub(r"(\\)",r"(\\\\\\)",telemac2d)),x[5])
  # tu4 = gsub("projectpath",as.character(gsub(r"(\\)",r"(\\\\\\)",dsnlayerC)),x[6])
  
  x[1]=tu1
  # x[3]=tu2
  # x[5]=tu3
  # x[6]=tu4
  
  
  writeLines(x,batfile,sep = "\n")
  #### NH VOIR POUR NE LANCER QUE CEUX QUI N EXISTENT PAS
  # cas_files <- list.files(dsnlayerC,pattern = "\\.cas$")
  # cas_files <- cas_files[which(cas_files!="Cartino2D.cas")]

  res_cas_files <- detect_res_files(dsnlayerC)
  res_files <- res_cas_files[[1]]
  res_exists_ <- file.exists(res_files)
  cas_files <- res_cas_files[[2]]
  cas_files <- cas_files[!res_exists_]
  casfiles <- basename(cas_files)
  
  if (length(casfiles)>0)
  {
    # for (icas in 1:(length(cas_files)-1)) {
    for (icas in 1:(length(cas_files))) {
      write(paste0("python %T2DEXEC% ", cas_files[icas]," --ncsize=",nb_process, " -s"),file = batfile,append = TRUE)
    }
  }
}
## gfortran_tools: chemin gfortran interne à R ( à trouver en executant la commande Sys.getenv("PATH") ) exemple : "C:\\rtools42\\x86_64-w64-mingw32.static.posix\\bin;"

## batfile : fichier d'execution de telemac2d contenant les infos sur les chemins et le type d'exécution

## execpath : chemin où se trouvent les fichiers d'entrée .cas .cli .slf user_fortran hyeto.txt ......

T2D_run <- function(batfile,execpath)
{
  
  ## retrouver les variables d'environnement 
  # pathr <- Sys.getenv("PATH") 
  # 
  # ## supprimer la variable d'environnement gfortran propre à R de façon temporaire 
  # if(nchar(gfortran_rtools)>0){
  #   pathrnew <- gsub(pattern = gfortran_rtools,x = pathr,replacement = "",fixed = TRUE)
  # }else{
  #   pathrnew <- pathr
  # }
  
  
  ## se mettre dans le dossier d'execution 
  setwd(execpath)
  
  ## exécution de telemac2d avec les nouvelles variables d'environnement de façon temporaire 
  # system(batfile)
  cat("\n")
  cat("calcul de ",batfile," - Ca peut etre long...\n")
  system(paste(batfile, " > NULL 2>&1"), intern = TRUE)
  
}
