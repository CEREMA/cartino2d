#################################################################################################
################   Etape8_PostVolDeb
#################################################################################################
motscles_ajout=function(Lignes,type)
{
  Nombre=sort(unique(substr(Lignes[grep(Lignes,pattern=type[1])],type[2],type[3])))
  if (length(Nombre)>1){Nombre=Nombre[length(Nombre)]}
  Nombre=as.numeric(Nombre)
  
  # cbind(sort(unique(substr(Lignes[grep(Lignes,pattern=type[1])],type[2],type[3]))),type[4],type[5])
  
  MotsClesaj=cbind(paste0(type[1],sprintf("%5d", 1:Nombre),type[6]),type[4],type[5])
  # print(MotsClesaj)
  return(MotsClesaj)
}


Etape8_PostVolDeb=function(dsnlayerC,nomcontour)
{
  debit=as.data.frame(matrix(NA,1,9))
  colnames(debit)= cbind("NOM","0002","0005","0010","0020","0050","0100","TPM2006","2010")
  debit[1]=nomcontour
  
  Listsortie=list.files(file.path(dsnlayerC),pattern="s.sortie")
  print(Listsortie)
  if (length(grep(Listsortie,pattern = '.txt'))>0){Listsortie=Listsortie[-grep(Listsortie,pattern = '.txt')]}
  print(Listsortie)
  if (length(Listsortie)>0){
    for (ils in 1:length(Listsortie)){
      nomsortie=Listsortie[ils]
      print(nomsortie)
      
      substr(nomsortie,1,nchar(nomsortie)-30)
      
      if (file.exists(file.path(dsnlayerC,substr(nomsortie,1,nchar(nomsortie)-30)))==T){
        filenamejpg = file.path(dsnlayerC,paste0(substr(nomsortie,1,nchar(nomsortie)-7),".jpg"))
        if(!file.exists(filenamejpg)){
          Lignes=readLines(con=file.path(dsnlayerC,substr(nomsortie,1,nchar(nomsortie)-30)))
          
          RecupTps=rbind("TIME STEP ")
          indic=regexpr(RecupTps[1,1],as.character(Lignes))
          Past_Ts=as.numeric(substr(Lignes[which(indic>-1)],38,nchar(Lignes[which(indic>-1)])))
          Lignes=readLines(con=file.path(dsnlayerC,nomsortie))
          
          
          type=cbind("FLUX BOUNDARY",19,23,25,42,"")
          MotsClesFB=motscles_ajout(Lignes,type)
          type=cbind("CULVERT        ",17 ,21,37,62,"  DIS")
          MotsClesCUd=motscles_ajout(Lignes,type)
          type=cbind("CULVERT        ",17 ,21,37,62,"  LIMI")
          MotsClesCUl=motscles_ajout(Lignes,type)

          # MotsClesFB=cbind(sort(unique(substr(Lignes[grep(Lignes,pattern="FLUX BOUNDARY")],6,23))),25,42)
          # MotsClesCUd=cbind(paste0(sort(unique(substr(Lignes[grep(Lignes,pattern="CULVERT")],2,21))),"  DIS"),37,62)
          # MotsClesCUl=cbind(paste0(sort(unique(substr(Lignes[grep(Lignes,pattern="CULVERT")],2,21))),"  LIMI"),9,22)
          
          MotsCles=rbind(cbind("RELATIVE ERROR IN VOLUME AT T",37,54),
                         cbind("RUNOFF_SCS_CN : ACCUMULATED RAINFALL",44,61),
                         cbind("VOLUME IN THE DOMAIN",28,44),
                         cbind("ADDITIONAL VOLUME DUE TO SOURCE TERMS",44,61),
                         cbind("RELATIVE ERROR IN VOLUME AT T",58,75),
                         MotsClesFB,
                         MotsClesCUd,
                         MotsClesCUl)
          # cbind("FLUX BOUNDARY    1",25,42),
          # cbind("FLUX BOUNDARY    2",25,42),
          # cbind("FLUX BOUNDARY    3",25,42),
          # cbind("FLUX BOUNDARY    4",25,42),
          # cbind("FLUX BOUNDARY    5",25,42),
          # cbind("FLUX BOUNDARY    6",25,42),
          # cbind("FLUX BOUNDARY    7",25,42),
          # cbind("FLUX BOUNDARY    8",25,42),
          # cbind("FLUX BOUNDARY    9",25,42),
          # cbind("FLUX BOUNDARY   10",25,42),
          # cbind("CULVERT            1  DIS",37,62),
          # cbind("CULVERT            2  DIS",37,62),
          # cbind("CULVERT            3  DIS",37,62),
          # cbind("CULVERT            4  DIS",37,62),
          # cbind("CULVERT            5  DIS",37,62),
          # cbind("CULVERT            5  DIS",37,62),
          # cbind("CULVERT            7  DIS",37,62),
          # cbind("CULVERT            8  DIS",37,62),
          # cbind("CULVERT            9  DIS",37,62),
          # cbind("CULVERT           10  DIS",37,62),
          # cbind("CULVERT           11  DIS",37,62),
          # cbind("CULVERT           12  DIS",37,62),
          # cbind("CULVERT           13  DIS",37,62),
          # cbind("CULVERT           14  DIS",37,62),
          # cbind("CULVERT           15  DIS",37,62),
          # cbind("CULVERT           16  DIS",37,62),
          # cbind("CULVERT           17  DIS",37,62),
          # cbind("CULVERT           18  DIS",37,62),
          # cbind("CULVERT           19  DIS",37,62),
          # cbind("CULVERT           20  DIS",37,62),
          # cbind("CULVERT           21  DIS",37,62),
          # cbind("CULVERT           22  DIS",37,62),
          # cbind("CULVERT           23  DIS",37,62),
          # cbind("CULVERT           24  DIS",37,62),
          # cbind("CULVERT           25  DIS",37,62),
          # cbind("CULVERT           26  DIS",37,62),
          # cbind("CULVERT           27  DIS",37,62),
          # cbind("CULVERT           28  DIS",37,62),
          # cbind("CULVERT           29  DIS",37,62),
          # cbind("CULVERT           30  DIS",37,62),
          # cbind("CULVERT           31  DIS",37,62),
          # cbind("CULVERT           32  DIS",37,62),
          # cbind("CULVERT           33  DIS",37,62),
          # cbind("CULVERT           34  DIS",37,62),
          # cbind("CULVERT           35  DIS",37,62),
          # cbind("CULVERT           36  DIS",37,62),
          # cbind("CULVERT           37  DIS",37,62),
          # cbind("CULVERT           38  DIS",37,62),
          # cbind("CULVERT           39  DIS",37,62),
          # cbind("CULVERT           40  DIS",37,62),
          # cbind("CULVERT           41  DIS",37,62),
          # cbind("CULVERT           42  DIS",37,62),
          # cbind("CULVERT           43  DIS",37,62),
          # cbind("CULVERT           44  DIS",37,62),
          # cbind("CULVERT           45  DIS",37,62),
          # cbind("CULVERT           46  DIS",37,62),
          # cbind("CULVERT           47  DIS",37,62),
          # cbind("CULVERT           48  DIS",37,62),
          # cbind("CULVERT           49  DIS",37,62),
          # cbind("CULVERT           50  DIS",37,62),
          # cbind("CULVERT           51  DIS",37,62),
          # cbind("CULVERT           52  DIS",37,62),
          # cbind("CULVERT           53  DIS",37,62),
          # cbind("CULVERT           54  DIS",37,62),
          # cbind("CULVERT           55  DIS",37,62),
          # cbind("CULVERT           56  DIS",37,62),
          # cbind("CULVERT           57  DIS",37,62),
          # cbind("CULVERT           58  DIS",37,62),
          # cbind("CULVERT           59  DIS",37,62),
          # cbind("CULVERT           60  DIS",37,62),
          # cbind("CULVERT           61  DIS",37,62),
          # cbind("CULVERT           62  DIS",37,62),
          # cbind("CULVERT           63  DIS",37,62),
          # cbind("CULVERT           64  DIS",37,62),
          # cbind("CULVERT           65  DIS",37,62),
          # cbind("CULVERT           66  DIS",37,62),
          # cbind("CULVERT           67  DIS",37,62),
          # cbind("CULVERT           68  DIS",37,62),
          # cbind("CULVERT           69  DIS",37,62),
          # cbind("CULVERT            1  LIMI",9,22),
          # cbind("CULVERT            2  LIMI",9,22),
          # cbind("CULVERT            3  LIMI",9,22),
          # cbind("CULVERT            4  LIMI",9,22),
          # cbind("CULVERT            5  LIMI",9,22),
          # cbind("CULVERT            6  LIMI",9,22),
          # cbind("CULVERT            7  LIMI",9,22),
          # cbind("CULVERT            8  LIMI",9,22),
          # cbind("CULVERT            9  LIMI",9,22),
          # cbind("CULVERT           10  LIMI",9,22),
          # cbind("CULVERT           11  LIMI",9,22),
          # cbind("CULVERT           12  LIMI",9,22),
          # cbind("CULVERT           13  LIMI",9,22),
          # cbind("CULVERT           14  LIMI",9,22),
          # cbind("CULVERT           15  LIMI",9,22),
          # cbind("CULVERT           16  LIMI",9,22),
          # cbind("CULVERT           17  LIMI",9,22),
          # cbind("CULVERT           18  LIMI",9,22),
          # cbind("CULVERT           19  LIMI",9,22),
          # cbind("CULVERT           20  LIMI",9,22),
          # cbind("CULVERT           21  LIMI",9,22),
          # cbind("CULVERT           22  LIMI",9,22),
          # cbind("CULVERT           23  LIMI",9,22),
          # cbind("CULVERT           24  LIMI",9,22),
          # cbind("CULVERT           25  LIMI",9,22),
          # cbind("CULVERT           26  LIMI",9,22),
          # cbind("CULVERT           27  LIMI",9,22),
          # cbind("CULVERT           28  LIMI",9,22),
          # cbind("CULVERT           29  LIMI",9,22),
          # cbind("CULVERT           30  LIMI",9,22),
          # cbind("CULVERT           31  LIMI",9,22),
          # cbind("CULVERT           32  LIMI",9,22),
          # cbind("CULVERT           33  LIMI",9,22),
          # cbind("CULVERT           34  LIMI",9,22),
          # cbind("CULVERT           35  LIMI",9,22),
          # cbind("CULVERT           36  LIMI",9,22),
          # cbind("CULVERT           37  LIMI",9,22),
          # cbind("CULVERT           38  LIMI",9,22),
          # cbind("CULVERT           39  LIMI",9,22),
          # cbind("CULVERT           40  LIMI",9,22),
          # cbind("CULVERT           41  LIMI",9,22),
          # cbind("CULVERT           42  LIMI",9,22),
          # cbind("CULVERT           43  LIMI",9,22),
          # cbind("CULVERT           44  LIMI",9,22),
          # cbind("CULVERT           45  LIMI",9,22),
          # cbind("CULVERT           46  LIMI",9,22),
          # cbind("CULVERT           47  LIMI",9,22),
          # cbind("CULVERT           48  LIMI",9,22),
          # cbind("CULVERT           49  LIMI",9,22),
          # cbind("CULVERT           50  LIMI",9,22),
          # cbind("CULVERT           51  LIMI",9,22),
          # cbind("CULVERT           52  LIMI",9,22),
          # cbind("CULVERT           53  LIMI",9,22),
          # cbind("CULVERT           54  LIMI",9,22),
          # cbind("CULVERT           55  LIMI",9,22),
          # cbind("CULVERT           56  LIMI",9,22),
          # cbind("CULVERT           57  LIMI",9,22),
          # cbind("CULVERT           58  LIMI",9,22),
          # cbind("CULVERT           59  LIMI",9,22),
          # cbind("CULVERT           60  LIMI",9,22),
          # cbind("CULVERT           61  LIMI",9,22),
          # cbind("CULVERT           62  LIMI",9,22),
          # cbind("CULVERT           63  LIMI",9,22),
          # cbind("CULVERT           64  LIMI",9,22),
          # cbind("CULVERT           65  LIMI",9,22),
          # cbind("CULVERT           66  LIMI",9,22),
          # cbind("CULVERT           67  LIMI",9,22),
          # cbind("CULVERT           68  LIMI",9,22),
          # cbind("CULVERT           69  LIMI",9,22))
          
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
          write.table(tab,
                      file=file.path(dsnlayerC,paste0(substr(nomsortie,1,nchar(nomsortie)-7),"_sortie.txt")),
                      sep=";")
          
          
          
          # Suppression du volume initial (confition limite)
          if (length(tab)>2){
            tab[,3]=tab[,3]-tab[1,3]
            
            
            # for (ic in 2:dim(MotsCles)[1])
            # {
            #   plot(tab[,1],tab[,ic],main=colnames(tab)[ic])
            # }
            ###GRAVE
            # browser()
            TimeStep=tab[1,1] # 60
            CumulDebit=matrix(0,dim(tab)[1],10)
            nsortie=0
            
            
            
            for (ip in PosFB){ #6:15)
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
            
            
            
            tab[,4]=tab[,4]/Past_Ts
            
            CumulAddVol=sapply(1:dim(tab)[1], 
                               function(x) {TimeStep*sum(tab[1:x,4])})
            
            couleurs=cbind("green","magenta","brown","orange","yellow","gray","cyan","black")
            couleurs=cbind(couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs)
            titSort=paste0("Sortie ",1:length(PosFB))
            # legend = titSort[1:nsortie],
            # col=couleurs[1:nsortie],
            # lty=ltyl[1:nsortie],
            
            
            jpeg(filename = file.path(dsnlayerC,paste0(substr(nomsortie,1,nchar(nomsortie)-7),".jpg")),
                 width = 47.25, height = 66.825, units = "cm", quality = 75, res = 150)
            x1=0.025; x2=0.975 ;y1=0.81; y2=0.99
            par(fig=c(x1,x2,y1,y2),new=TRUE)
            # plot(tab[,1],CumulAddVol,main=paste0(nomsortie," Volumes et Débits"),type="l",
            
            plot(tab[,1],CumulAddVol,main=nomsortie,type="l",
                 xlab="Temps Telemac (s)",ylab="Volume (m3)",col="black")
            # lines(tab[,1],tab[,2])
            lines(tab[,1],tab[,3],col="red")
            for (isor in 1:nsortie){
              lines(tab[,1],CumulDebit[,isor],col=couleurs[isor])
            }
            
            lines(tab[,1],tab[,3]+CumulDebit[,1]+CumulDebit[,2]+CumulDebit[,3]+CumulDebit[,4]+CumulDebit[,5],col="blue")
            legend("topleft",
                   legend = rbind("Injecte","Domaine","Domaine + sorties",
                                  as.matrix(titSort[1:nsortie])),
                   col=rbind(rbind("black","red","blue"),as.matrix(couleurs[1:nsortie])), #col=cbind("black","red","blue",couleurs[1:nsortie]),
                   lty=cbind(1,1,1,1,1,1,1,1),
                   pch==cbind(1,1,1,1,1,1,1,1),
                   title = "Volume")
            
            
            x1=0.025; x2=0.975 ;y1=0.61; y2=0.79
            par(fig=c(x1,x2,y1,y2),new=TRUE)
            plot(tab[,1],tab[,2],main="Pluie Injectee",typ="l",xlab="Temps Telemac (s)",ylab="Cumul (m)")
            
            x1=0.025; x2=0.975 ;y1=0.41; y2=0.59
            par(fig=c(x1,x2,y1,y2),new=TRUE)
            plot(tab[,1],tab[,4],main="Volume net",type="b",xlab="Temps Telemac (s)",ylab="Apport a chaque pas de calcul (m3)")
            
            x1=0.025; x2=0.975 ;y1=0.21; y2=0.39
            par(fig=c(x1,x2,y1,y2),new=TRUE)
            
            ltyl=cbind(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
            pchl=cbind(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
            
            #### Débits sortie
            
            print(c(0,-min(tab[,PosFB])))
            decal=min(PosFB)-1
            plot(tab[,1],-tab[,min(PosFB)],main="Debit Sorties", ylim=c(0,-min(tab[,PosFB])),typ="l",col=couleurs[min(PosFB)-decal],xlab="Temps Telemac (s)",ylab="Debit (m3/s)")
            nsortie=0
            for (ip in PosFB)#7:15)
            {
              if (max(abs(tab[,ip]))>0)
              {
                lines(tab[,1],-tab[,ip],col=couleurs[ip-decal])
                if (min(nsortie)==0) {nsortie=ip-decal}else{nsortie=cbind(nsortie,ip-decal)}
              }
            }
            
            legend("topleft",
                   legend = titSort[PosFB[nsortie]-decal],
                   col=couleurs[PosFB[nsortie]-decal],
                   lty=ltyl[PosFB[nsortie]-decal],
                   pch=pchl[PosFB[nsortie]-decal],
                   title = "Debits")
            
            x1=0.025; x2=0.975 ;y1=0.01; y2=0.19
            par(fig=c(x1,x2,y1,y2),new=TRUE)
            
            #### Débits sortie
            print(c(0,-min(tab[,6]+tab[,7]+tab[,8]+tab[,9]+tab[,10])))
            decal=min(PosCu)-1
            
            
            #### Culverts
            nculv=0
            # browser()
            ylimmax=max(tab[,PosCu][which(is.na(tab[,PosCu])==F)])
            print(ylimmax)
            plot(tab[,1],-tab[,min(PosCu)],main="Debit Ouvrages", ylim=c(0,ylimmax),typ="l",col=couleurs[min(PosCu)-decal],xlab="Temps Telemac (s)",ylab="Debit (m3/s)")
            titCulv=paste0("Culvert ",1:length(PosCu))      
            for (ip in PosCu)
            {
              if (max(abs(tab[,ip][which(is.na(tab[,ip])==F)]))>0)
              {
                lines(tab[,1],tab[,ip],col=couleurs[ip-decal])
                if (min(nculv)==0) {nculv=ip-decal}else{nculv=cbind(nculv,ip-decal)}
              }
            }
            if (min(nculv)>0)
            {
              
              # cbind("Culvert 1","Culvert 2","Culvert 3","Culvert 4","Culvert 5","Culvert 6","Culvert 7","Culvert 8",
              #             "Culvert 9","Culvert 10","Culvert 11","Culvert 12","Culvert 13","Culvert 14","Culvert 15")
              legend("topleft",
                     legend = titCulv[as.vector(nculv)],
                     col=couleurs[PosCu[as.vector(nculv)]-decal],
                     lty=ltyl[as.vector(nculv)],
                     pch=pchl[as.vector(nculv)],
                     title = "Ouvrages")
            }
            dev.off()
            
            # sapply(PosCu, function(x) {max(tab[,x])})
            
            if (file.exists(file.path(dsnlayerC,"OuvHydrau_Select.shp"))==TRUE)
            {
              # browser()
              OuvHydrau=st_read(dsn = dsnlayerC,
                                layer = "OuvHydrau_Select")
              OuvHydrau$MaxQ=sapply(PosCu, function(x) {round(max(tab[,x]),2)})[1:dim(OuvHydrau)[1]]
              OuvHydrau$LDE=sapply(PosCuL, function(x) {length(which((tab[,x])>0))})[1:dim(OuvHydrau)[1]]
              # if (max(OuvHydrau$LDE)>0)
              # {
              #   browser()
              # }
              st_write(OuvHydrau,dsn=file.path(dsnlayerC,paste0(substr(nomsortie,1,nchar(nomsortie)-7),"_OH.gpkg")), delete_layer=T, quiet=T)
              file.copy(file.path(dsnlayerC,"MaxQ_OuvHydrau_Select.qml"),
                        file.path(dsnlayerC,paste0(substr(nomsortie,1,nchar(nomsortie)-7),"_OH.qml")))
            }
            
            # récupération de la somme des débits max
            for (ic in 1:dim(debit)[2])
            {
              
              indic=regexpr(colnames(debit)[ic],nomsortie)
              if (indic>-1)
              {
                if (is.na(debit[ic])==TRUE)
                {
                  debit[ic]=round(-min(tab[,6]+tab[,7]+tab[,8]+tab[,9]+tab[,10]),2)
                }else{
                  if (debit[ic]==round(-min(tab[,6]+tab[,7]+tab[,8]+tab[,9]+tab[,10]),2))
                  {
                    print(paste0("Même débit, deux fichiers sorties pour le même scénario, nom du dernier: ",nomsortie))
                  }else{
                    print(paste0("BUG, duex fichiers sorties pour le même scénario, nom du dernier: ",nomsortie))
                    # BUG=BUGGGG
                  }
                }
              }
              # recherche mot clé
              # mise à jour du champ avec verif qu'il n'existe pas
            }
          }
        }}
    }
  }
  print(debit)
  return(debit)
}


Etape8_fn <- function(){
  tryCatch({          if (ETAPE[8] == 1)
  {
    debit_sc[[ic]] = Etape8_PostVolDeb(dsnlayerC, nomcontour)
  }}, error = function(e) {skip_to_next <<- TRUE})
  
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "problème étape 8"
    contours$STEP_PRE[nsecteur[ic]] <- -2
  }
}

Etape8_fn_parallel <- function(){
  if (ETAPE[8] == 1)
  {
    debits = Etape8_PostVolDeb(dsnlayerC, nomcontour)
    return(debits)
  }
}

