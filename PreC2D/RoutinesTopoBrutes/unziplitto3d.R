dsnlayer="C:/Users/Precision7875/Downloads"
listeZIP=list.files(dsnlayer,"7z")
print(listeZIP)

# Spécifiez le répertoire de destination
destination_path <- "E:/Litto3D/"

for (ilist in listeZIP)
{
  # unzip(file.path(dirname(listeZIP),ilist))#,exdir="E:/Litto3D")
  
  # Spécifiez le chemin vers le fichier .7z
  file_path <- file.path(dsnlayer,ilist)
  
  # Utilisez la commande système pour décompresser le fichier dans le répertoire de destination
  cmd=paste0(shQuote("C:/Program Files/7-Zip/7z.exe")," x ", file_path, " -o", destination_path)
  system(cmd)
  # file.rename(
  
  # source_path=file.path(destination_path,substr(ilist,1,nchar(ilist)-3))
  # 
  # file.rename(source_path,destination_path)
}
