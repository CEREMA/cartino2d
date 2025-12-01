# chem_routine=dirname(rstudioapi::getActiveDocumentContext()$path)

source(file.path(chem_routine,"PreC2D","PreC2D_6_2_Decrenelage_job.R"))
# Exemple post C2D 25m
#Reso=2.5
#nomrtovect <- "C:\\Cartino2D\\France\\Formation\\C0002_147875km_X904216Y6272477\\Post\hyeto_C0086_705174km_X883359Y6260813_SceMaxMedContFrance_00h_00m_12h_00m_HWM_m_ZI0.1.gpkg"
#AireMin_Inter=630
#AireMin_Exter=630
#Decrenelage(nomrtovect,AireMin_Inter,AireMin_Exter,EPSG,Reso)

# Exemple post C2D 5m
 Reso=2.5
 nomrtovect <- "C:/Cartino2D/France/MAMP2025_5m/C0086_705174km_X883359Y6260813/Post/hyeto_C0086_705174km_X883359Y6260813_SceMaxMedContFrance_00h_00m_12h_00m_HWM_m_ZI0.1.gpkg"
 AireInterieureSupp=300
 AireExterieureSupp=300
 Decrenelage(nomrtovect,AireInterieureSupp,AireExterieureSupp,EPSG,Reso)

# Exemple post C2D desctructuré
# Reso=1
# nomrtovect <- "C:\Cartino2D\France/CNIR/C51_448775km_X908527Y6275414/POST/shyreg_spPB_C51_448775km_X908527Y6275414_T0500_D12_PIC06_HWH_m_ZI0.05.gpkg"
# AireInterieureSupp=300
# AireExterieureSupp=300
# Decrenelage(nomrtovect,AireInterieureSupp,AireExterieureSupp,EPSG,Reso)










# Reso <- 1
# 
# 
# nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240808_MaJ\\ALEA\\BufPlus3_Moins2\\A_4_PPRi_ZI1.gpkg"
# Decrenelage(nomrtovect,300,300,EPSG,Reso)
# 
# nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240808_MaJ\\ALEA\\BufPlus3_Moins2\\A_4_PPRi_ZI2.gpkg"
# Decrenelage(nomrtovect,100,100,EPSG,Reso)
# 
# nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240808_MaJ\\ALEA\\BufPlus3_Moins2\\A_4_PPRi_ZI3.gpkg"
# Decrenelage(nomrtovect,100,100,EPSG,Reso)
# 
# nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240808_MaJ\\ALEA\\BufPlus3_Moins2\\A_4_Resi_ZI1.gpkg"
# Decrenelage(nomrtovect,500,500,EPSG,Reso)

# 
# Reso=5
# nomrtovect <- "C:\\Cartino2D\\France\\MTPCLL2024_MOSSON\\C04_002815km_X765771Y6278123\\Post\\hyeto_C04_002815km_X765771Y6278123_PQN_00h_00m_30h_00m_Q_Resi_HWH_m_ZI0.001.gpkg"
# Decrenelage(nomrtovect,10000,10000,EPSG,Reso)

# Reso=1
# nomrtovect <- "C:\\Cartino2D\\France\\MTPCLL2024_MOSSON\\C03_240638km_X765849Y6278594\\Post\\ALEA\\A_0_hyeto_C03_240638km_X765849Y6278594_PQN_00h_00m_28h_00m_Q_PPRi__ZI1.gpkg"
# Decrenelage(nomrtovect,300,300,EPSG,Reso)
# 
# nomrtovect <- "C:\\Cartino2D\\France\\MTPCLL2024_MOSSON\\C03_240638km_X765849Y6278594\\Post\\ALEA\\A_0_hyeto_C03_240638km_X765849Y6278594_PQN_00h_00m_28h_00m_Q_PPRi__ZI2.gpkg"
# Decrenelage(nomrtovect,100,100,EPSG,Reso)
# 
# nomrtovect <- "C:\\Cartino2D\\France\\MTPCLL2024_MOSSON\\C03_240638km_X765849Y6278594\\Post\\ALEA\\A_0_hyeto_C03_240638km_X765849Y6278594_PQN_00h_00m_28h_00m_Q_PPRi__ZI3.gpkg"
# Decrenelage(nomrtovect,100,100,EPSG,Reso)

# nomrtovect <- "C:\\Cartino2D\\France\\MTPCLL2024_MOSSON\\C03_240638km_X765849Y6278594\\Post\\ALEA\\A_0_hyeto_C03_240638km_X765849Y6278594_PQN_00h_00m_28h_00m_Q_Resi__ZI1.gpkg"
# Decrenelage(nomrtovect,500,500,EPSG,Reso)


# Reso=5
# nomrtovect <- "C:\\Cartino2D\\France\\_ResultsNASSIM\\MOSSON5m_DEB\\hyeto_C114_168242km_X768711Y6274845_PQN_00h_00m_14h_00m_SOURCEFILE_HWH_m_ZI0.gpkg"
# Decrenelage(nomrtovect,10000,10000,EPSG,Reso)

# 
# ####################
# nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240715_1120\\ALEA\\BufPlus3_Moins2\\A_4_PPRi_ZI1.gpkg"
# source(file.path(chem_routine,"Decrenelage_function.R"))
# 
# nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240715_1120\\ALEA\\BufPlus3_Moins2\\A_4_PPRi_ZI2.gpkg"
# source(file.path(chem_routine,"Decrenelage_function.R"))
# 
# nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240715_1120\\ALEA\\BufPlus3_Moins2\\A_4_PPRi_ZI3.gpkg"
# source(file.path(chem_routine,"Decrenelage_function.R"))
# 
# 
# ####################
# nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240715_1120\\ALEA\\BufPlus2_Moins2\\A_4_PPRi_ZI1.gpkg"
# source(file.path(chem_routine,"Decrenelage_function.R"))
# 
# nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240715_1120\\ALEA\\BufPlus2_Moins2\\A_4_PPRi_ZI2.gpkg"
# source(file.path(chem_routine,"Decrenelage_function.R"))
# 
# nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240715_1120\\ALEA\\BufPlus2_Moins2\\A_4_PPRi_ZI3.gpkg"
# source(file.path(chem_routine,"Decrenelage_function.R"))

# dossi="C:/AFFAIRES/Montpellier/14_ALEA20240715/Res20240715_1120/ALEA"
# 
# listeGPKG=list.files(dossi,pattern="\\.gpkg")
# 
# ici=grep(listeGPKG,pattern="aux")
# if (length(ici)>0){listeGPKG=listeGPKG[-ici]}
# 
# ici=grep(listeGPKG,pattern="A_")
# if (length(ici)>0){listeGPKG=listeGPKG[ici]}
# 
# for (ilist in listeGPKG)
# {
#   nomsortie=file.path(dossi,ilist)
#   cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",shQuote(nomsortie)," 2 4 8 16 32 64 128 256")
#   print(cmd);toto=system(cmd)
#   print(toto)
# }