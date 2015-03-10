# H. Achicanoy Estrella
# Universidad del Valle
# 2014

# ======================================================================= #
# Cargar paquetes
# ======================================================================= #

library(xlsx)
library(usdm)
library(plspm)
library(FactoMineR)

# ======================================================================= #
# Cargar datos
# ======================================================================= #

dir <- "G:/Mis documentos/Proyectos/Universidad del Valle/Innovacion/Data_sets/Excel_files"
inn_data <- read.xlsx(paste(dir,"/Innovacion_all_formated.xlsx",sep=""),
                      sheetName="R_data",as.data.frame=T,header=T)

# ======================================================================= #
# Eliminar variables con baja frecuencia por categorías
# ======================================================================= #

low_freq <- c("P8_4", "P9_2", "P9_4", "P10_2", "P11_3", "P18_3",
              "P20_1", "P20_4", "P20_5",
              "P39_4", "P41_5",
              "ID")

mtch <- match(low_freq,names(inn_data))
mtch <- setdiff(1:ncol(inn_data),mtch)
inn_data <- inn_data[,mtch]
dim(inn_data); rm(mtch); rm(low_freq)

# ======================================================================= #
# Dar formato a todas las variables
# ======================================================================= #

# Escala nominal

for(i in 1:ncol(inn_data)){
  inn_data[,i] <- as.character(inn_data[,i])
  inn_data[,i] <- as.numeric(inn_data[,i])
  inn_data[,i] <- as.factor(inn_data[,i])
}; rm(i)

# Escala ordinal

ord_var <- c("P3", "P4", "P6_9", "P17_4", "P17_6", "P17_18", "P34_3", "P34_5", "P34_9")

for(ord in ord_var){
  inn_data[,ord] <- ordered(inn_data[,ord])
}; rm(ord); rm(ord_var)

str(inn_data)

# ======================================================================= #
# Test Chi-cuadrado para evaluar independencia entre pares de variables
# ======================================================================= #

options(warn=-1)
p.chisq = matrix(0, nrow=ncol(inn_data), ncol=ncol(inn_data), byrow=T)
for(i in 1:ncol(inn_data)){
  for(j in 1:ncol(inn_data)){
    p.chisq[i,j] = round(chisq.test(inn_data[,i],inn_data[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq) = NA
colnames(p.chisq) = colnames(inn_data)
rownames(p.chisq) = colnames(inn_data)

heatmap(p.chisq)

# write.csv(p.chisq,"G:/Mis documentos/Proyectos/Universidad del Valle/Innovacion/chisq_matrix_formated.csv",row.names=T)

# ======================================================================= #
# Eliminar variables que tienen una escasa relación con el resto
# ======================================================================= #

exc_var <- c("P8_1","P8_3","P20_10")
# P8_1: impresora matricial
# P8_3: impresora inyección
# P20_10: programas otros

mtch <- match(exc_var,names(inn_data))
mtch <- setdiff(1:ncol(inn_data),mtch)
inn_data <- inn_data[,mtch]
dim(inn_data); rm(mtch); rm(exc_var)

# ======================================================================= #
# Definir variables de Hardware
# ======================================================================= #

hard_var <- c(grep(names(inn_data),pattern="P8_"),
              grep(names(inn_data),pattern="P9_"),
              grep(names(inn_data),pattern="P10_"),
              grep(names(inn_data),pattern="P11_"),
              grep(names(inn_data),pattern="P18_"))

# ======================================================================= #
# Definir variables de Software
# ======================================================================= #

soft_var <- c(grep(names(inn_data),pattern="P17_6"),
              grep(names(inn_data),pattern="P17_18"),
              grep(names(inn_data),pattern="P20_"),
              grep(names(inn_data),pattern="P47_2"), # missing
              grep(names(inn_data),pattern="P47_3"),
              grep(names(inn_data),pattern="P47_4")) # missing

# ======================================================================= #
# Definir variables de Redes
# ======================================================================= #

netw_var <- c(grep(names(inn_data),pattern="P34_3"), # missing
              grep(names(inn_data),pattern="P34_5"),
              grep(names(inn_data),pattern="P36_"),
              grep(names(inn_data),pattern="P38"),
              grep(names(inn_data),pattern="P39_"),
              grep(names(inn_data),pattern="P41_"),
              grep(names(inn_data),pattern="P45_"),
              grep(names(inn_data),pattern="P46_"),
              grep(names(inn_data),pattern="P47_1"))

# ======================================================================= #
# Definir variables de Direccionamiento Estratégico
# ======================================================================= #

dir_var <- c(grep(names(inn_data),pattern="P6_9"),
             grep(names(inn_data),pattern="P17_4"),
             grep(names(inn_data),pattern="P34_9"))

# ======================================================================= #
# Análisis factorial múltiple con variables categóricas
# ======================================================================= #

mfa_data <- cbind(inn_data[,hard_var],
                  inn_data[,soft_var],
                  inn_data[,netw_var],
                  inn_data[,dir_var])

mfa_res <- MFA(mfa_data, group=c(length(hard_var),
                                 length(soft_var),
                                 length(netw_var),
                                 length(dir_var)),
               type=rep("n",4),
               name.group=c("Hardware",
                            "Software",
                            "Redes",
                            "Direccionamiento estratégico"))

# Relación entre tablas de datos
mfa_res$group$RV

# El direccionamiento estratégico se encuentra relacionado con:
# 1. Innovación en software
# 2. Innovación en redes
# 3. Innovación en hardware

# La innovación en redes se encuentra relacionada con:
# 1. Innovación en software
# 2. Innovación en hardware
# 3. Direccionamiento estratégico*

# La innovación en software se encuentra relacionada con:
# 1. Innovación en redes
# 2. Direccionamiento estratégico*
# 3. Innovación en hardware

# La innovación en hardware se encuentra relacionada con:
# 1. Innovación en redes
# 2. Innovación en software
# 3. Direccionamiento estratégico*

# ======================================================================= #
# Análisis de componentes principales categórico
# ======================================================================= #

source("G:/Mis documentos/Proyectos/Universidad del Valle/Innovacion/Scripts/cpca_analysis.R")

# ======================================================================= #
# PLS Path Modeling
# ======================================================================= #

# (Eliminando variables con alta cantidad de missing)

#mtch <- match(c("P34_3","P47_2","P47_4"),names(inn_data))
#mtch <- setdiff(1:ncol(inn_data),mtch)

#inn_data <- inn_data[,mtch]

# Definir relaciones entre constructos
HARD = c(0, 0, 0, 0)
SOFT = c(0, 0, 0, 0)
NETW = c(0, 0, 0, 0)
DIRE = c(1, 1, 1, 0)
dir_path = rbind(HARD, SOFT, NETW, DIRE)
colnames(dir_path) = rownames(dir_path)
rm(list=c("HARD","SOFT","NETW","DIRE"))

innerplot(dir_path)

# Definir bloques de variables
dir_blocks = list(hard_var,
                  soft_var,
                  netw_var,
                  dir_var)

# Escala de medición de las variables
dir_scales = list(c(rep("NOM",12)),
                  c(rep("ORD",2),rep("NOM",6)), # 6
                  c(rep("ORD",2),rep("NOM",27)), # 2
                  c(rep("ORD",3)))

# Definir el modo de relación entre variables observadas y variables latentes
dir_modes = rep("B", 4)

dir_pls = plspm(Data=inn_data, path_matrix=dir_path, blocks=dir_blocks,
                modes=dir_modes, scaling=dir_scales, scheme="centroid",
                scaled=TRUE)

# Problemas al parecer con el paquete plspm

# ======================================================================= #
# ======================================================================= #
