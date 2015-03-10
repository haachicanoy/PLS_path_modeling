# H. Achicanoy Estrella
# Partial Least Squares Path Modeling using categories like numeric values
# Based in Sanchez (2013)

# ======================================================================= #
# Load packages
# ======================================================================= #

library(plspm)
library(FactoMineR)

# ======================================================================= #
# Load data
# ======================================================================= #

dir <- "G:/Mis documentos/Proyectos/Universidad del Valle/Innovacion/Data_sets/Excel_files"
inn_data <- read.csv(paste(dir,"/Innovacion_without_transformation.csv",sep=""),header=T)

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

inn_data[,3] <- factor(inn_data[,3],levels=c("Rank1","Rank2","Rank3"),ordered=T)
inn_data[,4:ncol(inn_data)] <- apply(inn_data[,4:ncol(inn_data)],2,as.numeric)

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

heatmap(p.chisq,Rowv=NA,Colv=NA)

# write.csv(p.chisq,"G:/Mis documentos/Proyectos/Universidad del Valle/Innovacion/chisq_matrix_non_formated.csv",row.names=T)

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
               type=rep("s",4),
               name.group=c("Hardware",
                            "Software",
                            "Redes",
                            "Direccionamiento estratégico"))

# Relación entre tablas de datos
mfa_res$group$RV

# ======================================================================= #
# Análisis de componentes principales Hardware
# ======================================================================= #

pca_hard <- PCA(inn_data[,hard_var],scale.unit=F,ncp=5)

# ======================================================================= #
# Análisis de componentes principales Software
# ======================================================================= #

pca_soft <- PCA(inn_data[,soft_var],scale.unit=F,ncp=5)

# ======================================================================= #
# Análisis de componentes principales Redes
# ======================================================================= #

pca_netw <- PCA(inn_data[,netw_var],scale.unit=F,ncp=5)

# ======================================================================= #
# Análisis de componentes principales Direccionamiento
# ======================================================================= #

pca_dire <- PCA(inn_data[,dir_var],scale.unit=F,ncp=5)

# ======================================================================= #
# Análisis exploratorio de las variables seleccionadas
# ======================================================================= #

library(RColorBrewer)

# Hardware variables
for (j in hard_var){
  distribution=table(inn_data[,j])/nrow(inn_data)
  barplot(distribution, border=NA, col=brewer.pal(8,"Blues")[2:8],
          axes=FALSE, main="", cex.main=1)
  # add vertical axis, and rectangle around figure
  axis(side=2, las=2)
  box("figure", col="gray70")
}

# Software variables
for (j in soft_var){
  distribution=table(inn_data[,j])/nrow(inn_data)
  barplot(distribution, border=NA, col=brewer.pal(8,"Blues")[2:8],
          axes=FALSE, main="", cex.main=1)
  # add vertical axis, and rectangle around figure
  axis(side=2, las=2)
  box("figure", col="gray70")
}

# Networks variables
for (j in netw_var){
  distribution=table(inn_data[,j])/nrow(inn_data)
  barplot(distribution, border=NA, col=brewer.pal(8,"Blues")[2:8],
          axes=FALSE, main="", cex.main=1)
  # add vertical axis, and rectangle around figure
  axis(side=2, las=2)
  box("figure", col="gray70")
}

# Direccionamiento variables
for (j in dir_var){
  distribution=table(inn_data[,j])/nrow(inn_data)
  barplot(distribution, border=NA, col=brewer.pal(8,"Blues")[2:8],
          axes=FALSE, main="", cex.main=1)
  # add vertical axis, and rectangle around figure
  axis(side=2, las=2)
  box("figure", col="gray70")
}

# ======================================================================= #
# PLS Path Modeling
# ======================================================================= #

library(plspm)

# Definir relaciones entre constructos
#CTRL = c(0, 0, 0, 0, 0)
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
dir_scales = list(rep("NUM",12),
                  rep("NUM",8),
                  rep("NUM",29),
                  rep("NUM",3))

# Modo de relación entre variables observadas y variables latentes
dir_modes = rep("B", 4)

# inn_data <- inn_data[complete.cases(inn_data),]

dir_pls = plspm(Data=inn_data, path_matrix=dir_path, blocks=dir_blocks,
                modes=dir_modes, scheme="centroid",
                scaled=FALSE) # scaling=dir_scales, 

# Seleccionar datos de entrenamiento y validación del modelo

library(dismo)
# set.seed(1244)
sam <- kfold(inn_data, k=2)

train <- inn_data[which(sam==1),]
testn <- inn_data[which(sam==2),]

dir_train <- plspm(Data=train, path_matrix=dir_path, blocks=dir_blocks,
                   modes=dir_modes, scheme="centroid")

dir_test <- plspm(Data=testn, path_matrix=dir_path, blocks=dir_blocks,
                  modes=dir_modes, scheme="centroid")

# (Eliminando variables con alta cantidad de missing)

mtch <- match(c("P34_3","P47_2","P47_4"),names(inn_data))
mtch <- setdiff(1:ncol(inn_data),mtch)

inn_data <- inn_data[,mtch]

# Definir bloques de variables
dir_blocks2 = list(hard_var,
                   soft_var,
                   netw_var,
                   dir_var)

