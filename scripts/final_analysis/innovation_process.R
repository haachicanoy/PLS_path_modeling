
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Innovación en tecnologías de la información
# Universidad del Valle - CIAT
# H. Achicanoy, 2015
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Load packages
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

options(warn=-1)
if(!require(mi)){install.packages('mi');library(mi)} else{library(mi)}
if(!require(mice)){install.packages('mice');library(mice)} else{library(mice)}
if(!require(xlsx)){install.packages('xlsx');library(xlsx)} else{library(xlsx)}
if(!require(usdm)){install.packages('usdm');library(usdm)} else{library(usdm)}
if(!require(plspm)){install.packages('plspm');library(plspm)} else{library(plspm)}
if(!require(FactoMineR)){install.packages('FactoMineR');library(FactoMineR)} else{library(FactoMineR)}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Read data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

dir <- "C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/data_sets/excel"
idata <- read.xlsx(paste0(dir,"/innovacion_variables_codificadas.xlsx"),sheetIndex=1,as.data.frame=T,header=T)
lapply(names(idata), function(x){table(idata[,paste(x)])})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Formatting data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Nominal scale
for(i in 1:ncol(idata)){
  idata[,i] <- as.character(idata[,i])
  idata[,i] <- as.numeric(idata[,i])
  idata[,i] <- as.factor(idata[,i])
}; rm(i)

# Ordinal scale
ord_var <- c("P3","P4","P6_9","P17_4","P17_6","P17_18","P34_3","P34_5","P34_9")
for(ord in ord_var){
  idata[,ord] <- ordered(idata[,ord])
}; rm(ord); rm(ord_var)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Identify variables with few responses
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

miss_data_perc <- unlist(lapply(names(idata), function(x){sum(is.na(idata[,paste(x)]))/107}))
miss_data_perc <- data.frame(Variable=names(idata), Percent=miss_data_perc)

# P34_3: 35%
idata <- idata[,-match("P34_3",names(idata))]

idata <- idata[-40,]
rownames(idata) <- 1:nrow(idata)
rm(miss_data_perc)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Independence test
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Chi-square test for original data
options(warn=-1)
p.chisq = matrix(0, nrow=ncol(idata), ncol=ncol(idata), byrow=T)
for(i in 1:ncol(idata)){
  for(j in 1:ncol(idata)){
    p.chisq[i,j] = round(chisq.test(idata[,i],idata[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq) = NA
colnames(p.chisq) = colnames(idata)
rownames(p.chisq) = colnames(idata)

library(gplots)
color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/correlation_analysis/association_matrix.png", width=1500, height=1500, pointsize=25)
  plot.new()
  heatmap.2(p.chisq,
            main="Independence test",
            key.title="Chi-square test",
            key.xlab="p-value",
            Rowv=NULL,
            Colv=NULL,
            col=color_scale,
            linecol=NULL,
            tracecol=NULL,
            density.info="density",
            denscol="blue",
            margins=c(5,5))
dev.off()

rm(p.chisq)
rm(color_scale)

#== Writting association matrix original
# write.csv(p.chisq,"C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/correlation_analysis/chisq_matrix_original.csv",row.names=T)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#== pendiente: hacer estadísticas descriptivas para todas las variables !!!
# Descriptive analysis for original data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

library(RColorBrewer)

freqCol <- lapply(colnames(idata), function(x){table(idata[paste(x)])})
names(freqCol) <- colnames(idata)

# CONTROL variables

ctrl_questions <- c("Sector", "Tamaño", "Antigüedad")
ctrl_labels <- list(sector=c("Primario", "Secundario", "Terciario"),
                    tamano=c("Pequeñas", "Medianas"),
                    antiguedad=c("1 a 5 años", "6 a 14 años", "15 o más años"))

op <- par(mfrow=c(1, 3), mar=c(2.5, 4.5, 2, 0.8)) ## 11 * 6 pdf
for(i in 1:3){
  distribution <- freqCol[[i]]/sum(freqCol[[i]]) * 100
  barplot(distribution, border=NA, col=brewer.pal(8, "Blues")[2:8],
          axes=FALSE, main=ctrl_questions[i], cex.main = 1,
          names.arg=ctrl_labels[[i]], ylab="Porcentaje (%)")
  # add vertical axis, and rectangle around figure
  axis(side = 2, las=2)
  box("figure", col="gray70")
}
par(op)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Multiple imputation
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Explore patterns
png("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/missing_data/pattern_missing_items.png", width=1500, height=1500, pointsize=25)
  par(mar=c(5, 6, 4, 2) + 0.1) # bottom, left, top, right
  missing.pattern.plot(idata, gray.scale=TRUE, main="",
                       xlab="Pattern of missing items")
dev.off()

png("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/missing_data/pattern_missing_items2.png", width=1500, height=1500, pointsize=25)
  par(mar=c(5, 6, 4, 2) + 0.1) # bottom, left, top, right
  mp.plot(idata, y.order=TRUE, x.order=TRUE, gray.scale=TRUE, main="")
dev.off()

library(mice)

md.pattern(idata) # Pattern of missing data
md.pairs(idata)

# Plotting missing and observed data
library(VIM)
marginplot(idata[,c("P2", "P47_1")], col=mdc(1:2), cex=1.2, cex.lab=1.2, cex.numbers=1.3, pch=19)

# Imputing data
IMP2 <- mice(idata, m=50, seed=1235)
plot(IMP2) # Verify stability of estimates
print(IMP2)

# Save workspace
save.image(file="C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/results_innovation.RData")

# Load workspace
load("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/results_innovation.RData")

mImp_var <- IMP2$imp[names(IMP2$nmis)[which(IMP2$nmis>0)]]
varList <- names(mImp_var)

imp_idata <- idata

imp_process <- function(var, ...){
  
  nr <- nrow(mImp_var[[var]]);
  id_r <- as.numeric(rownames(mImp_var[[var]]));
  
  if(var %in% c("P47_1","P47_2","P47_3","P47_4")){
    
    imp_val <- numeric(nr)
    for(i in 1:nr){
      value      <- as.numeric(names(table(as.numeric(mImp_var[[var]][i,]))))
      #value      <- value-1
      count      <- as.vector(table(as.numeric(mImp_var[[var]][i,])))
      mtch       <- which.max(count)
      imp_val[i] <- value[mtch]
    }; rm(i)
    names(imp_val) <- id_r
    
    imp_idata[id_r, paste(var)] <- imp_val
    return(imp_idata)
    
  } else {
    imp_val <- numeric(nr)
    for(i in 1:nr){
      value      <- as.numeric(names(table(as.numeric(mImp_var[[var]][i,]))))
      count      <- as.vector(table(as.numeric(mImp_var[[var]][i,])))
      mtch       <- which.max(count)
      imp_val[i] <- value[mtch]
    }; rm(i)
    names(imp_val) <- id_r
    
    imp_idata[id_r, paste(var)] <- imp_val
    return(imp_idata)
  }
  
}

for(j in 1:length(varList)){
  imp_idata <- imp_process(varList[j])
}; rm(j)

rm(varList)
rm(mImp_var)
rm(imp_process)

save.image(file="C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/results_innovation.RData")
write.csv(imp_idata, "C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/data_sets/excel/innovacion_variables_imputed.csv",row.names=F)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Verify similarity between independence tests
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

load("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/results_innovation.RData")

# Chi-square test for imputed data
options(warn=-1)
p.chisq.imp = matrix(0, nrow=ncol(imp_idata), ncol=ncol(imp_idata), byrow=T)
for(i in 1:ncol(imp_idata)){
  for(j in 1:ncol(imp_idata)){
    p.chisq.imp[i,j] = round(chisq.test(imp_idata[,i],imp_idata[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq.imp) = NA
colnames(p.chisq.imp) = colnames(imp_idata)
rownames(p.chisq.imp) = colnames(imp_idata)

#== Writting association matrix original
# write.csv(p.chisq.imp,"C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/correlation_analysis/chisq_matrix_imputed.csv",row.names=T)

library(gplots)
color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/correlation_analysis/association_matrix_imputed.png", width=1500, height=1500, pointsize=25)
plot.new()
heatmap.2(p.chisq.imp,
          main="Independence test",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=NULL,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(5,5))
dev.off()

rm(p.chisq.imp)

# Compare association matrices

# Original data
p.chisq.org <- read.csv("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/correlation_analysis/chisq_matrix_original.csv",row.names=1)

# Imputed data
p.chisq.imp <- read.csv("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/correlation_analysis/chisq_matrix_imputed.csv",row.names=1)

# Identify percent of significative associations
sig.asso.org <- as.matrix(p.chisq.org)
sig.asso.imp <- as.matrix(p.chisq.imp)

sig.asso.org <- sig.asso.org[upper.tri(sig.asso.org, diag=F)]
sig.asso.imp <- sig.asso.imp[upper.tri(sig.asso.imp, diag=F)]

sum(sig.asso.org < 0.05)/length(sig.asso.org)
sum(sig.asso.imp < 0.05)/length(sig.asso.imp)

rm(sig.asso.org); rm(sig.asso.imp)

p.chisq.org <- as.dist(p.chisq.org)
p.chisq.imp <- as.dist(p.chisq.imp)

# Mantel test
library(ade4)
mantel.res <- mantel.rtest(p.chisq.org, p.chisq.imp, nrepet=10000)

library(vegan)
mantel(p.chisq.org, p.chisq.imp, method="pearson", permutations=10000)
mantel(p.chisq.org, p.chisq.imp, method="spearman", permutations=10000)

# Observated correlation: 0.97
# Statistically significative

rm(p.chisq.org); rm(p.chisq.imp)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#== pendiente: hacer estadísticas descriptivas para todas las variables !!!
# Descriptive analysis for imputed data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

## Descriptive analysis
library(RColorBrewer)

freqCol <- lapply(colnames(imp_idata), function(x){table(imp_idata[paste(x)])})
names(freqCol) <- colnames(imp_idata)

# For CONTROL variables

ctrl_questions <- c("Sector", "Tamaño", "Antigüedad")
ctrl_labels <- list(sector=c("Primario", "Secundario", "Terciario"),
                    tamano=c("Pequeñas", "Medianas"),
                    antiguedad=c("1 a 5 años", "6 a 14 años", "15 o más años"))

op <- par(mfrow=c(1, 3), mar=c(2.5, 4.5, 2, 0.8)) # second 3.2
for(i in 1:3){
  distribution <- freqCol[[i]]/sum(freqCol[[i]]) * 100
  barplot(distribution, border=NA, col=brewer.pal(8, "Blues")[2:8],
          axes=FALSE, main=ctrl_questions[i], cex.main = 1,
          names.arg=ctrl_labels[[i]], ylab="Porcentaje")
  # add vertical axis, and rectangle around figure
  axis(side = 2, las=2)
  box("figure", col="gray70")
}
par(op)

# For HARDWARE variables

hard_var <- c(grep(names(imp_idata),pattern="P8_"),
              grep(names(imp_idata),pattern="P9_5"),
              grep(names(imp_idata),pattern="P10_"),
              grep(names(imp_idata),pattern="P11_1"),
              grep(names(imp_idata),pattern="P11_2"),
              grep(names(imp_idata),pattern="P11_6"),
              grep(names(imp_idata),pattern="P18_"))

length(hard_var)

hard_questions <- c("Impresora laser", "Impresora térmica", "L. código barras",
                    "Biometría dactilar", "Biometría escritura manual",
                    "Proyector", "TV plasma/LCD/LED", "Cámaras de seguridad",
                    "Computador portátil", "Equipos móviles")
hard_labels <- list(i_laser=c("No", "Si"), i_termica=c("No", "Si"),
                    c_barra=c("No", "Si"), dactilar=c("No", "Si"),
                    e_mano=c("No", "Si"), proyector=c("No", "Si"),
                    tv=c("No", "Si"), camaras=c("No", "Si"),
                    portatil=c("No", "Si"), movil=c("No", "Si"))

op <- par(mfrow=c(4, 3), mar=c(2.5, 4.5, 2, 0.8)) # second 3.2
for(i in 1:length(hard_var)){
  distribution <- freqCol[[hard_var[i]]]/sum(freqCol[[hard_var[i]]]) * 100
  barplot(distribution, border=NA, col=brewer.pal(8, "Blues")[2:8],
          axes=FALSE, main=hard_questions[i], cex.main = 1,
          names.arg=hard_labels[[i]], ylab="Porcentaje")
  # add vertical axis, and rectangle around figure
  axis(side = 2, las=2)
  box("figure", col="gray70")
}
par(op)

# For SOFTWARE variables

soft_var <- c(grep(names(imp_idata),pattern="P17_6"),
              grep(names(imp_idata),pattern="P17_18"),
              grep(names(imp_idata),pattern="P20_"),
              grep(names(imp_idata),pattern="P47_2"),
              grep(names(imp_idata),pattern="P47_3"),
              grep(names(imp_idata),pattern="P47_4"))

# For NETWORK variables

netw_var <- c(grep(names(imp_idata),pattern="P34_3"),
              grep(names(imp_idata),pattern="P34_5"),
              grep(names(imp_idata),pattern="P36_"),
              grep(names(imp_idata),pattern="P38"),
              grep(names(imp_idata),pattern="P39_"),
              grep(names(imp_idata),pattern="P41_"),
              grep(names(imp_idata),pattern="P45_"),
              grep(names(imp_idata),pattern="P46_"),
              grep(names(imp_idata),pattern="P47_1"))


# For STRATEGIC MANAGEMENT variables
dir_var <- c(grep(names(imp_idata),pattern="P6_9"),
             grep(names(imp_idata),pattern="P17_4"),
             grep(names(imp_idata),pattern="P34_9"))

dire_questions <- c("Estrategias-Equipos", "Estrategias-SO", "Estrategias-Internet")
dire_labels <- list(est_eq=c("En desacuerdo", "Neutral", "De acuerdo", "Totalmente de acuerdo"),
                    est_so=c("En desacuerdo", "Neutral", "De acuerdo", "Totalmente de acuerdo"),
                    est_it=c("En desacuerdo", "Neutral", "De acuerdo", "Totalmente de acuerdo"))

op <- par(mfrow=c(1, 3), mar=c(2.5, 4.5, 2, 0.8)) # second 3.2
for(i in 1:length(dir_var)){
  distribution <- freqCol[[dir_var[i]]]/sum(freqCol[[dir_var[i]]]) * 100
  barplot(distribution, border=NA, col=brewer.pal(8, "Blues")[2:8],
          axes=FALSE, main=dire_questions[i], cex.main = 1,
          names.arg=dire_labels[[i]], ylab="Porcentaje")
  # add vertical axis, and rectangle around figure
  axis(side = 2, las=2)
  box("figure", col="gray70")
}
par(op)

...

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Construct path model with categorical data without transformations
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Load imputed matrix
load("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/results_innovation.RData")
rm(idata); rm(dir); rm(IMP2)

identify.variables.to.model <- read.csv("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/correlation_analysis/chisq_matrix_imputed.csv", row.names=1)
identify.variables.to.model <- as.matrix(identify.variables.to.model)
identify.variables.to.model <- colnames(identify.variables.to.model)[as.numeric(which(apply(identify.variables.to.model, 2, function(x){sum(x<0.05, na.rm=T)}) > 0))]

imp_idata <- imp_idata[,identify.variables.to.model]
rm(identify.variables.to.model)

library(plspm)

# Path matrix (inner model realtionships)

HARD <- c(0, 0, 0, 0)
SOFT <- c(0, 0, 0, 0)
NETW <- c(0, 0, 0, 0)
DIRE <- c(1, 1, 1, 0)
dir_path <- rbind(HARD, SOFT, NETW, DIRE)
colnames(dir_path) <- rownames(dir_path)
rm(list=c("HARD","SOFT","NETW","DIRE"))

innerplot(dir_path)

# List indicating what variables are associated with what latent variables

hard_var <- c(grep(names(imp_idata),pattern="P8_"),
              grep(names(imp_idata),pattern="P9_"),
              grep(names(imp_idata),pattern="P10_"),
              grep(names(imp_idata),pattern="P11_"),
              grep(names(imp_idata),pattern="P18_"))

soft_var <- c(grep(names(imp_idata),pattern="P17_6",fixed=T),
              grep(names(imp_idata),pattern="P17_18",fixed=T),
              grep(names(imp_idata),pattern="P20_"),
              grep(names(imp_idata),pattern="P47_2",fixed=T),
              grep(names(imp_idata),pattern="P47_3",fixed=T),
              grep(names(imp_idata),pattern="P47_4",fixed=T))

netw_var <- c(grep(names(imp_idata),pattern="P34_3",fixed=T),
              grep(names(imp_idata),pattern="P34_5",fixed=T),
              grep(names(imp_idata),pattern="P36_"),
              grep(names(imp_idata),pattern="P38",fixed=T),
              grep(names(imp_idata),pattern="P39_"),
              grep(names(imp_idata),pattern="P41_"),
              grep(names(imp_idata),pattern="P45_"),
              grep(names(imp_idata),pattern="P46_"),
              grep(names(imp_idata),pattern="P47_1",fixed=T))

dir_var <- c(grep(names(imp_idata),pattern="P6_9",fixed=T),
             grep(names(imp_idata),pattern="P17_4",fixed=T),
             grep(names(imp_idata),pattern="P34_9",fixed=T))

# con_var <- c(grep(names(imp_idata),pattern="P2",fixed=T),
#              grep(names(imp_idata),pattern="P3",fixed=T),
#              grep(names(imp_idata),pattern="P4",fixed=T))

dir_blocks <- list(hard_var,
                  soft_var,
                  netw_var,
                  dir_var)

# Specifying scale of measurement

dir_scale <- list(rep("NOM",length(hard_var)),
                  c(rep("ORD",2),rep("NOM",9)),
                  c(rep("ORD",1),rep("NOM",25)),
                  rep("ORD",3))

# All latent variables are measured in a (reflective) way

dir_modes <- rep("B", 4)

# Run model

dir_pls <- plspm(imp_idata, dir_path, dir_blocks, scaling=dir_scale,
                 modes=dir_modes, scheme="centroid", plscomp=c(1,1,1,1), tol=0.0000001)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Construct path model with quantified data
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

library(plspm)

identify.variables.to.model <- read.csv("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/results/correlation_analysis/chisq_matrix_imputed.csv", row.names=1)
identify.variables.to.model <- as.matrix(identify.variables.to.model)
identify.variables.to.model <- colnames(identify.variables.to.model)[as.numeric(which(apply(identify.variables.to.model, 2, function(x){sum(x<0.05, na.rm=T)}) > 0))]

q_idata <- read.csv("C:/Users/haachicanoy/Documents/GitHub/PLS_path_modeling/data_sets/excel/innovacion_variables_cuantificadas.csv")
q_idata <- q_idata[,identify.variables.to.model]
rm(identify.variables.to.model)

q_idata <- scale(x=q_idata, center=T, scale=T)
q_idata <- as.data.frame(q_idata)

# Path matrix (inner model realtionships)

HARD <- c(0, 0, 0, 0)
SOFT <- c(0, 0, 0, 0)
NETW <- c(0, 0, 0, 0)
DIRE <- c(1, 1, 1, 0)
dir_path <- rbind(HARD, SOFT, NETW, DIRE)
colnames(dir_path) <- rownames(dir_path)
rm(list=c("HARD","SOFT","NETW","DIRE"))

# png("G:/Mis documentos/Proyectos/Universidad del Valle/Innovacion/results/model/inner_model.png", width=1500, height=1500, pointsize=25)
# innerplot(dir_path)
# dev.off()

# List indicating what variables are associated with what latent variables

hard_var <- c(grep(names(q_idata),pattern="P8_"),
              grep(names(q_idata),pattern="P9_"),
              grep(names(q_idata),pattern="P10_"),
              grep(names(q_idata),pattern="P11_"),
              grep(names(q_idata),pattern="P18_"))

soft_var <- c(grep(names(q_idata),pattern="P17_6",fixed=T),
              grep(names(q_idata),pattern="P17_18",fixed=T),
              grep(names(q_idata),pattern="P20_"),
              grep(names(q_idata),pattern="P47_2",fixed=T),
              grep(names(q_idata),pattern="P47_3",fixed=T),
              grep(names(q_idata),pattern="P47_4",fixed=T))

netw_var <- c(grep(names(q_idata),pattern="P34_3",fixed=T),
              grep(names(q_idata),pattern="P34_5",fixed=T),
              grep(names(q_idata),pattern="P36_"),
              grep(names(q_idata),pattern="P38",fixed=T),
              grep(names(q_idata),pattern="P39_"),
              grep(names(q_idata),pattern="P41_"),
              grep(names(q_idata),pattern="P45_"),
              grep(names(q_idata),pattern="P46_"),
              grep(names(q_idata),pattern="P47_1",fixed=T))

dir_var <- c(grep(names(q_idata),pattern="P6_9",fixed=T),
             grep(names(q_idata),pattern="P17_4",fixed=T),
             grep(names(q_idata),pattern="P34_9",fixed=T))

# con_var <- c(grep(names(q_idata),pattern="P2",fixed=T),
#              grep(names(q_idata),pattern="P3",fixed=T),
#              grep(names(q_idata),pattern="P4",fixed=T))

dir_blocks <- list(hard_var,
                   soft_var,
                   netw_var,
                   dir_var)

# PCA ANALYSIS BY LATENT VARIABLE

pca.hard <- PCA(q_idata[,hard_var], scale.unit=F, graph=FALSE)
plot.PCA(pca.hard, axes=c(1,2), choix="ind")
plot.PCA(pca.hard, axes=c(1,2), choix="var",title="Variables factor map (PCA)\nfor Hardware component")

pca.soft <- PCA(q_idata[,soft_var], scale.unit=F, graph=FALSE)
plot.PCA(pca.soft, axes=c(1,2), choix="ind")
plot.PCA(pca.soft, axes=c(1,2), ellipse=T, choix="var",title="Variables factor map (PCA)\nfor Software component")

pca.netw <- PCA(q_idata[,netw_var], scale.unit=F, graph=FALSE)
plot.PCA(pca.netw, axes=c(1,2), choix="ind")
plot.PCA(pca.netw, axes=c(1,2), choix="var",title="Variables factor map (PCA)\nfor Network component")

pca.dir <- PCA(q_idata[,dir_var], scale.unit=F, graph=FALSE)
pca.dir

# All latent variables are measured in a formative way

dir_modes <- rep("B", 4)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Run model
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# CASE 1. With all variables
set.seed(1235)
dir_pls <- plspm(q_idata, dir_path, dir_blocks, modes=dir_modes,
                 scheme="path", maxiter=500, tol=1e-10)
# Explore coefficients
plot(dir_pls)
# Elevado número de variables con cargas y pesos de signos cambiados
plot(dir_pls, what="loadings")
plot(dir_pls, what="weights")

# REPLANTEAR MODELO!!!

# CASE 2. Majority related with first principal component in PCA's analyses
hard_var2 <- rownames(pca.hard$var$contrib)[which(pca.hard$var$contrib[,1]>pca.hard$var$contrib[,2])]
soft_var2 <- rownames(pca.soft$var$contrib)[which(pca.soft$var$contrib[,1]>pca.soft$var$contrib[,2])]
netw_var2 <- rownames(pca.netw$var$contrib)[which(pca.netw$var$contrib[,1]>pca.netw$var$contrib[,2])]
dir_var2 <- c("P6_9", "P17_4","P34_9")

dir_blocks2 <- list(hard_var2,
                    soft_var2,
                    netw_var2,
                    dir_var2)

set.seed(1235)
dir_pls2 <- plspm(q_idata, dir_path, dir_blocks2, modes=dir_modes,
                  scheme="path", maxiter=500, tol=1e-10)
# Explore coefficients
plot(dir_pls2)
# Elevado número de variables con cargas y pesos de signos cambiados
plot(dir_pls2, what="loadings")
plot(dir_pls2, what="weights")

# REPLANTEAR MODELO!!!

# CASE 3. Loadings and weights positive from model with all variables

dir_blocks3 <- list(hard_var3=as.character(dir_pls$outer_model$name[which(dir_pls$outer_model$block=="HARD" & dir_pls$outer_model$weight>0 & dir_pls$outer_model$loading>0)]),
                    soft_var3=as.character(dir_pls$outer_model$name[which(dir_pls$outer_model$block=="SOFT" & dir_pls$outer_model$weight>0 & dir_pls$outer_model$loading>0)]),
                    netw_var3=as.character(dir_pls$outer_model$name[which(dir_pls$outer_model$block=="NETW" & dir_pls$outer_model$weight>0 & dir_pls$outer_model$loading>0)]),
                    dir_var2)

set.seed(1235)
dir_pls3 <- plspm(q_idata, dir_path, dir_blocks3, modes=dir_modes,
                  scheme="path", maxiter=500, tol=1e-10)
plot(dir_pls3)
plot(dir_pls3, what="loadings")
plot(dir_pls3, what="weights")

# Cross-loadings
library(ggplot2)
library(reshape)

xloads = melt(dir_pls3$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")

# bar-charts of crossloadings by block
p <- ggplot(data=xloads, aes(x=name, y=value, fill=block))
p <- p + geom_hline(yintercept=0, color="gray75")
p <- p + geom_hline(yintercept=0.5, color="gray70", linetype=2)
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + facet_wrap(block ~ LV)
p <- p + theme(axis.text.x = element_text(angle=90),
               line = element_blank(),
               plot.title = element_text(size=12))
p <- p + ggtitle("Crossloadings")
p

# Explore multicollinearity between variables of each block
library(usdm)
vifcor(q_idata[,dir_blocks3[[1]]]) # Hardware block: ok
vifcor(q_idata[,dir_blocks3[[2]]]) # Software block: ok
vifcor(q_idata[,dir_blocks3[[3]]]) # Network block: ok

# Problemas de multicolinealidad
dir.pls3.boot <- plspm(q_idata, dir_path, dir_blocks3, modes=dir_modes,
                       scheme="path", maxiter=500, tol=1e-10, boot.val=T, br=100)

dir_pls3$effects

dir_pls$scores
pairs(dir_pls3$scores, pch=20, cex=1.2, col="forestgreen")

round(dir_pls$inner_model$DIRE, 3)

scores2 <- dir_pls3$scores
scores2 <- as.data.frame(scores2)

cor(scores2)

# Regresión Ridge para los constructos obtenidos
library(ridge)
fitRidge <- linearRidge(DIRE~., data=scores2)
summary(fitRidge)

# Ajustar un modelo de regresión lineal múltiple con corrección de
# multicolinealidad mediante técnicas de regularización

library(glmnet)
fitReg <- glmnet(x=as.matrix(scores2[,1:3]), y=scores2[,4],family="gaussian")
summary(fitReg)
fitReg$beta # Coeficientes del modelo

# ========================================================================= #
# Test with regularization process
# ========================================================================= #

set.seed(100)
train.index <- sample(1:dim(scores2)[1],74,replace=FALSE)
tr <- scores2[train.index,]
vl <- scores2[-train.index,]

lm.fit1 <- lm(DIRE~., data=tr)
summary(lm.fit1)

lm.pred1 <- predict(lm.fit1, newdata=vl)
sqrt(mean((lm.pred1 - vl$DIRE)^2))

library(glmnet)

# USING RIDGE REGRESSION

x.tr <- model.matrix(DIRE~., data=tr)[,-1]
y.tr <- tr$DIRE
x.vl <- model.matrix(DIRE~., data=vl)[,-1]
y.vl <- vl$DIRE

# CV to obtain best lambda
set.seed(10)
rr.cv <- cv.glmnet(x.tr, y.tr, alpha = 0)
plot(rr.cv)

rr.bestlam <- rr.cv$lambda.min
rr.goodlam <- rr.cv$lambda.1se

# predict validation set using best lambda and calculate RMSE
rr.fit <- glmnet(x.tr, y.tr, alpha = 0)
plot(rr.fit, xvar="lambda", label=TRUE)

rr.pred <- predict(rr.fit, s=rr.bestlam, newx=x.vl)
sqrt(mean((rr.pred - y.vl)^2))

# USING LASSO REGULARIZATION PROCESS

# CV to obtain best lambda
set.seed(10)
las.cv <- cv.glmnet(x.tr, y.tr, alpha=1)
plot(las.cv)

las.bestlam <- las.cv$lambda.min
las.goodlam <- las.cv$lambda.1se

# predict validation set using best lambda and calculate RMSE
las.fit <- glmnet(x.tr, y.tr, alpha=1)
plot(las.fit, xvar="lambda", label=TRUE)

las.pred <- predict(las.fit, s=las.bestlam, newx=x.vl)
sqrt(mean((las.pred - y.vl)^2))

# ========================================================================= #

write.csv(scores2, "G:/Mis documentos/Proyectos/Universidad del Valle/Innovacion/results/model/scores_final_model.csv", row.names=FALSE)

scores <- read.csv("G:/Mis documentos/Proyectos/Universidad del Valle/Innovacion/results/model/scores_sorted.csv")
scores$SCORE_TYPE <- gsub(pattern="HARD",replacement="Hardware",scores$SCORE_TYPE)
scores$SCORE_TYPE <- gsub(pattern="SOFT",replacement="Software",scores$SCORE_TYPE)
scores$SCORE_TYPE <- gsub(pattern="NETW",replacement="Redes",scores$SCORE_TYPE)

library(ggplot2)
library(lattice)

fit.hard <- lm(scores2$DIRE~scores2$HARD)
fit.soft <- lm(scores2$DIRE~scores2$SOFT)
fit.netw <- lm(scores2$DIRE~scores2$NETW)

min=fit.hard$fitted.values

predict()

# Opción 1
p <- ggplot(data=scores, aes(x=SCORE,y=DIRE,colour=SCORE_TYPE))# + theme_bw()
#p <- p + geom_blank()
p <- p + xlab(label="Innovación") + ylab("Direccionamiento estratégico")
p <- p + geom_point(stat="identity",size=3.0, alpha=.5)
p <- p + theme(panel.grid.minor.x = element_blank(),
               panel.grid.minor.y = element_blank(),
               axis.text.x = element_text(size=15),
               axis.text.y = element_text(size=15),
               axis.title.x = element_text(face="bold",size=16),
               axis.title.y = element_text(face="bold",size=16))
p <- p + scale_colour_manual(name="Componente",
                             labels=c("Hardware","Redes","Software"),
                             values=c("blue","darkgreen","red3"))
p <- p + geom_smooth(method=lm, se=FALSE, alpha=.5, size=5)
p


geom_ribbon()

layer(data=pred.similarity.all,
      geom='ribbon',mapping=aes(
        x=Year,ymin=ploSim,ymax=phiSim,fill=Measurement),alpha=0.7)

# Opción 2
p <- ggplot(data=scores, aes(x=SCORE,y=DIRE,colour=SCORE_TYPE))# + theme_bw()
#p <- p + geom_blank()
p <- p + facet_wrap(~SCORE_TYPE)
p <- p + xlab(label="Innovación") + ylab("Direccionamiento estratégico")
p <- p + geom_point(stat="identity",size=3.0, alpha=.5)
p <- p + theme(panel.grid.minor.x = element_blank(),
               panel.grid.minor.y = element_blank(),
               axis.text.x = element_text(size=15),
               axis.text.y = element_text(size=15),
               axis.title.x = element_text(face="bold",size=16),
               axis.title.y = element_text(face="bold",size=16),
               strip.text.x = element_text(size=17,face="bold"),
               strip.text.y = element_text(size=17,face="bold"))
p <- p + scale_colour_manual(name="Componente",
                             breaks=c("HARD","NETW","SOFT"),
                             labels=c("Hardware","Redes","Software"),
                             values=c("blue","darkgreen","red3"))
p <- p + geom_smooth(method=lm, se=TRUE, size=.8, alpha=.5)
p
ggsave(p, file="G:/Mis documentos/Proyectos/Universidad del Valle/Innovacion/results/model/innovacion_vs_dir_estrategico.png", width=10, height=6)





model <- lm(DIRE ~ SCORE + factor(SCORE_TYPE), data=scores)
grid <- with(scores, expand.grid(SCORE=seq(min(SCORE),max(SCORE),length=20), SCORE_TYPE=levels(factor(SCORE_TYPE))))
grid$DIRE <- stats::predict(model, newdata=grid)

qplot(wt, mpg, data=mtcars, colour=factor(cyl)) + geom_line(data=grid)







coef <- rbind(fit.hard$coefficients,
              fit.soft$coefficients,
              fit.netw$coefficients)
coef <- as.data.frame(coef)
names(coef) <- c("Intercept","Slope")
coef <- round(coef, 3)
coef$min <- algo


pairs(scores, col="blue")

cor(scores)

fit <- lm(DIRE~HARD+SOFT+NETW, scores)
plot(fit)

hist(fit$residuals)

library(nortest)

shapiro.test(fit$residuals)

cor.test(scores$HARD, fit$residuals, method="pearson")
cor.test(scores$SOFT, fit$residuals, method="pearson")
cor.test(scores$NETW, fit$residuals, method="pearson")

cor.test(fit$fitted.values, fit$residuals, method="pearson")

plot(fit$fitted.values, fit$residuals)
plot(scores$DIRE, fit$residuals)

# Incluir caret para validar resultados del modelo.

library(caret)

lp.plspm <- list(type="Regression",
                       library="plspm",
                       loop=NULL)

prm <- data.frame(parameter = c(),
                  class = c(),
                  label = c())

inTrain <- createDataPartition()



