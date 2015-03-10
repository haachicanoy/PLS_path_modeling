# H. Achicanoy Estrella
# Universidad del Valle
# 2014

library(plspm)
library(FactoMineR)

# ======================================================================= #
# Cargar datos cuantificados
# ======================================================================= #

h_data <- read.csv(paste(dir,"/hardware.csv",sep=""),header=T)
s_data <- read.csv(paste(dir,"/software.csv",sep=""),header=T)
n_data <- read.csv(paste(dir,"/networks.csv",sep=""),header=T)
d_data <- read.csv(paste(dir,"/direccionamiento.csv",sep=""),header=T)
c_data <- read.csv(paste(dir,"/control.csv",sep=""),header=T)

# ======================================================================= #
# PCA Hardware
# ======================================================================= #

names(h_data)
for(i in 1:ncol(h_data)){
  h_data[,i] <- as.character(h_data[,i])
  h_data[,i] <- as.numeric(h_data[,i])
}; rm(i)

pca_hard <- PCA(X=h_data,scale.unit=F,ncp=5,quanti.sup=1:3)

# ======================================================================= #
# PCA Software
# ======================================================================= #

names(s_data)
for(i in 1:ncol(s_data)){
  s_data[,i] <- as.character(s_data[,i])
  s_data[,i] <- as.numeric(s_data[,i])
}; rm(i)

pca_soft <- PCA(X=s_data,scale.unit=F,ncp=5,quanti.sup=1:3)
#pca_soft$var$contrib[,1]

# Potenciales variables a excluir:
# P17_6: Sistemas de información en el desarrollo de mejores productos
# P17_18: Renovación de software

# ======================================================================= #
# PCA Redes
# ======================================================================= #

names(n_data)
for(i in 1:ncol(n_data)){
  n_data[,i] <- as.character(n_data[,i])
  n_data[,i] <- as.numeric(n_data[,i])
}; rm(i)

pca_netw <- PCA(X=n_data,scale.unit=F,ncp=5,quanti.sup=1:3)
#pca_netw$var$contrib[,1]

# Potenciales variables a excluir:
# P34_3: Red de datos entre sucursales

# ======================================================================= #
# PCA Direccionamiento estratégico
# ======================================================================= #

names(d_data)
for(i in 1:ncol(d_data)){
  d_data[,i] <- as.character(d_data[,i])
  d_data[,i] <- as.numeric(d_data[,i])
}; rm(i)

pca_dire <- PCA(X=d_data, scale.unit=F, ncp=5)

# ======================================================================= #
# PCA Control
# ======================================================================= #

names(c_data)
for(i in 1:ncol(c_data)){
  c_data[,i] <- as.character(c_data[,i])
  c_data[,i] <- as.numeric(c_data[,i])
}; rm(i)

pca_ctrl <- PCA(X=c_data, scale.unit=F, ncp=5)

# ======================================================================= #
# Análisis factorial múltiple con datos faltantes
# ======================================================================= #

qmfa_data <- cbind(h_data[,-c(1:3)],
                   s_data[,-c(1:3)],
                   n_data[,-c(1:3)],
                   d_data, c_data)

qmfa_res <- MFA(qmfa_data, group=c(ncol(h_data[,-c(1:3)]),
                                  ncol(s_data[,-c(1:3)]),
                                  ncol(n_data[,-c(1:3)]),
                                  ncol(d_data), ncol(c_data)),
               type=rep("s",5),
               name.group=c("Hardware",
                            "Software",
                            "Redes",
                            "Direccionamiento estratégico",
                            "Control"))

# Relación entre tablas de datos
round(qmfa_res$group$RV,3)

# ======================================================================= #
# Análisis factorial múltiple con datos completos
# ======================================================================= #

qmfa_data1 <- qmfa_data
qmfa_data1 <- qmfa_data1[complete.cases(qmfa_data1),]
rownames(qmfa_data1) <- 1:nrow(qmfa_data1)

qmfa_res1 <- MFA(qmfa_data1, group=c(ncol(h_data[,-c(1:3)]),
                                     ncol(s_data[,-c(1:3)]),
                                     ncol(n_data[,-c(1:3)]),
                                     ncol(d_data), ncol(c_data)),
                 type=rep("s",5),
                 name.group=c("Hardware",
                              "Software",
                              "Redes",
                              "Direccionamiento estratégico",
                              "Control"))

# Relación entre tablas de datos
round(qmfa_res1$group$RV,3)

# ======================================================================= #
# PLS-PM: model 0
# Se incluyen los constructos de interés: innovación en Hardware, Software,
# Redes y Direccionamiento estratégico (sin considerar las v. de control)
# ======================================================================= #

# Path matrix
HARD <- c(0, 0, 0, 0)
SOFT <- c(0, 0, 0, 0)
NETW <- c(0, 0, 0, 0)
DIRE <- c(1, 1, 1, 0)
dir_path0 <- rbind(HARD, SOFT, NETW, DIRE)
colnames(dir_path0) <- rownames(dir_path0)
rm(HARD, SOFT, NETW, DIRE)

# Model visualization
innerplot(dir_path0)

# Define list of indicators
dir_blocks0 <- list(as.numeric(na.omit(match(names(h_data[,-c(1:3)]),names(qmfa_data1)))),
                    as.numeric(na.omit(match(names(s_data[,-c(1:3)]),names(qmfa_data1)))),
                    as.numeric(na.omit(match(names(n_data[,-c(1:3)]),names(qmfa_data1)))),
                    as.numeric(na.omit(match(names(d_data),names(qmfa_data1)))))

# Formative mode for all blocks
dir_modes0 <- c("B", "B", "B", "B")

## Run plspm analysis
dir_pls0 <- plspm(Data=qmfa_data1, path_matrix=dir_path0, blocks=dir_blocks0,
                  modes=dir_modes0, scaled=TRUE, scheme="path")

## Plotting results
innerplot(dir_pls0)
outerplot(dir_pls0)

Scores <- as.data.frame(dir_pls0$scores)
colnames(Scores) <- c("Hardware","Software","Redes","Direccionamiento\nestratégico")
pairs(Scores, pch=20, upper.panel=NULL, lower.panel=panel.smooth, col="blue", cex=1.2)

## Validate model with bootstrap procedure
dir_plsBoot0 <- plspm(Data=qmfa_data1, path_matrix=dir_path0, blocks=dir_blocks0,
                      modes=dir_modes0, scaled=TRUE, scheme="path", boot.val=TRUE,
                      maxiter=200)
dir_plsBoot0$boot

# Gráfico de cargas cruzadas
library(ggplot2)
library(reshape)
xloads = melt(dir_pls0$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")
p <- ggplot(data=xloads, aes(x=name, y=value, fill=block))
p <- p + geom_hline(yintercept=0, color="gray75")
p <- p + geom_hline(yintercept=0.5, color="gray70", linetype=2)
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + facet_wrap(block ~ LV)
p <- p + theme(axis.text.x=element_text(angle=90),
               line=element_blank(),
               plot.title=element_text(size=14,face="bold"))
p <- p + ggtitle("Cargas cruzadas")
p <- p + labs(fill="Constructo")
p <- p + scale_fill_discrete(labels=c("Hardware", "Software", "Redes", "Direccionamiento\nestratégico"))
p <- p + xlab("Variable")
p <- p + ylab("")
p


############################################################################
## Seleccionar variables con cargas mayores al |50%|
############################################################################

varPLS <- as.character(dir_pls0$outer_model$name[abs(dir_pls0$outer_model$loading) > 0.5])
blcPLS <- as.character(dir_pls0$outer_model$block[abs(dir_pls0$outer_model$loading) > 0.5])

dir_blockSel0 <- list(0)
for(i in 1:length(unique(blcPLS)))
{
  dir_blockSel0[[paste(unique(blcPLS)[i])]] <- varPLS[blcPLS==unique(blcPLS)[i]]
}
dir_blockSel0 <- dir_blockSel0[-1]
rm(varPLS, blcPLS)

# Reajustar modelo
dir_plsRe0 <- plspm(Data=qmfa_data1, path_matrix=dir_path0, blocks=dir_blockSel0,
                    modes=dir_modes0, scaled=TRUE, scheme="path")

innerplot(dir_plsRe0)
outerplot(dir_plsRe0)

reScores <- as.data.frame(dir_plsRe0$scores)
colnames(reScores) <- c("Hardware","Software","Redes","Direccionamiento\nestratégico")
pairs(reScores, pch=20, upper.panel=NULL, lower.panel=panel.smooth, col="blue", cex=1.2)

# Validar modelo reajustado
dir_plsRe0Boot <- plspm(Data=qmfa_data1, path_matrix=dir_path0, blocks=dir_blockSel0,
                        modes=dir_modes0, scaled=T, scheme="path", boot.val=T, maxiter=500)
dir_plsRe0Boot$boot # Pasa el procedimiento de validación para Software

# Gráfico de cargas cruzadas
library(ggplot2)
library(reshape)
xloads = melt(dir_plsRe0$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")
p <- ggplot(data=xloads, aes(x=name, y=value, fill=block))
p <- p + geom_hline(yintercept=0, color="gray75")
p <- p + geom_hline(yintercept=0.5, color="gray70", linetype=2)
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + facet_wrap(block ~ LV)
p <- p + theme(axis.text.x=element_text(angle=90),
               line=element_blank(),
               plot.title=element_text(size=14,face="bold"))
p <- p + ggtitle("Cargas cruzadas")
p <- p + labs(fill="Constructo")
p <- p + scale_fill_discrete(labels=c("Hardware", "Software", "Redes", "Direccionamiento\nestratégico"))
p <- p + xlab("Variable")
p <- p + ylab("")
p

# ======================================================================= #
# PLS-PM: model 1
# Se incluyen los constructos de interés: innovación en Hardware, Software,
# Redes, Control y Direccionamiento estratégico. En caso de no ser
# significativos de forma individual, se exploran las interacciones
# entre los constructos y las variables de control.
# ======================================================================= #

# Path matrix
HARD <- c(0, 0, 0, 0, 0)
SOFT <- c(0, 0, 0, 0, 0)
NETW <- c(0, 0, 0, 0, 0)
CTRL <- c(0, 0, 0, 0, 0)
DIRE <- c(1, 1, 1, 1, 0)
dir_path1 <- rbind(HARD, SOFT, NETW, CTRL, DIRE)
colnames(dir_path1) <- rownames(dir_path1)
rm(HARD, SOFT, NETW, CTRL, DIRE)

# Model visualization
innerplot(dir_path1)

# Define list of indicators
dir_blocks1 <- list(as.numeric(na.omit(match(names(h_data[,-c(1:3)]),names(qmfa_data1)))),
                    as.numeric(na.omit(match(names(s_data[,-c(1:3)]),names(qmfa_data1)))),
                    as.numeric(na.omit(match(names(n_data[,-c(1:3)]),names(qmfa_data1)))),
                    as.numeric(na.omit(match(names(c_data),names(qmfa_data1)))),
                    as.numeric(na.omit(match(names(d_data),names(qmfa_data1)))))

# Formative mode for all blocks
dir_modes1 <- c("B", "B", "B", "B", "B")

## Run plspm analysis
dir_pls1 <- plspm(Data=qmfa_data1, path_matrix=dir_path1, blocks=dir_blocks1,
                  modes=dir_modes1, scaled=TRUE, scheme="path")

## Plotting results
innerplot(dir_pls1)
outerplot(dir_pls1)
pairs(dir_pls1$scores, pch=20, col="blue", cex=1.2)

## Validate model with bootstrap procedure
dir_plsBoot1 <- plspm(Data=qmfa_data1, path_matrix=dir_path1, blocks=dir_blocks1,
                      modes=dir_modes1, scaled=TRUE, scheme="path", boot.val=TRUE,
                      maxiter=200)
dir_plsBoot1$boot

############################################################################
## Seleccionar variables con cargas mayores al |50%|
############################################################################

varPLS <- as.character(dir_pls1$outer_model$name[abs(dir_pls1$outer_model$loading) > 0.5])
blcPLS <- as.character(dir_pls1$outer_model$block[abs(dir_pls1$outer_model$loading) > 0.5])

dir_blockSel1 <- list(0)
for(i in 1:length(unique(blcPLS)))
{
  dir_blockSel1[[paste(unique(blcPLS)[i])]] <- varPLS[blcPLS==unique(blcPLS)[i]]
}
dir_blockSel1 <- dir_blockSel1[-1]
rm(varPLS, blcPLS)

# Reajustar modelo
dir_plsRe1 <- plspm(Data=qmfa_data1, path_matrix=dir_path1, blocks=dir_blockSel1,
                    modes=dir_modes1, scaled=TRUE, scheme="path")

innerplot(dir_plsRe1)
outerplot(dir_plsRe1)
pairs(dir_plsRe1$scores, pch=20, col="blue", cex=1.2)

# Validar modelo reajustado
dir_plsRe1Boot <- plspm(Data=qmfa_data1, path_matrix=dir_path1, blocks=dir_blockSel1,
                        modes=dir_modes1, scaled=T, scheme="path", boot.val=T, maxiter=200)
dir_plsRe1Boot$boot

## Include interactions
dir_Scores1 <- as.data.frame(dir_plsRe1$scores)
dir_Scores1$HARD_CTRL <- dir_Scores1$HARD * dir_Scores1$CTRL
dir_Scores1$SOFT_CTRL <- dir_Scores1$SOFT * dir_Scores1$CTRL
dir_Scores1$NETW_CTRL <- dir_Scores1$NETW * dir_Scores1$CTRL

op = par(mfrow = c(2, 4), mar = c(4, 5, 2, 2), bty = "n")
for (j in 1:8)
{
  # calculate density
  score.dens = density(dir_Scores1[,j])
  # plot window but don't show the density
  plot(score.dens, main = names(dir_Scores1)[j], xlab = "", type = "n")
  # add polygon
  polygon(score.dens$x, score.dens$y, col = "gray90", border = "gray80")
}
par(op)

# Include moderating effects
two_path1 <- matrix(c(0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,
                      0,0,0,0,0,0,0,0,
                      1,1,1,1,1,1,1,0),nrow=8,ncol=8,byrow=T)
rownames(two_path1) <- c("HARD","SOFT","NETW","CTRL",
                        "HARD_CTRL","SOFT_CTRL","NETW_CTRL","DIRE")
colnames(two_path1) <- c("HARD","SOFT","NETW","CTRL",
                        "HARD_CTRL","SOFT_CTRL","NETW_CTRL","DIRE")

# define list of blocks
two_blocks1 = list(1, 2, 3, 4, 6, 7, 8, 5)

# define reflective indicators
two_modes1 = rep("A", 8)

# run plspm analysis with bootstrap validation (200 resamples)
two_pls1 <- plspm(dir_Scores1, two_path1, two_blocks1, modes=two_modes1,
                  boot.val=TRUE, br=200)

innerplot(two_pls1)
outerplot(two_pls1)
round(two_pls1$boot$paths, 4)
pairs(two_pls1$scores, col="blue", cex=1.2)

# Hasta aquí tengo bien definido el primer modelo. Incluyendo la validación
# mediante bootstrap. SIN ELIMINAR NINGÚN INDICADOR.

# ======================================================================= #
# REAJUSTAR MODELO CONSERVANDO: cargas superiores a 50 puntos
# ======================================================================= #

varPLS <- as.character(dir_pls1$outer_model$name[abs(dir_pls1$outer_model$loading) > 0.5])
blcPLS <- as.character(dir_pls1$outer_model$block[abs(dir_pls1$outer_model$loading) > 0.5])

dir_blockSel1 <- list(0)
for(i in 1:length(unique(blcPLS)))
{
  dir_blockSel1[[paste(unique(blcPLS)[i])]] <- varPLS[blcPLS==unique(blcPLS)[i]]
}
dir_blockSel1 <- dir_blockSel1[-1]

# Reajustar el modelo

dir_plsRe1 <- plspm(Data=qmfa_data, path_matrix=dir_path, blocks=dir_blockSel1,
                    modes=dir_modes, scaled=TRUE, scheme="path")

innerplot(dir_plsRe1)
outerplot(dir_plsRe1)

# Validar modelo reajustado
dir_plsRe1Boot <- plspm(Data=qmfa_data, path_matrix=dir_path, blocks=dir_blockSel1,
                        modes=dir_modes, scaled=T, scheme="path", boot.val=T, maxiter=200)
dir_plsRe1Boot$boot

# Include interactions

Scores <- as.data.frame(dir_plsRe1$scores)
Scores$Hardware_control <- Scores$HARD*Scores$CTRL
Scores$Software_control <- Scores$SOFT*Scores$CTRL
Scores$Redes_control <- Scores$NETW*Scores$CTRL

op = par(mfrow = c(2, 4), mar = c(4, 5, 2, 2), bty = "n")
for (j in 1:8)
{
  # calculate density
  score.dens = density(Scores[,j])
  # plot window but don't show the density
  plot(score.dens, main = names(Scores)[j], xlab = "", type = "n")
  # add polygon
  polygon(score.dens$x, score.dens$y, col = "gray90", border = "gray80")
}
par(op)

# With moderating effects (falta)

two_path <- matrix(c(0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,
                     1,1,1,1,1,1,1,0),nrow=8,ncol=8,byrow=T)
rownames(two_path) <- c("HARD","SOFT","NETW","CTRL",
                        "HARD_CTRL","SOFT_CTRL","NETW_CTRL","DIRE")
colnames(two_path) <- c("HARD","SOFT","NETW","CTRL",
                        "HARD_CTRL","SOFT_CTRL","NETW_CTRL","DIRE")

# define list of blocks
two_blocks = list(1, 2, 3, 4, 6, 7, 8, 5)

# define reflective indicators
two_modes= rep("A", 8)

# run plspm analysis with bootstrap validation (200 resamples)
two_pls = plspm(Scores, two_path, two_blocks, modes = two_modes,
                boot.val = TRUE, br = 200)

innerplot(two_pls)
outerplot(two_pls)
round(two_pls$boot$paths, 4)

# Other option
# Linear regression

pairs(Scores)

# Validation

dir_pls_val <- plspm(Data=qmfa_data, path_matrix=dir_path, blocks=dir_blocks,
                     modes=dir_modes, scaled=TRUE, scheme="path", boot.val=T, br=200)

# ======================================================================= #
# Seleccionar variables con un aporte positivo a la construcción del
# primer eje factorial y tienen una alta contribución en el ACP
# ======================================================================= #

pca_results <- list(Hardware = pca_hard,
                    Software = pca_soft,
                    Networks = pca_netw,
                    Direccionamiento = pca_dire)

rm(list=c("pca_hard","pca_soft","pca_netw","pca_dire"))

select_fun <- function(pca){
  
  pca <- pca
  var <- rownames(pca$var$coord)
  #coor <- pca$var$coord
  #coor <- coor[,1]
  cont <- pca$var$contrib
  cont1 <- cont[,1]
  cont2 <- cont[,2]
  
  select <- 0
  for(i in 1:length(var)){
    if(cont1[i]>mean(cont1) | cont2[i]>mean(cont)){
      select[i] <- var[i]
    } else {
      select[i] <- NA
    }
  }; rm(i)
  
  select <- na.omit(select)
  select <- as.character(select)
  return(select)
  
}
select_var <- lapply(pca_results, select_fun)

# ======================================================================= #
# PLS-PM con variables seleccionadas a través del criterio PCA
# ======================================================================= #

dir_blocks1 <- list(select_var$Hardware,
                    select_var$Software,
                    select_var$Networks,
                    select_var$Direccionamiento)

dir_pls2 <- plspm(Data=qmfa_data,
                  maxiter=1000,
                  tol=0.00000000000000000000000001,
                  path_matrix=dir_path,
                  blocks=dir_blocks1,
                  modes=dir_modes,
                  scaled=TRUE,
                  scheme="path", boot.val=T, br=200)

innerplot(dir_pls2)
outerplot(dir_pls2)

dir_pls2$boot

# ======================================================================= #
# PLS-PM con variables de contribución positiva en el PLS-PM previo
# ======================================================================= #

# Definir bloques de variables
dir_blocks2 <- list(c("P8_2","P11_1","P18_2"),
                    c("P47_3","P47_4"),
                    c("P41_3","P46_2","P46_3","P47_1"),
                    c("P6_9","P17_4","P34_9"))

dir_pls3 <- plspm(Data=qmfa_data, path_matrix=dir_path, blocks=dir_blocks2,
                  modes=dir_modes, scaled=TRUE, scheme="path")#, boot.val=T, br=200)

innerplot(dir_pls3)
outerplot(dir_pls3)

summary(dir_pls3)
dir_pls3$outer_model
dir_pls3$inner_model
dir_pls3$inner_summary
dir_pls3$unidim
dir_pls3$scores
dir_pls3$crossloadings
dir_pls3$gof

library(ggplot2)
library(reshape)

xloads = melt(dir_pls3$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")

ggplot(data = xloads, aes(x = name, y = value, fill = block)) +
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(block ~ LV) +
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  ggtitle("Crossloadings")
  

dir_pls3$inner_model
dir_pls3$inner_summary

dir_pls3$gof

# Validation of model

dir_val <- plspm(Data=qmfa_data, path_matrix=dir_path, blocks=dir_blocks2,
                 modes=dir_modes, scaled=FALSE, scheme="path", boot.val=T,
                 br=200)

dir_val$boot

# Otro modelo

# Definir bloques de variables
dir_blocks3 <- list(c("P8_2","P11_1","P18_2"),
                    c("P47_3","P47_4"),
                    c("P46_2","P46_3"),
                    c("P6_9","P34_9"))

dir_pls3 <- plspm(Data=qmfa_data, path_matrix=dir_path, blocks=dir_blocks3,
                  modes=dir_modes, scaled=TRUE, scheme="path")#, boot.val=T, br=200)

innerplot(dir_pls3)
outerplot(dir_pls3)

dir_pls3$scores

plot(dir_pls3$scores[,1], dir_pls3$scores[,4],
     xlab="Innovación en Hardware",
     ylab="Direccionamiento estratégico",
     pch=20, cex=2, col=4)

plot(dir_pls3$scores[,2], dir_pls3$scores[,4],
     xlab="Innovación en Software",
     ylab="Direccionamiento estratégico",
     pch=20, cex=2, col=4)

plot(dir_pls3$scores[,3], dir_pls3$scores[,4],
     xlab="Innovación en Redes",
     ylab="Direccionamiento estratégico",
     pch=20, cex=2, col=4)

# Validation

dir_val3 <- plspm(Data=qmfa_data, path_matrix=dir_path, blocks=dir_blocks3,
                 modes=dir_modes, scaled=TRUE, scheme="path", boot.val=T,
                 br=200)

dir_val3$boot
