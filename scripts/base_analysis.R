# Universidad del Valle
# Innovation article
# H. Achicanoy, 2014


# Load packages
library(gplots)
library(FactoMineR)

# Define directories
path <- "G:/Mis documentos/Proyectos/Universidad del Valle/Innovacion"

# Load data
data <- read.csv(paste(path,"/Data_sets/innovation.csv",sep=""),header=T)
summary(data)
dim(data)
# 107 companies
# 100 variables

# Number of missing data for variable
missing <- cbind(unlist(apply(data,2,function(x){sum(is.na(x))})))
colnames(missing) = "Count"
missing
rm(missing)

# Filtering data
# 1. Omitir variables cuantitativas
data <- data[,-1]
dim(data)

# 2. Omitir variables que provienen de una pregunta filtro
miss_var <- c("eq_escritorio_windows",
              "eq_escritorio_linux",
              "eq_escritorio_mac",
              "eq_portatil_windows",
              "eq_portatil_linux",
              "eq_portatil_mac",
              "eq_servidores_windows",
              "eq_servidores_linux",
              "eq_servidores_solaris",
              "eq_servidores_otro",
              "eq_servidores_no_sabe",
              "eq_movil_windows",
              "eq_movil_mac",
              "eq_movil_android",
              "eq_movil_otro",
              "eq_movil_no_sabe")
mtch <- match(miss_var,colnames(data))
mtch <- setdiff(1:ncol(data),mtch)
data <- data[,mtch]

rm(mtch)
rm(miss_var)
dim(data)
# 107 companies
# 83 variables

# 3. Omitir variables sin variabilidad, con un aporte mínimo en términos
# de información, es decir, con bajo número de respuestas por categoria
wtot_var <- c("idatos_tdigitalizadora",
              "idatos_l_optico",
              "audv_control_turnos",
              "audv_tab_digitales",
              "eq_escritorio",
              "soft_prezi_desktop",
              "soft_ibm_lotus",
              "soft_prezi_online",
              "soft_ms_office365",
              "soft_ninguno",
              "red_datos",
              "sc_internet")
mtch <- match(wtot_var,colnames(data))
mtch <- setdiff(1:ncol(data),mtch)
data <- data[,mtch]

rm(mtch)
rm(wtot_var)

dim(data)
# 107 companies
# 71 variables

# Formating data

data$antigüedad <- factor(as.character(data$antigüedad),
                          levels=c("1","2","3"),ordered=T)

ord_var <- c("efectividad_pc",
             "eq_estrategias",
             "SI_estrategias",
             "efectividad_soft",
             "SI_desarrollo_productos",
             "Renovacion_soft",
             "SI_productividad",
             "red_sucursales",
             "red_eq_internet",
             "internet_estrategias",
             "aprov_efectivo_red")

mlevels <- c("Strongly_Disagree","Disagree","Neutral","Agree","Strongly_Agree")

# Define categories for each variable
for(j in 1:length(ord_var)){
  
  var <- data[,ord_var[j]]
  var <- unique(as.character(var))
  
  if(length(var) > 1){
    var <- var[na.omit(match(mlevels, var))]
    data[,ord_var[j]] <- factor(data[,ord_var[j]], levels=var, ordered=TRUE)
  } else {
    data[,ord_var[j]] <- NULL
  }
  
}; rm(j); rm(var); rm(ord_var); rm(mlevels)


# Test Chi-square, matrix of p-values

options(warn=-1)
p.chisq = matrix(0, nrow=ncol(data), ncol=ncol(data), byrow=T)
for(i in 1:ncol(data)){
 for(j in 1:ncol(data)){
   p.chisq[i,j] = round(chisq.test(data[,i],data[,j])$p.value,3)
 }
}; rm(i); rm(j)

diag(p.chisq) = NA
colnames(p.chisq) = colnames(data)
rownames(p.chisq) = colnames(data)

# write.csv(p.chisq,paste(path,"/Analysis/chisq_matrix_formated.csv",sep=""),row.names=T)
# p.chisq <- read.csv(paste(path,"/Analysis/chisq_matrix_formated.csv",sep=""),header=T,row.names=1)

p.chisq <- as.matrix(p.chisq)
color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
heatmap.2(p.chisq, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density", denscol="blue", margins=c(10,10))

# Percent of p-values less than significance level 5%
# 0.09295775 ~ 9%
# (sum(p.chisq<0.05,na.rm=T)/2)/((ncol(p.chisq)*(ncol(p.chisq)-1))/2)

# ----- Para las variables con una alta asociación,
# ----- se efectua un Análisis Factorial Múltiple

hard_var = c("imp_laser",
             "imp_plotter",
             "imp_termica",
             "idatos_ninguno",
             "idatos_lector_bandas",
             "idatos_codigo_barras",
             "lrasgos_dactilar",
             "lrasgos_geometria",
             "lrasgos_no_tiene",
             "audv_videobeam",
             "audv_tvdigital",
             "audv_pantalla_tactil",
             "audv_camaras_seguridad",
             "audv_no_tiene",
             "eq_portatil",
             "eq_servidores",
             "eq_movil")

soft_var = c("SI_desarrollo_productos",
             "Renovacion_soft",
             "soft_ms_office",
             "soft_openoffice",
             "soft_corel",
             "soft_ms_officeWeb",
             "soft_google_docs",
             "app_erp",
             "app_crm",
             "app_scm")

netw_var = c("red_sucursales",
             "red_eq_internet",
             "sc_tvdigital",
             "sc_telefonia_c",
             "sc_telefonia_d",
             "conexion_internet",
             "red_LAN",
             "red_MAN",
             "red_WAN",
             "red_punto",
             "red_internet",
             "pweb_contenidos",
             "pweb_consulta_bd",
             "pweb_redes_sociales",
             "pweb_alianzas",
             "pweb_pagos",
             "pweb_idiomas",
             "pweb_videos",
             "pweb_anuncios",
             "pweb_no_tiene",
             "rs_lab_interno",
             "rs_lab_externo",
             "rs_personal",
             "rs_no_tiene",
             "serv_internet_alarmas",
             "serv_internet_chat",
             "serv_internet_videoconf",
             "serv_internet_gps",
             "serv_internet_remoto",
             "serv_internet_no_tiene",
             "e_commerce")

strat_var = c("eq_estrategias",
              "SI_estrategias",
              "internet_estrategias")

library(xlsx)

data.1 <- read.xlsx(paste(path,"/Data_sets/Optimal_quantification/opt_quan_all.xlsx",sep=""),sheetIndex=2,header=T,as.data.frame=T)
data.1 <- cbind(data.1[,hard_var],data.1[,soft_var],data.1[,netw_var],data.1[,strat_var])

# PLS path modeling

library(plspm)

HARD = c(0, 0, 0, 0)
SOFT = c(0, 0, 0, 0)
NETW = c(0, 0, 0, 0)
DIRE = c(1, 1, 1, 0)
dir_path = rbind(HARD, SOFT, NETW, DIRE)
colnames(dir_path) = rownames(dir_path)
rm(list=c("HARD","SOFT","NETW","DIRE"))

dir_blocks = list(match(hard_var,colnames(data.1)),
                  match(soft_var,colnames(data.1)),
                  match(netw_var,colnames(data.1)),
                  match(strat_var,colnames(data.1)))

dir_modes = rep("B", 4)

#data.1.1 <- data.1[complete.cases(data.1),]

dir_pls = plspm(data.1, dir_path, dir_blocks,
                modes=dir_modes, scaled=FALSE, scheme="centroid")

png(paste0(path,"/direccionamiento_pls.png"), width=3072,height=3072, res=300, pointsize=13)
plot(dir_pls)
dev.off()

png(paste0(path,"/direccionamiento_cargas.png"), width=3072,height=3072, res=300, pointsize=13)
plot(dir_pls, what="loadings")
dev.off()

plot(dir_pls, what="weights")

dir_pls$unidim
dir_pls$outer_model
dir_pls$crossloadings

library(ggplot2)
library(reshape)

xloads = melt(dir_pls$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")

# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(x = name, y = value, fill = block)) +
  # add horizontal reference lines
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
  # indicate the use of car-charts
  geom_bar(stat = "identity", position = "dodge") +
  # panel display (i.e. faceting)
  facet_wrap(block ~ LV) +
  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  # add title
  ggtitle("Crossloadings")

dir_pls$inner_model
dir_pls$inner_summary

dir_val <- plspm(data.1, dir_path, dir_blocks, modes=dir_modes,
                 scaled=F, scheme="centroid", boot.val=T, br=20)

plot(dir_val)
plot(dir_pls)

save(file=paste(path,"/bootstrap.RData",sep=""),dir_val)

library(dismo)

id_set <- kfold(data.1, k=2)
pls_model <- function(DATA){
  
  # paths
  HARD = c(0, 0, 0, 0)
  SOFT = c(0, 0, 0, 0)
  NETW = c(0, 0, 0, 0)
  DIRE = c(1, 1, 1, 0)
  dir_path = rbind(HARD, SOFT, NETW, DIRE)
  colnames(dir_path) = rownames(dir_path)
  rm(list=c("HARD","SOFT","NETW","DIRE"))
  
  # blocks
  dir_blocks = list(match(hard_var,colnames(DATA)),
                    match(soft_var,colnames(DATA)),
                    match(netw_var,colnames(DATA)),
                    match(strat_var,colnames(DATA)))
  
  # modes
  dir_modes = rep("B", 4)
  
  # model
  dir_pls <- plspm(DATA, dir_path, dir_blocks, modes=dir_modes,
                   scaled=F, scheme="centroid")
  
  return(dir_pls)
  
}

gen_results <- list()
for(i in 1:2){
  
  data = data.1[id_set==i,]
  gen_results[[i]] = pls_model(data)
  
}; rm(i)

plot(gen_results[[1]])
plot(gen_results[[2]])

# set number 2
data.2 <- read.xlsx(paste(path,"/Data_sets/Optimal_quantification/opt_quan_all2.xlsx",sep=""),sheetIndex=2,header=T,as.data.frame=T)
#data.2 <- cbind(data.2[,hard_var],data.2[,soft_var],data.2[,netw_var],data.2[,strat_var])



# Multiple factor analysis (MFA)

mfa_results <- MFA(data.1, group=c(length(hard_var),length(soft_var),length(netw_var),length(strat_var)),
                  type=rep("n",4),name.group=c("Hardware", "Software", "Redes", "Direccionamiento estratégico"))

# Variabilidad explicada
mfa_results$eig

plot(1:106, round(mfa_results$eig[,3]), xlab="Número de dimensiones",
     ylab="Porcentaje acumulado de variabilidad explicada (%)",
     col="gray", pch=20)

# Análisis de variables
mfa_results$quali.var$contrib[,1]

# Análisis de grupos
mfa_results$group$Lg
mfa_results$group$RV
mfa_results$group$contrib

# ----- Se repite el análisis solo con las variables altamente asociadas
# ----- con las variables de control

hard_var2 = c("imp_laser",
              "imp_plotter",
              "audv_videobeam",
              "eq_portatil")

soft_var2 = c("SI_desarrollo_productos",
              "Renovacion_soft",
              "app_erp",
              "app_crm")

netw_var2 = c("red_sucursales",
              "sc_telefonia_d",
              "red_LAN",
              "red_WAN",
              "pweb_contenidos",
              "pweb_no_tiene",
              "e_commerce")

strat_var = c("eq_estrategias",
              "SI_estrategias",
              "internet_estrategias")

data.2 <- cbind(data[,hard_var2],data[,soft_var2],data[,netw_var2],data[,strat_var])

# Multiple factor analysis (MFA)

mfa_results2 <- MFA(data.2, group=c(length(hard_var2),length(soft_var2),length(netw_var2),length(strat_var)),
                    type=rep("n",4),name.group=c("Hardware", "Software", "Redes", "Direccionamiento estratégico"))

# Análisis de grupos
mfa_results2$group$Lg
mfa_results2$group$RV
mfa_results2$group$contrib

# -------------------------------------- #
# Partial Least Squares - Path Moldeling #
# -------------------------------------- #

library(plspm)

# path matrix (inner model realtionships)
HARD = c(0, 0, 0, 0)
SOFT = c(0, 0, 0, 0)
NETW = c(0, 0, 0, 0)
DIRS = c(1, 1, 1, 0)
dir_path = rbind(HARD, SOFT, NETW, DIRS)
colnames(dir_path) = rownames(dir_path)

# plot the path matrix
innerplot(dir_path)

# list indicating what variables are associated with what latent variables
dir_blocks = list(match(hard_var,colnames(data.1)),
                  match(soft_var,colnames(data.1)),
                  match(netw_var,colnames(data.1)),
                  match(strat_var,colnames(data.1)))

# scale of variables
dir_scales = list(rep("NOM",17),
                  c(rep("ORD",2),rep("NOM",8)),
                  c(rep("ORD",2),rep("NOM",29)),
                  rep("ORD",3))

# all latent variables are measured in a reflective way
dir_modes = rep("B", 4)

data.1.1 <- data.1[complete.cases(data.1),]

dir_pls = plspm(data.1.1, dir_path, dir_blocks,
                modes=dir_modes, scaled=TRUE,
                scaling=dir_scales, scheme="centroid")


# Example package
data(russa)
data(russb)

rus_path = rbind(c(0, 0, 0), c(0, 0, 0), c(1, 1, 0))
rownames(rus_path) = c("AGRI", "IND", "POLINS")
colnames(rus_path) = c("AGRI", "IND", "POLINS")
rus_blocks = list(1:3, 4:5, 6:9)
rus_scaling = list(c("NUM", "NUM", "NUM"),
                   c("NUM", "NUM"),
                   c("NUM", "NUM", "NUM", "NUM"))
rus_modes = c("A", "A", "A")

rus_pls1 = plspm(russa, rus_path, rus_blocks, scaling = rus_scaling, 
                 modes = rus_modes, scheme = "centroid", plscomp = c(1,1,1), tol = 0.0000001)
plot(rus_pls1)

rus_scaling2 = list(c("NUM", "NUM", "NUM"),
                    c("ORD", "ORD"),
                    c("NUM", "NUM", "NUM", "NOM"))

rus_pls2 = plspm(russa, rus_path, rus_blocks, scaling = rus_scaling2, 
                 modes = rus_modes, scheme = "centroid", plscomp = c(1,1,1), tol = 0.0000001)


rus_pls3 = plspm(russb, rus_path, rus_blocks, scaling = rus_scaling2, 
                 modes = rus_modes, scheme = "centroid", plscomp = c(1,1,1), tol = 0.0000001)

# blocks
rus_blocchi = list(
  c("gini", "farm", "rent"),
  c("gnpr", "labo"),
  c("inst", "ecks", "death", "demo"))

# scaling
rus_scaling3 = list(c("numeric", "numeric", "numeric"),
                    c("ordinal", "ORDINAL"),
                    c("NuM", "numer", "NUM", "nominal"))

# modes new A
rus_modes3 = c("newa", "NEWA", "NewA")

# PLS-PM using data set 'russb'
rus_pls5 = plspm(russb, rus_path, rus_blocchi, scaling = rus_scaling3, 
                 modes = rus_modes3, scheme = "CENTROID", plscomp = c(1,1,1), tol = 0.0000001)

# outer model
rus_pls5$outer_model










# ----- Control variables VOY ACÁ
control_var = c("sector","num_empl","tam_emp","tiempo_ant_cat")
mtch = match(control_var, colnames(data4))
control_data = p.chisq[mtch,mtch]; rm(mtch)

png(paste0(path,"/chisq_control.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(control_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue",margins=c(13,13))
dev.off()

### ================================== ###
# With control variables
### ================================== ###

## Technology innovation

# Hardware set
hard_var = c("impr_matricial","impr_laser","impr_inyeccion","impr_plotter",
             "impr_termica","idatos_ninguno","idatos_lector_bandas",
             "idatos_codigo_barras","lrasgos_dactilar","lrasgos_geometria",
             "lrasgos_no_tiene","audiov_videobeam","audiov_tvdigital",
             "audiov_pantalla_tactil","audiov_camaras_seguridad",
             "audiov_no_tiene","eq_portatil","eq_servidores",
             "eq_movil")
hard_var = c(control_var,hard_var)
mtch = match(hard_var, colnames(data4))
hard_data = p.chisq[mtch,mtch]; rm(mtch)

png(paste0(path,"/chisq_hardware.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(hard_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue",margins=c(11,11))
dev.off()

# Software set
soft_var = c("SI_desarrollo_productos","Renovacion_soft",
             "eq_escritorio_linux","soft_ms_office","soft_openoffice",
             "soft_corel","soft_ms_officeWeb","soft_google_docs",
             "soft_otros","app_erp","app_crm","app_scm")
soft_var = c(control_var,soft_var)
mtch = match(soft_var, colnames(data4))
soft_data = p.chisq[mtch,mtch]; rm(mtch)

png(paste0(path,"/chisq_software.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(soft_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue",margins=c(11,11))
dev.off()

# Networks set
netw_var = c("red_sucursales","red_eq_internet","sc_tvdigital",
             "sc_telefonia_c","sc_telefonia_d","conexion_internet",
             "red_LAN","red_MAN","red_WAN","red_punto","red_internet",
             "pweb_contenidos","pweb_consulta_bd","pweb_redes_sociales",
             "pweb_alianzas","pweb_pagos","pweb_idiomas","pweb_videos",
             "pweb_anuncios","pweb_no_tiene","rs_lab_interno",
             "rs_lab_externo","rs_personal","rs_no_tiene",
             "serv_internet_alarmas","serv_internet_chat",
             "serv_internet_videoconf","serv_internet_gps",
             "serv_internet_remoto","serv_internet_no_tiene",
             "serv_internet_no_sabe","e_commerce")
netw_var = c(control_var,netw_var)
mtch = match(netw_var, colnames(data4))
netw_data = p.chisq[mtch,mtch]; rm(mtch)

png(paste0(path,"/chisq_networks.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(netw_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue",margins=c(9,9))
dev.off()

# Innovation technological

# inn_tech = c(control_var,hard_var,soft_var,netw_var)
inn_tech = c(hard_var,soft_var,netw_var)
mtch = match(inn_tech, colnames(data4))
inn_tech_data = p.chisq[mtch,mtch]; rm(mtch)

# Percent of p-values less than significance level 5%
# 0.1018141 ~ 10%
(sum(inn_tech_data<0.05,na.rm=T)/2)/((ncol(inn_tech_data)*(ncol(inn_tech_data)-1))/2)

png(paste0(path,"/chisq_innovacion.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(inn_tech_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue",margins=c(10,10))
dev.off()

## Technology effectiveness
effect_var = c("efectividad_pc","efectividad_soft","aprov_efectivo_red")
effect_var = c(control_var, effect_var)
mtch = match(effect_var, colnames(data4))
effect_data = p.chisq[mtch,mtch]; rm(mtch)

png(paste0(path,"/chisq_efectividad.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(effect_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue", margins=c(12,12))
dev.off()

## Strategic management
strat_var = c("equipos_estrategia_emp","SI_decisiones_strategias","internet_estrategias")
strat_var = c(control_var, strat_var)
mtch = match(strat_var, colnames(data4))
strat_data = p.chisq[mtch,mtch]; rm(mtch)

png(paste0(path,"/chisq_estrategias.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(strat_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue",margins=c(14,14))
dev.off()

## Productivity
prod_var = c("SI_productividad")
prod_var = c(control_var, prod_var)
mtch = match(prod_var, colnames(data4))
prod_data = p.chisq[mtch,mtch]; rm(mtch)

png(paste0(path,"/chisq_productividad.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(prod_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue",margins=c(12,12))
dev.off()

### ================================== ###
# With interest variables
### ================================== ###

# Efectividad tecnológica (con innovación tecnológica)
ef_tch = c(inn_tech,effect_var)
mtch = match(ef_tch, colnames(data4))
ef_tch_data = p.chisq[mtch,mtch]; rm(mtch)

png(paste0(path,"/chisq_eftecnologica_inn.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(ef_tch_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue",margins=c(10,10))
dev.off()

# Direccionamiento estratégico (con innovación tecnológica)
strat_dir = c(inn_tech,strat_var)
mtch = match(strat_dir, colnames(data4))
strat_dir_data = p.chisq[mtch,mtch]; rm(mtch)

png(paste0(path,"/chisq_direstrategico_inn.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(strat_dir_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue",margins=c(10,10))
dev.off()

# Productividad (con direccionamiento estratégico)
prod_tech = c(inn_tech,prod_var)
mtch = match(prod_tech, colnames(data4))
prod_tech_data = p.chisq[mtch,mtch]; rm(mtch)

png(paste0(path,"/chisq_productividad_inn.png"), width=3072,height=3072, res=300, pointsize=13)
plot.new()
heatmap.3(prod_tech_data, Rowv=NULL, Colv=NULL, col=color_scale, linecol=NULL, tracecol=NULL, density.info="density",denscol="blue",margins=c(10,10))
dev.off()

# "Hardware", "Software", "Redes", "Efectividad tecnológica", " Direccionamiento estratégico", "Productividad"

data4mfa <- cbind(data4[,hard_var],data4[,soft_var],data4[,netw_var],
                  data4[,effect_var],data4[,strat_var],data4[,prod_var])
colnames(data4mfa)[70] <- "SI_productividad"

require(FactoMineR)

mfaAdj <- MFA(data4mfa,group=c(length(hard_var),length(soft_var),length(netw_var),
                               length(effect_var),length(strat_var),length(prod_var)),
              type=rep("n",6),name.group=c("Hardware", "Software", "Redes",
                                           "Efectividad tecnológica", 
                                           "Direccionamiento estratégico",
                                           "Productividad"))

save.image(file=paste(path,"/results.RData",sep=""))


