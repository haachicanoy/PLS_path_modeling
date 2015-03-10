# Explorar interacciones con variables de control

### Innovaci�n en Hardware
p <- ggplot(as.data.frame(dir_pls0$scores), aes(x=dir_pls0$scores[,"HARD"], y=dir_pls0$scores[,"DIRE"], colour=as.factor(qmfa_data1$P2)), size=5) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
p <- p + xlab("Innovaci�n en Hardware")
p <- p + ylab("Direccionamiento estrat�gico")
p <- p + scale_colour_manual(name="Sector econ�mico",
                             labels=c("Secundario","Terciario"),
                             palette="Set1")
p

ggplot(as.data.frame(dir_pls0$scores), aes(x=dir_pls0$scores[,"HARD"], y=dir_pls0$scores[,"DIRE"], colour=as.factor(qmfa_data1$P3)), size=5) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
ggplot(as.data.frame(dir_pls0$scores), aes(x=dir_pls0$scores[,"HARD"], y=dir_pls0$scores[,"DIRE"], colour=as.factor(qmfa_data1$P4)), size=5) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

### Innovaci�n en Software
ggplot(as.data.frame(dir_pls0$scores), aes(x=dir_pls0$scores[,"SOFT"], y=dir_pls0$scores[,"DIRE"], colour=as.factor(qmfa_data1$P2)), size=5) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
ggplot(as.data.frame(dir_pls0$scores), aes(x=dir_pls0$scores[,"SOFT"], y=dir_pls0$scores[,"DIRE"], colour=as.factor(qmfa_data1$P3)), size=5) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
ggplot(as.data.frame(dir_pls0$scores), aes(x=dir_pls0$scores[,"SOFT"], y=dir_pls0$scores[,"DIRE"], colour=as.factor(qmfa_data1$P4)), size=5) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

### Innovaci�n en Redes
ggplot(as.data.frame(dir_pls0$scores), aes(x=dir_pls0$scores[,"NETW"], y=dir_pls0$scores[,"DIRE"], colour=as.factor(qmfa_data1$P2)), size=5) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
ggplot(as.data.frame(dir_pls0$scores), aes(x=dir_pls0$scores[,"NETW"], y=dir_pls0$scores[,"DIRE"], colour=as.factor(qmfa_data1$P3)), size=5) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
ggplot(as.data.frame(dir_pls0$scores), aes(x=dir_pls0$scores[,"NETW"], y=dir_pls0$scores[,"DIRE"], colour=as.factor(qmfa_data1$P4)), size=5) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)






