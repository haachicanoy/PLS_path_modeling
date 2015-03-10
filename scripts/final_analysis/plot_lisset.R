
datos <- read.csv("C:/Users/haachicanoy/Documents/graph.csv")

library(ggplot2)

ggplot(datos, aes(x=answer, y=data, fill=over)) + facet_grid(.~variable,scales="free") + geom_bar(position="dodge")

p <- ggplot(datos, aes(x=answer, y=data, fill=answer))
p <- p + geom_bar(aes(y=data),stat="identity",position="dodge")
p <- p + facet_grid(over~variable)
p <- p + scale_fill_manual(values=c("dodgerblue3", "cadetblue2"))
p <- p + xlab("")
p <- p + ylab("Percent (%)")
p <- p + theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank(),
               panel.grid.major.y = element_blank(),
               panel.grid.minor.y = element_blank(),
               axis.text.x = element_text(size=14,face="bold"),
               axis.text.y = element_text(size=14,angle=90,face="bold"),
               axis.title.x = element_text(face="bold",size=14),
               axis.title.y = element_text(face="bold",size=14),
               legend.text = element_text(size=3),
               legend.title = element_text(face="bold",size=3),
               plot.title = element_text(face="bold", size=8),
               strip.text.x = element_text(size=12,angle=90,face="bold"),
               strip.text.y = element_text(size=12,angle=360,face="bold"))
p <- p + guides(fill=FALSE)
p
