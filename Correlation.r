library(corrplot)
library("ggpubr")
nucleo=read.csv("output/project_nucleo.csv", header=T)
nucleo=na.omit(nucleo)
nucleo$transitivity <- as.list(nucleo$transitivity)
nucleo$turnover.sifnificance <- as.list(nucleo$turnover.sifnificance)
st_nucleo=nucleo[,c(3:27)]
#mnucleo<-round(cor(st_nucleo,method="spearman"),2)

#corrplot(mnucleo, type="upper", method="square")


plot <- ggscatter(nucleo, x = "transitivity", y = "turnover.sifnificance", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Transitivity", ylab = "Significance")
ggarrange(plot)
ggexport(filename = "plotTransySig.png")