library(corrplot)

nucleo=read.csv("output/project_nucleo.csv", header=T)
st_nucleo=nucleo[,c(3:27)]

mnucleo<-round(cor(st_nucleo,method="spearman"),2)

corrplot(mnucleo, type="upper", method="square")
