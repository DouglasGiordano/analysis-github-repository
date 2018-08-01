library(corrplot)

rails=read.csv("resultado_rails.csv", header=T)
st_rails=rails[,c(7:11,14:21)]

elixir=read.csv("resultado_elixir.csv", header=T)
st_elixir=elixir[,c(7:11,14:21)]

nucleo=read.csv("projetos_usuarios_nucleo.csv", header=T)
st_nucleo=nucleo[,c(8:12,15:21)]

mrails <-round(cor(st_rails, method="spearman"),2)

melixir<-round(cor(st_elixir,method="spearman"),2)

mnucleo<-round(cor(st_nucleo,method="spearman"),2)

corrplot(mrails, method="square")

corrplot(melixir, method="square")

corrplot(mnucleo, method="square")
