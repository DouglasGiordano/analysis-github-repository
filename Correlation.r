library(corrplot)
library("ggpubr")

create.correlation = function(){
  create.plot("input/static/usuario.csv", "usuario")
  create.plot("input/static/usuario_all.csv", "usuario_all")
  create.plot("input/last/usuario_last.csv", "usuario_last")
  create.plot("input/last/usuario_last_all.csv", "usuario_last_all")
}

create.plot = function(location, name){
  data=read.csv(location, header=T)#read file
  data[is.na(data)] = 0#replace NA to 0
  data = filter_data(data)#filter only columns for correlation
  
  data_spearman <-round(cor(data,method="spearman"),2)
  
  png(filename=paste0("output/",name,"_round_corr_spearman.png"), width = 1000, height = 1000)
  corrplot(data_spearman, type="upper", method = "number")
  dev.off()
  
  data_pearson <-round(cor(data,method="pearson"),2)
  
  png(filename=paste0("output/",name,"_round_corr_pearson.png"), width = 1000, height = 1000)
  corrplot(data_pearson, type="upper", method = "number")
  dev.off()
  
  data_kendall <-round(cor(data,method="kendall"),2)
  
  png(filename=paste0("output/",name,"_round_corr_kendall.png"), width = 1000, height = 1000)
  corrplot(data_kendall, type="upper", method = "number")
  dev.off()
}

filter_data = function(df.usuario){
  turnover = c()
  significance = df.usuario[,c('significance', 'num_interaction', 'days_no_interaction')]
  grubbs = read.csv('input/significance5.csv', header=T)
  for(i in 1:nrow(significance)){
    user.signi = as.numeric(significance$significance[i])
    if (user.signi <= 0){
      days_no_interaction = as.numeric(significance$days_no_interaction[i])
      if (days_no_interaction > 180){
        turnover = append(turnover, 1)
      }else{
        turnover = append(turnover,0)
      }
    } else{
      interaction = as.numeric(significance$num_interaction[i])
      position = which.min(abs(grubbs$obs - interaction))
      grubs.significance = grubbs$significance[position]
      if (grubs.significance <= user.signi){
        turnover = append(turnover, 1)
      }else{
        turnover = append(turnover,0)
      }
    }
    
  }
  
  df.usuario = df.usuario[,c('betweenness', 'degree_in', 'degree_out', 'degree_total', 'closeness', 'eigenvector', 'coreness',
                                         'mean_positive', 'mean_negative', 'received_negative', 'received_positive', 'num_interaction',
                                         'mean_interval', 'num_active_days')]
  df.usuario$turnover = turnover
  return(df.usuario)
}
