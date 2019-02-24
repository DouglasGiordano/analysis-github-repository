source("Querys.r")
remove.users.ghost <- function(mydb, metrics){
  users = get.user.ghost(mydb);
  '%ni%'<- Negate('%in%') 
  print(nrow(metrics))
  for(i in 1:nrow(users)){
    user_now = users$author[i]
    metrics = subset(metrics, metrics$user != user_now)
  }
  
  print(nrow(metrics))
  return(metrics)
}

mydb = get.connection.bd()
metrics = query.metricusers.with.significance(get.connection.bd())
metrics = remove.users.ghost(mydb, metrics)
write.csv(metrics, file="output/usuario.csv", row.names = FALSE)

metrics_all = query.metricusers(get.connection.bd())
metrics_all = remove.users.ghost(mydb, metrics_all)
write.csv(metrics_all, file="output/usuario_all.csv", row.names = FALSE)


metrics.last = query.metricusers.with.significance.last(get.connection.bd())
metrics.last = remove.users.ghost(mydb, metrics.last)
write.csv(metrics.last, file="output/usuario_last.csv", row.names = FALSE)

metrics.last.all = query.metricusers.last(get.connection.bd())
metrics.last.all = remove.users.ghost(mydb, metrics.last.all)
write.csv(metrics.last.all, file="output/usuario_last_all.csv", row.names = FALSE)


