library(RMySQL)

create.metrics.last <- function(){
  library(igraph)
  library(RColorBrewer)
  source(file = "NetworkMetric.R")
  source(file = "TurnoverMetric.R")
  source(file = "SentiMetric.R")
  source(file = "CounterMetric.R")
  source(file ="Querys.r")
  projects = search.projects()
  mydb = get.connection.bd()
  n.projects = nrow(projects)
  for(i in 1:n.projects){
    owner = as.character(projects[i,]$owner)
    name = as.character(projects[i,]$name)
    message.l(owner, name, paste0(i, " of ", n.projects))
    status = get.status.processing(mydb, owner, name)
    metrics = NULL
    
    if(is.na(status$createMetricsLast)){
      interactions.sent <- na.omit(query.interaction.project.sentiment(mydb, owner, name))
      interactions <- na.omit(query.interaction.project(mydb, owner, name))
      user = unique(interactions$user)
      user.sent = unique(interactions.sent$user)
      #get and save metric of vertices
      
      metrics.senti = get.sentiment.median.metric.last(interactions.sent, user.sent)
      metrics.count = get.count.metric.last(interactions, user)
      metrics.turnover = get.turnover.metric(interactions, user)
      metrics.network = create.metric.network.last(projects[i,], mydb)
      
      message.l(owner, name,"Calculating metrics... ")
      metrics = merge.metrics.last(metrics, metrics.senti)
      metrics = merge.metrics.last(metrics, metrics.count)
      metrics = merge.metrics.last(metrics, metrics.turnover)
      metrics = merge.metrics.last(metrics, metrics.network)
      metrics$owner = owner
      metrics$name = name
      message.l(owner, name,"Metrics complete... ")
      save.metric.user.last(metrics, mydb)
      update.status.create.metrics.last(mydb, owner, name)
    }
  }
}


#second
create.metric.network.last <- function(project, mydb){
  edges = query.edge.project(mydb, project$owner, project$name)
  edges = na.omit(edges)
  users = unique(as.list(edges$source))
  edges$time = as.POSIXct(edges$time, "UTC", "%Y-%m-%d %H:%M:%S")
  
  #get and save metric of vertices
  metric.links = get.network.metric.last(edges, users)
  
  return(metric.links)
}


merge.metrics.last <- function(metrics, data){
  if(is.null(metrics)){
    return(data)
  } else {
    return(merge(metrics, data, by="user", all = T))
  }
}


message.l <- function (owner, name, message){
  message(Sys.time(), " - (", owner, " ", name,") ", message)
}
