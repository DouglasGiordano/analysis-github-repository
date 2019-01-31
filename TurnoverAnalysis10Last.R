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
    message.i(owner, name, paste0(i, " of ", n.projects))
    status = get.status.processing(mydb, owner, name)
    metrics = NULL
    metrics = create.metric.network.last(projects[i,], mydb)
  }
}


#github,fetch teste
create.metric.sentiment.last <- function(project, mydb){
  owner = as.character(project$owner)
  name = as.character(project$name)
  interactions <- na.omit(query.interaction.project.sentiment(mydb, owner, name))
  
  user = unique(interactions$user)
  
  #get and save metric of vertices
  metrics.senti = get.sentiment.median.metric.last(interactions, user)
  return(metrics.senti)
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


create.metric.count <- function(project, mydb){
  owner = as.character(project$owner)
  name = as.character(project$name)
  interactions <- na.omit(query.interaction.project(mydb, owner, name))
  
  user = unique(interactions$user)
  
  #get and save metric of vertices
  metrics = get.count.metric.last(interactions, user)
  return(metrics)
}