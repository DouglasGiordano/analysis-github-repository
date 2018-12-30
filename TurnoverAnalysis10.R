library(RMySQL)
library(rvest)#https://www.datacamp.com/community/tutorials/r-web-scraping-rvest use for scraping data html
library(purrr)#function map to get unique element html
# lapply(dbListConnections(MySQL()), dbDisconnect)

#get connection for bd
get.connection.bd <- function(){
  mydb = dbConnect(MySQL(), user='root', password='root', dbname='simple_github', host='127.0.0.1')
  return(mydb)
}

#first
create.data.network <-function(){
  source(file = "CreateSocialNetwork.r")
  source(file ="Querys.r")
  projects = search.projects()
  mydb = get.connection.bd()
  for(i in 1:nrow(projects)){
    create.social.network(mydb, projects[i,]$name, projects[i,]$owner)
  }
}

#second
create.metric.network <- function(){
  source(file = "NetworkMetric.R")
  source(file ="Querys.r")
  library(igraph)
  library(RColorBrewer)
  projects = search.projects()
  mydb = get.connection.bd()
  for(i in 1:nrow(projects)){
    edges = query.edge.project(mydb, projects[i,]$owner, projects[i,]$name)
    edges = na.omit(edges)
    users = unique(c(as.list(edges$source), as.list(edges$target)))
    users = na.omit(users)
    print(length(users))
    edges$time = as.POSIXct(edges$time, "UTC", "%Y-%m-%d %H:%M:%S")
    max.edges = as.Date(max(edges$time))
    min.edges = as.Date(min(edges$time))
    message(min.edges, " ", max.edges)
    
    #create sequence days for looping in graph and get temporal metrics
    sequence.days = seq(from=min.edges, to=max.edges, by='days' )
    
    #create graph witch edges and vertices
    graph = graph.data.frame(edges, directed = T)
    #graph <- simplify(graph, remove.multiple = F, remove.loops = T, edge.attr.comb=list(weight="sum","ignore"))
    vertices = V(graph)$name
    
    #get and save metric of vertices
    metric.project.name = data.frame(matrix(NA, nrow = length(vertices), ncol = 1))
    metric.project.name$owner = projects[i,]$owner
    metric.project.name$name = projects[i,]$name
    metric.links = get.network.metric.links(vertices, graph)
    metric.links = cbind(metric.project.name, metric.links)
    print(names(metric.links))
    metric.links = metric.links[2:11]
    save.metric.link(metric.links, mydb)
    
    #get and save metric of repository
    metric = get.network.metric(graph)
    metric = cbind(metric.project.name[1,2:3], metric)
    save.metric(metric, mydb)
    # for ( i in seq_along(sequence.days))
    # {
    #   edges.now = filter(edges, grepl(sequence.days[i], time, fixed = TRUE))
    #   edes.now <- na.omit(edges.now)
    #   graph = graph.data.frame(edges.now, directed=TRUE)
    #   measure.betweenness = centr_betw(graph, directed=TRUE, normalized=FALSE)
    #   print(nrow(measure.betweenness))
    # }
  }
}
#
create.metric.turnover <- function(){
  source(file ="Querys.r")
  projects = search.projects()
  mydb = get.connection.bd()
  for(i in 1:nrow(projects)){
    owner = as.character(projects[i,]$owner)
    name = as.character(projects[i,]$name)
    message("(", owner, " ", name,") Calculating turnover metrics... ")
    interactions <- na.omit(query.interaction.project(mydb, owner, name))
    
    user = na.omit(unique(interactions$user))
    
    #get and save metric of vertices
    metric.turnover = as.data.frame(user)
    metric.turnover$owner = owner
    metric.turnover$name = name
    metric.turnover$significance = as.numeric(get.turnover.metric(interactions, user))
    save.metric.turnover(metric.turnover, mydb)
    print(metric.turnover)
    message("(", owner, " ", name,") Complete... ", owner, " ", name)
  }
}

search.projects <- function(){
  projects <- read.csv(file = "input/repositories.csv", sep = ",")
  return(projects)
}