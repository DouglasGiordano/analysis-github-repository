library(RMySQL)
library(rvest)#https://www.datacamp.com/community/tutorials/r-web-scraping-rvest use for scraping data html
library(purrr)#function map to get unique element html
# lapply(dbListConnections(MySQL()), dbDisconnect)

#geração da rede social

get.connection.bd <- function(){
  mydb = dbConnect(MySQL(), user='root', password='root', dbname='simple_github', host='127.0.0.1')
  return(mydb)
}

create.data.network <-function(){
  source(file = "CreateSocialNetwork.r")
  source(file ="Querys.r")
  projects = search.projects()
  mydb = get.connection.bd()
  for(i in 1:nrow(projects)){
    create.social.network(mydb, projects[i,]$name, projects[i,]$owner)
  }
}

create.metric.network <- function(){
  source(file ="Querys.r")
  library(igraph)
  library(RColorBrewer)
  projects = search.projects()
  mydb = get.connection.bd()
  for(i in 1:nrow(projects)){
    edges = query.edge.project(mydb, projects[i,]$owner, projects[i,]$name)
    edges$time = as.POSIXct(edges$time, "UTC", "%Y-%m-%d %H:%M:%S")
    
    max.edges = as.Date(max(edges$time))
    min.edges = as.Date(min(edges$time))
    message(min.edges, " ", max.edges)
    sequence.days = seq(from=min.edges, to=max.edges, by='days' )
    for ( i in seq_along(sequence.days))
    {
      edges.now = filter(edges, grepl(sequence.days[i], time, fixed = TRUE))
      graph = graph.data.frame(edges.now, directed=TRUE)
      measure.betweenness = centr_betw(graph, directed=TRUE, normalized=FALSE)
      print(nrow(measure.betweenness))
    }
  }
}

search.projects <- function(){
  projects <- read.csv(file = "input/repositories.csv", sep = ",")
  return(projects)
}