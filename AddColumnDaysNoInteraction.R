source(file = "TurnoverMetric.R")
source(file ="Querys.r")

search.projects <- function(){
  projects <- read.csv(file = paste0("input/repositories.csv"), sep = ",")
  return(projects)
}

addColumnDaysNoInteraction <- function(){
  projects = search.projects()
  n.projects = nrow(projects)
  for(i in 1:n.projects){
    owner = as.character(projects[i,]$owner)
    name = as.character(projects[i,]$name)
    message(owner," - ", name, ": ",i," - ",n.projects)
    metrics = create.metric.turnover(projects[i,], mydb)
    n.metrics = nrow(metrics)
    for(j in 1:n.metrics){
      update.metric.days.no.interaction(mydb, metrics[j,]$user, owner, name, metrics[j,]$days_no_interaction)
    }
  }
}

create.metric.turnover <- function(project, mydb){
  owner = as.character(project$owner)
  name = as.character(project$name)
  interactions <- na.omit(query.interaction.project(mydb, owner, name))
  
  user = na.omit(unique(interactions$user))
  
  #get and save metric of vertices
  metric.turnover = get.turnover.metric(interactions, user)
  return(metric.turnover)
}