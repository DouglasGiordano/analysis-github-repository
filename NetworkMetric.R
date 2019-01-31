#format decimal numbers
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#create metrics users
get.network.metric.links <- function(vertices, graph){
  #betweness
  measure.betweenness = centr_betw(graph, directed=T, normalized=T)$res
  #degreee
  measure.in.degree = centr_degree(graph, mode=c("out"))$res
  measure.out.degree = centr_degree(graph, mode=c("in"))$res
  measure.total.degree = centr_degree(graph, mode=c("total"))$res
  #closeness
  measure.closeness = centr_clo(graph, mode="all", normalized=T)$res
  #Eigenvector
  measure.eigenvector = centr_eigen(graph, directed=T, normalized=T)$vector
  #measure coreness
  measure.coreness = graph.coreness(graph, mode="all")
  
  metrics = data.frame(matrix(NA, nrow = length(vertices), ncol = 1))
  metrics$user = as.character(vertices)
  metrics$betweenness = as.numeric(measure.betweenness)
  metrics$degree_in =  as.numeric(measure.in.degree)
  metrics$degree_out = as.numeric(measure.out.degree)
  metrics$degree_total = as.numeric(measure.total.degree)
  metrics$closeness = as.numeric(measure.closeness)
  metrics$eigenvector = as.numeric(measure.eigenvector)
  metrics$coreness = as.numeric(measure.coreness)
  metrics = metrics[2:9]
  return(metrics)
}


#create metrics users
get.network.metric.last <- function(edges, users){
  n.vertices = length(users)
  for(i in 1:n.vertices){
    user = users[i]
    user.edges = edges[edges$source == user,]
    #filter for 30 days befores last interation
    end = max(user.edges$time)
    start = end - 30*86400
    now.edges = edges[edges$time >= start & edges$time <= end,]
    graph = graph.data.frame(now.edges, directed = T)
    metrics = get.network.metric.links(V(graph)$name, graph)
    metric = metrics[metrics$user == user,]
  }
}

#create matric repository
get.network.metric <- function(graph){
  #densidade da rede
  measure.density = edge_density(graph, loops=F)
  #recriprocidade
  measure.reciprocity = reciprocity(graph)
  #transitivity
  measure.transitivity = transitivity(graph, type="global")
  #diameter
  measure.diameter = diameter(graph, directed=F, weights=NA)
  
  metrics = data.frame(matrix(NA, nrow = length(vertices), ncol = 1))
  metrics$density = measure.density
  metrics$reciprocity = measure.reciprocity
  metrics$transitivity = measure.transitivity
  metrics$diameter = measure.diameter
  metrics = metrics[2:5]
  return(metrics)
}