
get.count.metric <-function(interactions, users){
  interactions$date  = gsub("[A-Za-z]"," " , interactions$date ,ignore.case = TRUE)
  interactions$date = substring(interactions$date, 1, 19)
  interactions$date = as.Date(as.POSIXct(interactions$date, "UTC", "%Y-%m-%d %H:%M:%S"))
  
  num.interaction = c()
  mean.interval = c()
  num.active.days = c()
  
  for (row in 1:length(users)) {
    user = users[row]
    user.interactions = interactions[interactions$user == user,]
    dates = sort(user.interactions$date)
    mean.interval = c(mean.interval, mean(diff(dates)))
    num.interaction = c(num.interaction, nrow(user.interactions))
    num.active.days = c(num.active.days, difftime(max(dates),min(dates), units = "days"))
  }
  
  metrics = data.frame(matrix(NA, nrow = length(users), ncol = 1))
  metrics$user = users
  metrics$num_interaction = num.interaction
  metrics$mean_interval = mean.interval
  metrics$num_active_days = num.active.days
  
  return(metrics[2:5])
}

get.count.metric.last <-function(interactions, users){
  interactions$date  = gsub("[A-Za-z]"," " , interactions$date ,ignore.case = TRUE)
  interactions$date = substring(interactions$date, 1, 19)
  interactions$date = as.Date(as.POSIXct(interactions$date, "UTC", "%Y-%m-%d %H:%M:%S"))
  
  num.interaction = c()
  mean.interval = c()
  num.active.days = c()
  
  for (row in 1:length(users)) {
    user = users[row]
    user.interactions = interactions[interactions$user == user,]
    end = max(user.interactions$date)
    start = end - 30
    user.interactions = interactions[interactions$date >= start & interactions$date <= end & interactions$user == user,]
    dates = sort(user.interactions$date)
    mean.interval = c(mean.interval, mean(diff(dates)))
    num.interaction = c(num.interaction, nrow(user.interactions))
    num.active.days = c(num.active.days, difftime(max(dates),min(dates), units = "days"))
  }
  
  metrics = data.frame(matrix(NA, nrow = length(users), ncol = 1))
  metrics$user = users
  metrics$num_interaction = num.interaction
  metrics$mean_interval = mean.interval
  metrics$num_active_days = num.active.days
  
  return(metrics[2:5])
}