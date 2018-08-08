.libPaths(c(.libPaths(), "/home/gpscom/R/x86_64-pc-linux-gnu-library/3.2"))
library(readr)
library(igraph)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(RMySQL)
library(gtools)
# theme(
#   axis.title.x = element_blank(),
#   axis.title.y = element_blank(),
#   panel.border = element_blank(),
#   panel.grid=element_blank(),
#   axis.ticks = element_blank(),
#   plot.title=element_text(size=14, face="bold")
# )
# blank_theme <- theme_minimal()+

dbDisconnectAll <- function(){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons)
    dbDisconnect(con)
}

gerar_relacoes <- function(mensagens, usuarios){
  relacoes.geradas <- data.frame(source= character(0), 
                                 target= character(0), 
                                 mensagem = character(0))
  qtdMensagens <- nrow(mensagens)
  qtdUsuarios <- nrow(usuarios)
  issues <- as.list(unique(mensagens$issue))
  if(qtdMensagens != 0 || qtdUsuarios !=0){
    for(i in 1:length(issues)){
      issue_now <- filter(mensagens, issue == issues[i])
      issue_usuario <- unique(as.numeric(issue_now$user))
      usuarios_relacao <- NULL
      if(length(issue_usuario) > 1){
        usuarios_relacao <- permutations(n=length(issue_usuario),r=2,v=issue_usuario,repeats.allowed=F)
        colnames(usuarios_relacao) <- c("source", "target")
        data.frame.relacoes.usuario <- as.data.frame(usuarios_relacao)
        data.frame.relacoes.usuario["mensagem"] <- issues[i]
        relacoes.geradas <- rbind(data.frame.relacoes.usuario, relacoes.geradas)
      } 
    }
  }
  return(relacoes.geradas)
}


analisar_projeto <- function (mensagens, usuarios, relacoes, tabelasignificancias, projeto){
  message("criando objeto saida")
  projeto.percentuais.usuario <- data.frame(
    projeto = character(0),
    usuarios= numeric(0),
    p.usuarios.removidos= numeric(0), 
    usuarios.unico.dia = numeric(0),
    p.usuarios.unico.dia = numeric(0),
    usuarios.uma.relacao = numeric(0),
    p.usuarios.uma.relacao = numeric(0),
    usuarios.pouca.interacao = numeric(0),
    p.usuarios.pouca.interacao = numeric(0),
    usuarios.sem.relacao = numeric(0),
    p.usuarios.sem.relacao = numeric(0))
  message("objeto saida criado")
  # relacoes <- issue
  g <- graph.data.frame(relacoes,directed=F, vertices = usuarios)
  
  data_inicial <- min (as.Date(mensagens$dated))
  data_final <- max(as.Date(mensagens$dated))
  
  gt <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  
  is_simple(gt)
  ops<-list(sym=c(FALSE), complex=c(FALSE))
  message("gerou grafico")
  measure.evcent <- centralization.evcent(gt, directed=FALSE, scale=TRUE, options=ops)
  measure.in.degree <- centr_degree(gt, mode=c("out"))
  measure.out.degree <- centr_degree(gt, mode=c("in"))
  measure.total.degree <- centr_degree(gt, mode=c("total"))
  #measure.closeness <- closeness(components[i], mode="total", normalized=TRUE)
  
  
  measure.betweenness <- centr_betw(gt, directed=FALSE, normalized=TRUE)
  measure.transitivity <- transitivity(gt, type="local")
  measure.coreness <- graph.coreness(gt, mode="all")
  message("criando objetos")
  dataEntrada <- vector()
  dataSaida <- vector()
  total <-vector()
  mediaP <- c()
  mediaN <- vector()
  mediaS <- vector()
  mediaP_recebidas <- vector()
  mediaN_recebidas <- vector()
  mediaS_recebidas <- vector()
  quantidade_mens_enviadas <- vector()
  quantidade_mens_recebidas <- vector()
  turnoverIntervalo <- vector()
  turnover180dias <- vector()
  significancia <- vector()
  mensagens$user <- as.numeric(mensagens$user)
  mensagens$dated <- as.Date(mensagens$dated)
  #dados removidos
  remove <- data.frame(rmv_interacaoMesmoDia= numeric(0), 
                       rmv_menosDe3Interacoes= integer(0), 
                       rmv_interacaoComOutroUsuario = character(0),
                       rmv_nenhumaInteracao = logical(0),
                       rmv = logical(0))
  #dados removidos
  message("iniciando analise dos usuarios")
  for (row in 1:nrow(usuarios)) {
    my_mensagens = filter(mensagens, user == as.numeric(usuarios[row, "id"]))
    quantidade_mens_enviadas <- nrow(my_mensagens)
    my_issues = unique(my_mensagens$issue)
    my_mensagens_recebidas = filter(mensagens, issue %in% my_issues & user != as.numeric(usuarios[row, "id"]))
    quantidade_mens_recebidas <- nrow(my_mensagens_recebidas)
    menor <- min(my_mensagens$dated)
    maior <- max(my_mensagens$dated)
    
    dataEntrada <-  append(dataEntrada, as.Date(menor))
    dataSaida<-  append(dataSaida, as.Date(maior))
    mediaP <- append(mediaP, mean(as.numeric(my_mensagens$p)))
    mediaN <- append(mediaN, mean(as.numeric(my_mensagens$n)))
    mediaS <- append(mediaS, mean(as.numeric(my_mensagens$s)))
    mediaP_recebidas <- append(mediaP_recebidas, mean(as.numeric(my_mensagens_recebidas$p)))
    mediaN_recebidas <- append(mediaN_recebidas, mean(as.numeric(my_mensagens_recebidas$n)))
    mediaS_recebidas <- append(mediaS_recebidas, mean(as.numeric(my_mensagens_recebidas$s)))
    dates <- format(as.Date(my_mensagens$dated, format="%d/%m/%Y"),"%m/%Y")
    my_mensagens$mesAno <- dates
    total <- append(total, mean(aggregate(id ~ mesAno, my_mensagens, FUN="length")$id))
    #removerDados
    remove <- rbind(remove, verifica_remocoes(mensagens = my_mensagens, usuario.id = usuarios[row, "id"]))
    #removerDados
    diasSemInteracoes <- difftime(data_final, as.Date(maior), units = "days")
    #verifica turnover
    numeroInteracoes <- length(my_mensagens$dated)
    diasPermitidos <- NULL
    significanciaUser <- NULL
    if(numeroInteracoes > 3){
      d <- diff(sort(as.Date(my_mensagens$dated)))
      d <- as.numeric(d)
      significanciaUser <- ((as.numeric(diasSemInteracoes) - mean(d))/sd(d))
    } else {
      significanciaUser <- NA
    }
    significancia <- append(significancia, significanciaUser)
    
    if(is.na(significanciaUser)){
      if(diasSemInteracoes > 180){
        turnoverIntervalo <- append(turnoverIntervalo, TRUE)
      } else {
        turnoverIntervalo <- append(turnoverIntervalo, FALSE)
      }
    } else {
      numeroObservacoes <- numeroInteracoes - 1
      peso <- filter(tabelasignificancias, observacoes == numeroObservacoes)
      if(nrow(peso) != 0){
        if(significanciaUser > peso$significancia){
          turnoverIntervalo <- append(turnoverIntervalo, TRUE)
        } else {
          turnoverIntervalo <- append(turnoverIntervalo, FALSE)
        } 
      } else if(significanciaUser > max(tabelasignificancias$significancia)){
        turnoverIntervalo <- append(turnoverIntervalo, TRUE)
      } else {
        turnoverIntervalo <- append(turnoverIntervalo, FALSE)
      }
    }
    if(diasSemInteracoes > 180){
      turnover180dias <- append(turnover180dias, TRUE)
    } else {
      turnover180dias <- append(turnover180dias, FALSE)
    }
  }
  message("finalizando analise dos usuarios")
  usuarios$dataEntrada <- dataEntrada
  usuarios$dataSaida <- dataSaida
  usuarios$mes <- total
  usuarios$mediaP <- mediaP
  usuarios$mediaN <- mediaN
  usuarios$mediaS <- mediaS
  usuarios$mediaP_recebidas <- mediaP_recebidas
  usuarios$mediaN_recebidas <- mediaN_recebidas
  usuarios$mediaS_recebidas <- mediaS_recebidas
  usuarios$quantidade_mens_recebidas <- quantidade_mens_recebidas
  usuarios$quantidade_mens_enviadas <-quantidade_mens_enviadas
  usuarios$significancia <- significancia
  usuarios$turnoverIntervalo <- turnoverIntervalo
  usuarios$turnover180dias <- turnover180dias
  usuarios <- data.frame(usuarios, measure.evcent$vector)
  usuarios <- data.frame(usuarios, measure.in.degree$res)
  usuarios <- data.frame(usuarios, measure.out.degree$res)
  usuarios <- data.frame(usuarios, measure.total.degree$res)
  #usuarios <- data.frame(usuarios, measure.closeness$res)
  usuarios <- data.frame(usuarios, measure.betweenness$res)
  usuarios$measure.transitivity <- measure.transitivity
  usuarios$measure.coreness <- measure.coreness
  usuarios$remove_interacaoMesmoDia <- remove$rmv_interacaoMesmoDia
  usuarios$remove_rmv_menosDe3Interacoes <- remove$rmv_menosDe3Interacoes
  usuarios$remove_rmv_interacaoComOutroUsuario <- remove$rmv_interacaoComOutroUsuario
  usuarios$remove_rmv_nenhumaInteracao <- remove$rmv_nenhumaInteracao
  usuarios$remove_rmv <- remove$rmv
  #gerar.graficos.remocao()
  #gerar.graficos.remocao.usuario.sem.relacao()
  #gerar.graficos.remocao.usuario.pouca.iteracao()
  #gerar.graficos.remocao.usuario.uma.relacao()
  #gerar.graficos.remocao.usuario.unico.dia()
  projeto.percentuais.usuario <- rbind(projeto.percentuais.usuario, gerar.porcentagem.removao(usuarios = usuarios, projeto.name = projeto))
  write.csv(filter(usuarios, remove_rmv == FALSE), file = paste(projeto,"_resultado_usuarios.csv"))
  write.csv(filter(usuarios, remove_rmv == TRUE), file = paste(projeto,"_resultado_usuarios_removidos.csv"))
  return(projeto.percentuais.usuario)
}

gerar.porcentagem.removao <-function(usuarios, projeto.name){
  message("Entrou para gerar as porcentagens")
  n.removidos <- nrow(filter(usuarios, remove_rmv == TRUE))
  total <- nrow(usuarios)
  p.removidos <- percent(n.removidos/total)
  
  n.usuarios.unico.dia <- nrow(filter(usuarios, remove_interacaoMesmoDia == TRUE))
  p.usuarios.unico.dia <- percent(n.usuarios.unico.dia/ total)
  
  n.usuarios.uma.relacao <- nrow(filter(usuarios, remove_rmv_interacaoComOutroUsuario == TRUE))
  p.usuarios.uma.relacao <- percent(n.usuarios.uma.relacao/ total)
  
  n.usuarios.pouca.interacao <- nrow(filter(usuarios, remove_rmv_menosDe3Interacoes == TRUE))
  p.usuarios.pouca.interacao <- percent(n.usuarios.pouca.interacao/ total)
  
  n.usuarios.sem.relacao <- nrow(filter(usuarios, remove_rmv_nenhumaInteracao == TRUE))
  p.usuarios.sem.relacao <- percent(n.usuarios.sem.relacao/ total)
  usuarios.remove <- data.frame(
    projeto = character(0),
    usuarios= numeric(0), 
    p.usuarios.removidos= numeric(0), 
    usuarios.unico.dia = numeric(0),
    p.usuarios.unico.dia = numeric(0),
    usuarios.uma.relacao = numeric(0),
    p.usuarios.uma.relacao = numeric(0),
    usuarios.pouca.interacao = numeric(0),
    p.usuarios.pouca.interacao = numeric(0),
    usuarios.sem.relacao = numeric(0),
    p.usuarios.sem.relacao = numeric(0)
  )
  usuarios.remove <- rbind(usuarios.remove, data.frame(projeto.name, total, p.removidos, 
                                                       n.usuarios.unico.dia, p.usuarios.unico.dia,
                                                       n.usuarios.uma.relacao, p.usuarios.uma.relacao,
                                                       n.usuarios.pouca.interacao, p.usuarios.pouca.interacao,
                                                       n.usuarios.sem.relacao, p.usuarios.sem.relacao))
  message("saiu")
  return(usuarios.remove)
}

verifica_remover_interacaoMesmoDia <- function (datas_interacao){
  datas_sem_repeticao <- unique(datas_interacao)
  if(length(datas_sem_repeticao)<=1){
    TRUE
  } else {
    FALSE
  }
}
verifica_remover_menosDe3Interacoes <- function (datas_interacao){
  if(length(datas_interacao)<4){# < que 4 porque entre 3 valores existe apenas 2 interacoes
    TRUE
  } else {
    FALSE
  }
}

verifica_remover_interacaoComOutroUsuario <- function (usuario.id){
  my_relacoes <- filter(relacoes, source == as.character(usuario.id))
  usuariosRelacionados <- as.character(unique(my_relacoes$target))
  if(length(usuariosRelacionados) <= 1){
    TRUE
  } else {
    FALSE
  }
}

verifica_remover_nenhumaInteracao <- function (usuario.id){
  my_relacoes <- filter(relacoes, source == as.character(usuario.id))
  if(nrow(my_relacoes) == 0){
    TRUE
  } else {
    FALSE
  }
}

verifica_remocoes <- function(mensagens, usuario.id){
  x <- verifica_remover_interacaoMesmoDia(as.Date(mensagens$dated))
  y <- verifica_remover_menosDe3Interacoes(as.Date(mensagens$dated))
  w <- verifica_remover_interacaoComOutroUsuario(usuario.id)
  z <- verifica_remover_nenhumaInteracao(usuario.id)
  r <- FALSE
  if(x || y || w || z){
    r <- TRUE
  }
  return(data.frame(rmv_interacaoMesmoDia=x, rmv_menosDe3Interacoes=y, 
                    rmv_interacaoComOutroUsuario=w,rmv_nenhumaInteracao=z,
                    rmv = r))
}

gerar.graficos.remocao <-function(){
  n.excluidos <- nrow(filter(remove, rmv == TRUE))
  n.n.excluidos <- nrow(filter(remove, rmv == FALSE))
  total <- n.excluidos + n.n.excluidos
  message("Remoção: De um total de ",total," foram removidos ",n.excluidos, "(",percent(n.excluidos/total),")")
  df <- data.frame(
    group = c("Removidos", "Não Removidos"),
    value = c(n.excluidos, n.n.excluidos)
  )
  
  plot<- gerar.grafico.pie(df, "Usuários removidos")
  ggsave("grafico_usuarios_removidos.png", width = 4, height = 4)
}

gerar.graficos.remocao.usuario.unico.dia <-function(){
  n.excluidos <- nrow(filter(remove, rmv_interacaoMesmoDia == TRUE))
  n.n.excluidos <- nrow(filter(remove, rmv_interacaoMesmoDia == FALSE))
  total <- n.excluidos + n.n.excluidos
  message("Interação de um único dia: De um total de ",total," foram encontrados ",n.excluidos, "(",percent(n.excluidos/total),")")
  df <- data.frame(
    group = c("Removidos", "Não Removidos"),
    value = c(n.excluidos, n.n.excluidos)
  )
  
  plot<- gerar.grafico.pie(df, "Usuários Unico Dia")
  ggsave("grafico_rmv_unico_dia.png", width = 4, height = 4)
}

gerar.graficos.remocao.usuario.uma.relacao <-function(){
  n.excluidos <- nrow(filter(remove, rmv_interacaoComOutroUsuario == TRUE))
  n.n.excluidos <- nrow(filter(remove, rmv_interacaoComOutroUsuario == FALSE))
  total <- n.excluidos + n.n.excluidos
  message("Interação com uma relação: De um total de ",total," foram encontrados ",n.excluidos, "(",percent(n.excluidos/total),")")
  df <- data.frame(
    group = c("Removidos", "Não Removidos"),
    value = c(n.excluidos, n.n.excluidos)
  )
  
  plot<- gerar.grafico.pie(df, "Usuários Uma Relação")
  ggsave("grafico_rmv_usuario_uma_relacao.png", width = 4, height = 4)
}

gerar.graficos.remocao.usuario.pouca.iteracao <-function(){
  n.excluidos <- nrow(filter(remove, rmv_menosDe3Interacoes == TRUE))
  n.n.excluidos <- nrow(filter(remove, rmv_menosDe3Interacoes == FALSE))
  total <- n.excluidos + n.n.excluidos
  message("Interação com menos de 3 ocorrências: De um total de ",total," foram encontrados ",n.excluidos, "(",percent(n.excluidos/total),")")
  df <- data.frame(
    group = c("Removidos", "Não Removidos"),
    value = c(n.excluidos, n.n.excluidos)
  )
  
  plot<- gerar.grafico.pie(df, "Usuários < 3 Interações")
  ggsave("grafico_rmv_usuario_interacao_pouca.png", width = 4, height = 4)
}

gerar.graficos.remocao.usuario.sem.relacao <-function(){
  n.excluidos <- nrow(filter(remove, rmv_nenhumaInteracao == TRUE))
  n.n.excluidos <- nrow(filter(remove, rmv_nenhumaInteracao == FALSE))
  total <- n.excluidos + n.n.excluidos
  message("Interação sem relação: De um total de ",total," foram encontrados ",n.excluidos, "(",percent(n.excluidos/total),")")
  df <- data.frame(
    group = c("Removidos", "Não Removidos"),
    value = c(n.excluidos, n.n.excluidos)
  )
  
  plot<- gerar.grafico.pie(df, "Usuários Sem Relação")
  ggsave("grafico_rmv_usuario_sem_relacao.png", width = 4, height = 4)
}

gerar.grafico.pie <-function(df, nome){
  pie <- ggplot(df, aes(x="", y=value, fill=group))+
    geom_bar(width = 4, stat = "identity") + coord_polar("y", start=0)
  return(pie + scale_fill_brewer(nome) + blank_theme +
           theme(axis.text.x=element_blank())+
           geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                         label = percent(value/nrow(usuarios))), size=5))
}
args <- commandArgs(trailingOnly = TRUE)
pagination <- args[1]
inicio <- ((as.numeric(pagination) - 1) * 50)
fim <- inicio + 50
message(inicio," ", fim)
inicioPasta <- (inicio+1)
pasta <- paste(inicioPasta,"-",fim, sep = "")
diretorio <- paste("/home/gpscom/Douglas - Mestrado/Analise Contribuidor Casual/",pasta, sep = "")
message(diretorio)
setwd(diretorio)
dbDisconnectAll()
tabelasignificancias <- na.omit(read.csv("/home/gpscom/Douglas - Mestrado/Analise Contribuidor Casual/tabelaSignificancia.csv", sep=";"))
mydb = dbConnect(MySQL(), user='root', password='root', dbname='github_extract', host='127.0.0.1')
rs = dbSendQuery(mydb, paste("SELECT * FROM github_extract.project  where name NOT LIKE '%Font-Awesome%' order by id limit ", inicio,",", 50,";"))
source(file = "/home/gpscom/Douglas - Mestrado/Analise Contribuidor Casual/iteracao.R")
