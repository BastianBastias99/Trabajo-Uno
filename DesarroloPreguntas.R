set.seed(10)
sample(c("SI","NO"),10,replace=T)
 VotosSI<-("SI")
 VotosNO<-("NO")
 
 install.packages("tidyverse")
 library(tidyverse)
 
 setwd("~/GitHub/Trabajo-Uno")
 
 rm(list = ls())

 # Funcion votaciones, recibe los parametros total; votosSI; votosNO, con la condicion de que votosSI + votosNO = total, debido al supuesto de que no hay votos blancos ni nulos
 votaciones <- function(total,votosSI,votosNO){
   if (votosSI >= ((total/2)+1)) {
     print("Ha ganado el SI")
   } else{
     print("votosSI no tiene Quorum")
     if (votosSI >= total*0.3) {
       print("votosSI tiene un 30% de Quorum")
       if (votosSI >= total*0.3 & votosNO < total*0.3) {
         print("Ha ganado el SI")
       } else{
         if (votosNO >= ((total/2)+1)) {
           print("Ha ganado el NO")
         } else{
           print("votosNO tiene un 30% de Quorum")
           if (votosSI == votosNO) {
             print("Los Quorums son identicos, ha ganado el NO debido al empate")
           } else{
             if (votosSI > votosNO) {
               print("Ha ganado el SI")
             } else{
               print("Ha ganado el NO")
             }
           }
         }
       }
     } else{
       print("Ha ganado el NO")
     }
   }
 }
 # Ejecuta la funcion, recibe los parametros total; votosSI; votosNO.
 votaciones(9,5,2)
 # Funcion votaciones_solo_total, recibe solamente el parametro "total" de votos, los votosSi y votosNO se calculan a traves de una seed de aleatoriedad dada por el total de votos (ejemplo: set.seed(total))
 votaciones_solo_total <- function(total){
   set.seed(total)
   padron <- sample(c("SI","NO"),total,replace = TRUE)
   padron <- as.data.frame(padron)
   names(padron) <- c("votos")
   votosSI <- sum(with(padron,votos == "SI"))
   votosNO <- sum(with(padron,votos == "NO"))
   
   if (votosSI >= ((total/2)+1)) {
    print("Ha ganado el SI")
   } else{
     print("votosSI no tiene Quorum")
     if (votosSI >= total*0.3) {
     print("votosSI tiene un 30% de Quorum")
       if (votosSI >= total*0.3 & votosNO < total*0.3) {
     print("Ha ganado el SI")
       } else{
         if (votosNO >= ((total/2)+1)) {
           print("Ha ganado el NO")
         } else{
           print("votosNO tiene un 30% de Quorum")
           if (votosSI == votosNO) {
           print("Los Quorums son identicos, ha ganado el NO debido al empate")
           } else{
             if (votosSI > votosNO) {
           print("Ha ganado el SI")
             } else{
           print("Ha ganado el NO")
             }
           }
         }
       }
     } else{
       print("Ha ganado el NO")
     }
   }
 }
 # Ejecuta la funcion, solo hay que ingresar el total de votos, 10 por defecto segun el enunciado.
 votaciones_solo_total(10)
 
 ###Pregunta 2 
 
 rm(list = ls())
 
 datos_judiciales <- list(c("mp","Juan","Christofer"),
                          c("of","av01","ampr"),
                          c("of","av01","ante"),
                          c("of","av08","arme"),
                          c("of","av02","ante"),
                          c("of","av07","ampr"),
                          c("of","av03","dape"),
                          c("of","av01","meca"),
                          c("of","av02","dape"),
                          c("mp","Antonia"),
                          c("mp","Christian","Mario"),
                          c("mp","Jose","Pedro","Antonela"),
                          c("of","av05","meca"),
                          c("of","av04","dape"),
                          c("of","av02","arme"))
 
 contar_mp(datos_judiciales)
 contar_mp <- function(datos_judiciales){
   ninios <- c()
   contador <- list()
   estadistica <- list()
   for (i in datos_judiciales) {
     if (i[1] == "mp") {
       contador <- length(i[-1])
       ninios <- c(ninios,unlist(contador))
     }
   }
   unicos <- unique(ninios)
   for (contador_ninios in unicos) {
     estadistica[length(estadistica)+1] <- unlist(contador_ninios)
   }
   nueva_estadistica<- estadistica
   for (equis in 1:length(unicos)) {
     nueva_estadistica <- c(estadistica[[equis]][1], length(ninios[ninios == unicos[equis]]))
     print(paste("Se cuentan con",nueva_estadistica[2], "mp de",nueva_estadistica[1], "niños"))
   }
 }
 
 # Ejecuta la funcion, solo recibe el parametro datos judiciales
 contar_mp(datos_judiciales)