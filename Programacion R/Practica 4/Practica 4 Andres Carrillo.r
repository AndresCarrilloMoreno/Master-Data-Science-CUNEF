# 1. Crear un nuevo proyecto denominado practica 4.

# 2. Mediante la libreria readr, o mediante los menus de RStudio, leer los datasets sleep.csv  y activities.csv
# ambos archivos deben estar previamente en la carpeta del proyecto creado
library(readr)
library(tidyverse)
library(dplyr)
library(rmarkdown)



# 3.Comprobar el contenido  con View y contar cuantos NAs hay en la columna GPS del dataset activities

view(actividades)
sum(is.na(actividades$GPS))
# 4. Crear un objeto R denominado act_new que contenga solo las variables 
# siguientes: 1,2,5-6

act_new <- select(actividades, 1,2, 5, 6)

# 5. Renombrar la variable 'Activity type' con el nombre 'tipo' y la variable 'Time zone' como 'ciudad' # Con renam

act_new <- rename(act_new, tipo = "Activity type", ciudad = "Timezone")


# 6. Realizar un recuento de tipo de actividad con summary. Para ello 
# debes transformar previamente la variable tipo a factor con as.factor.
# Crea un grafico de barras con dicha variable par visualizar las frecuencias.
# Haz lo mismo para la variable ciudad

act_new$tipo <- as.factor(act_new$tipo)
summary(act_new$tipo)
plot(act_new$tipo)

act_new$ciudad <- as.factor(act_new$ciudad)
summary(act_new$ciudad)
plot(act_new$ciudad)


#7. Filtrar los registros de act_new que correspondan con ciudad Amsterdam en otro objeto
# y lo mismo con Madrid. Con esos nuevos objetos determina los deportes que 
# no se practican en Amsterdam y s? en Madrid y viceversa. Genera graficos para visualizar los resultados

act_new_Amsterdam <- filter(act_new, ciudad == "Europe/Amsterdam")

act_new_Madrid <- filter(act_new, ciudad == "Europe/Madrid")

actividad_no_roma<- filter(act_new, act_new$ciudad != "Europe/Rome")
deporte_ams_mad <- select(actividad_no_roma, c(3:4))
deporte_amsmad <- select(actividad_no_roma, ciudad:tipo) #En este caso, pretendia comprobar el motivo de que antes no funcionase y fue debido a referenciarlo
rm(list = "deporte_amsmad")
ggplot(data = deporte_ams_mad) + geom_point(mapping = aes(x = ciudad, y =tipo)) #Este tipo de vista, nos permite ver rapidamente aquellos deportes que si que se practicamente en Madrid pero no en Amsterdam y viceversa


#8. Encontrar las fechas en las que se ha practicado bicicleta o pilates en Amsterdam en el a?o 2019

bic_pil_ams <- filter(act_new_Amsterdam,  tipo =='Cycling' | tipo == 'Pilates', str_detect(de, "2019"), str_detect(a, "2019")) #La funcion str_detect nos permite filtrar los elementos de una columna que incluyan lo que nos interese
#Siendo en este caso bastante util para poder filtrar el 2019 sin necesidad de modificar el tipo de variable, esta funcion forma parte de stingr dentro de tidyverse
filter(act_new_Amsterdam,  tipo =='Cycling' | tipo == 'Pilates', str_detect(de, "2019"), str_detect(a, "2019")) #En esta linea, queria comprobar si el filtro que se introdujo arriba funcionaba o no


#9. Crear una nueva variable dif con los minutos de realizaci?n de cada actividad en Amsterdam
# y realizar una representaci?n gr?fica de los resultados con plot y determinar que deporte o deportes
# se han practicado durante dos horas o mas

Dif <- mutate(act_new_Amsterdam, dif = a- de)

Dif <- Dif %>% group_by(tipo) %>% summarize(agregado = sum(dif))


ggplot(data = Dif) + geom_col(mapping = aes (x = tipo, y =agregado))

filter(Dif, agregado >= 120) #En este caso, el filtro nos sirve para poder saber aquellos deportes que se han practicado, en total, durante mas de dos horas


#10. Guardar el nuevo dataset en un archivo llamado  "act_new.csv"

write.csv(act_new, "act_new.csv")

#-------------------------------
#-----SEGUNDA PARTE-------------
# 11. Cargar el dataset sleep en un objeto llamado sleep

sleep <- read.csv("sleep.csv")


#12. crear un nuevo data set llamado sleep_new que contenga solo las variables
#que contengan informaci?n, que no sean todo cero.


#Para saber todas aquellas columnas que tienen todos los valores como 0, utilizare la funcion sum e ire mirando aquellas en las que los primeros elementos sean 0
sum(sleep[,5]== 0)
sum(sleep[,10]== 0) #En esta aparece NA, pero se puede observar que hay valores que son mayores que cero
sum(sleep[,11]== 0)
sum(sleep[,12]== 0)
sum(sleep[,13]== 0)
sum(sleep[, 14] == 0) #Gracias a esto sabemos que columnas tienen todos los valores iguales a cero y podemos eliminarlas


sleep_new <- sleep[, c(-5, -11, -12, -13, -14)] #Filtramos aquellas columnas cuyos elementos son = 0



#13. Renombrar las variables de sleep_new a nombres cortos:

sleep_new <- rename(sleep_new, ligero = 3, profundo = 4, despierto =5 , dormirse = 7, levantarse = 8, ronquidos = 9)



#14. Eliminar todas las filas que contengan alg?n NA


sleep_new <- na.omit(sleep_new) #Esta funcion nos ayuda a poder eliminar, de manera directa, todas aquellas filas que tengan valores NA
sum(is.na(sleep_new)) #Para poder saber si la funcion na.omit ha cumplido o no con su cometido,  utilizamos sum para poder ver cuantos na hay


# 15. Calcular cuanto tiempo en total se ha dormido cada noche: ligero+profundo

sleep_new<- mutate(sleep_new, total= ligero+profundo) #Se utiliza mutate para poder introducir una nueva columna que tenga el total de tiempo de sueño, en segundo
sleep_new <- mutate(sleep_new, total_min = total/60)
sleep_new <- mutate(sleep_new, total_h = total_min/60) #Para poder saber el numero de horas total de sueño, el "." simplemente indica que los decimales que hay despues


# 16. Visualizacion de la relacion ligero-profundo-total

plot(sleep_new$ligero, sleep_new$profundo)
plot(sleep_new$ligero, sleep_new$total)
plot(sleep_new$profundo, sleep_new$total)

# A la vista de los resultados, que tipo de sue?o es mas relevante?
# 17. Realizar un analisis de diferencias entre los dos tipos de sue?o e interpretar los resultados
# usar la funci?n ICalpha o el 'One sample t-test' de TeachingDemos: t.test()

t.test(x = ((sleep_new$profundo) - (sleep_new$ligero)), mu=0)

#Como el 0 no esta dentro del intervalo, si que existen diferencias significativas



#18. Crear una nueva variable 'ciudad' en sleep_new con la informacion de act_new.

#Como lo ha hecho la profesora:
for( i : in)

fecha <- substr(act_new$de, 1, 10)
act_new$fecha <- as.POSIXct(fecha)
act_new$de <- as.POSIXct(act_new$de)
act_new$a <- as.POSIXct(act_new$a)
fecha <- substr(sleep_new$de, 1, 10)
sleep_new$fecha <- as.POSIXct(fecha)
sleep_new$fecha <- as.POSIXct(sleep_new$de)
sleep_new$fecha <- as.POSIXct(fecha)
sleep_new <- inner_join(act_new, sleep_new)

#No soy capaz de finalizar este apartado, estuve intentando hacer diferentes comprobaciones a la hora de intentar convertir a posixct para intentar juntarlos

#19. Representar la relaci?n totalsleep y profundo usando como facetas el factor ciudad


#20. Guardar el dataset sleep_new en un archivo "sleep_new.csv"
write.csv(sleep_new, "sleep_new.csv")

#21. Guardar el proyecto completo. Subir la carpeta del proyecto al campus.