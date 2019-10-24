#Ejercicio 2, calificaciones
#En este primer apartado, usare la funcion nrow para saber el numero total de alumnos que hay y luego la funcion mean eleminando todos los que no se presentaron
nrow(Calificaciones) #Para el numero total de almunos, con saber el numero total de columnas que existen debiera ser valido

n_alumnos <- nrow(Calificaciones)
print(n_alumnos)
mean(Calificaciones$`Ex. JUNIO-12P`, na.rm = TRUE) #Para poder calcular la media del final sin considerar aquellos que tienen un NA, es decir, un no presentado

#En el segundo apartaddo, simplemente utilizaremos la funcion subset para poder filtrar y conocer el nombre de aquellos que que han aprobado el final aun habiendo asistido a menos del 50 por ciento de las clases

subset(Calificaciones, Calificaciones$`Asistencia -1P` < 0.5 & Calificaciones$`Ex. JUNIO-12P` > 5)

#En el tercer apartado , para poder cambiar los nombres dentro de la grafica hemos de referenciarla y luego simplemente encadenar ifelses para que se vayan modificiando los valores que queremos modificiar, es importante el is.na ya que si no no se tendrian en cuenta los suspensos previos y se sobreescribirian
Calificaciones$CALIFICACION <- ifelse(Calificaciones$Nota_FINAL >= 5 & Calificaciones$Nota_FINAL <= 6.99 & is.na(Calificaciones$CALIFICACION), "Aprobado",
                                               ifelse(Calificaciones$Nota_FINAL >= 7 & Calificaciones$Nota_FINAL <= 8.99 & is.na(Calificaciones$CALIFICACION), "Notable",
                                               ifelse(Calificaciones$Nota_FINAL >=9 & Calificaciones$Nota_FINAL <= 10 & is.na(Calificaciones$CALIFICACION), "Sobresaliente" ,"SUSPENSO")))

#En el cuarto apartado, guardamos la nueva grafica

#write.csv(Calificaciones, file = "Calificaciones2019.csv") #Antes de generar el Rmarkdown, es importante comentar TODAS LAS OPERACIONES de entrada y salida para que esto no nos de problemas a la hora de generar el html

#rm ( List = "calificaciones_ECO_2019")
