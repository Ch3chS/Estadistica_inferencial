##Para lectura de archivos xlsx
library(xlsx)
data <- data <- read.xlsx("EP02-datos.xlsx", sheetIndex = 1, stringsAsFactors = TRUE)
data %>% filter(dificultad=="PARAMETRO A FILTRAR")

##Para lectura de archivos csv
datos = read.csv2("EP01 Datos Casen 2017.csv", stringsAsFactors = TRUE)

##Verificar normalidad
#QQ-graph
g <- ggqqplot(data = orientales_previos,
              x = "orientales.Previo",
              color = "pink",
              xlab = "Desviaciones",
              ylab = "Tiempo previo para Orientales",
              title = "Grafico QQ") + theme_minimal()
##Shapiro
shapiro.test(orientales_previos$orientales.Previo)

##Para largos distintos de las muestras (Wilcox)
##datos corresponde al data frame, 44 el número total de muestras que se desean
muestra_1 <- datos[sample(nrow(datos), 44),]
a <- muestra_1$tiempo.A[1:20]
b<- muestra_1$tiempo.B[21:44]

##Para comprobar si las muestras son independientes, tanto a y b son un vector
any(a%in%b)
# Si es False, las muestras son independientes

##Diferencias significativas con prueba no paramétrica y muestras independientes, prueba de suma de rangos de Wilcoxon
## a, b son vectores de distinto tamaño y alfa es el nivel de significancia
prueba <- wilcox.test(a, b, alternative = "two.sided", conf.level = 1 - alfa)
##alternative para establecer si las muestras son mayor o menor en relación a otra. Two sided unicamente distintas entre si

##Para pasar a formato largo (mas de dos muestras), todas las muestras son vectores y se juntan en uno. Se les asigna columna iterando la cantidad de veces necesaria
## y se crea dataframe
tiempos <- c(muestra_3_A, muestra_3_B, muestra_3_C)
alg <- c(rep("A", length(muestra_3_A)), rep("B", length(muestra_3_B)), rep("C", length(muestra_3_C)))
datos_fin <- data.frame(alg, tiempos)

##Kruskal setea variable dependiente e independiente y utiliza datos en formato ancho
kruskal.test(tiempos ~ alg, data = datos_fin)

##Guardar una columna como vector
facil = data %>% filter(dificultad=="Baja")
computacion = facil %>% filter(area=="Computación")
computacion = computacion[["tiempo"]]

##Para dejar en formato largo se usa pivot longer, los vectores de materias se juntan en data frame y luego se pivotan para dejarlas únicamente
##en las dos columnas deseadas
datos = data.frame(computacion, literatura, quimica)
datos = datos %>% pivot_longer (c( "computacion", "literatura" , "quimica" ), names_to ="area" , values_to = "tiempo")