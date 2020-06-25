###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### TALLER DE INTRODUCCI?N A R                                            -----      
### MANIPULACI?N Y VISUALIZACI?N DE DATOS EN R Y TIDYVERSE (PARTE 1)      -----
### 
### Autor: Amadeo Guzm?n
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#
##
###   Algunas definiciones y consideraciones previas ----
##
#

# Cada l铆nea de texto que se escribe despu茅s del s铆mbolo # R no lo considera como un elemento a ejecutar --> Sirve para hacer comentarios, poner t铆tulos de secci贸n, links de pag. web, etc.
# En general, NO se recomienda usar acentos y ocupar la letra ?
# R reconoce diferencias entre letras mayusculas y minusculas
# R es un lenguaje de programaci贸n orientado a OBJETOS --> todo lo podemos guardar en un objeto y ocuparlo cuando sea necesario
# Un paquete es un conjunto de funciones y datos espec铆ficos para cada tema.
# Una funci贸n es una secuencia de c贸digos guardada como un OBJETO de R que toma variables de entrada, realiza un proceso y nos devuelve un resultado
# Un data frame es una colecci贸n rectangular de variables (columnas) y observaciones (filas) -> Cada columna DEBE ser una variable y cada fila DEBE ser una observaci贸n


# IMPORTANTE --> Para ejecutar un codigo en R puedes: (1) poner el cursor sobre el c贸digo y apretar Ctrl + Enter; (2) seleccionar el c贸digo y hacer clik en "Run"


#lo mas bacio que debemos saber.......
2+2

a <- 5 #guardamos el valor 5 como un objeto de nombre "a"
a      #llamamos al objeto "a" para conocer su valor

(b <- 15) #usando parentesis R nos muestra inmediatamente el resultado del objeto

#podemos hacer operaciones con estos objetos
b/a
a*b
(exp(a)-sqrt(b))/100

#podemos guardar el resultado de una operacion matematica como un objeto
(resultado <-  b/a)

#..... y ocuparlo o consultar su valor en cualquier momento durante la sesion actual de RStudio
resultado

#....... incluso podemos seguir haciendo operaciones con el valor del objeto "resultado"
resultado + 20

#....... podemos ocuparlo de cualquier forma
paste0("el resultado de b/a es = ", resultado)





#
##
### Cargar librerias al entorno de trabajo -----
##
#

# instalar paquetes (librer?as) en el pc --> Solo se hace 1 vez 

#install.packages(c("tidyverse", "lubridate", "janitor", "ggthemes", "scales"))


#Cada vez que abrimos una nueva sesion de R debemos cargar las librerias que vamos a ocupar (No instalar, solo cargar)
library(tidyverse)
library(scales)
library(ggthemes)
library(palmerpenguins)

library(skimr) #si no te reconoce esta libreria la debes instalar y despues cargar
library(readxl)
library(ggrepel)


#
##
###   PARTE 1: BASE DE DATOS CON MEDICIONES DE ALETA Y CULMEN EN PING?INOS DE LA ESTACI?N PALMER ----------------------------------------------------------------------------------------------------
##
#


#
#   1.1.- Datos ----- 
#

#En esta primera parte vamos a trabajar con datos "simples"... despu茅s trabajaremos con bases de datos que requieren mayor manipulaci贸n

#R puede cargar diferentes tipos de archivos(Excel, csv, txt, html, json, pdf, etc.)
#En estos ejemplos vamos a usar en primer lugar Excel tradicional (.xlsx) y en la segunda parte vamos a usar Excel separado por comas (.csv)


# cargar datos desde nuestra carpeta de trabajo
getwd() # con esta funci?n vamos a conocer cual es el directorio de trabajo actual

datos <- read_excel("bd_pinguinos.xlsx", sheet = 1)


# con la funci?n head() vemos por defecto las primeras 6 lineas de cada base de datos
head(datos)
tail(datos)

head(datos, 20) #tambien podemos elegir el n?mero de filas que queremos ver

View(datos)    #para ver toda la base de datos





#
#   1.2.- Explorar la base de datos -----
#

dim(datos)     #cantidad de filas y columnas del dataframe
names(datos)   #nombres de las variables
glimpse(datos) #resumen de las caracter?sticas de cada variable


#resumen general de todo el data set
skim(datos)

#si las variables que aprecen como <chr> las convertimos en factor (as.factor) nos entrega un mejor resumen de sus datos
skim(datos %>% 
       mutate(species = as.factor(species),
              island = as.factor(island),
              sex = as.factor(sex))
     )


#otra forma de obtener un resumen estad?stico
summary(datos)

#Tambien podemos generar tablas de contingencia que nos permite conocer la distribuci?n de datos entre categor?as
table(datos$species, datos$island)


########################## -- EJERCICIO -- #############################+
# como es la distribucion de datos segun la especie de pinguino y sexo


########################################################################+



#
#   1.3.- Visualizar datos con ggplot2 -----
#


# ggplot2 es un paquete que pertenece al universo Tidyverse y utiliza la gram?tica de los gr?ficos para construir sus visualizaciones
# funciona en base a capas de informaci?n



# _1.3.1 Elementos de Est?tica o aes() ----


ggplot()

ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g))
  
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()


# Podemos modificar algunas caracter?sticas generales FIJAS: color y forma
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = "blue", shape=3)


#Podemos agregar mas elementos al grafico, pero esta vez asociado a las VARIABLES de NUESTROS DATOS
#Esto es necesario hacerlo dentro del argumento aes()

#color
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point()


#color + forma
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species, shape = sex)) +
  geom_point()



#alternar est?ticas visuales y caracter?sticas fijas
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size=3)


#agregar transparencia a los puntos (para disminuir el efecto de la sobreposici?n de puntos)
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size=3, alpha = 0.4) #alpha toma valores entre 0 y 1



#NOTA: Una vez que mapeamos variables con propiedades est?ticas, el paquete ggplot2 selecciona una escala razonable para usar con la est?tica elegida 
#y construye una leyenda que explica la relaci?n entre niveles y valores.



#################################### -- EJERCICIO -- ######################################################+
# Modificar los grAficos anteriores esta vez utilizando las variables "culmen_length_mm" y "culmen_depth_mm"
# y agregar una capa de color con una variable categ?rica que no sea la especie



#############################################################################################################+


#de momento hemos asignado el color solo a variables categ?ricas. Qu? pasa si lo hacemos con una variable num?rica?
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = culmen_length_mm)) +
  geom_point()


#al mismo ejemplo anterior le agregamos una nueva capa para mejorar la escala de color
ggplot(data = datos,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = culmen_length_mm)) +
  geom_point() +
  scale_color_viridis_c() # existen una gran cantidad de escalas de color predise?adas..... y tambi?n puedes hacer las tuyas



##########################################  EJERCICIO  ####################################################+
# Que pasa si asignamos el color a una variable cuantitativa, pero con una condicion?? 
# ocupa la siguiente condicion: color = culmen_length_mm >45




##########################################################################################################+


#IMPORTANTE: Hacer visualizaciones no es solo poner alguna variable en cada eje, algo de color y listo. Debemos explorar los datos e intentar encontrar las relaciones que puedan existir. Debemos abordar este problema desde diferentes perspectivas y evitar caer en posibles "sesgos" o malas interpretaciones.

#Ejemplo
#Existe algun tipo de relacion entre las variables "culmen_length_mm" y "culmen_depth_mm"??

ggplot(data = datos,
       mapping = aes(x = culmen_length_mm, y = culmen_depth_mm)) +
  geom_point(size=2.5)

#Ajustamos una recta de regresi?n a estos datos
ggplot(data = datos,
       mapping = aes(x = culmen_length_mm, y = culmen_depth_mm)) +
  geom_point(size=2.5) +
  geom_smooth(method = "lm")


#Esta relacion es real?? Existen otras variables que puedan modificar el sentido de esta relaci?n??
ggplot(data = datos,
       mapping = aes(x = culmen_length_mm, y = culmen_depth_mm, color=species)) +
  geom_point(size=2.5) +
  geom_smooth(method = "lm")



#Nota: Lo que acabamos de ver es un ejemplo de lo que en estad?stica y probabilidad se conoce como la "Paradoja de Simpson" --> Una tendencia que aparece en varios grupos de datos desaparece cuando estos grupos se combinan y en su lugar aparece la tendencia contraria para los datos agregados (https://es.wikipedia.org/wiki/Paradoja_de_Simpson#:~:text=En%20probabilidad%20y%20estad%C3%ADstica%2C%20la,contraria%20para%20los%20datos%20agregados.)




#
# _1.3.2  Otros objetos geometricos disponibles ----
#

## Histogramas
ggplot(data = datos, 
       mapping = aes(x=flipper_length_mm)) +
  geom_histogram(color="white")


#el histograma tiene 1 problema --> Su aspecto puede variar dependiendo del n?mero de rangos-barras (bins) que se ocupen
ggplot(data = datos, 
       mapping = aes(x=flipper_length_mm)) +
  geom_histogram(color="white", bins = 5)


## Una buena alternativa - > Distribuci?n de densidad
ggplot(data = datos, 
       mapping = aes(x=flipper_length_mm)) +
  geom_density()



#################################### -- EJERCICIO -- ############################################################+
# Como es la distribuci?n de datos de la variable "flipper_length_mm" por especie. Puedes usar histograma,
# o un gr?fico de densidad



# Si pudiste hacer el gr?fico anterior..... ahora prueba usando el argumento "fill" dentro de aes()



#################################################################################################################+


## Gr?fico de barras -> Recuentos
ggplot(data = datos,
       mapping = aes(x=species)) +
  geom_bar()

#como podemos ordenar la barras -> paquete "forcats"
ggplot(data = datos,
       mapping = aes(x = forcats::fct_infreq(species))) +
  geom_bar()


ggplot(data = datos,
       mapping = aes(x = forcats::fct_rev(fct_infreq(species)))) +
  geom_bar()



#vamos a guardar nuestro gr?fico como un objeto de nombre "g1"
(g1 <- ggplot(data = datos,
       mapping = aes(x = forcats::fct_rev(fct_infreq(species)), fill=sex)) +
  geom_bar()
)


#barras horizontales
g1 + 
  coord_flip()

#Que pasa si olvidamos los par?ntesis?? -> Mira los warnings en la consola
g1 + 
  coord_flip



## Tipo Violin
ggplot(data = datos,
       mapping = aes(x= species, y=flipper_length_mm, fill= species)) +
  geom_violin(color="white", alpha=.7)


## Boxplot 
ggplot(data = datos,
       mapping = aes(x= species, y=flipper_length_mm)) +
  geom_boxplot()


## Boxplot  - horizontal
ggplot(data = datos,
       mapping = aes(x= species, y=flipper_length_mm)) +
  geom_boxplot() +
  coord_flip()


## Boxplot agregando otra variable de inter?s
ggplot(data = datos,
       mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot()


#si quieres eliminar la categor?a NA (datos faltantes) puedes correr el siguiente c?digo... vez algun cambio en la sintaxis..... pipe (%>%)
datos %>% 
  filter(!is.na(sex)) %>% 
  ggplot(mapping = aes(x= species, y=flipper_length_mm, fill=sex)) +
  geom_boxplot(alpha=.7) +
   #en la siguiente linea vamos a asignar colores manualmente
  scale_fill_manual(values = c("#DE982E","#245F96")) # puedes elegir colores desde este link: https://htmlcolorcodes.com/es/




#Nota: eliminar los datos faltantes y realizar otro tipo de manipulaciones de datos te permitira explorar de mejor forma tus datos... esto es todo un tema por si mismo y se podria abordar en otro taller..... quiz?s......??


#################################### -- EJERCICIO -- ############################################################+
# Una de las ventajas de hacer gr?ficos con ggplot2 es la posibilidad de combinar capas geom?tricas....
# Haz un gr?fico combinando dos o m醩 capas geom?tricas 




#################################################################################################################+



#
# _1.3.3  Agregar otra capa de informaci?n con facet_ ----
#


# IMPORTANTE: desde aqui seguiremos ocupando la sintaxis con (%>%)

#volvemos a ocupar un g?rafico de los que ya hicimos anteriormente....
datos %>% 
  #manipulaci?n de datos
  filter(!is.na(sex)) %>% 
  #gr?fico
  ggplot(mapping = aes(culmen_length_mm, culmen_depth_mm, color=species)) +
  geom_point()


datos %>% 
  #manipulaci?n de datos
  filter(!is.na(sex)) %>% 
  #gr?fico
  ggplot(mapping = aes(culmen_length_mm, culmen_depth_mm, color=species)) +
  geom_point() +
  facet_wrap(~sex)


datos %>% 
  #manipulaci?n de datos
  filter(!is.na(sex)) %>% 
  #gr?fico
  ggplot(mapping = aes(culmen_length_mm, culmen_depth_mm, color=species)) +
  geom_point() +
  facet_grid(island~sex)



#
# _1.3.4  Agregar t?tulos, modificaci?n de escalas y del aspecto general del gr?fico con theme() ----
#


(gra_final <- datos %>% 
  #manipulaci?n de datos
  filter(!is.na(sex)) %>% 
  #gr?fico
  ggplot(mapping = aes(species, flipper_length_mm, fill=species)) +
  geom_violin(alpha=.1, color="white") +
  geom_jitter(width = 0.1, shape=21, size=3, alpha=.25) +
  #geom_quasirandom(shape=21, size=2.5, alpha=.25)+
  geom_boxplot(alpha=.8, outlier.color = NA) +
  facet_wrap(~sex) +
    #titulos
  labs(title="Relacion entre el largo de la aleta y la especie de ping?ino observada",
       subtitle="Registros de 3 especies (Adelle, Chinstrap y Gentoo) en la esaci?n PALMER",
       x="\n Especie",
       y="Largo de aleta (mm) \n", 
       caption = "Fuente: Long Term Ecological Research Network") +
    #escalas
   scale_y_continuous(limits = c(160, 240), breaks = seq(100, 250, 20)) +
    #escalas de color-fill
   scale_fill_tableau() +
    #aspecto general de la trama
  theme_light() +
   theme(legend.position = "none",
         plot.caption = element_text(face = "italic", color = "grey60"),
         strip.background.x = element_rect(fill="#14334F"),
         panel.border = element_rect(color = "#14334F"),
         axis.ticks.x = element_line(color = "#14334F"),
         axis.text.x = element_text(color="black"))
)


gra_final + theme_dark()
gra_final + theme_fivethirtyeight()
gra_final + theme_solarized()
gra_final + theme_igray()
gra_final + theme_map()
gra_final + theme_economist()
gra_final + theme_hc()


#Quieres guardar t? gr?fico???
ggsave("graf_final.png", gra_final, dpi = 500, units = "cm", width = 28, height = 14)

 



#
##
###   PARTE 2: BASE DE DATOS FIFA 2020 -------------------------------------------------------------------------------------------------------------------------------
##
#

#En esta oportunidad vamos a cargar una base de datos de formato .csv -> Antes de cargarla en R abrela directamente en Excel y mira como se ve
datos_fifa <- read_csv("players_20.csv")

head(datos_fifa)


glimpse(datos_fifa)
skim(datos_fifa)


# Algunas manipulaciones basicas de la base de datos 

#TOP 5 en base a puntuaci贸n general 
datos_fifa %>% 
  select(short_name, overall) %>% 
  arrange(desc(overall)) %>% 
  top_n(5)

#TOP 3 en base a precio de mercado
datos_fifa %>% 
  select(short_name, value_eur) %>% 
  arrange(desc(value_eur)) %>% 
  top_n(3)


#.....Chile TOP5 en base a puntuaci贸n
datos_fifa %>% 
  filter(nationality == "Chile") %>%
  select(short_name, club, overall) %>%
  arrange(desc(overall)) %>% 
  top_n(5)


#planilla mas cara
datos_fifa %>% 
  group_by(club) %>% 
  summarize(total_eur = sum(value_eur)) %>% 
  arrange(desc(total_eur))





# Veamos algunos gr谩ficos generales combinando manipulaci贸n y visualizacion

#puntaje promedio por pa韘
datos_fifa %>% 
  group_by(nationality) %>% 
  summarize(ptje_mean = mean(overall),
            n = n()) %>% 
    #aqui parte el gafico
  ggplot(aes(nationality, ptje_mean)) +
  geom_col()

#.... no se ve nada. Mejoremos el gr醘ico anterior
#pero antes veamos cual es el valor promedio general de todos los paises
(prom_total <- datos_fifa %>% 
    group_by(nationality) %>% 
    summarize(ptje_mean = mean(overall)) %>% 
    summarize(promedio = mean(ptje_mean)) 
)

#... y el de chile
(prom_chile <- datos_fifa %>% 
  group_by(nationality) %>% 
  summarize(ptje_mean = mean(overall),
            n = n()) %>% 
  filter(nationality == "Chile")
)



#ahora si....Mejoremos el gr醘ico anterior
(graf_col_chile <- datos_fifa %>% 
  group_by(nationality) %>% 
  summarize(ptje_mean = mean(overall),
            n = n()) %>% 
  filter(n >= 100) %>% #aplicamos un filtro para selccionar paises con una determinada caracteristica
      #aqui parte el gafico
  ggplot(aes(fct_reorder(nationality, ptje_mean), ptje_mean, fill=if_else(nationality=="Chile", "Chile", "Otro"))) +
  geom_col() +
  scale_fill_manual(values = c("firebrick", "grey60")) +
  labs(title= "Puntaje promedio por pa铆s en FIFA 2020",
       subtitle = "Solo se consideran paises con mas de 100 jugadores",
       y="Overall",
       x=NULL,
       caption = "Fuente: FIFA 2020 | EA Sports") +
  geom_hline(yintercept = prom_total$promedio, lty=2, color="darkblue") +
  annotate("text", x=3, y=prom_total$promedio+0.5, label=paste0("Promedio {", round(prom_total$promedio,1), "}"), size=2.5, angle=90, color="darkblue") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_line(),
        plot.caption = element_text(face = "italic", size=8, color="grey70")) +
  coord_flip(expand = FALSE) +
  geom_text(data=prom_chile, aes(label=round(ptje_mean,1)), size=2.8, hjust= 1.1, color="white", vjust=0.25)
)



#Ejemplo2: relaci髇 entre edad y puntaje general en jugadores nacionales (Chile).... adem醩 identificaremos cuantos tienen 
#reputaci髇 internacional > 2
(graf_2 <- datos_fifa %>% 
  filter(nationality == "Chile") %>% 
  ggplot() +
  geom_jitter(aes(age, overall, color = international_reputation > 2, drop=short_name), alpha=.5, size=2.5) +
  geom_smooth(aes(age, overall)) +
  theme_minimal() +
  theme(legend.position = "none")
)


#install.packages("plotly")
library(plotly)

ggplotly(graf_2) #agregamos un poco de interactividad....


#Ejemplo 3: Relacion entre la potencia del disparo, la capacidad de finiquito en ataque y el pie dominante
(fifa_1 <- datos_fifa %>% 
  filter(overall>85) %>% 
  ggplot(aes(power_shot_power, attacking_finishing, label = short_name, color = preferred_foot)) +
  geom_text_repel(size=3)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  geom_jitter(alpha = 0.3, size = 2.5, width = 0.3, height = 0.3)+
  geom_smooth(method = "lm", color = "gray40", lty = 2, se = TRUE, size = 0.6, alpha=.1)+
  scale_color_manual(values = c("orangered","steelblue")) +
  labs(title = "Relacion entre la potencia del disparo, la capacidad de finiquito en ataque y el pie dominante",
       subtitle = "Jugadores con valoraci贸n (overall) mayor > 85 en FIFA 2020")
)


ggplotly(fifa_1) #interactivo



#Guarda tu grafico... debes completar los elementos que necesitas
ggsave()



#############################################  FIN  ####################################################+



### IMPORTANTE

#
# Errores comunes -----
#

#Comprueba la parte izquierda de tu consola: si es un +, significa que R no cree que hayas escrito una expresi?n completa y est? esperando que la termines. En este caso, generalmente es mejor comenzar de nuevo desde cero presionando ESCAPE para cancelar lo que estabas haciendo y que quedo incompleto.

#Colocar el + en el lugar equivocado: SIEMPRE debe ubicarse al final de la l?nea, no al inicio.


#
# Donde buscar ayuda -----
#

#En la consola de R escribe ?nombre_de_la_funcion y presiona enter. Te aparecer? la ayuda oficial que tiene R de capa paquete o funci?n
#SAN GOOGLE -> una buena opci?n es copiar y pegar el mensaje de error tal cual en google o escribir lo que quieres hacer (es mejor si buscas en ingl?s). Generalmente las primeras opciones en la b?squeda te van a enviar a un foro de "Stack Overflow"..... ah? estas en el lugar correcto
#Puedes encontrar informaci?n y ejemplos de c?mo usar cada paquete o funci?n escribiendo el nombre de lo que quieres buscar agregando la letra R. Existen muchas p?ginas, blogs y talleres sobre diferentes temas relacionados a R
#En Twitter usando #Rstats


#Como no perderme en estas casi 650 lineas de codigo?? --> Mira en la parte superior derecha del editor (script) aparecen unas l?neas horizonatales en forma de ?ndice... haz click en ellas.....




