#########################################################+
###      Manipulación y transformación de datos       ###
###                <Amadeo Guzmán C.>                 ### 
#########################################################+

# Librerias
library(tidyverse)
library(skimr)
library(readxl)


if (!require("ggrepel")) install.packages("ggrepel")
library(ggrepel)

#
##
###   BASE DE DATOS FIFA 2020 -------------------------------------------------------------------------------------------------------------------------------
##
#

#En esta oportunidad vamos a cargar una base de datos de formato .csv -> Antes de cargarla en R abrela directamente en Excel y mira como se ve
datos_fifa <- read_csv("players_fifa_20.csv")

head(datos_fifa)

dim(datos_fifa)

glimpse(datos_fifa)
skim(datos_fifa)


# Algunas manipulaciones basicas de la base de datos 

#TOP 5 en base a puntuacion general 
datos_fifa %>% 
  select(short_name, overall) %>% 
  arrange(desc(overall)) %>% 
  top_n(5)

#TOP 3 en base a precio de mercado
datos_fifa %>% 
  select(short_name, value_eur) %>% 
  arrange(desc(value_eur)) %>% 
  top_n(3)


#.....Chile TOP5 en base a puntuacion
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



#
### Veamos algunos grÃ¡ficos generales combinando manipulaciÃ³n y visualizacion ----
#


###   Ejemplo 1: puntaje promedio por país
datos_fifa %>% 
  group_by(nationality) %>% 
  summarize(ptje_mean = mean(overall),
            n = n()) %>% 
  #aqui parte el gafico
  ggplot(aes(nationality, ptje_mean)) +
  geom_col()


#.... no se ve nada. Mejoremos el gráfico anterior
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


#ahora si....Mejoremos el gráfico anterior
(graf_col_chile <- datos_fifa %>% 
    group_by(nationality) %>% 
    summarize(ptje_mean = mean(overall),
              n = n()) %>% 
    filter(n >= 100) %>% #aplicamos un filtro para selccionar paises con una determinada caracteristica
      #aqui parte el gafico
    ggplot(aes(fct_reorder(nationality, ptje_mean), ptje_mean, fill=if_else(nationality=="Chile", "Chile", "Otro"))) +
    geom_col() +
    scale_fill_manual(values = c("firebrick", "grey60")) +
    labs(title= "Puntaje promedio por pais en FIFA 2020",
         subtitle = "Solo se consideran paises con mas de 100 jugadores",
         y="Overall",
         x=NULL,
         caption = "Fuente: FIFA 2020 | EA Sports") +
    geom_hline(yintercept = prom_total$promedio, lty=2, color="darkblue") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          axis.ticks.y = element_line(),
          plot.caption = element_text(face = "italic", size=8, color="grey70")) +
    coord_flip(expand = FALSE) +
    geom_text(data=prom_chile, aes(label=round(ptje_mean,1)), size=2.8, hjust= 1.1, color="white", vjust=0.25) +
    annotate("text", x=3, y=prom_total$promedio+0.5, label=paste0("Promedio {", round(prom_total$promedio,1), "}"), size=2.5, angle=90, color="darkblue")
)


#Guarda tu grafico... debes completar los elementos que necesitas
ggsave()





###   Ejemplo2: relación entre edad y puntaje general en jugadores nacionales (Chile).... además identificaremos cuantos tienen 
#reputación internacional > 2

(graf_2 <- datos_fifa %>% 
    filter(nationality == "Chile") %>% 
    ggplot() +
    geom_jitter(aes(age, overall, color = international_reputation >= 2, drop=short_name), alpha=.5, size=2.5) +
    geom_smooth(aes(age, overall)) +
    theme_minimal() +
    theme(legend.position = "none")
)


#No solo graficos estaticos....tambien los podemos hacer interactivos facilmente con la libreria {Plotly}
if (!require("plotly")) install.packages("plotly")

library(plotly)

ggplotly(graf_2) #agregamos un poco de interactividad....






###   Ejemplo 3: Relacion entre la potencia del disparo, la capacidad de finiquito en ataque y el pie dominante
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
         subtitle = "Jugadores con valoracion (overall) mayor > 85 en FIFA 2020")
)


ggplotly(fifa_1) #interactivo



#Guarda tu grafico... debes completar los elementos que necesitas
ggsave()


