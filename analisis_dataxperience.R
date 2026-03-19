analisis_dataxperience.R
# librerias
install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

# datos
animales <- data.frame(
  especie = c("Tigre","Elefante","Rinoceronte","Panda",
              "Gorila","Jaguar","Lince","Orangutan",
              "Leopardo","Tortuga Marina"),
  
  poblacion = c(3900,415000,27000,1864,
                1060,15000,2000,104000,
                700000,60000),
  
  perdida_habitat = c(85,70,80,60,
                      75,65,78,82,
                      55,88),
  
  caza_ilegal = c(90,75,95,40,
                  60,55,50,65,
                  45,70)
)

animales

# exploracion
summary(animales)
mean(animales$poblacion)
mean(animales$perdida_habitat)
mean(animales$caza_ilegal)

# grafico barras
ggplot(animales, aes(x = especie, y = poblacion)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Poblacion por especie",
       x = "Especie",
       y = "Poblacion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# histograma
ggplot(animales, aes(x = perdida_habitat)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Perdida de habitat",
       x = "Porcentaje",
       y = "Frecuencia")

# boxplot
ggplot(animales, aes(y = caza_ilegal)) +
  geom_boxplot(fill = "red") +
  theme_minimal() +
  labs(title = "Caza ilegal",
       y = "Nivel")

# correlacion
cor(animales[,c("poblacion","perdida_habitat","caza_ilegal")])

# modelo
modelo <- lm(poblacion ~ perdida_habitat + caza_ilegal, data = animales)
summary(modelo)

# grafico modelo
ggplot(animales, aes(x = perdida_habitat, y = poblacion)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Relacion habitat vs poblacion",
       x = "Perdida de habitat",
       y = "Poblacion")

# prediccion
nuevo <- data.frame(
  perdida_habitat = 90,
  caza_ilegal = 80
)

predict(modelo, nuevo)
