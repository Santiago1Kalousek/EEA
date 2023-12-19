library(dplyr)
# Cargar el paquete tidyverse
library(tidyverse)

# Instalar y cargar el paquete 'ada' si aún no lo has hecho
if (!requireNamespace("ada", quietly = TRUE)) {
  install.packages("ada")
}


library(ada)

# Ruta al archivo CSV
file_path <- "C:/Users/Kalou/OneDrive/Escritorio/EEA/Netflix/new_df.csv"

# Leer el archivo CSV
df_1 <- read.csv(file_path, header = TRUE)

print(df_1)

# Dividir el conjunto de datos en entrenamiento y prueba
set.seed(42)  # Asegurar reproducibilidad
index <- sample(1:nrow(df), 0.8 * nrow(df))
X_train <- df_1[index, c("User", "Movie_1", "Movie_2")]
X_test <- df_1[-index, c("User", "Movie_1", "Movie_2")]
y_train <- df_1$Preferencia[index]
y_test <- df_1$Preferencia[-index]


# Definir el modelo RankBoost
rankboost_model <- ada::ada(X_train, y_train, iter = 50, type = "discrete")

# Predecir en el conjunto de prueba
predictions <- predict(rankboost_model, X_test)


# Convertir el factor a numérico para realizar comparaciones
predictions_numeric <- as.numeric(as.character(predictions))

# Evaluar el rendimiento
accuracy <- sum(predictions_numeric == y_test) / length(y_test)
print(paste("Accuracy:", accuracy))


