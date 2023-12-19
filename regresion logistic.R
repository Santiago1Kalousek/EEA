# Instalar y cargar el paquete data.table si no está instalado
# install.packages("data.table")
library(data.table)
library(tidyverse)
library(caTools)

# Cargar los datos utilizando fread, ignorando líneas problemáticas y usando "," como separador
file_path <- "C:/Users/Kalou/OneDrive/Escritorio/EEA/Netflix/df_completo.csv"
df <- fread(file_path)
# Imprimir información sobre el conjunto de datos
cat("Dataset 1 shape:", dim(df), "\n")
# Establecer la columna 'V1' como índice
row.names(df) <- df$V1
df$V1 <- NULL  # Eliminar la columna 'V1' si es necesario


result <- df %>%
  filter(!is.na(Rating) & Rating > 0) %>%  # Filtrar solo los ratings positivos
  group_by(Movie) %>%
  summarise(
    AvgRating = mean(Rating, na.rm = TRUE),
    NumUsers = n_distinct(User)
  )

# Fusionar el resultado con el dataframe original
df <- left_join(df, result, by = "Movie")

df

# Separar la columna 'Date' en 'Year', 'Month' y 'Day'
df <- df %>%
  mutate(Date = as.Date(Date)) %>%  # Asegurarse de que 'Date' sea de tipo fecha
  arrange(row.names(df)) %>%  # Ordenar por índice
  separate(Date, into = c("Year", "Month", "Day"), sep = "-")

# Asegurarse de que las nuevas columnas sean de tipo numérico si es necesario
df <- df %>%
  mutate(across(c(Year, Month, Day), as.integer))

# Mostrar el DataFrame resultante
print(df)

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
split <- sample.split(df$Vio, SplitRatio = 0.8)
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)

test_data

# Suponiendo que 'train_data' es tu conjunto de entrenamiento
model <- glm(Vio ~ User + Movie + AvgRating + NumUsers, data = train_data, family = binomial)

# Hacer predicciones en el conjunto de prueba
predictions <- predict(model, newdata = test_data, type = "response")

# Convertir las probabilidades a etiquetas binarias
predicted_labels <- ifelse(predictions > 0.5, 1, 0)

# Comparar las predicciones con las etiquetas reales
confusion_matrix <- table(predicted_labels, test_data$Vio)
print(confusion_matrix)

library(pROC)
roc_curve <- roc(test_data$Vio, predictions)
auc_score <- auc(roc_curve)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])

print(precision)
print(sensitivity)
print(specificity)
