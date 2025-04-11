# Datos de frecuencia cardíaca (modificados)
frec_fisica <- c(70, 75, 80, 78, 72, 82, 79, 81, 77, 77,
                 75, 76, 80, 77, 73, 82, 79, 85, 76, 82,
                 88, 80, 81, 79, 75, 78, 74, 79, 77, 81)

frec_sedentaria <- c(95, 100, 98, 97, 95, 102, 103, 99, 96, 94,
                     101, 97, 100, 102, 96, 104, 101, 99, 98, 95,
                     94, 100, 99, 96, 101, 102, 97, 103, 98, 99)

# Calcular estadísticas directamente
media_fisica <- mean(frec_fisica)
media_sedentaria <- mean(frec_sedentaria)

sd_fisica <- sd(frec_fisica)
sd_sedentaria <- sd(frec_sedentaria)

cv_fisica <- (sd_fisica / media_fisica) * 100
cv_sedentaria <- (sd_sedentaria / media_sedentaria) * 100

# Prueba t de Student
t_prueba <- t.test(frec_fisica, frec_sedentaria)

# Resultados
cat("\nEstadísticas descriptivas:\n")
cat("Frecuencia física - Media:", media_fisica, 
    ", Desviación estándar:", sd_fisica, 
    ", CV:", cv_fisica, "%\n")
cat("Frecuencia sedentaria - Media:", media_sedentaria, 
    ", Desviación estándar:", sd_sedentaria, 
    ", CV:", cv_sedentaria, "%\n\n")

# Grafico 
boxplot(frec_fisica, frec_sedentaria,
        names = c("Físicos", "Sedentarios"),
        col = c("lightblue", "salmon"),
        main = "Distribución de Frecuencia Cardíaca por Grupo",
        ylab = "Frecuencia cardíaca (lpm)")

# Mostrar resultado de la prueba t
cat("\nResultados de la prueba t de Student:\n")
cat("Valor p:", t_prueba$p.value, "\n")

# Hipótesis y conclusión
if (t_prueba$p.value < 0.05) {
  cat("Se rechaza H₀ y se acepta H₁: hay una diferencia significativa entre las frecuencias cardíacas de los dos grupos.\n")
} else {
  cat("No se rechaza H₀: no hay evidencia suficiente para afirmar que existe una diferencia significativa.\n")
}