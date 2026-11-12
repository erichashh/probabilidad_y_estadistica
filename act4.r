
#PARTE 1
tiempo = c(5, 7, 6, 8, 5, 6, 7, 8, 7, 6, 5, 8, 7,
  6, 5, 7, 8, 5, 6, 7, 8, 5, 6, 7, 8, 6,
  5, 7, 8, 6, 5, 7, 6, 8, 5, 6, 7, 8, 6,
  5, 7, 8, 5, 6, 7, 8, 7, 6, 5, 7, 8, 5,
  6, 7, 8, 7, 6, 5, 7, 8, 5, 6, 7, 8, 7)

n = length(tiempo)
media = mean(tiempo)
desviacion = sd(tiempo)

cat("Número de pedidos:", n, "\n")
cat("Media:", media, "días\n")
cat("Desviación estándar:", desviacion, "días\n")

#Histograma
hist(
    tiempo,
    breaks = seq(4.5, 8.5, by = 1),
    probability = TRUE,
    col = "lightblue",
    border = "white",
    main = "Histograma tiempo de entrega",
    xlab = "Tiempo de entrega en días",
    ylab = "Densidad",
    xlim = c(3,10), 
    ylim = c(0,0.4)
)

curve(
    dnorm(x, mean = media, sd = desviacion),
    from = 3, to =10,
    add = TRUE,
    col = "red",
    lwd = 2
)

legend(
    "topright",
    legend = c("Distribución normal"),
    col = c("red"),
    lwd = 2,
    bty = "n"
)

#calcular el intervalo de 95.4

limite_inferior = media - 2 * desviacion
limite_superior = media + 2 * desviacion

cat(
    "intervalo aproximado del 95.4%:",
    limite_inferior, "a", limite_superior, "dias\n"
)


#calcular la probabilidad de menos de 4 dias
probabilidad_menos_4 = pnorm(4, mean = media, sd = desviacion)
probabilidad_menos_4 * 100

cat("P(x < 4):", probabilidad_menos_4, "\n")
cat("Porcentaje:", probabilidad_menos_4 * 100, "%\n")


#PARTE 2

#intervalo de confianza del 95%
i_95 = t.test(tiempo, conf.level = 0.95)$conf.int
cat("Intervalo de confianza del 95%", i_95[1], "a", i_95[2], "dias\n")

#intervalo de confianza del 90%
i_90 = t.test(tiempo, conf.level = 0.90)$conf.int
cat("Intervalo de confianza del 90%", i_90[1], "a", i_90[2], "dias\n")
