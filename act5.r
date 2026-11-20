
#punto 2
PKTQ001 = c(21, 13, 13, 33, 20, 22, 14, 22, 22, 28, 35, 30, 25, 35, 21, 20, 23, 13,
29, 13, 16, 35, 15, 33, 20, 22, 35, 23, 35, 20, 20, 24, 23, 14, 15, 16,
17, 22, 20, 27, 17, 19, 33, 24, 16, 23, 31, 26, 27, 15, 22, 25, 15, 16,
23, 34, 25, 27, 26, 15, 16, 21, 22, 19, 26, 30, 21, 20, 32, 17, 24, 24,
33, 35, 21, 16, 23, 31, 28, 17, 30, 30, 32, 29, 22, 27, 27, 33, 26, 18,
20, 13, 34, 23, 16, 34, 22, 33, 33, 26, 28, 20, 25, 22, 35, 30, 25, 24,
28, 18, 26, 19, 33, 23, 15, 21, 23, 14, 35, 32, 27, 15, 16, 24, 19, 30,
35, 20, 30, 30, 29, 28, 22, 31, 30, 33, 26, 18, 20, 14, 34, 23, 16, 34,
22, 33, 33, 26, 28, 20, 25, 22, 35, 30, 25, 24, 28, 18, 26, 19, 33, 23,
16, 34, 15, 13, 27, 32)


PKTQ002 = c(28, 21, 18, 19, 24, 32, 29, 22, 21, 24, 34, 24, 19, 24, 14, 34, 34, 33,
19, 16, 34, 34, 30, 35, 26, 32, 26, 17, 35, 16, 22, 14, 23, 32, 15, 14,
18, 32, 25, 32, 31, 16, 18, 35, 23, 35, 33, 27, 30, 35, 21, 18, 31, 17,
25, 29, 14, 14, 17, 21, 13, 22, 31, 27, 19, 32, 33, 34, 32, 32, 26, 14,
26, 15, 27, 19, 15, 26, 28, 33, 13, 23, 27, 25, 21, 34, 32, 16, 26, 21,
23, 20, 17, 27, 27, 32, 18, 19, 19, 27, 32, 32, 32, 32, 26, 16, 19, 19,
14, 17, 34, 27, 19, 19, 27, 32, 18, 19, 19, 27, 32, 32, 32, 32, 26, 14,
26, 15, 27, 19, 15, 26, 28, 33, 13, 23, 27, 25, 21, 34, 32, 16, 26, 21,
23, 20, 17, 27, 27, 32, 18, 19, 19, 27, 32, 32, 33, 26, 28, 20, 25, 22,
35, 30, 25, 24, 28, 18)

media1 = mean(PKTQ001)
media2 = mean(PKTQ002)

cat("Media de PKTQ001:", media1, "\n")
media1
cat("Media de PKTQ002:", media2, "\n")
media2

varianza1 = var(PKTQ001)
varianza2 = var(PKTQ002)

cat("Varianza de PKTQ001:", varianza1, "\n")
varianza1
cat("Varianza de PKTQ002:", varianza2, "\n")
varianza2

#punto 3
diferencia = media1 - media2
cat("Diferencia de medias:", diferencia, "\n")

#tamaño de muestras
n1 = length(PKTQ001)
n2 = length(PKTQ002)

error_estandar =sqrt(varianza1/n1 + varianza2/n2)
cat("Error estándar de la diferencia de medias:", error_estandar, "\n")

#punto 4 intervalo de confianza
resultado = t.test(
    PKTQ001, PKTQ002,
    paired = FALSE,
    var.equal = FALSE,
    conf.level = 0.95
)
cat("Intervalo de confianza del 95% es:", resultado$conf.int, "\n")

#punto 5, prueba de hipotesis
#H0: las medias poblacionales son iguales
#H1: las medias poblacionales son diferentes

#nivel de significancia 5%
alpha = 0.05
cat("\nEstadístico t:\n")
resultado$statistic

#valor de p
cat("\nValor p:\n")
resultado$p.value

#calcular valor critico
t_critico = qt(1 - alpha/2, df = resultado$parameter)

#region de rechazo
cat("\nSe rechaza H0 si t <", -t_critico, "o t >", t_critico, "\n")