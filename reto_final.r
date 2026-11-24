#Leer la base de datos CSV
datos = read.csv("C:/Users/erice/workspace/probabilidad_y_estadistica/IBM_RH_base_completa.csv")

#--1. BINOMIAL--
#probabilidad estimada de trabajar horas extras
total_empleados = nrow(datos)
empleados_horas_extras = sum(datos$OverTime == "Yes")
p = empleados_horas_extras / total_empleados

#resultados
cat("Total de empleados:", total_empleados, "\n")
cat("Trabajan horas extras", empleados_horas_extras, "\n")
cat("No trabajan horas extras", total_empleados - empleados_horas_extras, "\n")
cat("Probabilidad estimada de trabajar horas extras", p, "\n")
cat("Porcentaje", p * 100, "%\n")

cat("\n")
#probabilidad de que exactamente 3 de 10 empleados trabajen horas extras
n = 10
prob_exactamente_3 = dbinom(3, n,p)
cat("Probabilidad de que 3 de 10 empleados trabajen hrs extras:", prob_exactamente_3, "\n")
cat("Porcentaje", prob_exactamente_3 * 100, "%\n")

#más de 3 empleados trabajen horas extras
prob_mas_de_3 = pbinom(3,n,p,lower.tail = FALSE)
cat("probabilidad de que mas de 3 empleados trabajen horas extras:", prob_mas_de_3, "\n")
cat("Porcentaje", prob_mas_de_3 * 100, "%\n")

#probabilidad de que ocurran entre un rango específico de eventos
prob_entre_2_y_4 =  sum(dbinom(2:4, n, p))
cat("Probabilidad de que entre 2 y 4 empleados trabajen horas extras:", prob_entre_2_y_4, "\n")
cat("Porcentaje", prob_entre_2_y_4 * 100, "%\n")

cat("\n")
#--2. DISTRIBUCION DE POISSON--
#solicitudes simuladas en 20 días hábilies

solicitudes =  c(2, 4, 1, 3, 2, 0, 5, 3, 1, 2,
                4, 3, 2, 6, 1, 3, 4, 2, 3, 1)

lambda = mean(solicitudes)

cat("Días habiles:", length(solicitudes),"\n")
cat("Total de solicitudes:", sum(solicitudes),"\n")
cat("Promedio de solicitudes por día:", lambda, "\n")

#ninguna solicitud dia habil

prob_cero = dpois(0, lambda)
cat("Probabilidad de no recibir solicitud:", prob_cero, "\n")
cat("Porcentaje", prob_cero * 100, "%\n")

#recibir menos de 3 solicitudes en un día hábil
prob_menos_de_3 = ppois(2, lambda)
cat("Probabilidad de recibir menos de 3 solicitudes:", prob_menos_de_3, "\n")
cat("Porcentaje", prob_menos_de_3 * 100, "%\n")

#--3. DISTRIBUCION NORMAL--
#revisar la distribución de la edad

edades = datos$Age
hist(edades, main="Distribucion de la edad de los empleados",
    xlab = "Edad en años", ylab = "Numero de empleados",
    col = "lightblue")

#media, desviacion estandar y curva normal
media_edad = mean(edades)
desviacion_edad = sd(edades)

cat("Edad promedio:", media_edad, "años", "\n")
cat("Desviación estándar de la edad:", desviacion_edad, "años", "\n")

curve(dnorm(x, mean = media_edad, sd = desviacion_edad),
      from = media_edad - 3 * desviacion_edad,
      to = media_edad + 3 * desviacion_edad,
      main = "Curva normal aproximada de las edades",
      xlab = "Edad en años", ylab = "Densidad",
      col = "blue", lwd = 2)

# probabilidad de tener entre 30 y 40 años
prob_edad_30_a_40 = pnorm(40.5, mean = media_edad, sd = desviacion_edad) - pnorm(29.5, mean = media_edad, sd = desviacion_edad)

cat("Probabilidad de tener entre 30 y 40 años:", prob_edad_30_a_40, "\n")
cat("Porcentaje", prob_edad_30_a_40 * 100, "%\n")

#probabilidad de tener mas de 40 años
# 3.4.2. Probabilidad de tener más de 40 años
prob_mas_de_40 = pnorm(40.5, mean = media_edad, sd = desviacion_edad, lower.tail = FALSE)
cat("Probabilidad de tener más de 40 años:", prob_mas_de_40, "\n")
cat("Porcentaje:", prob_mas_de_40 * 100, "%\n")

cat("\n")

#--4.ESTIMACION E INTERVALO DE CONFIANZA--
intervalo_edad = t.test(edades, conf.level = 0.95)$conf.int
cat("Estimacion puntual de la edad promedio:", media_edad, "\n")
cat("Intervalo de confianza del 95%:", intervalo_edad[1], intervalo_edad[2], "\n")

#--5. PRUEBA T BILATERAL PARA LA EDAD PROMEDIO--
prueba_edad = t.test(edades, mu = 38, alternative = "two.sided")
cat("Estadistico t:", prueba_edad$statistic, "\n")
cat("Valor p:", prueba_edad$p.value, "\n")

#--6. INTERVALODE CONFIANZA PARA LA DESVIACION ESTANDAR--
grados_libertad = length(edades) - 1

limite_inferior = sqrt(grados_libertad * desviacion_edad^2 / qchisq(0.975, grados_libertad))
limite_superior = sqrt(grados_libertad * desviacion_edad^2 / qchisq(0.025, grados_libertad))
cat("Intervalo de confianza del 95%:", limite_inferior, limite_superior, "\n")

#prueba para una desviacion estandar
sigma_referencia = 9

estadistico_chi = grados_libertad * desviacion_edad^2 / sigma_referencia^2
cola_inferior = pchisq(estadistico_chi, df=grados_libertad)
cola_superior = pchisq(estadistico_chi, df=grados_libertad, lower.tail=FALSE)
valor_p_sigma = 2 * min(cola_inferior, cola_superior)
cat("Estadistico chi cuadrado:", estadistico_chi, "\n")
cat("Valor p:", valor_p_sigma, "\n")