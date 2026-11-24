#Leer la base de datos CSV
datos = read.csv("/Users/eric/workspace/probabilidad_y_estadistica/IBM_RH_base_completa.csv")

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

