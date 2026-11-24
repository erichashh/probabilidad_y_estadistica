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