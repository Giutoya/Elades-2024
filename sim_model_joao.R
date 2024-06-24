##Modelo SIM estimado usando el paquete "sfcr"

#En este artículo mostramos cómo se puede utilizar el paquete sfcr para replicar el modelo SIM de Godley y Lavoie (2007 cap. 3)..
#Comenzamos cargando el paquete sfcr y los  paquetes tidyverse, que se van a utilizar para manipular los datos y generar las gráficas:
  
#install.packages("sfcr")
#install.packages("tidyverse")
library(sfcr)
library(tidyverse)

#Ecuaciones
#El primer paso es anotar las ecuaciones, variables exógenas y parámetros:
  

sim_eqs <- sfcr_set(
  TXs ~ TXd,
  YD ~ W * Ns - TXs,
  Cd ~ alpha1 * YD + alpha2 * Hh[-1],
  Hh ~ YD - Cd + Hh[-1],
  Ns ~ Nd,
  Nd ~ Y / W,
  Cs ~ Cd,
  Gs ~ Gd,
  Y ~ Cs + Gs,
  TXd ~ theta * W * Ns,
  Hs ~ Gd - TXd + Hs[-1]
)

sim_ext <- sfcr_set(
  Gd ~ 20,
  W ~ 1,
  alpha1 ~ 0.6,
  alpha2 ~ 0.4,
  theta ~ 0.2
)

#Baseline
#Con estos objetos definidos, podemos simular el modelo SIM. Haremos que funcione durante 100 períodos. También definiremos la ecuación oculta de antemano, asegurándonos de que el modelo esté escrito correctamente:

sim <- sfcr_baseline(
  equations = sim_eqs, 
  external = sim_ext, 
  periods = 100, 
  hidden = c("Hh" = "Hs"),
  method = "Broyden")
#NOTA:Los solucionadores de ecuaciones simultáneas ("Gauss", "Broyden" o "Newton") solo están llamados a resolver los valores de las variables cíclicas en el modelo.
#Podemos comprobar que el modelo genera los mismos resultados a largo plazo que el modelo presentado en la página 69 Godley y Lavoie (2007)..

sim %>%
  filter(period %in% c(1, 2, 3, 50)) %>%
  select(period, Gs, Y, TXd, YD, Hs) %>%
  t() %>%
  round(digits = 0)

#Un aumento permanente en el gasto público
#¿Qué sucede si hay un aumento permanente en el gasto público? Para comprobarlo, necesitamos usar las  funciones sfcr_shock() y sfcr_scenario():
  

shock1 <- sfcr_shock(
  variables = list(
    Gd ~ 25
  ),
  start = 5,
  end = 50
)

sim2 <- sfcr_scenario(sim, shock1, 50)

#Para visualizar las consecuencias del choque con el paquete ggplot2, primero debemos transformar el modelo en formato largo. Podemos hacerlo con  la  función pivot_longer() de dplyr.
#La sintaxis puede parecer complicada, pero en realidad es simple. Con la función pivot_longer() queremos transformar todas las columnas del  objeto sim2 en el formato largo, indexado por la  columna de punto. Es por eso que en cols "eliminamos" la columna de punto.
#Para trazar, filtramos el objeto sim2_long para mantener solo Y
#. Como podemos ver, un aumento en el gobierno conduce a un aumento en la producción a largo plazo.


sim2_long <- sim2 %>%
  pivot_longer(cols = -period)

sim2_long %>%
  filter(name == "Y") %>%
  ggplot(aes(x = period, y = value)) +
  geom_line()

#Para trazar múltiples variables en la misma gráfica, necesitamos filtrar no para una variable específica, sino para un grupo de variables. Lo hacemos usando el operador R %in% en lugar de ==.
#Veamos cómo trazar YD, Cd y Hh



sim2_long %>%
  filter(name %in% c("YD", "Cd", "Hh")) %>%
  ggplot(aes(x = period, y = value)) +
  geom_line(aes(linetype = name, color = name))


# SIMEX model

#El modelo SIMEX amplía el modelo SIM introduciendo expectativas en el modelo. Técnicamente, modifica una de las ecuaciones y añade dos nuevas ecuaciones.
#También usamos la función sfcr_set() para modificar un conjunto de ecuaciones. Para modificar una ecuación existente, debemos escribir su nueva versión y excluir la antigua del conjunto con el  argumento exclude. Para encontrar el id correcto para excluir, podemos usar la  función sfcr_set_index() y luego filtrar los lhs del conjunto original:
  

sfcr_set_index(sim_eqs) %>%
  filter(lhs == "Cd")

simex_eqs <- sfcr_set(
  sim_eqs,
  Cd ~ alpha1 * YDE + alpha2 * Hh[-1],
  Hd ~ Hd[-1] + YDE - Cd,
  YDE ~ YD[-1],
  exclude = 3
)


#Utilizamos estas nuevas ecuaciones para simular el modelo SIMEX:

simex <- sfcr_baseline(simex_eqs, sim_ext, 50, hidden = c("Hh" = "Hs"))

#Y inmediatamente agregue un choque a la propensión a consumir en este modelo:

shock2 <- sfcr_shock(
  variables = sfcr_set(alpha1 ~ 0.7),
  start = 5,
  end = 50
)

simex2 <- sfcr_scenario(simex, shock2, 50)

#Luego terminamos este artículo trazando las consecuencias de un aumento en la propensión a consumir en el consumo, el ingreso disponible y la riqueza:

simex2_long <- simex2 %>%
  pivot_longer(cols = -period)

simex2_long %>%
  filter(name %in% c("Cd", "YD", "Hh")) %>%
  ggplot(aes(x = period, y = value)) +
  geom_line(aes(linetype = name))

#Un lector atento notaría que la Figura anterior no es la misma que la Figura 3.8 de Godley y Lavoie (2007). Esto se debe a que la Figura 3.8 choca con el modelo SIM y no con el modelo SIMEX. Dejaré como ejercicio para que el lector genere la Figura 3.8.


#Funcionalidades adicionales del  paquete sfcr
#Matrices del modelo SIM
#El paquete sfcr también proporciona funciones para escribir y validar las matrices de balance y flujo de transacciones de cualquier modelo SFC. Además, garantiza que el modelo sea consistente con el flujo de existencias. Aquí, las funciones principales son sfcr_matrix(), para escribir las matrices, y sfcr_validate(), para comprobar si la matriz es coherente con el modelo simulado.
#Sin embargo, la matriz de balance del modelo SIM es demasiado simple y no vale la pena presentarla aquí, ya que requeriría que creáramos dos variables adicionales que faltan variables: la riqueza de los hogares y la deuda pública. Estas entradas son importantes porque todas las columnas de la matriz del balance deben sumar a cero.


tfm_sim <- sfcr_matrix(
  columns = c("Households", "Firms", "Government"),
  codes = c("h", "f", "g"),
  c("Consumption", h = "-Cd", f = "+Cs"),
  c("Govt. Exp.", f = "+Gs", g = "-Gd"),
  c("Factor Income", h = "W * Ns", f = "-W * Ns"),
  c("Taxes", h = "-TXs", g = "+TXd"),
  c("Ch. Money", h = "-d(Hh)", g = "d(Hs)")
)

sfcr_validate(tfm_sim, sim, which = "tfm")

#Diagrama de Sankey

#Otro beneficio de tener una matriz de flujo de transacciones que se valida en el modelo es que podemos construir con confianza una representación de Sankey de esta matriz con la  función sfcr_sankey():

sfcr_sankey(tfm_sim, sim)

#DAG: La estructura del modelo
#El paquete sfcr también proporciona una función incorporada para visualizar la representación DAG de las ecuaciones en el modelo:
#También es posible visualizar la estructura de bloques del modelo con la  función sfcr_dag_blocks_plot():

sfcr_dag_cycles_plot(sim_eqs, size = 10)

sfcr_dag_blocks_plot(sim_eqs)

#Referencias

#Godley, Wynne y Marc Lavoie. 2007. Economía monetaria: un enfoque integrado del crédito, el dinero, el ingreso, la producción y la riqueza. Palgrave Macmillan.
#Desarrollado por Joao Macalos.


