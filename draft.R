
import::from(magrittr, "%T>%", "%$%", .into = "operadores")
import::from(lubridate, .except = c("intersect", "setdiff", "union"))
import::from(conectigo, conectar_msql)
import::from(dbplyr, in_schema)
pacman::p_load(janitor, tidyverse)

con <- conectar_msql()


sitios <- tbl(con, in_schema("tkd", "w_sitios")) |>
 collect()


options(pillar.sigfig    = 5,
        tibble.print_min = 10,
        scipen = 999,
        digits = 7,
        readr.show_col_types = FALSE,
        dplyr.summarise.inform = FALSE)

# esta tabla nos dice cuanto tiempo estuvo fuera de servicio la RBS únicamente para cuando
# root_cause es igual a respaldo insuficiente y sin respaldo. duration + running_on_battery = power_cut (segundos más segundos menos)
# esta tabla respresenta el escenario en el que ya el MG dejó de funcionar, el MG aun no se ha encendido. esto último es importante
# ya que está registrando cada vez que las baterías trabajan aunque despúes el MG empiece a funcionar.
# las fallas de root_cause no son solo de energía, pueden ser de cualquier cosa, por esta
# razón no siempre se va a corresponder con la tabla de mg. lo que hay que hacer es ver cuales
# corresponde en la tabla de mg (que son las de verdad de energía) y ver si hay datos en esa fecha
# en la tabla root cause.
root_cause <- read_csv("site_root_cause.csv", lazy = FALSE, name_repair = make_clean_names)


# duration: cuanto duró la afectación del sitio.  se cayó totalmente la RBS. duracion de la caida de la RBS. Tomar en
# cuenta que solo se calcula si la afectación es mayor a 0.25 horas (15 min) para el caso de los "sin_respaldo"
# root_cause: los que dicen con_respaldo significa que no se cayó la RBS. esto se marca porque la central avisó que no encuentra el servicio.
# running_on_battery: cuanto tiempo trabajó la batería. los tiempos son pequeños porque se va la luz un ratito.
# power_cut: tiempo que duró el corte total de la energía y es la diferencia entre cleared_on and ocurred_on. Obviamente
# si hay respaldo de baterías el tiempo de caída no es igual al tiempo de power_cut.



# nos indica si el motor entró a trabajar o no. se relaciona con la tabla de baterías en el
# sentido de que en la de baterías se asumen varias cosas: el MG aun no entra a trabajar,
# el MG ya no tiene combustible o no funcionó. Entonces se deben combinar las mediciones
# de batería con MG, por lo que deberían coincidir las afectaciones de
# en esta tabla se guarda la alarma interna del equipo, en cambio en la de baterias se
# guarda la alarma que genera la central, por esta razón pueden haber inconsistencia en
# las alarmas, es decir, pueden generarse alarmas de la central, pero no generarse alarmas
# internas.
mg <- read_csv("site_mg_efficiency.csv", lazy = FALSE, name_repair = make_clean_names)


# is_transference: 0 no funcionó la transferencia y 1 es que si funcionó la transferencia, es decir el mg arranco.
# is_affected: 0 no hubo afectación y 1 significa que si hubo afectación.  convertir a 1 si la variabes es mayor que cero.
# running_on_mg: tiempo que el mg estuvo funcionando
# outage: es como el power cut, donde nos da el tiempo que duró la interrupción de energía.
# not_configured: no tomar en cuenta.
# status: si el registro es válido


root_cause |> filter(cilocation == "GUA072", root_cause == "sin respaldo") |>
 slice(3)


# buscar en la tabla root_cause solo aquellas que tengan running_on_battery mayor que cero.
# esto significa que si podemos decir que la falla fue de energía, luego buscamos en la
# tabla mg, pero considerar que habrá un posible drift de minutos (+- 10 min) especialmente
# cuando source == "NetEco" (tiene adelantado 10 min)

# para el caso de GUA037 donde el 2020-07-27 16:56:32 estuvo corriendo en baterías 2.76,
# vemos en la tabla mg dos registros en esta misma fecha y hora aproximada. Kenny sugiere
# tomar la duración más grande de la tabla mg.  Si te fijas la alarma de este sitio GUA037
# en esa fecha, duration en la tabla mg dice 4.0086 horas y en la tabla de root_cause dice
# que power_cut fue de 4 horas cerradas, ahí está la coincidencia.


# indica que tipo de mg tiene cada sitio
atributos <- read_csv("site_attribute.csv", lazy = FALSE, name_repair = make_clean_names)

# value: si hay motogenerador o no


atributos |>
 pivot_wider(names_from = "name", values_from = "value") |>
 clean_names()

# periodicidad: cada cuanto se le hace mantenimiento al motor en meses
# capacidad_tanque: galones

# si dice NO el mg los atributos deberían decir NA.

# CENTRO si tiene MG





