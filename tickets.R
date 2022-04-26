## _______________________________________________________________________ ###
## INICIO SCRIPT                                                           ###
 
## objetivo: cargar las vistas más importantes de ttkdoc para análisis de
## fallas.


##  ............................................................................
##  OPCIONES                                                                ####

options(
  scipen           = 999,
  digits           = 10,
  pillar.sigfig    = 10,
  tibble.print_min = 5,
  readr.show_col_types = FALSE
)

##  ............................................................................
##  PAQUETES                                                                ####


# paquestes
# library(conflicted)

# conflict_prefer("select", "dplyr", quiet = TRUE)
# conflict_prefer("filter", "dplyr", quiet = TRUE)
# conflict_prefer("between", "dplyr", quiet = TRUE)
# conflict_prefer("year", "lubridate", quiet = TRUE)
# conflict_prefer("last", "dplyr", quiet = TRUE)
# conflict_prefer("Id", "DBI", quiet = TRUE)
# conflict_prefer("here", "here", quiet = TRUE)
# conflict_prefer("first", "dplyr", quiet = TRUE)
# conflict_prefer("week", "lubridate", quiet = TRUE)
# conflict_prefer("month", "lubridate", quiet = TRUE)

import::from(magrittr, "%T>%", "%$%", .into = "operadores")


## cargar paquetes
pacman::p_load(tidyverse, 
               janitor, 
               conectigo, 
               lubridate,
               tictoc,
               fs, 
               fuzzyjoin,
               here,
               data.table,
               dbplyr,
               DBI)


##  ............................................................................

##  ............................................................................
##  EXTRAER DATOS                                                           ####


## conexión skynet
con <- conectar_msql()

## extraer fallas: 11.3 mb < 10 seg

vistas_ttkdoc <- c(
  
  "r_afectacion_tx",
  "r_ttks",
  "r_mttr",
  "r_item_falla",
  "r_sitios",
  "r_ubicacion"
)

## nombre de las vistas
nombres_vistas <- c(
  
  "afectacion",
  "ttks",
  "mttr",
  "items",
  "sitios",
  "ubicacion"
)

## crear función closure para extracción
leer_tkd <- leer_tabla('tkd')

## preparar objeto lista
tic()
ttkdoc <- map(vistas_ttkdoc, ~ leer_tkd(.x)) %>%
  set_names(nombres_vistas) %>%
  map(~ clean_names(.)) %>%
  map(~ mutate_if(., is.character, toupper)) %>%
  map(~ mutate_if(., is.character, str_trim, "both")) %>%
  map(~ mutate_if(., is.character, iconv, "UTF-8", "ASCII//TRANSLIT"))
toc()

## extraer tipo de cambio al día de hoy
# tc <- extraer_tipo_cambio()

source(here::here("R", "funciones_ttkdoc.R"))

  ruta <- path_wd("raw-data", "revenue_rbs_20210819", ext = "csv")
  
  # revenue_rbs_raw <- read_delim(ruta,
  #                      delim = ";",
  #                      lazy = FALSE,
  #                      show_col_types = FALSE,
  #                      col_select = c("YEAR",
  #                                     "MONTH",
  #                                     "siteid" = "SITE_ID",
  #                                     "sitename" = "SITE_NAME",
  #                                     "REVENUE_TOTAL")) %>%
  #   clean_names()
  
  
  revenue_rbs_raw <- read_delim(ruta,
                                delim = ";",
                                lazy = FALSE,
                                show_col_types = FALSE) %>%
    clean_names()
  
  

  revenue_rbs <- revenue_rbs_raw %>% 
    mutate(
      
      fecha = ISOdate(year = year, month = month, day = 1), 
      
      across(site_name, preparar), 
      
      revenue_hora = revenue_total / 720,
           
      .before = "site_id", 
      .keep = "unused"
    
    ) %>%
    
    filter(fecha == max(fecha)) %>%
    rename(sitename_revenue = site_name,
           siteid = site_id)
  
  

# analisis solola ----------------------------------------------------------------------

  
  
  # cuando es mutate: lo que vas a cambiar por el nuevo valor
  # mk_00 <- c("revenue_2g" = "2G",
  #            "revenue_3g" = "3G",
  #            "revenue_4g" = "LTE")
  #            
  #  
  # rev <- revenue_rbs |> 
  #   select(siteid, starts_with("revenue")) |> 
  #   select(-revenue_hora) |> 
  #   pivot_longer(cols = revenue_4g:revenue_2g, names_to = "tecnologia", values_to = "revenue_hora") |> 
  #   mutate(across(tecnologia, recode, !!!mk_00))
  # 
  # 
  # solola <- read_csv("./raw-data/solola.csv", lazy = FALSE, name_repair = make_clean_names)  
  # 
  # solola |> 
  #   left_join(rev, by = c("id_generico" = "siteid", "tecnologia")) |> 
  #   mutate(revenue_hora = revenue_hora / 720) |> 
  #   mostrar_en_excel()
  # 
  
  
    ## extrer listado de sitios fs
  # 2021-01-21: 5,218 sitios
  sitiosfs <- tbl(con, in_schema("fieldservice", "site_collate")) %>% 
    select(siteid = cilocation,
           sitename_fs = nombre,
           latitud_fs = latitude,
           longitud_fs = longitude,
           status) %>% 
    collect() %>% 
    distinct(siteid, .keep_all = TRUE)

  
  cat_tx <- tbl(con, in_schema("bds", "categoria_tx"))
  
  
  sitios_a_excluir <- c("ESV001", "GUA999", "HND001", "NAP001", "TOBEDE")
  
  # Extraer mejor shd_sitios para poder incluir los sitios Tx (e.g AMNET y
  # agencias)
  sitiostx <- tbl(con, in_schema("shd", "sitio")) %>% 
    select(siteid = id,
           categoria_tx_id,
           sitename_tx = nombre,
           latitud_tx = latitud,
           longitud_tx = longitud) %>% 
    left_join(cat_tx, by = "categoria_tx_id") %>% 
    select(-categoria_tx_id) %>% 
    filter(!siteid %in% sitios_a_excluir) %>%  
    collect() %>% 
    filter(str_detect(siteid, "^NTX", negate = TRUE))
    
 

  # sitiostx <- ttkdoc$sitios %>%
  #   select(siteid,
  #          sitename,
  #          latitud,
  #          longitud,
  #          categoria_sitio = categoria_tx)


##  ............................................................................
##  CONSOLIDADO GENERAL DE SITIOS                                           ####

#' La vista de sitos que tenemos en ttkdoc es solo la de sitios Tx.  Debemos
#' utilizar el listado que viene en la tabla de revenue
#' utilizamos full_join ya que hay sitios en tx que no están en el listado de
#' revenue


  # revenue_rbs %>% 
  #   rename(latitud_revenue = sitename) %>% 
  #   full_join(sitiostx %>% rename(latitud_tx = latitud_tx), by = "siteid") %>% 
  #   full_join(sitiosfs %>% rename(sitename_fs = sitename), by = "siteid") %>% 
  #   select(starts_with("lat")) %>%
  #   filter(if_any(everything(), ~ is.na(.x))) %>% 
  #   mutate(sitename_ultimate = coalesce(sitename_fs, sitename_tx, sitename_revenue)) %>% 
  #   View()
  #   inspectdf::inspect_na()
  
  
# 2021-01-21: 5,244 sitios.  
sitelist_0 <- revenue_rbs %>% 
    full_join(sitiostx, by = "siteid") %>% 
    full_join(sitiosfs, by = "siteid") %>% 
    mutate(
      
      sitename = coalesce(sitename_fs, sitename_tx, sitename_revenue),
      
      latitud  = coalesce(latitud_tx, latitud_fs),
      longitud = coalesce(longitud_tx, longitud_fs),
      
      across(categoria_tx, replace_na, replace = "F"),
      
      .keep  = "unused",
      .after = siteid
      
    )

  # consultar a fs
  # sin_coordenadas <- sitelist_0 %>% 
  #   filter(is.na(latitud), str_detect(siteid, "^BUC", negate = TRUE),
  #          siteid != "NAV015") %>% 
  #   select(siteid, sitename, latitud, longitud)
  # 
  # write_csv(sin_coordenadas, file = "./raw-data/sin_coordenadas.csv")
  
  
  # filter(!is.na(latitud), !is.na(longitud)) %>% 
  # mutate_if(is.character, iconv, "latin1", "ASCII//TRANSLIT")
  # mutate(across(is.character, iconv, "latin1", "ASCII//TRANSLIT"))




## 2020-02-18: agregar el departamento al que pertenece el sitio

  # cargar catalogo
  prefijos <- fread(here::here("raw-data", "catalogo_prefijo_sitios.csv")) %>%
     as_tibble()

  # hay un listado de sitios que hay que repararles el departamento
  df <- tibble(siteid = c("MIC980", "NAV022", "NAV023"), 
               depa = c("ALTA VERAPAZ", "IZABAL", "JUTIAPA"))


  # 5,366

  
  # 228 NAs
  # shd_sitios_01 <- shd_sitios_00 %>% 
  #   left_join(revenue_rbs %>% select(-sitename), 
  #             by = "siteid") %>% 
  #   left_join(sitiostx %>% select(!sitename:longitud), by = "siteid") %>% 
  #   mutate(across(categoria_sitio, replace_na, "F")) %>% 
  #   filter(str_detect(siteid, "^EVT|^TES", negate = TRUE)) %>% 
  #   mutate(prefijo = str_sub(siteid, start = 1, end = 3)) %>% 
  #   left_join(prefijos, by = "prefijo") %>% 
  #   left_join(df, by = "siteid") %>% 
  #   mutate(departamento_sitio = coalesce(depa, departamento_sitio)) %>% 
  #   select(-depa, -prefijo)
  
  # duplicados
  dup <- c("HTT455",
           "GUA400",
           "AMN011",
           "JLP008",
           "AMN004",
           "AGE027",
           "PTSMG ",
           "AGE032",
           "AGE033",
           "SRS151",
           "AGE034")
  
  
  rpx <- . %>% str_squish() %>% str_replace_all("\\s", "_")
  # borrar sitios que no aplican. había dos EVT (evento) y un TEST (pruebas)
  # el procedimiento de la línea 182 es una respuesta de SO que me dieron el
  # 2020-02-18 (@akrun)
  sitelist <- sitelist_0 %>%
    filter(str_detect(siteid, "^EVT|^TES", negate = TRUE)) %>%
    mutate(prefijo = str_sub(siteid, start = 1, end = 3)) %>%
    left_join(prefijos, by = "prefijo") %>% 
    left_join(df, by = "siteid") %>% 
    mutate(departamento_sitio = coalesce(depa, departamento_sitio), .keep = "unused") %>% 
    select(-fecha, -prefijo) %>% 
    relocate(revenue_hora, .after = sitename) %>% 
    filter(!is.na(latitud), !siteid %in% dup) %>% 
    mutate(across(sitename, ~ str_remove_all(.x, "[[:punct:]]")),
           across(sitename, ~ str_trim(.x, "both"))) %>% 
    select(
      siteid,
      revenue_hora,
      categoria_tx,
      sitename,
      latitud,
      longitud,
      status,
      departamento_sitio)
    
  # agregar guión bajo y eliminar espacios en blanco
  sitelist <- sitelist %>% mutate(across(sitename, rpx))
    # filter(siteid == "SMR180")

  # hay 184 sitios que están en shd_sitios_01 que no están en sitelist y han
  # 103 sitios que están en sitelist que no están en shd_sitios. La mejor
  # alternativa es realizar un full_join
  

##  ............................................................................
##  AGREGAR MARCA DE SITIOS RETINA                                          ####
  
  retina <- read_csv("./raw-data/sitios_retina.csv", lazy = FALSE, 
                     col_select = -sitename)
  
    # relocate(retina_nivel1, .before = retina_nivel2)
  

##  ............................................................................
##  W_UBICACION                                                             ####

  w_ubicacion <- ttkdoc$ubicacion %>% 
    select(-id_ubicacion) %>% 
    mutate(across(segmento, na_if, "N/A")) %>% 
    distinct(ticketid, .keep_all = TRUE) %>% 
    rowid_to_column(var = "id")
  

##  ............................................................................
##  W_TTKS                                                                  ####
  
  #' Utilizaremos la tabla principal de ttks como filtro para la tabla de
  #' afectación. Aquí aplicaremos todos los filtros necesarios para que en la
  #' tabla afectación solo estén los ttks necesarios.
   
  # 2021-01-21: 25,202 registros antes de modificar la vista en SQL para que
  # tengamos acceso al estado del ticket y que sea a partir de los tableros
  # que realicemos los filtros. Esto lo solicitó Choc. Después de eliminar los
  # filtros ahora tenemos en esta fecha 38,382
  
  w_ttks_all <- ttkdoc$ttks %>% 
    left_join(w_ubicacion, by = "ticketid")
    # filter(aplica == "SI")
  
  ttks_correctivos <- ttkdoc$ttks %>% 
    left_join(w_ubicacion, by = "ticketid") %>% 
    filter(aplica    == "SI")
           # estado    == "CERRADO",
           # tipo_mtto == "CORRECTIVO")
  
  # ttkdoc$ttks %>% anti_join(w_ubicacion, by = "ticketid")


  # Agregar el sitio_origen utilizando fuzzyjoin. Esto es para determinar el
  # sitio origen a partir de las coordenadas.  Lo hacemos por que aun no
  # consumimos del NOC esta información a través de las API.

 
   
  # con la ubicación de la fecha en w_ttks la unimos con sitelist para poder
  # saber que sitio fue afectado.  El problema es que no va coincidir siempre
  # las coordenadas
  
  tic()
  ttks_00 <- ttks_correctivos %>% 
    filter(!is.na(latitud)) %>%  
    geo_left_join(
      y            = sitelist,
      by           = c("latitud", "longitud"), 
      max_dist     = 0.1,  
      unit         = "km",
      distance_col = "distancia")
  toc()
  
  
  ttks_01 <- ttks_00 %>% 
    select(-c(comentarios:documentacion, 
              no_poste,
              no_mufa,
              id,
              status,
              usuario,
              configuracion,
              segmento,
              distancia,
              departamento_sitio,
              latitud.y,
              longitud.y)) %>% 
    rename(latitud = latitud.x,
           longitud = longitud.x) %>% 
    filter(!is.na(categoria)) %>% 
    relocate(siteid, sitename, .after = description) %>% 
    rename(sitio_origen = siteid) %>% 
    arrange(fecha)
    
  
  # 67 casos
  # ttks_01 %>% 
  #   filter(is.na(sitio_origen),
  #          categoria == "PLIN",
  #          topografia == "EN_SITIO",
  #          bu == "CORE")
  # 
  # # toc()
  
  
  
  # listado con ttks que si tienen sitio origen y que podemos clasificar si
  # es retina o no
  tickets_2019 <- ttks_01 %>%
    select(ticketid, sitio_origen, sitename) %>% 
    filter(!is.na(sitio_origen)) %>% 
    left_join(retina, c("sitio_origen" = "siteid")) %>%  
    replace_na(list(retina  = "SIN_RETINA")) %>% 
    left_join(sitelist %>% 
                select(siteid, 
                       revenue_hora_sitio_origen = revenue_hora,
                       categoria_sitio_origen = categoria_tx), 
              by = c("sitio_origen" = "siteid")) 


##  ............................................................................
##  W_AFECTACION                                                            ####
  
  # sitio afectado es la lista de sitios afectados que se ingresan en ttkdoc
  
  afectacion_00 <- ttkdoc$afectacion %>% 
  
    select(-categoria_tx, -latitud, -longitud) %>% 
    rename(servicio = nombre_servicio) %>% 
    mutate_at("sitio_afectado", str_replace_all, "^HFC@", "") %>% 
    mutate(categoria_enlace =
      case_when(
        tipo_servicio == "TX" ~ "TX",
        tipo_servicio == "RBS" ~ "RBS",
        tipo_servicio == "TIGOSTAR" ~ "TIGOSTAR",
        tipo_servicio == "FO OSCURA" ~ "FO_OSCURA",
        TRUE ~ "BUC")

      ) %>% 
    mutate_at("sitio_afectado", str_trim, "both")
    
##  ............................................................................
##  EXTRAER NODOS HOME                                                      ####

  ## extraer vista de hometec
  nodos_ts_raw <- fread(here::here("raw-data", 
                                   "revenue_nodos_tigostar.csv")) %>% 
    as_tibble()
  
  
  ts <- nodos_ts_raw %>% 
    mutate(revenuets_men = rowMeans(select(.,marzo:junio)),
           revenuets_hra = revenuets_men/720) %>% 
      select(nodo, revenuets_hra)
   
##  ............................................................................
##  ENRIQUECER AFECTACION                                                   ####

  # esta tabla ya va en la forma en que tableau mejor puede procesar los datos.
  # primero filtramos la tabla w_ttks2 y hasta despues aplicamos todas las
  # uniones.
  
  # No se puede utilizar la tabla de afectación para estadísticas, a menos que
  # se combine con la tabla principal. Lo que pasa es que esta tabla solo cuenta
  # aquellos ttks que tuvieron afectación de alguna manera, pero nos interesan
  # también ttks que no tuvieron afectación o son de otra naturaleza.
  w_afectacion <- afectacion_00 %>% 
    inner_join(w_ttks_all %>% 
                 rename(latitud_falla = latitud,
                        longitud_falla = longitud), by = "ticketid") %>%
    left_join(sitelist %>% 
                select(siteid,
                       sitename_sitio_afectado = sitename,
                       categoria_sitio_afectado = categoria_tx,
                       revenue_hora),
                by = c("sitio_afectado" = "siteid")) %>% 
    
    left_join(ts, by = c("sitio_afectado" = "nodo")) %>% 
    mutate(revenue_hora = case_when(
      is.na(revenue_hora) & !is.na(revenuets_hra) ~ revenuets_hra,
      TRUE ~ revenue_hora
      
    )) %>% 
    # la tabla de tickets_2019 nos dice si el sitio ORIGEN es o no RETINA
    left_join(tickets_2019 %>% 
                rename(isorigin_retina = retina,
                       sitename_origen = sitename),
              by = "ticketid") %>% 
    
    # le agregamos la tabla retina para saber si el sitio AFECTADO es retina o no.
    left_join(retina %>%
                rename(isafectado_retina = retina),
              by = c("sitio_afectado" = "siteid")) %>%
      
    replace_na(list(isafectado_retina = "SIN_RETINA")) %>% 

    select(
      ticketid,
      fecha,
      siteid_origen = sitio_origen,
      sitename_origen,
      isorigin_retina,
      categoria_sitio_origen,
      revenue_hora_sitio_origen,
      siteid_sitio_afectado = sitio_afectado,
      sitename_sitio_afectado,
      isafectado_retina,
      categoria_sitio_afectado,
      ttr,
      revenue_hora_sitio_afectado = revenue_hora,
      everything()
    ) %>% 
    select(-revenuets_hra) %>% 
    replace_na(list(
      isorigin_retina = "SIN_RETINA",
      isafectado_retina = "SIN_RETINA")) %>% 
    filter(!is.na(categoria)) %>% 
    mutate(across(where(is.character), iconv, "latin1", "ASCII//TRANSLIT"),
           across(where(is.character), str_trim, "both"))
  
    # relocate(sitio_origen, 
    #          nombre_sitio_origen,
    #          categoria_sitio_origen, .after = fecha) %>% 
    # relocate(servicio, categoria_sitio, .after = sitio_afectado) %>% 
    # mutate_if(is.character, iconv, "latin1", "ASCII//TRANSLIT") %>% 
    # mutate(across(is.character, iconv, "latin1", "ASCII//TRANSLIT")) %>% 
    # rename(categoria_sitio_afectado = categoria_sitio)
  
  
  
  w_afectacion_correctivo <- afectacion_00 %>% 
    inner_join(ttks_correctivos %>% 
                 rename(latitud_falla = latitud,
                        longitud_falla = longitud), by = "ticketid") %>% 
    left_join(sitelist %>% 
                select(siteid,
                       sitename_sitio_afectado = sitename,
                       categoria_sitio_afectado = categoria_tx,
                       revenue_hora),
              by = c("sitio_afectado" = "siteid")) %>% 
    
    left_join(ts, by = c("sitio_afectado" = "nodo")) %>% 
    mutate(revenue_hora = case_when(
      is.na(revenue_hora) & !is.na(revenuets_hra) ~ revenuets_hra,
      TRUE ~ revenue_hora
      
    )) %>% 
    # la tabla de tickets_2019 nos dice si el sitio ORIGEN es o no RETINA
    left_join(tickets_2019 %>% 
                rename(isorigin_retina = retina,
                       sitename_origen = sitename),
              by = "ticketid") %>% 
    
    # le agregamos la tabla retina para saber si el sitio AFECTADO es retina o no.
    left_join(retina %>%
                rename(isafectado_retina = retina),
              by = c("sitio_afectado" = "siteid")) %>%
    
    select(
      ticketid,
      fecha,
      siteid_origen = sitio_origen,
      sitename_origen,
      isorigin_retina,
      categoria_sitio_origen,
      revenue_hora_sitio_origen,
      siteid_sitio_afectado = sitio_afectado,
      sitename_sitio_afectado,
      isafectado_retina,
      categoria_sitio_afectado,
      ttr,
      revenue_hora_sitio_afectado = revenue_hora,
      everything()
    ) %>% 
    select(-revenuets_hra, -segmento, -no_poste, -id, -no_mufa,
           -servicio, -tipo_servicio, -categoria_enlace,
           -configuracion,
           -usuario,
           -revenue_hora_sitio_origen,
           -c(comentarios:documentacion)) %>% 
    replace_na(list(
      isorigin_retina = "SIN_RETINA",
      isafectado_retina = "SIN_RETINA")) %>% 
    filter(!is.na(categoria)) %>% 
    mutate(across(where(is.character), iconv, "latin1", "ASCII//TRANSLIT"),
           across(where(is.character), str_trim, "both"))
  
  
  #' En el análisis de NAs los ttrs que son NAs se debe a que son ttks que
  #' fueron atendidos por alguna empresa que debería estar incluida, tal como
  #' TIGO, TWIS, DESPACHO TX, etc.
##  ............................................................................
##  ITEMS                                                                   ####
  
  catalogo_categoria <- tbl(con, in_schema("spares", table = "categoria")) %>% 
    select(-activo) %>% 
    collect()
  
  
  # descargamos el item-master
  im_raw <- tbl(con, in_schema("shd", table = "item")) %>% 
    select(part_number,
           categoria_id, 
           description = nombre) %>% 
    collect()
    
  
  im <- im_raw %>% 
    left_join(catalogo_categoria, by = "categoria_id") %>%
    remover_acentos() %>% 
    mutate_if(is.character, toupper) %>% 
    # mutate(across(is.character, toupper)) %>% 
    filter(categoria %in% c("ELECTRONICA", "ENERGIA"),
           !part_number %in% c("N/A", "DAR DE BAJA", "NO ES TX"),
           part_number != "INDETERMINADO",
           !is.na(part_number)) %>% 
    select(-categoria_id, -categoria) %>% 
    mutate_at("description", str_sub, 1, 60) %>% 
    mutate_at("description", str_squish) %>% 
    # mutate(across(description, str_sub, 1, 60)) %>%  
    # mutate(across(description, str_squish)) %>% 
    distinct(part_number, .keep_all = TRUE)
    
  # extraer la fecha de cambio
  ttks_date <- w_ttks_all %>% 
    select(ticketid, fecha)
  
  # ingresar manualmente las fechas de cambio que no se incluyeron por que
  # el ttk no aplicaba ya que fue un ttk problema
  cam <- tribble(~ticketid, ~new_date,
                 6604376,     "2018-09-13 17:59:14",
                 6784202,     "2019-02-01 15:02:13",
                 7304165,     "2019-06-25 15:29:57",
                 7841191,     "2019-09-09 07:23:23",
                 8397635,     "2019-10-23 21:54:33",
                 12705646,    "2020-02-01 21:07:29",
                 13583167,    "2020-03-17 14:04:03",
                 14234968,    "2020-04-21 06:24:20"
  ) %>% 
  mutate_at("new_date", ymd_hms) 
  # mutate(across(new_date, ymd_hms))
  
  
  w_items <- ttkdoc$item %>% 
    # filter(part_number != "NO ES TX" | is.na(part_number)) %>% 
    # replace_na(list(precio_usd = 0)) %>% 
    mutate(across(precio_usd, as.numeric)) %>% 
    mutate(costo_total = cantidad * precio_usd) %>% 
    left_join(im, by = "part_number") %>% 
    left_join(ttks_date, "ticketid") %>% 
    left_join(cam, by = "ticketid") %>% 
    mutate(fecha_cambio = case_when(
      is.na(fecha_cambio) & !is.na(fecha) ~ fecha,
      is.na(fecha_cambio) & is.na(fecha) & !is.na(new_date) ~ new_date,
      TRUE ~ fecha_cambio
      
    )) %>% 
    select(-new_date, -fecha)
  

  
  wtk <- w_ttks_all %>% 
    select(ticketid, fecha, categoria, nivel1)
  
  itm <- w_items %>% 
    filter(status %in% c("INSTALADO"),
           categoria == "ELECTRONICA") %>% 
    select(ticketid, codigo_ebs, marca, modelo, part_number, description, cantidad)
  
  itm_01 <- itm %>% 
    left_join(wtk, by = "ticketid") %>% 
    filter(!is.na(fecha)) %>% 
    select(
      ticketid,
      fecha,
      descripcion = description,
      categoria,
      nivel1,
      cantidad    ) %>% 
    mutate(across(ticketid, as.integer)) %>% 
    mutate(nueva_fecha = case_when(
      
      ticketid == lag(ticketid) ~ lag(fecha),
      TRUE ~ fecha),
      
      validacion = fecha != nueva_fecha
      
      
    )
  
  itm_02 <- itm_01 %>% 
    group_by(ticketid, fecha = floor_date(nueva_fecha, unit = "month")) %>%
    summarise(cantidad = sum(cantidad), .groups = "drop")
  
  

    
  cat_catalogo <- tbl(con, in_schema("shd", "item_master_v")) %>% 
    select(codigo_ebs, categoria, nombre) %>% 
    collect()
  
  
  md_00 <- tbl(con, in_schema("md", "descarga_vista")) %>% 
    select(
      ticketid = ticket,
      fecha_uso,
      usuario,
      tipo_ticket,
      codigo_ebs,
      categoria,
      unidad_negocio,
      estado = estado_descarga_item,
      cantidad,
      contratista,
      material
    ) %>%  
    # filter(categoria == "ELECTRONICA") %>% 
    collect() %>% 
    mutate(across(where(is.character), toupper))
  
  codigos <- read_csv("./raw-data/codigos_pwr.csv", lazy = FALSE) %>% 
    pull(codigo_ebs)
  
  
  

  md_01 <- md_00 %>% 
    filter(categoria == "ELECTRONICA" | 
           codigo_ebs %in% codigos,
           estado != "RECHAZADO", 
           unidad_negocio %in% c("CORE", "BUC")) %>% 
    select(
      ticketid,
      fecha_uso,
      categoria,
      usuario,
      tipo_ticket,
      unidad_negocio,
      codigo_ebs,
      descripcion = material,
      cantidad
    ) %>% 
    mutate(across(descripcion, str_sub, 1, 55),
           across(ticketid, as.integer))
  
  
  
  md_02 <- md_01 %>% 
    group_by(ticketid, fecha = floor_date(fecha_uso, unit = "month"), usuario) %>%
    summarise(cantidad = sum(cantidad), .groups = "drop") %>%
    arrange(ticketid) %>% 
    mutate(nueva_fecha = case_when(
      
      ticketid == lag(ticketid) ~ lag(fecha),
      TRUE ~ fecha),
      
      validacion = fecha != nueva_fecha
      
      
    ) %>% 
    group_by(ticketid, fecha = floor_date(nueva_fecha, unit = "month"), usuario) %>%
    summarise(cantidad = sum(cantidad), .groups = "drop") %>% 
    arrange(desc(cantidad))
  
  itm_02 <- itm_01 %>% 
   bind_rows(md_01) %>% 
   distinct(ticketid, .keep_all = TRUE)

  # itm_02 %>% arrange(desc(cantidad))
  # 
  # itm_02 %>% 
  #   ggplot(aes(x = fecha, y = cantidad)) +
  #   geom_line() +
  #   geom_point() +
  #   theme_bw()
  
##  ............................................................................
##  MTTR                                                                    ####
  
  # Esta tabla sirve para poder agregar el ownergroup, la empresa y la sede
  # en que se originó la falla. La razón de separar el mttr es que no se puede
  # combinar con ninguna tabla.
  f <- w_ttks_all %>% select(ticketid) %>% pull(ticketid)
  
  w_mttr <- ttkdoc$mttr %>% 
    filter(ticketid %in% f) %>% 
    select(-tkstatusid)
  
  
  
##  ............................................................................
##  Modificar nombre de columna                                             ####
  
  sitelist <- sitelist %>% 
    rename(categoria_sitio = categoria_tx)
  
  
##  ............................................................................
##  INSERTAR TABLAS EN LA BASE DE DATOS                                     ####

  # prueba de desconexión
  # dbIsValid(con)
  # dbDisconnect(con)
  # dbIsValid(con)
# actualizar tabla: la primera vez que se utilice no agregar overwrite = TRUE

  # afectacion
  # 2021-02-02: 30.29 seg
  dbWriteTable(conn = con,
               name = Id(schema = "tkd", table = "w_afectacion"),
               value = w_afectacion,
               overwrite = TRUE)
  
  dbWriteTable(conn = con,
               name = Id(schema = "tkd", table = "w_afectacion_correctivo"),
               value = w_afectacion_correctivo,
               overwrite = TRUE)
  
  
  # dbWriteTable(conn = con,
  #              name = Id(schema = "tkd", table = "w_items_retina"),
  #              value = itm_02,
  #              overwrite = TRUE)
  
  dbWriteTable(conn = con,
               name = Id(schema = "tkd", table = "w_ttks"),
               value = w_ttks_all,
               overwrite = TRUE)
  

  dbWriteTable(conn = con,
               name = Id(schema = "tkd", table = "w_items"),
               value = w_items,
               overwrite = TRUE)
  
  dbWriteTable(conn = con,
               name = Id(schema = "tkd", table = "w_mttr"),
               value = w_mttr,
               overwrite = TRUE)
  
  dbWriteTable(conn = con,
               name = Id(schema = "tkd", table = "w_sitios"),
               value = sitelist,
               overwrite = TRUE)
  
  
##  ............................................................................
##  CERRAR LA CONEXIÓN                                                      ####

dbDisconnect(con)
  
## _______________________________________________________________________ ###
## FIN SCRIPT                                                              ###

# VOLIV A COLOCAR UNA LINEA
