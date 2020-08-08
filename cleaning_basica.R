library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(magrittr)
library(gghighlight)
library(viridis)

options(scipen=999)

# TRAFFIC FLOW BETWEEN MUNICIPALITIES ----------------------------------------------

files <- list.files(path = 'data-raw', pattern = '-01|-02|-03|-04|-05|-06', full.names = T)
files <- grep(pattern = 'basica2020', x = files, value = T)
basica <- lapply(files, fread) %>% rbindlist()

data_cols <- basica %>% select(id_basica, 
                               nr_voo, sg_empresa_icao,
                               sg_iata_origem, sg_iata_destino,
                               hr_partida_real, dt_partida_real, sg_icao_origem,
                               nm_municipio_origem, sg_uf_origem, nm_pais_origem, 
                               hr_chegada_real, dt_chegada_real,
                               ds_modelo,
                               sg_icao_destino, nm_municipio_destino, sg_uf_destino, nm_pais_destino,
                               nr_escala_destino,
                               nr_passag_pagos, nr_passag_gratis)

# set locale to english to convert weeknames
Sys.setlocale("LC_ALL","English")

# only brazilian flights
data_brazil <- data_cols[nm_pais_origem == "BRASIL" & nm_pais_destino == "BRASIL"]

# rename columns
setnames(data_brazil, 'nm_municipio_origem', 'origin')
setnames(data_brazil, 'nm_municipio_destino', 'destination')

# format cities names
data_brazil <- data_brazil %>% 
  mutate_at(c("origin", "destination"), stringi::stri_trans_general, id = "Latin-ASCII") %>%
  mutate_at(c("origin", "destination"), iconv, to="UTF-8") %>%
  mutate_at(c("origin", "destination"), tolower)


# criar coluna de datas
setDT(data_brazil)[, date := lubridate::as_date(dt_chegada_real)]
data_brazil[, year := lubridate::year(date)]
data_brazil[, month := lubridate::month(date)]
data_brazil[, day := lubridate::day(date)]
data_brazil[, day_week := lubridate::wday(date, label = TRUE)]

# open dists - all cities
dists <- fread("data-raw/airports_dist-df.csv")


# bring distance between origin and destination - only first cases
data_brazil_dists <- data_brazil %>%
  # create combination of muni and UF
  mutate(name_muni_uf_from = paste0(origin, "-", tolower(sg_uf_origem))) %>%
  mutate(name_muni_uf_to = paste0(destination, "-", tolower(sg_uf_destino))) %>%
  left_join(select(dists, name_muni_uf_from, name_muni_uf_to, lon_from, lat_from, lon_to, lat_to, distance), 
            by = c("name_muni_uf_from", "name_muni_uf_to"))

# filter only municipalities with first casa
data_brazil_dists_first <- data_brazil_dists %>%
  filter(name_muni_uf_from %in% dists$name_muni_uf_from, 
         name_muni_uf_to %in% dists$name_muni_uf_to)

# deop zero's and origin = destinatiob
data_brazil_dists_first <- data_brazil_dists_first[nr_passag_pagos != 0]
data_brazil_dists_first <- data_brazil_dists_first[name_muni_uf_from != name_muni_uf_to]

# summary statistics
data_brazil_dists_first <- data_brazil_dists_first %>%
  mutate(hora_saida = as.POSIXct(paste0(dt_partida_real, " ", hr_partida_real))) %>% 
  mutate(hora_chegada = as.POSIXct(paste0(dt_chegada_real, " ", hr_chegada_real))) %>%
  mutate(travel_time = hora_chegada - hora_saida)

# most used airplanes
top_airplanes <- data_brazil_dists_first %>% 
  count(name_muni_uf_from, name_muni_uf_to, sg_iata_origem, sg_iata_destino, ds_modelo) %>%
  group_by(name_muni_uf_from, name_muni_uf_to) %>%
  slice(which.max(n))

# sum number of pass_dist by day OD pair
odmatrix_passdist <- data_brazil_dists_first[,
                                             .(travel_time = mean(travel_time, na.rm = TRUE),
                                               lon_from = lon_from[1],
                                               lat_from = lat_from[1],
                                               lon_to = lon_to[1],
                                               lat_to = lat_to[1]
                                             ),
                                             by = .(name_muni_uf_from, name_muni_uf_to, 
                                                    sg_iata_origem, sg_iata_destino,
                                                    year) ]

# brig top airplanes
odmatrix_passdist <- odmatrix_passdist %>% left_join(top_airplanes, by = c("name_muni_uf_from",
                                                                           "name_muni_uf_to",
                                                                           "sg_iata_origem", 
                                                                           "sg_iata_destino"))


# join this with the previous dataset
data <- fread("data/air_odmatrix_month.csv") %>%
  left_join(select(odmatrix_passdist, name_muni_uf_from, name_muni_uf_to, 
                   sg_iata_origem, sg_iata_destino,
                   travel_time, top_plane = ds_modelo),
            by = c("name_muni_uf_from",
                   "name_muni_uf_to",
                   "sg_iata_origem", 
                   "sg_iata_destino"))

# some formatting
data <- data %>%
  mutate_at(vars(matches("name_muni_uf")), stringr::str_to_title) %>%
  mutate_at(vars(matches("name_muni_uf")), ~ gsub(pattern = "(De |Da | Do| Das)", replacement = "\\L\\1", x = .x, perl = TRUE)) %>%
  mutate_at(vars(matches("name_muni_uf")), ~ gsub(pattern = "(-)([[:alpha:]]{2})$", replacement = "\\1\\U\\2", x = .x, perl = TRUE))


# format travel time
data <- data %>%
  filter(!is.na(travel_time)) %>%
  # extrac hours
  mutate(travel_time = as.numeric(travel_time)) %>%
  mutate(hourss =    round(travel_time) %/% 60) %>%
  mutate(minutess = round(travel_time) %% 60) %>%
  mutate(travel_time = ifelse(hourss < 1, paste0(minutess, " minutos"),
                              ifelse(hourss == 1, paste0("1 hora e ", minutess, " minutos"),
                                     paste0(hourss, " horas e ", minutess, " minutos"))))
                                     
# save
fwrite(data, "data/air_odmatrix_month_new.csv")
