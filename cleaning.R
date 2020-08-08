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
files <- grep(pattern = 'combinada2020', x = files, value = T)
combinada <- lapply(files, fread) %>% rbindlist()

data_cols <- combinada %>% select(id_combinada, 
                                  nr_voo, sg_empresa_icao,
                                  sg_iata_origem, sg_iata_destino,
                                  hr_partida_real, dt_partida_real, sg_icao_origem,
                                  nm_municipio_origem, sg_uf_origem, nm_pais_origem, 
                                  hr_chegada_real, dt_chegada_real,
                                  sg_icao_destino, nm_municipio_destino, sg_uf_destino, nm_pais_destino,
                                  nr_escala_destino,
                                  ds_cotran, nr_passag_pagos, nr_passag_gratis)

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


# verify NAs
data_brazil_dists_first %>%
  select(name_muni_uf_from, name_muni_uf_to, distance) %>%
  filter(is.na(distance)) %>%
  View() # 0 - OK!

# filter only desembarques e "'
data_brazil_dists_first <- setDT(data_brazil_dists_first)[ds_cotran %in% c("DESEMBARQUE", "")]

# calculate number of passangers x distance of each flight
data_brazil_dists_first[, pass_dist := sum(nr_passag_pagos, nr_passag_gratis) * distance,
                        by = id_combinada]


data_brazil_dists_first <- data_brazil_dists_first[!is.na(nr_passag_pagos)]
data_brazil_dists_first <- data_brazil_dists_first[!is.na(nr_passag_gratis)]

fwrite(data_brazil_dists_first, "data/flights_passengers_complete.csv")


# aggregate by day ----------------------------------------

# sum number of pass_dist by day OD pair
odmatrix_passdist <- data_brazil_dists_first[,
                                             .(
                                               total_pass = sum(nr_passag_pagos, nr_passag_gratis, na.rm=T),
                                               dist_pair = distance[1],
                                               n_flights = .N,
                                               lon_from = lon_from[1],
                                               lat_from = lat_from[1],
                                               lon_to = lon_to[1],
                                               lat_to = lat_to[1]
                                             ),
                                             by = .(name_muni_uf_from, name_muni_uf_to, 
                                                    sg_iata_origem, sg_iata_destino,
                                                    date, year, month, day) ]



# filter only top 200 routes
top_200 <- data_brazil_dists_first %>% 
  group_by(sg_iata_origem, sg_iata_destino) %>% 
  summarise(sum_pass = sum(nr_passag_pagos, nr_passag_gratis, na.rm=T)) %>% 
  ungroup() %>%
  arrange(desc(sum_pass)) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 200) %>%
  mutate(pair = paste0(sg_iata_origem, "-", sg_iata_destino))

# filter only top 200 from data
odmatrix_passdist_top200 <- odmatrix_passdist %>% filter(paste0(sg_iata_origem, "-", sg_iata_destino) %in% top_200$pair)

# export data
fwrite(odmatrix_passdist_top200, "data/air_odmatrix.csv")

# aggregate by month ----------------------------------------

odmatrix_passdist_month <- odmatrix_passdist_top200 %>%
  group_by(name_muni_uf_from, name_muni_uf_to, sg_iata_origem, sg_iata_destino, year, month) %>%
  summarise(
    total_pass = sum(total_pass, na.rm=T),
    dist_pair = dist_pair[1],
    n_flights = sum(n_flights),
    lon_from = lon_from[1],
    lat_from = lat_from[1],
    lon_to = lon_to[1],
    lat_to = lat_to[1])

# export data
fwrite(odmatrix_passdist_month, "data/air_odmatrix_month.csv")
