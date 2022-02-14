library(tidyverse)
library(quickcountmx)
# leer datos
muestra_tbl <-
  read_delim("datos/Conteos-ConsPop21-Lista-MuestraCalculo.txt", delim = "|", skip = 1) |>
  mutate(id = paste(ID_ESTADO, ID_DISTRITO_FEDERAL, SECCION, TIPO_CASILLA, ID_CASILLA,
                    TIPO_SECCION))

muestra_selec_tbl <-
  read_delim("datos/Conteos-ConsPop21-Lista-MR-seleccionadasmuestra.txt", delim = "|") |>
  mutate(id = paste(ID_ESTADO, ID_DISTRITO_FEDERAL, SECCION, TIPO_CASILLA, ID_CASILLA,
                    TIPO_SECCION))

computos_tbl <-
  read_delim(
    "datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv",
    delim = "|", skip = 6)  |>
  mutate(ln = LISTA_NOMINAL_MRCP) |>
  mutate(id_num = as.numeric(factor(CLAVE_MRCP))) |>
  mutate(x = 1) |>
  mutate(ESTRATO = paste0(stringr::str_pad(ID_ENTIDAD, 2, pad = "0"),
                          stringr::str_pad(ID_DISTRITO_FEDERAL, 2, pad = "0")))

frac <- 1200 / nrow(computos_tbl)
set.seed(1999)
muestras_tbl <- map_df(1:200, function(num_muestra){
  muestra <- quickcountmx::select_sample_prop(computos_tbl, stratum = ESTRATO,
                                   frac = frac)
  muestra |> mutate(num_muestra = num_muestra)
})

write_csv(muestras_tbl, "muestras_csv/muestras_cp_2021_1200.csv")

frac <- 1800 / nrow(computos_tbl)
set.seed(61112)
muestras_tbl <- map_df(1:200, function(num_muestra){
  muestra <- quickcountmx::select_sample_prop(computos_tbl, stratum = ESTRATO,
                                              frac = frac)
  muestra |> mutate(num_muestra = num_muestra)
})

write_csv(muestras_tbl, "muestras_csv/muestras_cp_2021_1800.csv")



### muestras contaminadas
num_erroneos <- muestra_tbl |> mutate(error = (SI + NO + NULOS) == LISTA_NOMINAL) |>
  filter(error) |>
  nrow()
prop_erroneos <- num_erroneos/nrow(muestra_tbl)
prop_erroneos

set.seed(417)
computos_contam_tbl <- computos_tbl |>
  mutate(contaminar = rbernoulli(n(), prop_erroneos)) |>
  mutate(NULOS = ifelse(contaminar, LISTA_NOMINAL_MRCP - OPINION_SI - OPINION_NO, NULOS)) |>
  select(-contaminar)

set.seed(8999)
frac <- 1200 / nrow(computos_tbl)
muestras_contaminadas_tbl <- map_df(1:200, function(num_muestra){
  muestra <- quickcountmx::select_sample_prop(computos_contam_tbl,
    stratum = ESTRATO, frac = frac)
  muestra |> mutate(num_muestra = num_muestra)
})

write_csv(muestras_contaminadas_tbl, "muestras_csv/muestras_contaminadas_cp_2021_1200.csv")

set.seed(1494)
frac <- 1800 / nrow(computos_tbl)
muestras_contaminadas_tbl <- map_df(1:200, function(num_muestra){
  muestra <- quickcountmx::select_sample_prop(computos_contam_tbl,
                                              stratum = ESTRATO, frac = frac)
  muestra |> mutate(num_muestra = num_muestra)
})

write_csv(muestras_contaminadas_tbl, "muestras_csv/muestras_contaminadas_cp_2021_1800.csv")
