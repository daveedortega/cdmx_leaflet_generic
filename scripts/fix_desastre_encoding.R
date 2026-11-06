library(dplyr)
library(stringr)
library(tidyr)
library(fuzzyjoin)

key_name <- function(x) str_to_lower(str_remove_all(x, "[^A-Za-z0-9]"))

key_line <- function(x) {
  tok <- str_extract(as.character(x), "[A-Za-z0-9]+$")
  if_else(str_detect(tok, "^[0-9]+$"), as.character(as.integer(tok)), str_to_upper(tok))
}

df1 <- mapa_metro %>% as.data.frame() %>% count(nombre, linea, est) 
df1k <- df1 |> mutate(linea_k = key_line(linea), key = key_name(nombre))

df2 <- afluencia_desglose_stc %>% group_by(fecha, linea, estacion) %>% summarise(afluencia_diaria = sum(afluencia))%>% 
  group_by(linea, estacion) %>% 
  summarise(pd = mean(afluencia_diaria))

df2k <- df2 |>
  mutate(linea_k  = key_line(linea),
         key      = key_name(estacion),
         variante = if_else(str_detect(linea, "^Linea"), "plain", "mojibake")) |>
  group_by(variante, linea_k, key) |>               # sums the split Quevedo rows
  summarise(estacion_df2 = first(estacion), pd = sum(pd), .groups = "drop")

# Pass 1: exact on (line, stripped name)
exact <- left_join(df1k, df2k, by = c("linea_k", "key"))
hit   <- exact |> filter(!is.na(variante)) |> mutate(key_df2 = key)
miss  <- exact |> filter(is.na(variante)) |> select(all_of(names(df1k)))
spare <- anti_join(df2k, df1k, by = c("linea_k", "key"))

# Pass 2: fuzzy, only within the same line, only on leftovers
near <- function(a, b) {
  stringdist::stringdist(a, b, method = "osa") <= 2 | startsWith(a, b) | startsWith(b, a)
}

fuzzy <- fuzzy_left_join(miss, spare, by = c("linea_k", "key"),
                         match_fun = list(`==`, near)) |>
  rename(linea_k = linea_k.x, key = key.x, key_df2 = key.y) |>
  select(-linea_k.y)

long <- bind_rows(hit, fuzzy)

wide <- long |>
  filter(!is.na(variante)) |>
  pivot_wider(id_cols = c(linea_k, key), names_from = variante,
              values_from = pd, names_prefix = "n_")

joined <- left_join(df1k, wide, by = c("linea_k", "key")) |>
  select(-linea_k, -key)

# Checks
long |> filter(key != key_df2) |> distinct(nombre, linea, estacion_df2)  # should be the 5 stations above
joined |> filter(is.na(n_plain) | is.na(n_mojibake))                     # should be empty
nrow(joined) == nrow(df1)                                                # should be TRUE


joined
write_csv(joined, 'output/clave_afluencia.csv')

afluencia_desglose_stc %>% filter(estacion == 'Acatitla') %>% group_by(fecha) %>% 
  summarise(afluencia = sum(afluencia)) %>% summarise(mean(afluencia))



