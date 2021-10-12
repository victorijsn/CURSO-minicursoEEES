library(dplyr)
library(tidyr)
library(lubridate)
library(janitor)

dados <- readxl::read_xlsx("./bases/07_2021_PMS-78.xlsx", sheet = "Tabela 6442", skip = 2)
dados <- janitor::clean_names(dados)

glimpse(dados)

dados_tidy <- dados %>%
  fill(mes, unidade_territorial) %>%
  select(mes, unidade_territorial, variavel,
         indice_base_fixa_2014_100_numero_indice,
         indice_base_fixa_com_ajuste_sazonal_2014_100_numero_indice) %>%
  rename(indice_bf = indice_base_fixa_2014_100_numero_indice,
         indice_bf_sa = indice_base_fixa_com_ajuste_sazonal_2014_100_numero_indice) %>%
  mutate(variavel = case_when(variavel=="Índice de receita nominal de serviços" ~ "receita",
                              variavel=="Índice de volume de serviços" ~ "volume"),
         unidade_territorial = case_when(unidade_territorial=="Brasil" ~ "BR",
                                         unidade_territorial=="Espírito Santo" ~ "ES")) %>%
  filter(unidade_territorial %in% c("BR", "ES")) %>%
  pivot_wider(names_from = c(variavel, unidade_territorial),
              names_sep = "_",
              values_from = c(indice_bf, indice_bf_sa)) %>%
  mutate(data_mes=seq(from=as.Date("2011-01-01"), to=as.Date("2021-07-01"), by="month")) %>%
  relocate(data_mes, before = mes)
