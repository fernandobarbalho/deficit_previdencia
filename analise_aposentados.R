library(readr)
library(tidyverse)

remuneracao_aposentados_civis <- read_delim("202407_Aposentados_SIAPE/202407_Remuneracao.csv", 
                                  delim = ";", escape_double = FALSE, locale =locale(decimal_mark = ",", 
                                                                                              grouping_mark = ".", encoding = "LATIN1"), 
                                  trim_ws = TRUE)
remuneracao_aposentados_civis <- janitor::clean_names(remuneracao_aposentados_civis)

glimpse(remuneracao_aposentados_civis)

NROW(unique(remuneracao_aposentados_civis$id_servidor_portal))


cadastro_aposentados_civis <- read_delim("202407_Aposentados_SIAPE/202407_Cadastro.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

cadastro_aposentados_civis <- janitor::clean_names(cadastro_aposentados_civis)

glimpse(cadastro_aposentados_civis)


remuneracao_reserva_militares<-  read_delim("202407_Reserva_Reforma_Militares/202407_Remuneracao.csv", 
                                                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                                                      grouping_mark = ".", encoding = "LATIN1"), 
                                                                  trim_ws = TRUE)

remuneracao_reserva_militares <- janitor::clean_names(remuneracao_reserva_militares)

glimpse(remuneracao_reserva_militares)

remuneracao_servidores_inativos<-
  remuneracao_aposentados_civis %>%
  mutate(tipo = "Civil") %>%
  bind_rows(
    remuneracao_reserva_militares %>%
      mutate(tipo = "Militar")
  )

glimpse(remuneracao_servidores_inativos)


resumo_estatisticas<-
remuneracao_servidores_inativos %>%
  filter(!is.na(remuneracao_basica_bruta_r)) %>%
  summarise(remuneracao_bruta_total = sum(remuneracao_basica_bruta_r),
            media_remuneracao_bruta_total = mean(remuneracao_basica_bruta_r),
            mediana_remuneracao_bruta_tota = median(remuneracao_basica_bruta_r),
            remuneracao_liquida_total = sum(remuneracao_apos_deducoes_obrigatorias_r),
            media_remuneracao_liquida_total = mean(remuneracao_apos_deducoes_obrigatorias_r),
            mediana_remuneracao_liquida_total = median(remuneracao_apos_deducoes_obrigatorias_r),
            quantidade_inativos_remunerados = n(),
            .by = tipo)


resumo_estatisticas %>%
  writexl::write_xlsx("resumo_estatisticas.xlsx")


resumo_sem_tipo<-
remuneracao_servidores_inativos %>%
  filter(!is.na(remuneracao_basica_bruta_r)) %>%
  summarise(remuneracao_bruta_total = sum(remuneracao_basica_bruta_r),
            media_remuneracao_bruta_total = mean(remuneracao_basica_bruta_r),
            mediana_remuneracao_bruta_tota = median(remuneracao_basica_bruta_r),
            remuneracao_liquida_total = sum(remuneracao_apos_deducoes_obrigatorias_r),
            media_remuneracao_liquida_total = mean(remuneracao_apos_deducoes_obrigatorias_r),
            mediana_remuneracao_liquida_total = median(remuneracao_apos_deducoes_obrigatorias_r),
            quantidade_inativos_remunerados = n())
