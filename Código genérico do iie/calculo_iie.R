# Lupa Social e Instituto Natura
# Objetivo: Disseminação do código para o cálculo do IIE para todos os anos
# Acesso aos dados: Pnad Contínua, CES, SAEB e Extração via SEDAP

# Carregando pacotes necessários -----------------------------------------------------
library(haven)      
library(dplyr)      
library(tidyr)      
library(readxl)    
library(data.table) 

# Importando, os dados ---------------------------------------------------------------

# carregar bases publicas

#pnadc 2017
# variaveis: "UF", "Ano", "V20081", "V20082", "V3002", "V3003A", "V3009A", "V3013", "V1028"

#saeb 2017
# variáveis:"id_uf", "proficiencia_lp_saeb", "proficiencia_mt_saeb", "tx_resp_q004", "peso_aluno_lp"

# Tratamento Pnad  -------------------------------------------------------------------
# Alunos fora da escola (PNAD 2017)
pnad <- pnad %>%
  mutate(across(c(V20082, Ano), as.numeric),
         uf = UF,
         coorte = Ano - 17,
         mes_nasc = V20081,
         ano_nasc = V20082,
         idade_escolar = ifelse(ano_nasc == coorte, 1, NA)) %>%
  filter(!is.na(idade_escolar)) %>%
  mutate(var_fora = "_outros",
         var_fora = ifelse(V3002 == 2 & V3009A <= 8, "_Fora_sem_EM", var_fora),
         var_fora = ifelse(V3002 == 2 & V3009A %in% 9:11 & V3013 <= 2 & !is.na(V3013),
                           "_Fora_sem_EM", var_fora),
         var_fora = ifelse(V3002 == 2 & V3009A %in% 9:11 & V3013 >= 3 & !is.na(V3013),
                           "_Fora_com_EM", var_fora),
         var_fora = ifelse(V3002 == 2 & V3009A >= 12 & !is.na(V3009A),
                           "_Fora_com_EM", var_fora),
         var_fora = ifelse(V3002 == 1 & V3003A >= 8 & !is.na(V3003A),
                           "_Fora_com_EM", var_fora),
         var_fora_limpo = var_fora,
         var_fora_limpo = ifelse(is.na(V3002) | (V3002 == 2 & is.na(V3009A)) |
                                   (V3002 == 2 & V3009A %in% 9:11 & is.na(V3013)),
                                 "", var_fora_limpo)) %>%
  filter(var_fora_limpo != "")

pnad_summary <- pnad %>%
  group_by(uf, var_fora_limpo) %>%
  summarise(idade_escolar = sum(idade_escolar, na.rm = TRUE),
            amostra = sum(idade_escolar * V1028, na.rm = TRUE)) %>%
  pivot_wider(names_from = var_fora_limpo,
              values_from = c(idade_escolar, amostra), values_fill = 0)

pnad_summary <- pnad_summary %>%
  mutate(
    pop_coorte = idade_escolar__Fora_com_EM +
      idade_escolar__Fora_sem_EM + 
      idade_escolar__outros,
    pop_fora_sem_EM = idade_escolar__Fora_sem_EM,
    pop_fora_com_EM = idade_escolar__Fora_com_EM
  )

uf_mapping <- c("11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR",
                "15" = "PA", "16" = "AP", "17" = "TO", "21" = "MA",
                "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
                "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA",
                "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
                "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
                "51" = "MT", "52" = "GO", "53" = "DF")

pnad_summary <- pnad_summary %>%
  mutate(sg_uf = uf_mapping[as.character(uf)]) %>%
  select(uf, sg_uf, pop_coorte, pop_fora_sem_EM) %>%
  rename(coorte_PNAD = pop_coorte,
         coorte_fora_sem_EM_PNAD = pop_fora_sem_EM)

# Salvando em csv
write.csv(pnad_summary, 
          "SEU\\CAMINHO\\FORA_DA_ESCOLA_GERAL.csv",
          row.names = FALSE
) # se o caminho de salvamentos for sempre o mesmo é possivel otimizar os salvamentos ao longo do código


# Tratamento CES  --------------------------------------------------------------------
# Alinhamento serie do Censo escolar

years <- 2016:2018 # trocar oS anoS de interesse conforme o IIE calculado
output_base <- "SEU\\CAMINHO\\ALINHAMENTO_SERIE_CE_"

results_list <- list()  # Lista para armazenar os resultados

for (i in years) {
  data_path <- paste0(
    "CAMINHO\\OUTPUT\\SITUACAO_ALUNO_", 
    i, ".dta")
  
  # Check se todos os dados necessários existem na pasta
  if (!file.exists(data_path)) {
    message(paste("Arquivo não encontrado para o ano", i))
    next
  }
  
  print(paste("Processando o ano:", i))
  
  # Lê os dados
  ces <- read_dta(data_path, col_select = c("nu_ano", "tp_etapa_ensino", "co_uf", "tp_situacao")) %>%
    filter(nu_ano == 2000, !is.na(tp_etapa_ensino)) # trocar o ano para a coorte de interesse do IIE calculado
  
  if (nrow(ces) == 0) {
    message(paste("Nenhum dado restante após o filtro nu_ano == 2000 para", i))
    next
  }
  
  ces <- ces %>%
    mutate(
      coorte = 1,
      serie = case_when(
        tp_etapa_ensino <= 25 | tp_etapa_ensino == 30 | tp_etapa_ensino == 35 | tp_etapa_ensino == 41 ~ "1a_EM_menos",
        tp_etapa_ensino %in% c(26, 31, 36) ~ "2a_EM",
        tp_etapa_ensino %in% c(27, 32, 37, 28, 33, 38) ~ "3a_EM",
        TRUE ~ "missing"
      )
    ) %>%
    filter(!tp_etapa_ensino %in% c(29, 34, 39, 40), tp_etapa_ensino < 65)
  
  # Resumo dos dados
  ces_summary <- ces %>%
    group_by(co_uf, tp_situacao, serie) %>%
    summarise(coorte = sum(coorte, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(
      names_from = serie, 
      values_from = coorte, 
      values_fill = list(`1a_EM_menos` = 0, `2a_EM` = 0, `3a_EM` = 0)
    ) %>%
    pivot_wider(
      names_from = tp_situacao, 
      values_from = c(`1a_EM_menos`, `2a_EM`, `3a_EM`), 
      names_glue = "status_{tp_situacao}_{.value}", # Adiciona prefixo para evitar nomes inválidos
      values_fill = list(`1a_EM_menos` = 0, `2a_EM` = 0, `3a_EM` = 0)
    ) %>%
    mutate(across(matches("1a_EM_menos|2a_EM|3a_EM"), ~ replace_na(.x, 0))) %>%
    mutate(coorte = 2000)
  
  # Salva na lista
  results_list[[as.character(i)]] <- ces_summary
  
  # Exporta os dados
  write.csv(ces_summary, paste0(output_base, i, "_GERAL.csv"), row.names = FALSE)
}

# Verificar se a lista contém os três anos corretamente
print(lapply(results_list, nrow))

# Alinhamento serie -----------------------------------------------------------------
as_2017 <- as_2017 %>%# trocar as_2017  para o ano de interesse do código
  mutate(ano = 2017,
         coorte_emlinha_Censo = status_5_3a_EM) %>%
  select(co_uf, ano, coorte, coorte_emlinha_Censo)

# Cruzamento dos dados de alinhamento serie 2017 e 2018

as_merged <- left_join(as_2017, as_2018, by = c("co_uf", "coorte")) %>% # trocar as_2017 e 2018 para o ano de interesse do código
  mutate(coorte_atrasado_Censo = status_5_3a_EM,
         qtd_3ano_naoaprovado =  status_2_3a_EM + status_3_3a_EM +
           status_4_3a_EM + status_9_3a_EM,
         qtd_2ano_geral = status_2_2a_EM + status_3_2a_EM +
           status_4_2a_EM + status_5_2a_EM + status_9_2a_EM,
         qtd_1ano_geral = status_2_1a_EM_menos + status_3_1a_EM_menos +
           status_4_1a_EM_menos + status_5_1a_EM_menos + status_9_1a_EM_menos,
         coorte_atrasado2_Censo = qtd_3ano_naoaprovado +
           qtd_2ano_geral + qtd_1ano_geral) %>%
  select(co_uf, ano, coorte, coorte_emlinha_Censo,
         coorte_atrasado_Censo, coorte_atrasado2_Censo)

# Carregar dados de 2016 e mesclar

as_final <- left_join(as_merged, as_2016, by = "co_uf") %>% # trocar as_2016 para o ano de interesse do código
  mutate(coorte_adiantado_Censo = status_5_3a_EM) %>%
  select(co_uf, ano, coorte_2000_adiantado_Censo, coorte_emlinha_Censo,
         coorte_atrasado_Censo, coorte_atrasado2_Censo) %>%
  rename(uf = co_uf)

# Salvar os dados
save_path_csv <- "SEU\\CAMINHO\\ALINHAMENTO_SERIE_CE20xx-20xx_GERAL.csv" # trocar o X para os anos de interesse do código
write.csv(as_final, save_path_csv)

# Tratamento Saeb -------------------------------------------------------------------
# Criar variável de ano
saeb <- saeb %>% mutate(ano = 2017, uf = id_uf, total_Saeb = 1) # Trocar para o ano de interesse do código

# Remover observações sem nota de proficiência em LP e Matemática
saeb <- saeb %>% filter(!is.na(proficiencia_lp_saeb) &
                                    !is.na(proficiencia_mt_saeb))

# Criar variáveis de desempenho
saeb <- saeb %>% 
  mutate(
    abaixo = ifelse(proficiencia_lp_saeb < 300 |
                      proficiencia_mt_saeb < 300, 1, 0),
    basico = ifelse(proficiencia_lp_saeb >= 300 &
                      proficiencia_mt_saeb >= 300, 1, 0)
  )

# Criar variável de idade/série
saeb <- saeb %>% 
  mutate(
    idade_serie = case_when(
      tx_resp_q004 %in% c("B", "A") ~ "adiantado",
      tx_resp_q004 == "C" ~ "linha",
      tx_resp_q004 == "D" ~ "atrasado",
      tx_resp_q004 %in% c("H", "G", "F", "E") ~ "atrasado2",
      TRUE ~ "SI"
    )
  ) %>% 
  filter(idade_serie != "SI")  # Remover observações sem ano de nascimento

# Agregar os dados por UF e categoria de idade/série, ponderando pelo peso
saeb_agregado <- saeb %>%
  group_by(ano, uf, idade_serie) %>%
  summarise(
    total_Saeb = sum(total_Saeb * peso_aluno_lp, na.rm = TRUE),
    abaixo = sum(abaixo * peso_aluno_lp, na.rm = TRUE),
    basico = sum(basico * peso_aluno_lp, na.rm = TRUE),
    .groups = "drop"
  )

# Reformular os dados para formato wide
saeb_agregado <- saeb_agregado %>%
  pivot_wider(names_from = idade_serie,
              values_from = c(total_Saeb, abaixo, basico), values_fill = 0)

# Manter apenas as variáveis desejadas
saeb_agregado <- saeb_agregado %>%
  select(uf, ano, abaixo_adiantado, basico_adiantado, abaixo_linha, basico_linha, 
         abaixo_atrasado, basico_atrasado, abaixo_atrasado2, basico_atrasado2)

# Salvar os dados
output_csv_path <- "SEU\\CAMINHO\\PROFICIENCIA_SAEB2017_GERAL.csv"
write.csv(saeb_agregado, output_csv_path)

# Unido bases IIE  -------------------------------------------------------------
# bases: as_final, saeb_agregado,  pnad_summary

# Unir bases por 'uf'
iie <- saeb_agregado %>% 
  left_join(pnad_2017_summary, by = "uf") %>% 
  left_join(as_final, by = "uf")

# Reordenar colunas
iie <- iie %>% select(uf, sg_uf, ano.x, starts_with("coorte"),
                            starts_with("abaixo"), starts_with("basico"))  %>%
  rename(ano = ano.x)

# Salvar base consolidada
write.csv(iie, "SEU\\CAMINHO\\BASE_CONSOLIDADA_GERAL.csv")

# Colapsar por ano
iie_colapsado <- iie %>% 
  group_by(ano) %>% 
  summarise(across(starts_with("coorte"), sum, na.rm = TRUE),
            across(starts_with("abaixo"), sum, na.rm = TRUE),
            across(starts_with("basico"), sum, na.rm = TRUE)) %>%
  mutate(uf = 1, sg_uf = "BR")

# Adicionar à base consolidada
iie <- bind_rows(iie, iie_colapsado)

# Criar variáveis derivadas
iie <- iie %>%
  mutate(perc_fora_escola = coorte_fora_sem_EM_PNAD / coorte_PNAD,
         coorte_Censo = coorte_adiantado_Censo + 
           coorte_emlinha_Censo + coorte_atrasado_Censo +
           coorte_atrasado2_Censo,
           coorte_Censo_ajustada_PNAD = 
           coorte_Censo / (1 - perc_fora_escola),
           coorte_fora_escola = 
           coorte_Censo_ajustada_PNAD - coorte_Censo,
           coorte_atrasado2_Censo_aj = coorte_atrasado2_Censo +
           coorte_fora_escola,
           coorte_SAEB = abaixo_adiantado +
           basico_adiantado + abaixo_linha + basico_linha + 
           abaixo_atrasado + basico_atrasado + abaixo_atrasado2 + basico_atrasado2,
           coorte_SAEB_ajuste_PNAD = 
           coorte_SAEB / (1 - perc_fora_escola),
           coorte_SAEB_fora_escola = 
           coorte_SAEB_ajuste_PNAD - coorte_SAEB,
           abaixo_atrasado2_aj = abaixo_atrasado2 + coorte_SAEB_fora_escola,
           adiantado_abaixo = coorte_adiantado_Censo * 
           (abaixo_adiantado / (abaixo_adiantado + basico_adiantado)),
           adiantado_basico = coorte_adiantado_Censo * 
           (basico_adiantado / (abaixo_adiantado + basico_adiantado)),
           emlinha_abaixo = coorte_emlinha_Censo *
           (abaixo_linha / (abaixo_linha + basico_linha)),
           emlinha_basico = coorte_emlinha_Censo * 
           (basico_linha / (abaixo_linha + basico_linha)),
           atrasado_abaixo = coorte_atrasado_Censo *
           (abaixo_atrasado / (abaixo_atrasado + basico_atrasado)),
           atrasado_basico = coorte_atrasado_Censo *
           (basico_atrasado / (abaixo_atrasado + basico_atrasado)),
           atrasado2_abaixo = coorte_atrasado2_Censo_aj *
           (abaixo_atrasado2_aj / (abaixo_atrasado2_aj + basico_atrasado2)),
           atrasado2_basico = coorte_atrasado2_Censo_aj * 
           (basico_atrasado2 / (abaixo_atrasado2_aj + basico_atrasado2)),
         IIE_2017 = (adiantado_basico + emlinha_basico + atrasado_basico) / 
           (adiantado_abaixo + adiantado_basico + emlinha_abaixo + 
              emlinha_basico +  atrasado_abaixo + atrasado_basico +
              atrasado2_abaixo + atrasado2_basico))

# Reordenar colunas
iie <- iie %>% select(uf, sg_uf, ano, IIE)

# Salvar base consolidada
write.csv(iie, "SEU\\CAMINHO\\IIE_GERAL.csv")
