level1 <- paste("Mun. de", LocRef$Localidade)
tabela1 <- IDM %>%
  filter(!is.na(Valor) &
           Localidade == LocRef$Localidade & Ano == max(Ano)) %>%
  group_by(IDM) %>%
  summarise(Valor = mean(Valor, rm.na = TRUE)) %>%
  mutate(Referencia = level1)

level2 <- paste("RP", LocRef$RPSEGPLAN)
tabela2 <- IDM %>%
  merge(RegioesGoias) %>%
  filter(!is.na(Valor) &
           RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(Ano)) %>%
  group_by(Localidade, IDM) %>%
  summarise(Valor = mean(Valor, rm.na = TRUE)) %>%
  group_by(IDM) %>%
  summarise(Valor = median(Valor, rm.na = TRUE)) %>%
  mutate(Referencia = level2)

level3 <- "Estado de Goiás"
tabela3 <- IDM %>%
  filter(!is.na(Valor) & Ano == max(Ano)) %>%
  group_by(Localidade, IDM) %>%
  summarise(Valor = mean(Valor, rm.na = TRUE)) %>%
  group_by(IDM) %>%
  summarise(Valor = median(Valor, rm.na = TRUE)) %>%
  mutate(Referencia = level3)

IDMs <- rbind(tabela1, tabela2, tabela3) %>%
  mutate(Referencia = factor(
    Referencia,
    ordered = T,
    levels = c(level1, level2, level3)
  )) %>%
  dcast(Referencia ~ IDM, value.var = "Valor") %>%
  mutate("IDM Geral" = round((Economia + Educação + Infraestrutura + Saúde + Segurança + Trabalho) /
                               6,
                             2
  ))

tabela <- round(IDMs[, -1], 2)
row.names(tabela) <- IDMs$Referencia

kable(
  x = tabela,
  format = "latex",
  booktabs = TRUE,
  caption = paste(
    "IDMs",
    lubridate::year(max(IDM$Ano)),
    "no Município de",
    LocRef$Localidade,
    "e IDMs médios dos nunicípios da Região de Planejamento",
    LocRef$RPSEGPLAN,
    "e de Goiás."
  )
) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  row_spec(0, angle = 90) %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )