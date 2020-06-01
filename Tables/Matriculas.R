level1 <- paste("Município de", LocRef$Localidade)
tabela1 <- Matriculas %>%
  filter(Localidade == LocRef$Localidade & Ano == max(Ano)) %>%
  select(Modalidade, Rede, Total) %>%
  mutate(
    Rede = fct_relevel(
      Rede,
      "Federal",
      "Estadual",
      "Municipal",
      "Particular",
      "Comunitária, Confessional ou Filantrópica"
    ),
    Referencia = factor(level1)
  )

level2 <- paste("RP", LocRef$RPSEGPLAN)
tabela2 <- Matriculas %>%
  merge(RegioesGoias) %>%
  filter(RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(Ano)) %>%
  mutate(
    Rede = fct_relevel(
      Rede,
      "Federal",
      "Estadual",
      "Municipal",
      "Particular",
      "Comunitária, Confessional ou Filantrópica"
    )
  ) %>%
  group_by(Modalidade, Rede) %>%
  summarise(Total = sum(Total)) %>%
  mutate(Referencia = factor(level2)) %>%
  as.data.frame()

level3 <- "Estado de Goiás"
tabela3 <- Matriculas %>%
  merge(RegioesGoias) %>%
  filter(Ano == max(Ano)) %>%
  mutate(
    Rede = fct_relevel(
      Rede,
      "Federal",
      "Estadual",
      "Municipal",
      "Particular",
      "Comunitária, Confessional ou Filantrópica"
    )
  ) %>%
  group_by(Modalidade, Rede) %>%
  summarise(Total = sum(Total)) %>%
  mutate(Referencia = factor(level3)) %>%
  as.data.frame()

tabela <- rbind(tabela1, tabela2, tabela3) %>%
  select(Referencia, Modalidade, Rede, Total) %>%
  spread(Rede, Total)

rm(tabela1, tabela2, tabela3, level1, level2, level3)

Referencias <- unique(tabela$Referencia)
TabelaMatriculas <- tabela %>%
  select(-Referencia) %>%
  mutate_all(linebreak) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = paste(
      "Número de matrículas nas diferentes modalidades e redes de ensino no Município de",
      LocRef$Localidade,
      "na Região de Planejamento",
      LocRef$RPSEGPLAN,
      "e no Estado de Goiás."
    )
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  pack_rows(Referencias[1], 1, 3) %>%
  pack_rows(Referencias[2], 4, 6) %>%
  pack_rows(Referencias[3], 7, 9) %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )