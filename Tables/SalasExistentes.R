tabela <- SalasAula %>%
  merge(RegioesGoias) %>%
  filter(RPSEGPLAN == LocRef$RPSEGPLAN &
           Ano == max(SalasAula$Ano) & Situacao == "Existentes") %>%
  select(Localidade, Rede, Total) %>%
  spread(Rede, Total) %>%
  replace_na(list(Federal = 0, Particular = 0)) %>%
  mutate(Total = Estadual + Particular + Federal + Municipal) %>%
  adorn_totals() %>%
  format(big.mark = ".",
         decimal.mark = ",",
         scientific = FALSE)

TabelaSalasExistentes <- tabela %>%
  kable(
    x = tabela,
    format = "latex",
    booktabs = TRUE,
    col.names = c(
      "Município",
      "Federal",
      "Estadual",
      "Municipal",
      "Particular",
      "Total"
    ),
    caption = paste(
      "Salas de aulas disponíveis nos municípios na Região de Planejamento de",
      LocRef$RPSEGPLAN,
      "em",
      max(SalasAula$Ano)
    )
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )