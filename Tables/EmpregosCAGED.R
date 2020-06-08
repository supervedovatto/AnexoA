tabela <- Emprego %>%
  filter(RPSEGPLAN == LocRef$RPSEGPLAN &
           Ano >= max(Emprego$Ano) - 5) %>%
  select(Setor, Admitidos, Desligados) %>%
  group_by(Setor) %>%
  summarise(
    Admitidos = sum(Admitidos, na.rm = TRUE),
    Desligados = sum(Desligados, na.rm = TRUE)
  ) %>%
  mutate(Saldo = Admitidos - Desligados) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = paste(
      "Saldo acumulado da geração de empregos por setor entre",
      max(Emprego$Ano) - 5,
      "e",
      max(Emprego$Ano),
      "na Região de Planejamento",
      LocRef$RPSEGPLAN,
      "segundo a CAGED."
    )
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  column_spec(1, width = "25em") %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )