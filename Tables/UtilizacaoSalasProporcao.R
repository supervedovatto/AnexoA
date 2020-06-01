tabela <- SalasAula %>%
  merge(RegioesGoias) %>%
  filter(RPSEGPLAN == LocRef$RPSEGPLAN &
           Ano == max(SalasAula$Ano)) %>%
  spread(Situacao, Total) %>%
  mutate(TaxaUso = 100 * Utilizadas / Existentes) %>%
  select(Localidade, Rede, TaxaUso) %>%
  spread(Rede, TaxaUso) %>%
  replace_na(list(Federal = 0, Particular = 0)) %>%
  format(
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE,
    digits = 3
  )

TableUtilizacaoSalasProporcao <-  tabela %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c(
      "Cidade",
      "Federal (%)",
      "Estadual (%)",
      "Municipal (%)",
      "Particular (%)"
    ),
    caption = paste(
      "Percentual de utilização das salas de aula nas cidades da Região de Planejamento",
      LocRef$RPSEGPLAN,
      "em",
      max(SalasAula$Ano),
      "nas diferentes redes de ensino"
    )
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )