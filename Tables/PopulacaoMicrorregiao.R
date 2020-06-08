Micro.Area <- Area %>%
  merge(RegioesGoias) %>%
  filter(Ano == max(Area$Ano) &
           Microrregiao == LocRef$Microrregiao) %>%
  select(Localidade, AreaTerritorial)

tabela <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(Microrregiao == LocRef$Microrregiao & Ano == DataRef) %>%
  group_by(Localidade) %>%
  summarise(Populacao = sum(Quantidade)) %>%
  arrange(desc(Populacao)) %>%
  mutate(freq = Populacao / sum(Populacao)) %>%
  select(Localidade, Populacao) %>%
  merge(Micro.Area) %>%
  mutate(densidade = round(Populacao / AreaTerritorial, digits = 2)) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c(
      "Cidade",
      "População (hab)",
      "Área (km²)",
      "Densidade Populacional (hab/km²)"
    ),
    caption = paste(
      "Densidade Populacional da Microrregião de",
      LocRef$Microrregiao
    )
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )