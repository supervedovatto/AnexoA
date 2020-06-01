SEGPLAN.Areas <- Area %>%
  filter(Ano == max(Area$Ano) & RPSEGPLAN == LocRef$RPSEGPLAN) %>%
  group_by(Localidade) %>%
  summarise(Area = sum(AreaTerritorial))

TableCidadesRPSEGPLAN <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(Ano == DataRef & RPSEGPLAN == LocRef$RPSEGPLAN) %>%
  group_by(Localidade) %>%
  summarise(Populacao = sum(Quantidade)) %>%
  arrange(desc(Localidade)) %>%
  mutate(freq = Populacao / sum(Populacao)) %>%
  merge(SEGPLAN.Areas, by = "Localidade") %>%
  mutate(densidade = round(Populacao / Area, digits = 2)) %>%
  select(Localidade, Populacao, Area, densidade) %>%
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
      "Densidade populacional em cada cidade da Região de Planejamento",
      LocRef$RPSEGPLAN
    )
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )