SEGPLAN.Areas <- Area %>%
  filter(Ano == max(Area$Ano)) %>%
  group_by(RPSEGPLAN) %>%
  summarise(Area = sum(AreaTerritorial))

TablePopulacaoRPSEGPLAN <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(Ano == DataRef) %>%
  group_by(RPSEGPLAN) %>%
  summarise(Populacao = sum(Quantidade)) %>%
  arrange(desc(RPSEGPLAN)) %>%
  mutate(freq = Populacao / sum(Populacao),
         ypos = cumsum(freq) - freq / 2) %>%
  select(RPSEGPLAN, Populacao) %>%
  merge(SEGPLAN.Areas, by = "RPSEGPLAN") %>%
  mutate(densidade = round(Populacao / Area, digits = 2)) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c(
      "Região de Planejamento",
      "População (hab)",
      "Área (km²)",
      "Densidade Populacional (hab/km²)"
    ),
    caption = "Densidade populacional em cada região de planejamento definida pela SEGPLAN"
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )