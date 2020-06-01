AreaMicro <- Area %>%
  filter(Ano == max(Area$Ano) &
           Mesorregiao == LocRef$Mesorregiao) %>%
  group_by(Microrregiao) %>%
  summarise(AreaTerritorial = sum(AreaTerritorial))

TablePopulacaoMesorregiao <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(Mesorregiao == LocRef$Mesorregiao & Ano == DataRef) %>%
  group_by(Microrregiao) %>%
  summarise(Populacao = sum(Quantidade)) %>%
  merge(AreaMicro, by = "Microrregiao") %>%
  mutate(Densidade = round(Populacao / AreaTerritorial, digits = 2)) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c(
      "Microrregião",
      "População (hab)",
      "Área (km²)",
      "Densidade Populacional (hab/km²)"
    ),
    caption = paste("Densidade Populacional da Mesorregião de", LocRef$Mesorregiao)
  ) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  footnote(
    general_title = "Fonte:",
    general = "Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.",
    footnote_as_chunk = T,
    threeparttable = T
  )