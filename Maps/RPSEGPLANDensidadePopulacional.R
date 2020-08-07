SEGPLAN.Areas <- Area %>%
  filter(Ano == max(Area$Ano) & RPSEGPLAN == LocRef$RPSEGPLAN) %>%
  group_by(Localidade) %>%
  summarise(Area = sum(AreaTerritorial))

Dados <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(Ano == DataRef & RPSEGPLAN == LocRef$RPSEGPLAN) %>%
  group_by(Localidade) %>%
  summarise(Populacao = sum(Quantidade)) %>%
  arrange(desc(Localidade)) %>%
  mutate(freq = Populacao / sum(Populacao)) %>%
  merge(SEGPLAN.Areas, by = "Localidade") %>%
  mutate(Densidade = round(Populacao / Area, digits = 2)) %>%
  select(Localidade, Densidade)
  
MapaGoias %>% 
  merge(Dados) %>% 
  ggplot() +
    geom_sf(aes(fill = Densidade), size = 0.5) +
    geom_sf_label(aes(label = Localidade),
                  label.padding = unit(0.5, "mm"),
                  size = 2) +
    theme_bw() +
    scale_fill_viridis_c() +
    theme(
      title = element_text(size = 8),
      legend.position = c(0.99, 0.99),
      legend.justification = c("right", "top"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    ) +
    labs(
      caption = "Fonte: Elaborado pelo OMT/GYN com dados do BDE/IMB. Acesso: 19/03/2020.",
      subtitle = paste("Densidade Populacional na Região de Planejamento ",LocRef$RPSEGPLAN," (",lubridate::year(DataRef),")",sep=""),
      fill = "Densidade (hab/km²)"
    )