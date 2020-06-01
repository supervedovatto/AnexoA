LocRefMicro <- RegioesGoias %>%
  filter(Microrregiao == LocRef$Microrregiao) %>%
  select(Localidade)

PopulacaoMicro <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(Ano == DataRef & Microrregiao == LocRef$Microrregiao) %>%
  summarise(Populacao = sum(Quantidade))

AreaMicro <- Area %>%
  filter(Ano == max(Area$Ano) &
           Microrregiao == LocRef$Microrregiao) %>%
  select(AreaTerritorial) %>%
  summarise(Total = sum(AreaTerritorial))

LocRefPop <- PopulacaoProjecao %>%
  merge(RegioesGoias) %>%
  filter(Microrregiao == LocRef$Microrregiao) %>%
  group_by(Localidade) %>%
  summarise(Populacao = sum(Quantidade)) %>%
  arrange(desc(Populacao))