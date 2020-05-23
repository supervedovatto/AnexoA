load(file = "Dados/POCV.RData")

LocRefMicro <- RegioesGoias %>% 
  filter(Microrregiao == LocRef$Microrregiao) %>% 
  select(Localidade)

PopulacaoMicro <- PopulacaoProjecao %>% 
  filter(Ano == AnoRef & Microrregiao == LocRef$Microrregiao) %>% 
  summarise(Populacao = sum(Quantidade))

AreaMicro <- Area %>%
  filter(Ano == max(Area$Ano) & Microrregiao == LocRef$Microrregiao) %>%
  select(AreaTerritorial) %>%
  summarise(sum(AreaTerritorial))

LocRefPop <- PopulacaoProjecao %>%
  filter(Microrregiao == LocRef$Microrregiao) %>%
  group_by(Localidade) %>%
  summarise(Populacao = sum(Quantidade)) %>%
  arrange(desc(Populacao))
