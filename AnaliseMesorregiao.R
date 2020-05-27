LocRefMeso <- RegioesGoias %>% 
  filter(Mesorregiao == LocRef$Mesorregiao) %>% 
  select(Localidade) %>% 
  unique()

LocRefMicro <- RegioesGoias %>% 
  filter(Mesorregiao == LocRef$Mesorregiao) %>% 
  select(Microrregiao) %>% 
  unique()

PopulacaoMeso <- PopulacaoProjecao %>% 
  merge(RegioesGoias) %>% 
  filter(Ano == max(PopulacaoProjecao$Ano) & Mesorregiao == LocRef$Mesorregiao) %>% 
  summarise(sum(Quantidade))

AreaMeso <- Area %>% 
  merge(RegioesGoias) %>% 
  filter(Ano == max(Area$Ano) & Mesorregiao == LocRef$Mesorregiao) %>% 
  summarise(AreaTerritorial = sum(AreaTerritorial))

LocRefPop <- PopulacaoProjecao %>% 
  merge(RegioesGoias) %>% 
  filter(Mesorregiao == LocRef$Mesorregiao & Ano == max(PopulacaoProjecao$Ano)) %>% 
  group_by(Localidade) %>% 
  summarise(Quantidade = sum(Quantidade)) %>% 
  arrange(desc(Quantidade))
