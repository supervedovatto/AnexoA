LocRefMeso <- RegioesGoias %>% 
  filter(Mesorregiao == LocRef$Mesorregiao) %>% 
  select(Localidade)

LocRefMicro <- RegioesGoias %>% 
  filter(Mesorregiao == LocRef$Mesorregiao) %>% 
  select(Microrregiao) %>% 
  unique()

PopulacaoMeso <- PopulacaoProjecao %>% 
  filter(Ano == AnoRef & Mesorregiao == LocRef$Mesorregiao) %>% 
  summarise(sum(Quantidade))

AreaMeso <- Area %>% 
  filter(Ano == max(Area$Ano) & Mesorregiao == LocRef$Mesorregiao) %>% 
  summarise(AreaTerritorial = sum(AreaTerritorial))

LocRefPop <- PopulacaoProjecao %>% 
  filter(Mesorregiao == LocRef$Mesorregiao,Ano == AnoRef) %>% 
  group_by(Localidade) %>% 
  summarise(Quantidade = sum(Quantidade)) %>% 
  arrange(desc(Quantidade))