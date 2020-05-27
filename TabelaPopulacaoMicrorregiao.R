Micro.Area <- Area %>%
  merge(RegioesGoias) %>% 
  filter(Ano == max(Area$Ano) & Microrregiao == LocRef$Microrregiao) %>% 
  select(Localidade,AreaTerritorial)

tabela <- PopulacaoProjecao %>% 
  merge(RegioesGoias) %>% 
  filter(Microrregiao == LocRef$Microrregiao & Ano == DataRef) %>% 
  group_by(Localidade) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  arrange(desc(Populacao)) %>%
  mutate(freq = Populacao/sum(Populacao)) %>% 
  select(Localidade,Populacao) %>% 
  merge(Micro.Area) %>% 
  mutate(densidade = round(Populacao/AreaTerritorial,digits = 2))