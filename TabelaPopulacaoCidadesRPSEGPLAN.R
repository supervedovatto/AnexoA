load(file = "Dados/POCV.RData")

SEGPLAN.Areas <- Area %>%
  filter(Ano == max(Area$Ano) & RPSEGPLAN == LocRef$RPSEGPLAN) %>% 
  group_by(Localidade) %>% 
  summarise(Area = sum(AreaTerritorial))

tabela <- PopulacaoProjecao %>% 
  filter(Ano == AnoRef & RPSEGPLAN == LocRef$RPSEGPLAN) %>% 
  group_by(Localidade) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  arrange(desc(Localidade)) %>% 
  mutate(freq = Populacao/sum(Populacao)) %>%
  merge(SEGPLAN.Areas,by = "Localidade") %>% 
  mutate(densidade = round(Populacao/Area,digits = 2)) %>% 
  select(Localidade,Populacao,Area,densidade)