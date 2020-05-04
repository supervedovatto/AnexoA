load(file = "Dados/POCV.RData")

LocalSEGPLAN <- RegioesGoias %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN) %>% 
  select(Localidade)

PopulacaoSEGPLAN <- PopulacaoProjecao %>% 
  filter(Ano == AnoRef & RPSEGPLAN == LocRef$RPSEGPLAN) %>% 
  summarise(Populacao = sum(Quantidade))

AreaSEGPLAN <- Area %>% 
  filter(Ano == max(Area$Ano) & RPSEGPLAN == LocRef$RPSEGPLAN) %>% 
  summarise(sum(AreaTerritorial))

RPSEGPLANPop <- PopulacaoProjecao %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN) %>% 
  group_by(Localidade) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  arrange(desc(Populacao))