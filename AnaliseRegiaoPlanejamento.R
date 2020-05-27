load(file = "Dados/POCV.RData")

LocalSEGPLAN <- RegioesGoias %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN) %>% 
  select(Localidade)

PopulacaoSEGPLAN <- PopulacaoProjecao %>% 
  merge(RegioesGoias) %>% 
  filter(Ano == AnoRef & RPSEGPLAN == LocRef$RPSEGPLAN) %>% 
  summarise(Populacao = sum(Quantidade))

AreaSEGPLAN <- Area %>% 
  merge(RegioesGoias) %>% 
  filter(Ano == max(Area$Ano) & RPSEGPLAN == LocRef$RPSEGPLAN) %>% 
  summarise(sum(AreaTerritorial))

RPSEGPLANPop <- PopulacaoProjecao %>% 
  merge(RegioesGoias) %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN) %>% 
  group_by(Localidade) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  arrange(desc(Populacao))