SEGPLAN.Areas <- Area %>%
  filter(Ano == max(Area$Ano)) %>% 
  group_by(RPSEGPLAN) %>% 
  summarise(Area = sum(AreaTerritorial))

tabela <- PopulacaoProjecao %>% 
  merge(RegioesGoias) %>% 
  filter(Ano == DataRef) %>% 
  group_by(RPSEGPLAN) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  arrange(desc(RPSEGPLAN)) %>% 
  mutate(freq = Populacao/sum(Populacao),
         ypos = cumsum(freq)-freq/2) %>%
  select(RPSEGPLAN,Populacao) %>% 
  merge(SEGPLAN.Areas,by = "RPSEGPLAN") %>% 
  mutate(densidade = round(Populacao/Area,digits = 2))