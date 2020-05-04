load(file = "Dados/POCV.RData")

SEGPLAN.Areas <- Area %>%
  filter(Ano == max(Area$Ano)) %>% 
  group_by(RPSEGPLAN) %>% 
  summarise(Area = sum(AreaTerritorial))

tabela <- PopulacaoProjecao %>% 
  filter(Ano == AnoRef) %>% 
  group_by(RPSEGPLAN) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  arrange(desc(RPSEGPLAN)) %>% 
  mutate(freq = Populacao/sum(Populacao)) %>%
  mutate(ypos = cumsum(freq)-freq/2) %>% 
  select(RPSEGPLAN,Populacao) %>% 
  merge(SEGPLAN.Areas,by = "RPSEGPLAN") %>% 
  mutate(densidade = round(Populacao/Area,digits = 2)) %>% 
  kable(col.names = c("Região de Planejamento","População (hab)","Área (km²)","Densidade Populacional (hab/km²)"),
        caption = "Densidade populacional em cada região de planejamento definida pela SEGPLAN")