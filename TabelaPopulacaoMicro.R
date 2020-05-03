load(file = "Dados/POCV.RData")

AreaMicro <- Area %>% 
  filter(Ano == max(Area$Ano) & Mesorregiao == LocRef$Mesorregiao) %>% 
  group_by(Microrregiao) %>% 
  summarise(AreaTerritorial = sum(AreaTerritorial))

PopulacaoProjecao %>% 
  filter(Mesorregiao == LocRef$Mesorregiao,Ano == AnoRef) %>% 
  group_by(Microrregiao) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  merge(AreaMicro,by = "Microrregiao") %>% 
  mutate(Densidade = round(Populacao/AreaTerritorial,digits = 2)) %>% 
  kable(col.names = c("Microrregião","População (hab)","Área (km²)","Densidade Populacional (hab/km²)"),caption = paste("População, área e densidade populacional da Mesorregião",LocRef$Mesorregiao))
rm(AreaMicro)