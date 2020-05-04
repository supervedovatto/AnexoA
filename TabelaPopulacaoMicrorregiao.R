load(file = "Dados/POCV.RData")

Micro.Area <- Area %>%
  filter(Ano == max(Area$Ano) & Microrregiao == LocRef$Microrregiao) %>% 
  select(Localidade,AreaTerritorial)

tabela <- PopulacaoProjecao %>% 
  filter(Microrregiao == LocRef$Microrregiao) %>% 
  filter(Ano == AnoRef) %>%
  group_by(Localidade) %>% 
  summarise(Populacao = sum(Quantidade)) %>% 
  arrange(desc(Populacao)) %>%
  mutate(freq = Populacao/sum(Populacao)) %>% 
  select(Localidade,Populacao) %>% 
  merge(Micro.Area,by = "Localidade") %>% 
  mutate(densidade = round(Populacao/AreaTerritorial,digits = 2)) %>% 
  kable(col.names = c("Cidade",
                      "População (hab)",
                      "Área (km²)",
                      "Densidade Populacional (hab/km²)"),
        caption = paste("Densidade Populacional da Microrregião de",LocRef$Microrregiao))