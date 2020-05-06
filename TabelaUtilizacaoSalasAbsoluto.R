load(file = "Dados/POCV.RData")

tabela <- EstabelecimentosESalas %>% 
  merge(RegioesGoias,by = "Localidade") %>% 
  filter(Microrregiao == LocRef$Microrregiao & Ano == max(EstabelecimentosESalas$Ano)) %>%
  select(Localidade,Rede,`Salas Existentes`) %>%
  filter(!is.na(`Salas Existentes`)) %>% 
  reshape2::dcast(Localidade ~ Rede,sum,value.var="Salas Existentes") %>% 
  mutate(Total = Federal + Estadual + Municipal + Particular) %>% 
  format(big.mark=".",decimal.mark=",",scientific=FALSE) %>%
  kable(col.names = c("Cidade","Federal","Estadual","Municipal","Particular","Total"),
        caption = paste("Salas de aulas disponíveis nas cidades da Microrregião de",LocRef$Microrregiao,"em",max(EstabelecimentosESalas$Ano)))