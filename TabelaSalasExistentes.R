load(file = "Dados/POCV.RData")

tabela <- EstabelecimentosESalas %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(EstabelecimentosESalas$Ano)) %>%
  select(Localidade,Rede,`Salas Existentes`) %>%
  filter(!is.na(`Salas Existentes`)) %>% 
  reshape2::dcast(Localidade ~ Rede,sum,value.var="Salas Existentes") %>% 
  mutate(Total = Federal + Estadual + Municipal + Particular) %>% 
  adorn_totals() %>% 
  format(big.mark=".",decimal.mark=",",scientific=FALSE) %>%
  kable(col.names = c("Município","Federal","Estadual","Municipal","Particular","Total"),
        caption = paste("Salas de aulas disponíveis nos municípios na Região de Planejamento de",LocRef$RPSEGPLAN,"em",max(EstabelecimentosESalas$Ano)))