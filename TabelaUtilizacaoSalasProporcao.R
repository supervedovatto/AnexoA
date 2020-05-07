load(file = "Dados/POCV.RData")

tabela <- EstabelecimentosESalas %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(EstabelecimentosESalas$Ano)) %>%
  mutate(TaxaUso = 100*`Salas Utilizadas`/`Salas Existentes`) %>% 
  select(Localidade,Rede,TaxaUso) %>%
  filter(!is.na(TaxaUso)) %>% 
  reshape2::dcast(Localidade ~ Rede,sum,value.var="TaxaUso") %>% 
  format(big.mark=".",decimal.mark=",",scientific=FALSE,digits = 3)