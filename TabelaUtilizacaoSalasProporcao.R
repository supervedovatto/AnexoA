tabela <- SalasAula %>% 
  merge(RegioesGoias) %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(SalasAula$Ano)) %>%
  spread(Situacao,Total) %>% 
  mutate(TaxaUso = 100*Utilizadas/Existentes) %>% 
  select(Localidade,Rede,TaxaUso) %>% 
  spread(Rede,TaxaUso) %>% 
  replace_na(list(Federal = 0,Particular = 0)) %>% 
  format(big.mark=".",decimal.mark=",",scientific=FALSE,digits = 3)