tabela <- SalasAula %>% 
  merge(RegioesGoias) %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN & Ano == max(SalasAula$Ano) & Situacao == "Existentes") %>%
  select(Localidade,Rede,Total) %>%
  spread(Rede,Total) %>% 
  replace_na(list(Federal = 0,Particular = 0)) %>% 
  mutate(Total = Estadual + Particular + Federal + Municipal) %>% 
  adorn_totals() %>% 
  format(big.mark=".",decimal.mark=",",scientific=FALSE)