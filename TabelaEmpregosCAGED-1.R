load(file = "Dados/POCV.RData")

tabela <- EmpregoCAGED %>% 
  filter(!is.na(Quantidade) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= max(EmpregoCAGED$Ano)-5) %>% 
  reshape2::dcast(formula = Setor~Situacao,value.var = "Quantidade",fun.aggregate = sum) %>% 
  mutate(Saldo = Admitidos - Desligados) %>% 
  adorn_totals() %>% 
  kable(caption = paste("Saldo da geração de empregos por setor entre",max(EmpregoCAGED$Ano)-5,"e",max(EmpregoCAGED$Ano),"na Região de Planejamento",LocRef$RPSEGPLAN,"segundo a CAGED"))