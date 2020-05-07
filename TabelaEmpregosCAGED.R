load(file = "Dados/POCV.RData")

tabela <- EmpregoCAGED %>% 
  filter(!is.na(Quantidade) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= max(EmpregoCAGED$Ano)-5) %>% 
  reshape2::dcast(formula = Setor~Situacao,value.var = "Quantidade",fun.aggregate = sum) %>% 
  mutate(Saldo = Admitidos - Desligados)