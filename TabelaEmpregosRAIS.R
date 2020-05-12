load(file = "Dados/POCV.RData")

tabela <- Emprego %>% 
  filter(!is.na(Empregos) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= max(EmpregoRAIS$Ano)-5) %>% 
  select(Ano, Setor, Empregos) %>% 
  reshape2::dcast(formula = Setor~Ano,fun.aggregate = sum,value.var = "Empregos")