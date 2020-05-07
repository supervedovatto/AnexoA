load(file = "Dados/POCV.RData")

tabela <- EmpregoRAIS %>% 
  filter(!is.na(Vagas) & RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= max(EmpregoRAIS$Ano)-5  & Setor != "TotalEmpregosRAIS") %>% 
  reshape2::dcast(formula = Setor~Ano,fun.aggregate = sum,value.var = "Vagas")
  