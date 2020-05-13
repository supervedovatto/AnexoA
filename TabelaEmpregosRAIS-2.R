load(file = "Dados/POCV.RData")

tabela <- Emprego %>% 
  filter(!is.na(`Rendimento Médio`) & Localidade == LocRef$Localidade & Ano >= max(EmpregoRAIS$Ano)-4) %>% 
  select(Ano, Setor,`Rendimento Médio`) %>% 
  reshape2::dcast(formula = Setor~Ano,fun.aggregate = mean,value.var = "Rendimento Médio")