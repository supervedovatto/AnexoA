load(file = "Dados/POCV.RData")

tabela <- Emprego %>% 
  filter(!is.na(Empregos) & Setor != "Atividade não especificada ou classificada" & Localidade == LocRef$Localidade & Ano >= max(EmpregoRAIS$Ano)-4) %>% 
  select(Ano, Setor, Empregos) %>% 
  reshape2::dcast(formula = Setor~Ano,fun.aggregate = sum,value.var = "Empregos")