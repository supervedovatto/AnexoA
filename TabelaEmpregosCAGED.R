load(file = "Dados/POCV.RData")

tabela <- Emprego %>% 
  filter(RPSEGPLAN == LocRef$RPSEGPLAN & Ano >= max(Emprego$Ano)-5) %>% 
  select(Setor, Admitidos, Desligados) %>% 
  group_by(Setor) %>% 
  summarise(Admitidos = sum(Admitidos,na.rm = TRUE),Desligados = sum(Desligados,na.rm = TRUE)) %>% 
  mutate(Saldo = Admitidos - Desligados)