RMP.Docentes <- Docentes %>% 
  filter(Rede != "Total" & !is.na(Quantidade) & Quantidade>0)

RMP.Matriculas <- Matriculas %>% 
  group_by(Localidade,Ano,Rede) %>% 
  summarise(Matriculas = sum(Total)) %>% 
  filter(!is.na(Matriculas) & Matriculas>0)

RMP <- merge(RMP.Docentes,RMP.Matriculas) %>% 
  mutate(RMP = Matriculas/Quantidade)
  
RMP.Local <- RMP %>% 
  filter(Localidade == "Goiânia" & Rede == "Estadual")

RMP.Local