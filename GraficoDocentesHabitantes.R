load(file = "Dados/POCV.RData")

PopulacaoProjecao %>% 
  group_by(Localidade,Ano) %>% 
  summarise(Populacao =  sum(Quantidade)) %>% 
  merge(Docentes,by = c("Localidade","Ano")) %>% 
  group_by(Localidade,Ano,Rede) %>%  
  summarise(DocHab =  Quantidade/Populacao*100000) %>% 
  filter(Localidade == LocRef$Localidade) %>% 
  ggplot(aes(x = Ano, y = DocHab)) + 
    geom_line(aes(color = Rede), size = 1) +
    scale_color_brewer(palette="Set1") +
    labs(y = "Docentes para cada 100 mil habitantes",x="Faixa etária") +
    theme_minimal() +
    theme(legend.position="bottom")