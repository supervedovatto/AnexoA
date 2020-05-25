load(file = "Dados/POCV.RData")

dados <- PopulacaoProjecao %>% 
  group_by(Localidade,Ano) %>% 
  summarise(Populacao =  sum(Quantidade)) %>% 
  merge(Docentes,by = c("Localidade","Ano")) %>% 
  group_by(Localidade,Ano,Rede) %>%  
  summarise(DocHab =  Quantidade/Populacao*100000) %>% 
  filter(Localidade == LocRef$Localidade)

grafico <- dados %>% 
  ggplot(aes(x = Ano, y = DocHab)) + 
    geom_line(aes(color = Rede), size = 1) +
    scale_color_manual(values = mypallete) +
    labs(y = "Docentes para cada 100 mil habitantes",x="Ano") +
    scale_x_continuous(breaks = unique(dados$Ano)) +
    theme_minimal() +
    labs(caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
    theme(legend.position="bottom",axis.text.x = element_text(angle = 90))