Dados <- IDEB %>% 
  filter(!is.na(Valor) & Ano >= "2011-01-01" & Rede != "Federal") %>% 
  select(Localidade, Ano, Anos, Rede, Valor)

Dados %>% 
  ggplot(aes(x = factor(lubridate::year(Ano)), y = Valor)) + 
  geom_boxplot(color = mypallete[1]) + 
  geom_point(data = filter(Dados,Localidade == LocRef$Localidade), 
                                                 aes(x = factor(lubridate::year(Ano)), y = Valor, group = 1), color = mypallete[2], size = 2) +
  geom_line(data = filter(Dados,Localidade == LocRef$Localidade), 
            aes(x = factor(lubridate::year(Ano)), y = Valor, group = 1), color = mypallete[2], size = 1) +
  scale_color_manual(values = mypallete) +
  theme_bw() +
  labs(y = "IDEB",x="",
       caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
  facet_grid(cols = vars(Anos), rows = vars(Rede))