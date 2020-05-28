Dados <-  Taxas %>% 
  filter(Ano >= "2013-01-01") %>% 
  mutate(Valor = Valor/100)

grafico <- Dados %>% 
  ggplot(aes(x = factor(lubridate::year(Ano)), y = Valor)) + 
    geom_boxplot(color = mypallete[1]) +
    geom_point(data = filter(Dados,Localidade == LocRef$Localidade), 
              aes(x = factor(lubridate::year(Ano)), y = Valor, group = 1), color = mypallete[2], size = 2) +
    geom_line(data = filter(Dados,Localidade == LocRef$Localidade), 
            aes(x = factor(lubridate::year(Ano)), y = Valor, group = 1), color = mypallete[2], size = 1) +
    scale_color_manual(values = mypallete) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2),limits = c(0,NA)) +
    theme_bw() +
    labs(y = "Taxas",x="",
         caption = "Fonte: Elaborado pelo núcleo de base do OMT/GYN a partir de dados do BDE/IMB, com acesso em 19/03/2020.") +
    facet_wrap(~Taxa,ncol = 1,scales = "free_y")