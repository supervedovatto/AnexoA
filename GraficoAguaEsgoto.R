load(file = "Dados/POCV.RData")

dados <- AguaEsgoto %>% 
  filter(Localidade == LocRef$Localidade & Ano >= 2005) %>% 
  transmute(Ano,Percentual = Percentual/100,Servico)

grafico <- dados %>% 
  ggplot(aes(x = Ano, y = Percentual)) + 
    geom_line(aes(color = Servico), size = 1) +
    scale_color_manual(values = mypallete) +
    scale_x_continuous(breaks = unique(dados$Ano)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
    theme_minimal() +
    theme(legend.position="bottom",axis.text.x = element_text(angle = 90))
