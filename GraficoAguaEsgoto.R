load(file = "Dados/POCV.RData")

grafico <- AguaEsgoto %>% 
  filter(Localidade == LocRef$Localidade & Ano >= 2005) %>% 
  ggplot(aes(x = Ano, y = Percentual)) + 
    geom_line(aes(color = Servico), size = 1) +
    scale_color_brewer(palette="Set1") +
    theme_minimal() +
    theme(legend.position="bottom")