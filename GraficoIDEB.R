load(file = "Dados/POCV.RData")

grafico <- IDEB %>% 
  filter(!is.na(IDEB) & Localidade == LocRef$Localidade & Ano >= "2011-01-01") %>% 
  select(Localidade, Ano, Anos, Rede, IDEB) %>% 
  ggplot(aes(x = Ano, y = IDEB)) + 
    geom_line(aes(color = Rede), size = 1) +
    scale_color_manual(values = mypallete) +
    theme_minimal() +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90),
          legend.title = element_blank())+
    facet_wrap(~Anos,scales = "free_y")