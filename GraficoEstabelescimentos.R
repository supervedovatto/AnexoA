load(file = "Dados/POCV.RData")

grafico <- EstabelecimentosESalas %>% 
  filter(Localidade == LocRef$Localidade) %>%
  select(-starts_with("Salas"),-starts_with("Localidade")) %>% 
    ggplot(aes(x = Ano, y = Estabelecimentos)) + 
    geom_line(aes(color = Rede), size = 1) +
    scale_color_brewer(palette="Set1") +
    theme_minimal() +
    theme(legend.position="bottom")