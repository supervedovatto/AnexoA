load(file = "Dados/POCV.RData")

dados <- EstabelecimentosESalas %>% 
  filter(Localidade == LocRef$Localidade) %>%
  select(Ano,Rede,starts_with("Salas")) %>%
  mutate(TaxaUso = `Salas Utilizadas`/`Salas Existentes`) %>% 
  filter(!is.na(TaxaUso))

grafico <-  ggplot(data = dados,aes(x = Ano, y = TaxaUso)) + 
    geom_line(aes(color = Rede), size = 1) +
    scale_color_brewer(palette="Set1") +
    labs(y="Taxa de Utilização") +
    scale_x_continuous(breaks = unique(dados$Ano)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
    theme_minimal() +
    theme(legend.position="bottom",axis.text.x = element_text(angle = 90))