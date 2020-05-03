load(file = "Dados/POCV.RData")

LocRef <- RegioesGoias %>% 
  filter(Localidade == Municipio)

PopulacaoLocal <- PopulacaoProjecao %>%
    filter(Ano == AnoRef & Localidade == LocRef$Localidade) %>%
    summarise(Populacao = sum(Quantidade))

AreaLocal <- Area %>%
    filter(Ano == max(Area$Ano) & Localidade == LocRef$Localidade) %>%
    summarise(Area = sum(AreaTerritorial))