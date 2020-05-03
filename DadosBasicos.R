#O banco de dados contendo todos os dados deve ser lido pelo script LeituraBancoDados.R para fins de atualização.
#É importante que a estrutura das tabelas não seja modificada em caso de futuras atualizações.
load(file = "Dados/POCV.RData")

LocRef <- RegioesGoias %>% 
  filter(Localidade == Municipio)

#Identifica a População Projetada para a população local da cidade
PopulacaoLocal <- PopulacaoProjecao %>%
    filter(Ano == AnoRef & Localidade == LocRef$Localidade) %>%
    summarise(Populacao = sum(Quantidade))

#Identifica a Area Local da cidade
AreaLocal <- Area %>%
    filter(Ano == max(Area$Ano) & Localidade == LocRef$Localidade) %>%
    summarise(Area = sum(AreaTerritorial))